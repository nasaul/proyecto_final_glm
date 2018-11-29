library(dplyr)
library(readr)
library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)


df <- read_csv(
  "https://archive.ics.uci.edu/ml/machine-learning-databases/00211/CommViolPredUnnormalizedData.txt",
  col_names = FALSE,
  na = "?"
)

names(df) <- read_table(
  here::here("nombres.txt"),
  col_names = FALSE
) %>% 
  mutate(
    var_names = gsub(
      "(.*) (.*)", 
      "\\1",
      X2
    )
  ) %>% 
  pull(var_names) %>% 
  make.names()

x <- df %>% 
  mutate(
    State = State %>% as.factor %>% as.numeric
  ) %>% 
  select(State, murders, pop, communityname) %>% 
  na.omit()


# Primer Modelo -----------------------------------------------------------

primer_modelo <- stan_model(
  here::here("modelo_poisson.stan")
)


z <- sampling(
  primer_modelo,
  list(
    N = nrow(x),
    y = x$murders,
    n = x$pop,
    state = x$State,
    state_no = length(unique(x$State))
  ),
  chains = 1,
  iter = 2000,
  warmup = 500, 
  control = list(
    adapt_delta = .9
  )
)

prediction <- rstan::extract(z, "yn")$yn %>% 
  as_tibble() %>% 
  summarise_all(median) %>% 
  t()

x %>% 
  mutate(
    prediccion = prediction,
    mape   = abs(murders - prediccion),
    ecm = (murders - prediccion) ^ 2
  ) %>% 
  summarise(mape = mean(mape), ecm = mean(ecm))

# Segundo Modelo ----------------------------------------------------------

stan_glmer(
  formula = murders ~ (1 | communityname | State),
  data = x,
  family = binomial,
  offset = pop
)


segundo_modelo <- stan_model(here::here("modelo_2.stan"))

samp_2domodelo <- sampling(
  segundo_modelo,
  data = list(
    N = nrow(x),
    y = x$murders,
    n = x$pop,
    state = x$State,
    state_no = length(unique(x$State))
  ),
  chains = 1,
  iter = 1000,
  warmup = 100
)

ypred <- rstan::extract(samp_2domodelo, "yn")$yn %>% 
  as_tibble() %>% 
  summarise_all(median) %>% 
  t() %>% 
  as.vector()

x %>% 
  mutate(
    prediccion = ypred,
    mape   = abs(murders - prediccion),
    ecm = (murders - prediccion) ^2
  ) %>% 
  summarise(mape = mean(mape), ecm = mean(ecm))

x %>% 
  mutate(prediccion = ypred) %>% 
  ggplot(aes(x = murders, y = prediccion)) +
  geom_point() +
  geom_line(aes(x = 1:2215, y = 1:2215))

bayesplot::mcmc_areas(
  x = as.array(samp_2domodelo),
  pars= paste("theta[",1:48,"]", sep = "")
  )

