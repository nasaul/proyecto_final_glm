# Lectura de datos --------------------------------------------------------
library(dplyr)
library(readr)
library(rstan)
library(bayesplot)
rstan_options(auto_write = TRUE)
options(mc.cores = parallel::detectCores())

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

estados <- read_csv("estados_regiones") %>% 
  select(`State Code`, Division) %>% 
  rename(State = `State Code`)


x <- df %>% 
  left_join(estados, by = "State") %>% 
  mutate(
    State = State %>% as.factor,
    Division = Division %>% as.factor,
  ) %>% 
  select(
    State,
    murders,
    pop,
    Division,
    pctBlack,
    pctWhite,
    pctPoverty,
    pct12.17w2Par,
    pctNotSpeakEng,
    pctBornStateResid,
    # pctPolicWhite,
    # pctPolicBlack,
    # officDrugUnits
  ) %>% 
  na.omit()

division <- x %>% 
  group_by(State, Division) %>% 
  summarise() %>% 
  ungroup %>% 
  mutate(
      State = as.numeric(State),
      Division = as.numeric(Division)
  ) %>% 
  pull(Division)

modelo_tres <- readRDS(here::here("Resultados/modelo_tres.rds"))

predice(modelo_tres) %>% 
  mutate(
    rmse = (muertes - mediana)^2,
    mape = abs(muertes - mediana)
  ) %>% 
  summarise_at(
    vars(c(rmse, mape)),
    mean
  )

beta0    <- extract(modelo_tres, pars = "beta0")$beta0 

beta0_df <- tibble(
  media   = apply(beta0, MARGIN = 2, FUN = mean),
  mediana = apply(beta0, MARGIN = 2, FUN = median),
  int_baj = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0, MARGIN = 2, FUN = min),
  ymax    = apply(beta0, MARGIN = 2, FUN = max),
  state   = levels(x$State)
) %>% 
  mutate_if(
    is.numeric,
    function(x){
      1 - exp(-exp(x))
    }
  )

ggplot(
  data = beta0_df,
  aes(
    x = state
  )
) +
  geom_boxplot(
    aes(
      ymin   = ymin,
      lower  = int_baj,
      middle = media,
      upper  = int_al,
      ymax   = ymax
    ),
    stat = "identity"
  ) + 
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Probabilidad",
    y        = "Estado",
    title    = "Probabilidad base de asesinato",
    subtitle = "Efectos por estado"
  )

theta    <- extract(modelo_tres, pars = "theta")$theta

theta_df <- tibble(
  media   = apply(theta, MARGIN = 2, FUN = mean),
  mediana = apply(theta, MARGIN = 2, FUN = median),
  int_baj = apply(theta, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(theta, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(theta, MARGIN = 2, FUN = min),
  ymax    = apply(theta, MARGIN = 2, FUN = max),
  div     = levels(x$Division)
) %>% 
  mutate_if(
    is.numeric,
    function(x){
      1 - exp(-exp(x))
    }
  )

ggplot(
  data = theta_df,
  aes(
    x = div
  )
) +
  geom_boxplot(
    aes(
      ymin   = ymin,
      lower  = int_baj,
      middle = media,
      upper  = int_al,
      ymax   = ymax
    ),
    stat = "identity"
  ) + 
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Probabilidad",
    y        = "Estado",
    title    = "Probabilidad base de asesinato",
    subtitle = "Efectos por división"
  )

beta <- extract(modelo_tres, pars = "beta")$beta
cov_names <- x %>% 
  select(-c(State, murders, pop, Division)) %>% 
  names
beta_df <- tibble(
  media   = apply(beta, MARGIN = 2, FUN = mean),
  mediana = apply(beta, MARGIN = 2, FUN = median),
  int_baj = apply(beta, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta, MARGIN = 2, FUN = min),
  ymax    = apply(beta, MARGIN = 2, FUN = max),
  var     = cov_names
) 

ggplot(
  data = beta_df,
  aes(
    x = var
  )
) +
  geom_boxplot(
    aes(
      ymin   = ymin,
      lower  = int_baj,
      middle = media,
      upper  = int_al,
      ymax   = ymax
    ),
    stat = "identity"
  ) + 
  coord_flip() +
  theme_bw() +
  labs(
    title = "Parámetros asociados a cada variable",
    x = ""
  )


# Segundo modelo ----------------------------------------------------------

segundo_modelo <- stan_model(here::here("modelo_2.stan"))

modelo_dos <- sampling(
  segundo_modelo,
  list(
    N           = nrow(x),
    y           = x$murders,
    n           = x$pop,
    L           = 48,
    state       = as.numeric(x$State),
    division    = division,
    division_no = 9
  ),
  warmup = 500,
  seed = 123456,
  chains = 4
)

saveRDS(
  modelo_dos,
  file = here::here("Resultados/modelo_dos.rds")
)


# Tercer modelo -----------------------------------------------------------

tercer_modelo <- stan_model(here::here("modelo_3.stan"))

modelo_tres <- sampling(
  tercer_modelo,
  list(
    N           = nrow(x),
    y           = x$murders,
    n           = x$pop,
    L           = 48,
    state       = as.numeric(x$State),
    division    = division,
    division_no = 9,
    P = ncol(cov),
    X = as.matrix(cov)
  ),
  warmup = 500,
  iter = 2000,
  seed = 12345,
  chains = 4,
  control = list(
    adapt_delta = 0.85
  )
)

saveRDS(
  modelo_tres,
  file = here::here("Resultados/modelo_tres.rds")
)

# Cuarto Modelo -----------------------------------------------------------

cuarto_modelo <- stan_model(here::here("modelo_4.stan"))

modelo_cuatro <- sampling(
  cuarto_modelo,
  list(
    N           = nrow(x),
    y           = x$murders,
    n           = x$pop,
    L           = 48L,
    state       = as.numeric(x$State),
    division    = division,
    division_no = 9L,
    P = ncol(cov),
    X = as.matrix(cov)
  ),
  warmup = 500,
  iter = 2000,
  seed = 12345,
  chains = 4
  # control = list(
  #   adapt_delta = 0.9999
  # )
  # algorithm = "HMC"
)
modelo_cuatro

saveRDS(
  modelo_cuatro,
  file = here::here("Resultados/modelo_cuatro.rds")
)
