# Funciones ayuda ---------------------------------------------------------

predice <- function(modelo, nombre, liga){
  y <- tibble(
    media = extract(modelo, "yn")$yn %>% 
      apply(
        MARGIN = 2, 
        FUN    = mean
      ),
    mediana = extract(modelo, "yn")$yn %>% 
      apply(
        MARGIN = 2, 
        FUN    = median
      ),
    modelo  = nombre,
    liga    = liga,
    muertes =  x$murders
  )
  return(y)
}

# calculateDIC <-  function(y, theta_post, llFun) {
#   #Calculate L
#   theta_hat = apply(theta_post, 2, mean)
#   L = llFun(y, theta_hat)
#   
#   #Calculate P
#   S = nrow(theta_post) #S = number of iterations
#   #Add up the log likelihoods of each iteration
#   llSum = 0
#   for (s in 1:S) {
#     theta_s = as.numeric(theta_post[s,])
#     llSum = llSum + llFun(y, theta_s)
#   }
#   P = 2 * (L - (1 / S * llSum))
#   
#   #Calculate DIC
#   DIC = -2 * (L - P)
#   
#   #Return the results
#   list(DIC=DIC, P=P, L=L)
# }
# 
# likelihood_func <- function(y, theta, modelo, liga){
#   x <- theta[as.numeric(x$Division)]
#   if(liga == "logit"){
#     y <- log(x/(1 - x))
#   } else if(liga == "probit"){
#     y <- 
#   } else if(liga == "loglog"){
#     y <- exp(-exp(x))
#   } else if(liga == "cloglog"){
#     y <- 1 - exp(-exp(x))
#   }
# } 

# logit_likelihood <- function(y, theta){
#   x <- theta[1] + theta[2] * df$x50 + theta[3] * df$x100 +
#     theta[4] * df$x200 + theta[5] * df$x500
#   z <-  exp(x) / (1 + exp(x))
#   sum(dbinom(y, size = df$C, prob = z, log = TRUE))
# }
# 
# 
# cloglog_likelihood <- function(y, theta){
#   x <- theta[1] + theta[2] * df$x50 + theta[3] * df$x100 +
#     theta[4] * df$x200 + theta[5] * df$x500
#   z <-  1 - exp(-exp(x))
#   sum(dbinom(y, size = df$C, prob = z, log = TRUE))
# }

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
  here::here("Datos/nombres.txt"),
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

estados <- read_csv(
  here::here("Datos/estados_regiones")
) %>% 
  select(`State Code`, Division) %>% 
  rename(State = `State Code`)


x <- df %>% 
  left_join(estados, by = "State") %>% 
  mutate(
    State = State %>% as.factor,
    Division = Division %>% as.factor
  ) %>% 
  select(State, murders, pop, Division) %>% 
  na.omit()

# Modelo Stan -------------------------------------------------------------

intercepto_variante <- stan_model(
  here::here("Modelos Stan/modelo1.stan")
)

# Distribución Binomial ---------------------------------------------------

bin_logit <- sampling(
  intercepto_variante,
  data = list(
    N            = nrow(x),
    y            = x$murders,
    n            = x$pop,
    division     = as.numeric(x$Division),
    division_no  = 9L,
    liga         = 1L, # Liga Logit
    distribucion = 1L  # Distribución binomial
  ),
  warmup = 500,
	seed = 984984 
)

bin_probit <- sampling(
  intercepto_variante,
  data = list(
    N            = nrow(x),
    y            = x$murders,
    n            = x$pop,
    division     = as.numeric(x$Division),
    division_no  = 9L,
    liga         = 2L, # Liga Probit
    distribucion = 1L  # Distribución binomial
  ),
  warmup = 500,
	seed = 984984 
)

bin_cloglog <- sampling(
  intercepto_variante,
  data = list(
    N            = nrow(x),
    y            = x$murders,
    n            = x$pop,
    division     = as.numeric(x$Division),
    division_no  = 9L,
    liga         = 3L, # Liga Complementaria log-log
    distribucion = 1L  # Distribución binomial
  ),
  warmup = 500,
	seed = 984984 
)

bin_loglog <- sampling(
  intercepto_variante,
  data = list(
    N            = nrow(x),
    y            = x$murders,
    n            = x$pop,
    division     = as.numeric(x$Division),
    division_no  = 9L,
    liga         = 4L, # Liga log-log
    distribucion = 1L  # Distribución binomial
  ),
  warmup = 500,
	seed = 984984 
)

# Distribución Poisson ----------------------------------------------------

pois <- sampling(
  intercepto_variante,
  data = list(
    N            = nrow(x),
    y            = x$murders,
    n            = x$pop,
    division     = as.numeric(x$Division),
    division_no  = 9L,
    liga         = 5L, # Liga Exponencial
    distribucion = 2L  # Distribución poisson
  ),
  warmup = 500,
	seed = 984984 
)

# Comparación de modelos --------------------------------------------------

predicciones <- predice(
  bin_probit,
  "Binomial",
  "Probit"
) %>%
  full_join(
    predice(bin_logit, "Binomial", "Logit"),
    by = c("media", "mediana", "modelo", "liga", "muertes")
  ) %>% 
  full_join(
    predice(bin_loglog, "Binomial", "Log-Log"),
    by = c("media", "mediana", "modelo", "liga", "muertes")
  ) %>% 
  full_join(
    predice(bin_cloglog, "Binomial", "CLog-Log"),
    by = c("media", "mediana", "modelo", "liga", "muertes")
  ) %>% 
  full_join(
    predice(pois, "Poisson", "Exponencial"),
    by = c("media", "mediana", "modelo", "liga", "muertes")
  )

predicciones %>% 
  ggplot(
    aes(
      x = muertes,
      y = mediana,
      colour = liga
    )
  ) +
  geom_point() +
  facet_wrap(liga~modelo)

predicciones %>% 
  mutate(
    rmse = (muertes - mediana)^2,
    mape = abs(muertes - mediana)
  ) %>% 
  group_by(modelo, liga) %>% 
  summarise_at(vars(c(rmse, mape)), mean) %>% View

