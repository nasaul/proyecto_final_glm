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

# Lectura de datos --------------------------------------------------------
library(dplyr)
library(readr)
library(rstan)
library(bayesplot)
library(loo)
library(purrr)
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

performance <- tibble(
  modelo = c(
    bin_cloglog,
    bin_logit, 
    bin_loglog,
    bin_probit,
    pois
  )
) %>% 
  mutate(
    nombre   = c(rep("Binomial", 4), "Poisson"),
    liga     = c("Cloglog", "Logit", "Loglog", "Probit", "Exponencial"),
    log_lik  = map(modelo, extract_log_lik),
    waic_obj = map(log_lik, waic),
    waic     = map(waic_obj, ~.$waic) %>% flatten_dbl()
  ) %>% 
  select(
    nombre, liga, waic
  ) 

saveRDS(
  performance,
  file = here::here("Resultados/desempeño_primeros_modelos.rds")
)


