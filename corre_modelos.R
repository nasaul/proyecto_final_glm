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

cov <- x %>% 
  select(-c(State, murders, pop, Division)) %>% 
  mutate_all(function(x) x /100) %>% 
  as.matrix 

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
  warmup = 1000,
  iter = 4000,
  seed = 12345,
  chains = 4,
  control = list(
    adapt_delta = 0.99
  )
)

saveRDS(
  modelo_cuatro,
  file = here::here("Resultados/modelo_cuatro.rds")
)
