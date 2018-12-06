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
  select(
    State,
    murders,
    pop,
    Division,
    pctBlack,
    # pctWhite,
    pctPoverty,
    # pct12.17w2Par,
    pctNotSpeakEng,
    pctBornStateResid,
    pctNotHSgrad,
    pctWorkMom.18,
    pctFgnImmig.10
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

xcov <- x %>%
  select(-c(State, murders, pop, Division)) %>%
  mutate_all(function(x) x /100) %>%
  as.matrix

# Segundo modelo ----------------------------------------------------------

segundo_modelo <- stan_model(
  here::here("Modelos Stan/modelo2.stan")
  )

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
  chains = 4,
  thin = 2,
  control = list(
    adapt_delta = 0.85
  )
)

saveRDS(
  modelo_dos,
  file = here::here("Resultados/modelo_dos.rds")
)


# Tercer modelo -----------------------------------------------------------

tercer_modelo <- stan_model(
  here::here("Modelos Stan/modelo3.stan")
  )

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
    P = ncol(xcov),
    X = as.matrix(xcov)
  ),
  warmup = 500,
  iter = 3000,
  seed = 123456,
  chains = 4,
  thin = 4,
  control = list(
    adapt_delta = 0.85
  )
)

saveRDS(
  modelo_tres,
  file = here::here("Resultados/modelo_tres.rds")
)

# Cuarto Modelo -----------------------------------------------------------

cuarto_modelo <- stan_model(
  here::here("Modelos Stan/modelo4.stan")
)

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
    P = ncol(xcov),
    X = as.matrix(xcov)
  ),
  warmup = 500,
  iter = 3000,
  seed = 123456,
  chains = 4,
  thin = 4,
  control = list(
    adapt_delta = 0.85
  )
)

saveRDS(
  modelo_cuatro,
  file = here::here("Resultados/modelo_cuatro.rds")
)
