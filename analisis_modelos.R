library(bayesplot)
library(rstan)
library(dplyr)
library(ggplot2)
library(readr)

predice <- function(modelo){
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
    muertes =  x$murders
  )
  return(y)
}

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

# Modelo dos --------------------------------------------------------------


modelo_dos <- readRDS(here::here("Resultados/modelo_dos.rds"))

predice(modelo_dos) %>% 
  mutate(
    rmse = (muertes - mediana)^2,
    mape = abs(muertes - mediana)
  ) %>% 
  summarise_at(
    vars(c(rmse, mape)),
    mean
  )

beta0    <- extract(modelo_dos, pars = "beta0")$beta0 

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
    title    = "Probabilidad de asesinato",
    subtitle = "Efectos por estado"
  )

theta    <- extract(modelo_dos, pars = "theta")$theta

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
    title    = "Probabilidad de asesinato",
    subtitle = "Efectos por división"
  )


# Modelo Tres -------------------------------------------------------------

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


# Modelo cuatro -----------------------------------------------------------

modelo_cuatro <- readRDS(
  here::here("Resultados/modelo_cuatro.rds")
)

predice(modelo_cuatro) %>% 
  mutate(
    rmse = (muertes - mediana)^2,
    mape = abs(muertes - mediana)
  ) %>% 
  summarise_at(
    vars(c(rmse, mape)),
    mean
  )

beta0    <- extract(modelo_cuatro, pars = "beta0")$beta0 

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

theta    <- extract(modelo_cuatro, pars = "theta")$theta

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

beta <- extract(modelo_cuatro, pars = "beta[")
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


