library(bayesplot)
library(rstan)
library(dplyr)
library(ggplot2)
library(readr)
library(loo)
library(purrr)

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

# Modelo dos --------------------------------------------------------------


modelo_dos <- readRDS(
  here::here("Resultados/modelo_dos.rds")
)

predice(modelo_dos) %>%
  ggplot(aes(x = muertes, y = mediana)) +
  geom_point() +
  geom_line(aes(x = seq(1, 2000, length.out = 2215), y = seq(1, 2000, length.out = 2215)))

modelo_dos %>%
  extract_log_lik() %>%
  waic() %>%
  .$waic

beta0    <- extract(modelo_dos, pars = "beta0")$beta0

beta0_df <- tibble(
  media   = apply(beta0, MARGIN = 2, FUN = mean),
  mediana = apply(beta0, MARGIN = 2, FUN = median),
  int_baj = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0, MARGIN = 2, FUN = min),
  ymax    = apply(beta0, MARGIN = 2, FUN = max),
  state   = levels(x$State)
)

ggplot(
  data = beta0_df,
  aes(
    x = state
  )
) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    )
  ) +
  geom_point(aes(y = mediana)) +
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
)

phi <- extract(modelo_dos, pars = "phi_param")$phi_param
phi_df <- tibble(
  media = mean(phi),
  mediana = median(phi),
  int_baj = quantile(phi, probs = 0.025),
  int_al  = quantile(phi, probs = 0.975),
  div = "EUA"
)

ggplot(
  data = theta_df %>%
    full_join(phi_df),
  aes(
    x = div
  )
) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    )
  ) +
  geom_point(aes(y = mediana)) +
  geom_hline(aes(yintercept = phi_df$int_baj), colour = "blue") +
  geom_hline(aes(yintercept = phi_df$int_al), colour = "blue") +
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Probabilidad",
    y        = "Estado",
    title    = "Probabilidad de asesinato",
    subtitle = "Efectos por división"
  )


# Modelo Tres -------------------------------------------------------------

modelo_tres <- readRDS(
  here::here("Resultados/modelo_tres.rds")
)


predice(modelo_tres) %>%
  ggplot(aes(x = muertes, y = mediana)) +
  geom_point() +
  geom_line(aes(x = seq(1, 2000, length.out = 2215), y = seq(1, 2000, length.out = 2215)))

modelo_tres %>%
  extract_log_lik() %>%
  waic() %>%
  .$waic

beta0    <- extract(modelo_tres, pars = "beta0")$beta0

beta0_df <- tibble(
  media   = apply(beta0, MARGIN = 2, FUN = mean),
  mediana = apply(beta0, MARGIN = 2, FUN = median),
  int_baj = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0, MARGIN = 2, FUN = min),
  ymax    = apply(beta0, MARGIN = 2, FUN = max),
  state   = levels(x$State)
)

ggplot(
  data = beta0_df,
  aes(
    x = state
  )
) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    )
  ) +
  geom_point(aes(y = mediana)) +
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
)

phi <- extract(modelo_tres, pars = "phi_param")$phi_param
phi_df <- tibble(
  media = mean(phi),
  mediana = median(phi),
  int_baj = quantile(phi, probs = 0.025),
  int_al  = quantile(phi, probs = 0.975),
  div = "EUA"
)

ggplot(
  data = theta_df %>%
    full_join(phi_df),
  aes(
    x = div
  )

) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    )
  ) +
  geom_point(aes(y = mediana)) +
  geom_hline(aes(yintercept = phi_df$int_baj), colour = "blue") +
  geom_hline(aes(yintercept = phi_df$int_al), colour = "blue") +
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
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    ),
    stat = "identity"
  ) +
  geom_point(aes(y = mediana)) +
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
  ggplot(aes(x = muertes, y = mediana)) +
  geom_point() +
  geom_line(
    aes(
      x = seq(1, 2000, length.out = 2215),
      y = seq(1, 2000, length.out = 2215)
    )
  )

modelo_cuatro %>%
  extract_log_lik() %>%
  waic() %>%
  .$waic

beta0    <- extract(modelo_cuatro, pars = "beta0")$beta0

beta0_df <- tibble(
  media   = apply(beta0, MARGIN = 2, FUN = mean),
  mediana = apply(beta0, MARGIN = 2, FUN = median),
  int_baj = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0, MARGIN = 2, FUN = min),
  ymax    = apply(beta0, MARGIN = 2, FUN = max),
  state   = levels(x$State)
)

ggplot(
  data = beta0_df,
  aes(
    x = state
  )
) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    ),
    stat = "identity"
  ) +
  geom_point(aes(y = mediana)) +
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Probabilidad",
    y        = "Estado",
    title    = "Probabilidad base de asesinato",
    subtitle = "Efectos por estado"
  )

theta0    <- extract(modelo_cuatro, pars = "theta0")$theta0

theta0_df <- tibble(
  media   = apply(theta, MARGIN = 2, FUN = mean),
  mediana = apply(theta, MARGIN = 2, FUN = median),
  int_baj = apply(theta, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(theta, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(theta, MARGIN = 2, FUN = min),
  ymax    = apply(theta, MARGIN = 2, FUN = max),
  div     = levels(x$Division)
)

phi <- extract(modelo_cuatro, pars = "phi_param")$phi_param
phi_df <- tibble(
  media = mean(phi),
  mediana = median(phi),
  int_baj = quantile(phi, probs = 0.025),
  int_al  = quantile(phi, probs = 0.975),
  div = "EUA"
)

ggplot(
  data = theta_df %>%
    full_join(phi_df),
  aes(
    x = div
  )
) +
  geom_errorbar(
    aes(
      ymin   = int_baj,
      ymax   = int_al
    ),
    stat = "identity"
  ) +
  geom_point(aes(y = mediana)) +
  geom_hline(aes(yintercept = phi_df$int_baj), colour = "blue") +
  geom_hline(aes(yintercept = phi_df$int_al), colour = "blue") +
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Probabilidad",
    y        = "Estado",
    title    = "Probabilidad base de asesinato",
    subtitle = "Efectos por división"
  )

beta <- extract(modelo_cuatro, pars = "beta")$beta
cov_names <- x %>%
  select(-c(State, murders, pop, Division)) %>%
  names

map(
  1:7,
  function(i){
    beta_df <- tibble(
      media   = apply(beta[, , i], MARGIN = 2, FUN = mean),
      mediana = apply(beta[, , i], MARGIN = 2, FUN = median),
      int_baj = apply(beta[, , i], MARGIN = 2, FUN = quantile, probs = 0.025),
      int_al  = apply(beta[, , i], MARGIN = 2, FUN = quantile, probs = 0.975),
      ymin    = apply(beta[, , i], MARGIN = 2, FUN = min),
      ymax    = apply(beta[, , i], MARGIN = 2, FUN = max),
      var     = levels(x$State)
    )
    p <- ggplot(
      data = beta_df,
      aes(
        x = var
      )
    ) +
      geom_errorbar(
        aes(
          ymin   = int_baj,
          ymax   = int_al
        )
      ) +
      geom_point(aes(y = mediana)) +
      coord_flip() +
      theme_bw() +
      labs(
        title = paste("Efecto por covariable:", cov_names[i]),
        x = ""
      )
    print(p)
  }
)


theta <- extract(modelo_cuatro, pars = "theta")$theta

map(
  1:7,
  function(i){
    theta_df <- tibble(
      media   = apply(theta[, , i], MARGIN = 2, FUN = mean),
      mediana = apply(theta[, , i], MARGIN = 2, FUN = median),
      int_baj = apply(theta[, , i], MARGIN = 2, FUN = quantile, probs = 0.025),
      int_al  = apply(theta[, , i], MARGIN = 2, FUN = quantile, probs = 0.975),
      var     = levels(x$Division)
    )
    p <- ggplot(
      data = theta_df,
      aes(
        x = var
      )
    ) +
      geom_errorbar(
        aes(
          ymin   = int_baj,
          ymax   = int_al
        )
      ) +
      geom_point(aes(y = mediana)) +
      coord_flip() +
      theme_bw() +
      labs(
        title = paste("Efecto por covariable:", cov_names[i]),
        x = ""
      )
    print(p)
  }
)
