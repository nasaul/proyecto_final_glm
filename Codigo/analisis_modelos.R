# Lectura de datos --------------------------------------------------------

library(bayesplot)
library(rstan)
library(dplyr)
library(ggplot2)
library(readr)
library(loo)
library(purrr)
ggplot2::theme_set(theme_bw())

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
    intalto = extract(modelo, "yn")$yn %>%
      apply(
        MARGIN = 2,
        FUN    = quantile,
        probs = 0.975
      ),
    intbajo = extract(modelo, "yn")$yn %>%
      apply(
        MARGIN = 2,
        FUN    = quantile,
        probs = 0.025
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
  select(`State Code`, Division, State) %>%
  rename(
    Name  = State,
    State = `State Code`
    )


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

mod2_predvsval <- predice(modelo_dos) %>%
  filter(muertes < 250) %>% 
  ggplot(aes(x = muertes, y = mediana)) +
  geom_errorbar(aes(ymin = intbajo, ymax = intalto)) +
  geom_point() +
  geom_line(aes(x = seq(1, 250, length.out = 2205), y = seq(1, 250, length.out = 2205))) +
  labs(
    title = "Comparación entre valores reales y predicciones del modelo", 
    x = "Número de Muertes",
    y = "Predictiva Posterior",
    subtitle = paste(
      "WAIC:\t",
      modelo_dos %>%
        extract_log_lik() %>%
        waic() %>%
        .$waic %>% 
        round(2)
    )
  )
saveRDS(mod2_predvsval, file = here::here("Resultados/mod2_predvsval.rds"))

beta0_m1    <- extract(modelo_dos, pars = "beta0")$beta0

beta0_df_m1 <- tibble(
  media   = apply(beta0_m1, MARGIN = 2, FUN = mean),
  mediana = apply(beta0_m1, MARGIN = 2, FUN = median),
  int_baj = apply(beta0_m1, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0_m1, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0_m1, MARGIN = 2, FUN = min),
  ymax    = apply(beta0_m1, MARGIN = 2, FUN = max),
  state   = filter(estados, State %in% levels(x$State)) %>% pull(Name)
) %>% 
  mutate_if(is.numeric, ~(exp(.) * 100))

ggplot(
  data = beta0_df_m1,
  aes(
    y = forcats::fct_rev(reorder(state, state))
  )
) +
  geom_errorbarh(
    aes(
      xmin   = int_baj,
      xmax   = int_al
    )
  ) +
  geom_point(aes(x = mediana)) +
  labs(
    y        = "Estado",
    x        = "Tasa de asesinatos",
    title    = "Tasa de asesinato por estado",
    subtitle = "Efectos por estado"
  ) 

theta_m1    <- extract(modelo_dos, pars = "theta")$theta

theta_df_m1 <- tibble(
  media   = apply(theta_m1, MARGIN = 2, FUN = mean),
  mediana = apply(theta_m1, MARGIN = 2, FUN = median),
  int_baj = apply(theta_m1, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(theta_m1, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(theta_m1, MARGIN = 2, FUN = min),
  ymax    = apply(theta_m1, MARGIN = 2, FUN = max),
  div     = levels(x$Division)
) %>% 
  mutate_if(is.numeric, ~(exp(.) * 100))

phi_m1 <- extract(modelo_dos, pars = "phi_param")$phi_param
phi_df_m1 <- tibble(
  media = mean(phi_m1),
  mediana = median(phi_m1),
  int_baj = quantile(phi_m1, probs = 0.025),
  int_al  = quantile(phi_m1, probs = 0.975),
  div = "EUA"
) %>% 
  mutate_if(is.numeric, ~(exp(.) * 100))

ggplot(
  data = full_join(
    theta_df_m1,
    phi_df_m1,
    by = c("media", "mediana", "int_baj", "int_al", "div")
  ) %>% 
    mutate(
      div = factor(div, levels = 
                     c(
                       "EUA",
                       "West South Central",
                       "West North Central",
                       "South Atlantic",
                       "Pacific",
                       "New England",
                       "Mountain",
                       "Middle Atlantic",
                       "East South Central",
                       "East North Central"
                     )
      )
    ),
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
  coord_flip() +
  theme_bw() +
  labs(
    x        = "División",
    y        = "Tasa de asesinatos",
    title    = "Tasa de asesinato por división",
    subtitle = "Efectos por división"
  )


# Modelo Tres -------------------------------------------------------------

modelo_tres <- readRDS(
  here::here("Resultados/modelo_tres.rds")
)


mod3_predvsval <- predice(modelo_tres) %>%
  filter(muertes < 250) %>% 
  ggplot(aes(x = muertes, y = mediana)) +
  geom_errorbar(aes(ymin = intbajo, ymax = intalto)) +
  geom_point() +
  geom_line(aes(x = seq(1, 250, length.out = 2205), y = seq(1, 250, length.out = 2205))) +
  labs(
    title = "Comparación entre valores reales y predicciones del modelo", 
    x = "Número de Muertes",
    y = "Predictiva Posterior",
    subtitle = paste(
      "WAIC:\t",
      modelo_tres %>%
        extract_log_lik() %>%
        waic() %>%
        .$waic %>% 
        round(2)
    )
  )
saveRDS(mod3_predvsval, file = here::here("Resultados/mod3_predvsval.rds"))


beta0_m3    <- extract(modelo_tres, pars = "beta0")$beta0

beta0_df_m3 <- tibble(
  media   = apply(beta0_m3, MARGIN = 2, FUN = mean),
  mediana = apply(beta0_m3, MARGIN = 2, FUN = median),
  int_baj = apply(beta0_m3, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(beta0_m3, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(beta0_m3, MARGIN = 2, FUN = min),
  ymax    = apply(beta0_m3, MARGIN = 2, FUN = max),
  state   = filter(estados, State %in% levels(x$State)) %>% pull(Name)
) %>% 
  mutate_if(is.numeric, ~exp(.) * 100000)

beta0_graph <- ggplot(
  data = beta0_df_m3,
  aes(
    x = forcats::fct_rev(reorder(state, state))
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
    x        = "Estado",
    y        = "Tasa de asesinatos por cada 100,000 habitantes",
    title    = "Tasa de asesinatos base por estado por cada 100,000 habitantes"
  )
saveRDS(beta0_graph, here::here("Resultados/mod3_tasabase.rds"))
theta_m3    <- extract(modelo_tres, pars = "theta")$theta

theta_df_m3 <- tibble(
  media   = apply(theta_m3, MARGIN = 2, FUN = mean),
  mediana = apply(theta_m3, MARGIN = 2, FUN = median),
  int_baj = apply(theta_m3, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(theta_m3, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(theta_m3, MARGIN = 2, FUN = min),
  ymax    = apply(theta_m3, MARGIN = 2, FUN = max),
  div     = levels(x$Division)
) %>% 
  mutate_if(is.numeric, ~exp(.) * 100000)

phi_m3 <- extract(modelo_tres, pars = "phi_param")$phi_param
phi_df_m3 <- tibble(
  media = mean(phi_m3),
  mediana = median(phi_m3),
  int_baj = quantile(phi_m3, probs = 0.025),
  int_al  = quantile(phi_m3, probs = 0.975),
  div = "EUA"
) %>% 
  mutate_if(is.numeric, ~exp(.) * 100000)

mod3_tasabase_div <- ggplot(
  data = theta_df_m3 %>%
    full_join(phi_df_m3) %>% 
    mutate(
      div = factor(div, levels = 
                     c(
                       "EUA",
                       "West South Central",
                       "West North Central",
                       "South Atlantic",
                       "Pacific",
                       "New England",
                       "Mountain",
                       "Middle Atlantic",
                       "East South Central",
                       "East North Central"
                     )
      )
    ),
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
  coord_flip() +
  theme_bw() +
  labs(
    x        = "Estado",
    y        = "Tasa base de asesinatos por cada 100,000 habitantes",
    title    = "Tasa base de asesinato por División por cada 100,000 habitantes"
  )

saveRDS(mod3_tasabase_div, here::here("Resultados/mod3_tasabase_div.rds"))

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
  var     = cov_names,
  nombres = case_when(
    var == "pctBlack"           ~ "Porcentaje de población \nafroamericana",
    var == "pctPoverty"         ~ "Porcentaje de población \nen pobreza",
    var == "pctNotSpeakEng"     ~ "Porcentaje de población \nque no habla bien inglés",
    var == "pctBornStateResid"  ~ "Porcentaje de población \nque vive donde nacio",
    var == "pctNotHSgrad"       ~ "Porcentaje de población \nque no acabó la preparatoria",
    var == "pctWorkMom.18"      ~ "Porcentaje de madres\n con hijos menores a 18 años \nque trabajan",
    var == "pctFgnImmig.10"     ~ "Porcentaje de población \nde inmigrantes que \nllegaron en los últimos 10 años"
  )
) 


p <- ggplot(
  data = beta_df,
  aes(
    x = nombres
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
  theme_bw() +
  coord_flip() +
  labs(
    title = "Parámetros asociados a cada variable",
    subtitle = "Intervalos al 95% de credibilidad",
    x = "",
    y = ""
  )

saveRDS(p, file = here::here("Resultados/mod3_efectos.rds"))

# Modelo cuatro -----------------------------------------------------------

modelo_cuatro <- readRDS(
  here::here("Resultados/modelo_cuatro.rds")
)

mod4_predvsval <- predice(modelo_cuatro) %>%
  filter(muertes < 250) %>% 
  ggplot(aes(x = muertes, y = mediana)) +
  geom_errorbar(aes(ymin = intbajo, ymax = intalto)) +
  geom_point() +
  geom_line(aes(x = seq(1, 250, length.out = 2205), y = seq(1, 250, length.out = 2205))) +
  labs(
    title = "Comparación entre valores reales y predicciones del modelo", 
    x = "Número de Muertes",
    y = "Predictiva Posterior",
    subtitle = paste(
      "WAIC:\t",
      modelo_cuatro %>%
        extract_log_lik() %>%
        waic() %>%
        .$waic %>% 
        round(2)
    )
  )
saveRDS(mod4_predvsval, file = here::here("Resultados/mod4_predvsval.rds"))


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
  media   = apply(theta0, MARGIN = 2, FUN = mean),
  mediana = apply(theta0, MARGIN = 2, FUN = median),
  int_baj = apply(theta0, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(theta0, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(theta0, MARGIN = 2, FUN = min),
  ymax    = apply(theta0, MARGIN = 2, FUN = max),
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
  data = theta0_df %>%
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


cov_hiper <- rstan::extract(modelo_cuatro, pars = "cov_hiper")$cov_hiper

cov_hiper_df <- tibble(
  media   = apply(cov_hiper, MARGIN = 2, FUN = mean),
  mediana = apply(cov_hiper, MARGIN = 2, FUN = median),
  int_baj = apply(cov_hiper, MARGIN = 2, FUN = quantile, probs = 0.025),
  int_al  = apply(cov_hiper, MARGIN = 2, FUN = quantile, probs = 0.975),
  ymin    = apply(cov_hiper, MARGIN = 2, FUN = min),
  ymax    = apply(cov_hiper, MARGIN = 2, FUN = max),
  var     = cov_names,
  nombres = case_when(
    var == "pctBlack"           ~ "Porcentaje de población \nafroamericana",
    var == "pctPoverty"         ~ "Porcentaje de población \nen pobreza",
    var == "pctNotSpeakEng"     ~ "Porcentaje de población \nque no habla bien inglés",
    var == "pctBornStateResid"  ~ "Porcentaje de población \nque vive donde nacio",
    var == "pctNotHSgrad"       ~ "Porcentaje de población \nque no acabó la preparatoria",
    var == "pctWorkMom.18"      ~ "Porcentaje de madres\n con hijos menores a 18 años \nque trabajan",
    var == "pctFgnImmig.10"     ~ "Porcentaje de población \nde inmigrantes que \nllegaron en los últimos 10 años"
  )
) 


p <- ggplot(
  data = cov_hiper_df,
  aes(
    x = nombres
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
  theme_bw() +
  coord_flip() +
  labs(
    title = "Hiperparámetros de EUA asociados a cada variable",
    subtitle = "Intervalos al 95% de credibilidad",
    x = "",
    y = ""
  )

saveRDS(p, file = here::here("Resultados/mod4_efectos.rds"))
