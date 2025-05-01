# Create 1000 runs of a 1000-person study where a negative control outcome is
# correctly applied, and then there are mistakes! 

# Libraries
pacman::p_load(here, tidyverse)
set.seed(444)

# Begin with correct case -------------------------------------------------

create_correct_case_data <- function() {
  u <- rnorm(n = 100, mean = 1, sd = 10)
  l <- rnorm(n = 100, mean = 1, sd = 10)
  a <- rnorm(n = 100, mean = 1, sd = 10) + u + l
  y <- rnorm(n = 100, mean = 1, sd = 10) + a + u + l
  n <- rnorm(n = 100, mean = 1, sd = 10) + u + l
  
  dat <- data.frame(u = u,
                    l = l,
                    a = a,
                    y = y,
                    n = n)
  return(dat)
}


run_simulation_correct_model <- function(case_data){
  # model primary outcome of interest
  m1 <- lm(y ~ a + l, dat = case_dat)
  m2 <- lm(n ~ a + l, dat = case_dat)
  
  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]
  
  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )
  
  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )
  
  rownames(df_1) = NULL
  rownames(df_2) = NULL
  
  df <- bind_rows(df_1, df_2)
  
  return(df)
}


results <- data.frame()
for (i in 1:100) {
  case_dat <- create_correct_case_data()
  models <- run_simulation_correct_model(case_dat)
  results <- bind_rows(results, models)
  results$model = 'correct_case'
}

write_rds(results, here("data", "correct_case_coefs_confints.RDS"))



# Missing confounder ------------------------------------------------------

create_missing_confounder_data <- function() {
  u <- rnorm(n = 100, mean = 1, sd = 10)
  l <- rnorm(n = 100, mean = 1, sd = 10)
  a <- rnorm(n = 100, mean = 1, sd = 10) + u + l
  y <- rnorm(n = 100, mean = 1, sd = 10) + a + u + l
  n <- rnorm(n = 100, mean = 1, sd = 10) + l
  
  dat <- data.frame(
    u = u,
    a = a,
    y = y,
    l = l,
    n = n
  )
  return(dat)
}


run_simulation_missing_confounder_data <- function(case_dat) {
  # model primary outcome of interest
  m1 <- lm(y ~ a + l, dat = case_dat)
  m2 <- lm(n ~ a + l, dat = case_dat)
  
  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]
  
  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )
  
  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )
  
  rownames(df_1) = NULL
  rownames(df_2) = NULL
  
  df <- bind_rows(df_1, df_2)
  
  return(df)
}

results <- data.frame()
for (i in 1:100) {
  case_dat <- create_missing_confounder_data()
  models <- run_simulation_missing_confounder_data(case_dat)
  results <- bind_rows(results, models)
  results$model = 'missing confounder'
  
}

write_rds(results,
          here("data", "missing_confounder_coefs_confints.RDS"))


# Modelling error ---------------------------------------------------------

create_inadequately_modelled_confounder_data <- function() {
  l <- rnorm(n = 100, mean = 1, sd = 3)
  a <- rnorm(n = 100, mean = 1, sd = 3) + l^3
  y <- rnorm(n = 100, mean = 1, sd = 3) + a + l^3
  n <- rnorm(n = 100, mean = 1, sd = 3) + l

  dat <- data.frame(l = l,
                    a = a,
                    y = y,
                    n = n)
  return(dat)
}

run_simulation_model_error <- function(case_data){
  # model primary outcome of interest
  m1 <- lm(y ~ a + l, dat = case_dat)
  m2 <- lm(n ~ a + l, dat = case_dat)

  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]

  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )

  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )

  rownames(df_1) = NULL
  rownames(df_2) = NULL

  df <- bind_rows(df_1, df_2)

  return(df)
}

results <- data.frame()
for (i in 1:100) {
  case_dat <- create_inadequately_modelled_confounder_data()
  models <- run_simulation_model_error(case_dat)
  results <- bind_rows(results, models)
  results$model = 'unmodelled_confounder'

}

results <- results %>% filter(beta>-3 & beta < 3)

write_rds(results,
          here(
            "data",
            "inadequately_modelled_confounder_coefs_confints.RDS"
          ))



# Measurement error -------------------------------------------------------

create_inadequately_measured_confounder_data <- function() {
  # L <- runif(n = 100, min = 1, max = 10)
  # a <- rnorm(n = 100, mean = 1, sd = 10) + 10*(L < 5)
  # y <- rnorm(n = 100, mean = 1, sd = 10) + a + 10*(L < 5)
  # n <- rnorm(n = 100, mean = 1, sd = 10) + (L < 1)
  
  L <- sample(1:10, 100, replace = TRUE)
  a <- rnorm(n = 100, mean = 1, sd = 10) + 20*(L > 9)
  y <- rnorm(n = 100, mean = 1, sd = 10) + a + 20*(L > 9)
  n <- rnorm(n = 100, mean = 1, sd = 10) + 20*(L > 1)
  
  dat <- data.frame(L = L,
                    l_measured = (L > 1),
                    a = a,
                    y = y,
                    n = n)
  return(dat)
}

run_simulation_bad_measurement <- function(case_data){
  # model primary outcome of interest
  m1 <- lm(y ~ a + l_measured, dat = case_data)
  m2 <- lm(n ~ a + l_measured, dat = case_data)
  
  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]
  
  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )
  
  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )
  
  rownames(df_1) = NULL
  rownames(df_2) = NULL
  
  df <- bind_rows(df_1, df_2)
  
  return(df)
}

results <- data.frame()
for (i in 1:100) {
  case_dat <- create_inadequately_measured_confounder_data()
  models <- run_simulation_bad_measurement(case_dat)
  results <- bind_rows(results, models)
  results$model = 'inadequately_measured_confounder'
}

write_rds(results,
          here(
            "data",
            "inadequately_measured_confounder_coefs_confints.RDS"
          ))







# model
# model primary outcome of interest
m1 <- lm(y ~ a, dat = case_dat)
m2 <- lm(n ~ a, dat = case_dat)

a_y_beta = m1$coefficients[2]
a_n_beta = m2$coefficients[2]
a_y_confint_lower = confint(m1)[2, 1]
a_y_confint_upper = confint(m1)[2, 2]
a_n_confint_lower = confint(m2)[2, 1]
a_n_confint_upper = confint(m2)[2, 2]

df_1 <- data.frame(
  beta = a_y_beta,
  confint_lower = a_y_confint_lower,
  confint_upper = a_y_confint_upper,
  type = 'primary_assoc'
)

df_2 <- data.frame(
  beta = a_n_beta,
  confint_lower = a_n_confint_lower,
  confint_upper = a_n_confint_upper,
  type = 'neg_control'
)

rownames(df_1) = NULL
rownames(df_2) = NULL

df <- bind_rows(df_1, df_2)


# Weak negative control ---------------------------------------------------

create_weak_negative_control_data <- function() {
  u <- rnorm(n = 100, mean = 1, sd = 10)
  a <- rnorm(n = 100, mean = 1, sd = 10) + u
  y <- rnorm(n = 100, mean = 1, sd = 10) + a + u
  n <- rnorm(n = 100, mean = 1, sd = 10) + 0.001 * u
  
  dat <- data.frame(u = u,
                    a = a,
                    y = y,
                    n = n)
  return(dat)
}

run_simulation_weak_negative_control <- function(case_dat) {
  # model primary outcome of interest
  m1 <- lm(y ~ a, dat = case_dat)
  m2 <- lm(n ~ a, dat = case_dat)
  
  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]
  
  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )
  
  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )
  
  rownames(df_1) = NULL
  rownames(df_2) = NULL
  
  df <- bind_rows(df_1, df_2)
  
  return(df)
}

results <- data.frame()
for (i in 1:100) {
  case_dat <- create_weak_negative_control_data()
  models <- run_simulation_weak_negative_control(case_dat)
  results <- bind_rows(results, models)
  results$model = 'weak_negative_control'
}

write_rds(results,
          here("data", "weak_negative_control_coefs_confints.RDS"))


# Negative association ----------------------------------------------------


create_neg_assoc_control_data <- function() {
  u <- rnorm(n = 100, mean = 1, sd = 10)
  a <- rnorm(n = 100, mean = 1, sd = 10) + u
  y <- rnorm(n = 100, mean = 1, sd = 10) +  a + u
  n <- rnorm(n = 100, mean = 1, sd = 10) - u
  
  dat <- data.frame(u = u,
                    a = a,
                    y = y,
                    n = n)
  return(dat)
}

run_simulation_negative_assoc <- function(case_dat) {
  # model primary outcome of interest
  m1 <- lm(y ~ a, dat = case_dat)
  m2 <- lm(n ~ a, dat = case_dat)
  
  a_y_beta = m1$coefficients[2]
  a_n_beta = m2$coefficients[2]
  a_y_confint_lower = confint(m1)[2, 1]
  a_y_confint_upper = confint(m1)[2, 2]
  a_n_confint_lower = confint(m2)[2, 1]
  a_n_confint_upper = confint(m2)[2, 2]
  
  df_1 <- data.frame(
    beta = a_y_beta,
    confint_lower = a_y_confint_lower,
    confint_upper = a_y_confint_upper,
    type = 'primary_assoc'
  )
  
  df_2 <- data.frame(
    beta = a_n_beta,
    confint_lower = a_n_confint_lower,
    confint_upper = a_n_confint_upper,
    type = 'neg_control'
  )
  
  rownames(df_1) = NULL
  rownames(df_2) = NULL
  
  df <- bind_rows(df_1, df_2)
  
  return(df)
}

results <- data.frame()
for (i in 1:100) {
  case_dat <- create_neg_assoc_control_data()
  models <- run_simulation_negative_assoc(case_dat)
  results <- bind_rows(results, models)
  results$model = 'negative_assoc'
}


write_rds(results, here("data", "neg_assoc_coefs_confints.RDS"))

