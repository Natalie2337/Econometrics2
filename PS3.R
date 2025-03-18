#getwd()
library(dplyr)
library(ggplot2)
library(fixest)
library(tidyr)

###################################### Data Generation
set.seed(1234)

n_units <- 1000

#50 states
states <- 1:50
unit_states <- sample(states, n_units, replace = TRUE) ## assign 1-1000 to different states

#Policy years
g_list <- c(1996, 2002, 2008, 2014)
state_groups <- sample(g_list, 50, replace = TRUE)  # every state is assigned to a policy year
unit_groups <- state_groups[unit_states]

#years
years <- 1990:2020
n_years <- length(years)

#construct the panel data
data <- expand.grid(unit = 1:n_units, year = years)
data$state <- rep(unit_states, each = n_years)  
data$G <- rep(unit_groups, each = n_years)  # rep: replicate the element n times

#unit fixed effect α_i
alpha <- rnorm(n_units, mean = data$state / 1, sd = 1)
data$alpha <- rep(alpha, each = n_years)

#time fixed effect λ_t
data$lambda <- NA 
time_FE_errors <- rnorm(1, mean = 0, sd = 1)

for (g in g_list) {
  indices <- which(data$G == g)  ## the number of rows that G_i=g
  data$lambda[indices] <- 0.1 * (data$year[indices] - g) + time_FE_errors
}


#idiosyncratic error ε_it
data$epsilon <- rnorm(n_units * n_years, mean = 0, sd = 0.5)

#treatment effect τ_it
data$treatment_effect <- (data$year - data$G + 1) * (data$year >= data$G)

# Y_it
data$Y <- (2020 - data$G) + data$alpha + data$lambda + data$treatment_effect + data$epsilon

head(data)

## ATT
ATT_data <- data %>%
  group_by(year, G) %>%
  summarise(ATT = mean(treatment_effect), .groups = 'drop')

temp1 <- data %>%
  group_by(year, G) %>%
  filter(treatment_effect!=0) %>%
  summarise(mean_Y_treated = mean(Y), .groups = 'drop')

temp2 <- data %>%
  group_by(year, G) %>%
  filter(treatment_effect==0) %>%
  summarise(mean_Y_untreated = mean(Y), .groups = 'drop')

att_result <- temp1 %>%
  full_join(temp2, by = c("year", "G")) %>%
  mutate(ATT2 = mean_Y_treated - mean_Y_untreated) %>%
  select(year, G, ATT2)

## plot ATT & year t
ggplot(ATT_data, aes(x = year, y = ATT, color = factor(G))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "ATT over t",
       x = "t",
       y = "ATT",
       color = "Policy Enactment Year")


## plot ATT & t-g
ggplot(ATT_data, aes(x = year-G, y = ATT, color = factor(G))) +
  geom_line(size = 1) +
  theme_minimal() +
  labs(title = "ATT over t-g",
       x = "t-g",
       y = "ATT",
       color = "Policy Enactment Year")


############################## Monte Carlo and TWFE Regression

### DGP function
generate_data <- function(n_units = 1000, start_year = 1990, end_year = 2020) {
  set.seed(1234)  

  years <- start_year:end_year
  n_years <- length(years)
  policy_years <- c(1996, 2002, 2008, 2014)
  
  states <- 1:50
  unit_states <- sample(states, n_units, replace = TRUE)
  state_groups <- sample(policy_years, length(states), replace = TRUE)
  unit_groups <- state_groups[unit_states]
  
  data <- expand.grid(unit = 1:n_units, year = years)
  data$state <- rep(unit_states, each = n_years)
  data$G <- rep(unit_groups, each = n_years)
  
  alpha <- rnorm(n_units, mean = data$state / 1, sd = 1)
  data$alpha <- rep(alpha, each = n_years)
  
  data$lambda <- NA 
  time_FE_errors <- rnorm(1, mean = 0, sd = 1)
  
  for (g in g_list) {
    indices <- which(data$G == g)  ## the number of rows that G_i=g
    data$lambda[indices] <- 0.1 * (data$year[indices] - g) + time_FE_errors
  }
  
  
  data$event_time <- data$year - data$G
  
  data$treatment_effect <- ifelse(data$event_time >= 0, data$event_time, 0)
  
  data$epsilon <- rnorm(n_units * n_years, mean = 0, sd = 0.5)
  
  data$Y <- (2020 - data$G) + data$alpha + data$lambda + data$treatment_effect + data$epsilon
  
  return(data)
}


##### TWFE Regression

data2 <- generate_data()

## event time dummies
#[-5, 5]
for (k in -5:5) {
  if(k==-1){}
  else if (k<0){
    dummy_name <- paste0("D_neg", -k)  
  }else{
    dummy_name <- paste0("D_", k)
  }
  data2[[dummy_name]] <- ifelse(data2$event_time == k, 1, 0)
}
# <-5 or >5
data2$D_less_neg5 <- ifelse(data2$event_time < -5, 1, 0)
data2$D_larger_5 <- ifelse(data2$event_time > 5, 1, 0)

dummies <- grep("^D_", names(data2), value = TRUE) # all names start with "D_"
var_list <- c("alpha","lambda")
var_list <- c(var_list, dummies)
var_list <- unique(var_list) # remove duplicate

formula <- as.formula(paste("Y ~", paste(var_list, collapse = " + ")))
model <- lm(formula, data = data2)
summary(model)


## Create Dummies function

create_dummies <- function(mydata){
  for (k in -5:5) {
    if(k==-1){}
    else if (k<0){
      dummy_name <- paste0("D_neg", -k)  
    }else{
      dummy_name <- paste0("D_", k)
    }
    mydata[[dummy_name]] <- ifelse(mydata$event_time == k, 1, 0)
  }
  # <-5 or >5
  mydata$D_less_neg5 <- ifelse(mydata$event_time < -5, 1, 0)
  mydata$D_larger_5 <- ifelse(mydata$event_time > 5, 1, 0)
  return(mydata)
}

### Monte Carlo Simulation

n_simulation <- 100
result_list <- list()

for (sim in 1:n_simulation) {
  # Generate data
  sim_data <- generate_data()
  
  # Create event time dummies
  sim_data <- create_dummies(sim_data)
  
  dummies <- grep("^D_", names(sim_data), value = TRUE) # all names start with "D_"
  var_list <- c("alpha","lambda")
  var_list <- c(var_list, dummies)
  var_list <- unique(var_list) # remove duplicate
  
  # TWFE Regression
  formula <- as.formula(paste("Y ~", paste(var_list, collapse = " + ")))
  model <- lm(formula, data = sim_data)
  
  # Extract coefs
  event_coeffs <- as.data.frame(coef(model))
  colnames(event_coeffs)[1] <- "coef_estimation"
  event_coeffs$var_name <- rownames(event_coeffs)
  event_coeffs$sim_num <- sim
  
  result_list[[sim]] <- event_coeffs
}

results_df <- bind_rows(result_list)
head(results_df)

## only thetas and taus, and rename the columns
res <- results_df %>%
  filter(!(var_name %in% c("(Intercept)", "alpha", "lambda")))

res <- res %>%
  mutate(coef_name = case_when(
    grepl("^D_neg", var_name) ~ paste0("theta_", gsub("^D_", "", var_name)),
    grepl("^D_[0-9]+$", var_name) ~ paste0("tau_", gsub("^D_", "", var_name)),
    var_name == "D_less_neg5" ~ "theta_less_neg5",
    var_name == "D_larger_5" ~ "tau_larger_5",
    TRUE ~ var_name
  ))

head(res)

results_summary <- res %>%
  group_by(coef_name) %>%
  summarise(mean_estimate_coef = mean(coef_estimation)) %>%
  ungroup() %>%
  mutate(t_minus_g = case_when(
    grepl("^tau_[0-9]+$", coef_name) ~ gsub("^tau_", "", coef_name),
    grepl("^theta_neg[0-9]+$", coef_name) ~ paste0("-", gsub("^theta_neg", "", coef_name)),
    coef_name == "theta_less_neg5" ~ "-6",  
    coef_name == "tau_larger_5" ~ "6",    
    TRUE ~ "NA"
  )) %>%
  mutate(t_minus_g = as.numeric(t_minus_g)) %>%
  filter(!is.na(t_minus_g)) 

# theta_-1 = 0
results_summary <- results_summary %>%
  bind_rows(data.frame(
    coef_name = "theta_neg1",
    mean_estimate_coef = 0,  #\theta_{-1} = 0
    t_minus_g = -1
  ))

# true_treatment_df
true_treatment_df <- data.frame(
  t_minus_g = -5:5
)
true_treatment_df$true_treatment_effect <- ifelse(true_treatment_df$t_minus_g > 0, true_treatment_df$t_minus_g, 0)


# plot
ggplot() +
  geom_point(data = results_summary, aes(x = t_minus_g, y = mean_estimate_coef), color = "blue") +
  geom_line(data = results_summary, aes(x = t_minus_g, y = mean_estimate_coef), color = "blue") +
  geom_line(data = true_treatment_df, aes(x = t_minus_g, y = true_treatment_effect), color = "red", linetype = "dashed") +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") + 
  theme_minimal() +
  labs(title = "Monte Carlo Event Study: TWFE vs. True Effect",
       x = "Event Time (Years Relative to Treatment)",
       y = "Estimated Effect",
       caption = "Blue: TWFE Estimates | Red Dashed: True Treatment Effect")




################################################# Group-time ATT ??? Some problem

# I really don't know where the problem is....

# Still use the data generated in part1
data3 <- data

data3 <- data3 %>%
  mutate(
    has_treated_1996 = case_when(
      G > 1996 ~ 0,
      G <= 1996 ~ 1
    ),
    has_treated_2002 = case_when(
      G > 2002 ~ 0,
      G <= 2002 ~ 1
    ),
    has_treated_2008 = case_when(
      G > 2008 ~ 0,
      G <= 2008 ~ 1
    ),
    has_treated_2014 = case_when(
      G > 2014 ~ 0,
      G <= 2014 ~ 1
    )
  )

# Cross-term, group by G and year
data3$gt <- interaction(data3$G, data3$year, sep = "_")

ATT_res <- data3 %>%
  group_by(G,year) %>%
  summarise(
    mean_Y_treated_1996 = mean(Y[has_treated_1996==1], na.rm = TRUE),
    mean_Y_untreated_1996 = mean(Y[has_treated_1996==0], na.rm = TRUE),
    diff_Y_1996 = mean_Y_treated_1996 - mean_Y_untreated_1996,
    mean_Y_treated_2002 = mean(Y[has_treated_2002==1], na.rm = TRUE),
    mean_Y_untreated_2002 = mean(Y[has_treated_2002==0], na.rm = TRUE),
    diff_Y_2002 = mean_Y_treated_2002 - mean_Y_untreated_2002,
    mean_Y_treated_2008 = mean(Y[has_treated_2008==1], na.rm = TRUE),
    mean_Y_untreated_2008 = mean(Y[has_treated_2008==0], na.rm = TRUE),
    diff_Y_2008 = mean_Y_treated_2008 - mean_Y_untreated_2008,
    mean_Y_treated_2008 = mean(Y[has_treated_2008==1], na.rm = TRUE),
    mean_Y_untreated_2008 = mean(Y[has_treated_2008==0], na.rm = TRUE),
    diff_Y_2008 = mean_Y_treated_2008 - mean_Y_untreated_2008,
    .groups = "drop"
  )

print(ATT_res)


ATT_res2 <- data3 %>%
  group_by(G) %>%
  summarise(
    mean_Y_treated_1996 = mean(Y[has_treated_1996==1], na.rm = TRUE),
    mean_Y_untreated_1996 = mean(Y[has_treated_1996==0], na.rm = TRUE),
    diff_Y_1996 = mean_Y_treated_1996 - mean_Y_untreated_1996,
    mean_Y_treated_2002 = mean(Y[has_treated_2002==1], na.rm = TRUE),
    mean_Y_untreated_2002 = mean(Y[has_treated_2002==0], na.rm = TRUE),
    diff_Y_2002 = mean_Y_treated_2002 - mean_Y_untreated_2002,
    mean_Y_treated_2008 = mean(Y[has_treated_2008==1], na.rm = TRUE),
    mean_Y_untreated_2008 = mean(Y[has_treated_2008==0], na.rm = TRUE),
    diff_Y_2008 = mean_Y_treated_2008 - mean_Y_untreated_2008,
    mean_Y_treated_2008 = mean(Y[has_treated_2008==1], na.rm = TRUE),
    mean_Y_untreated_2008 = mean(Y[has_treated_2008==0], na.rm = TRUE),
    diff_Y_2008 = mean_Y_treated_2008 - mean_Y_untreated_2008,
    .groups = "drop"
  )

print(ATT_res2)


################################################ Group-time ATT
### Correct!!!!!!!!

data4 <- generate_data()

## Compute Group-time ATT Function:
compute_ATT <- function(my_data, g, t) {
  # 选择在 g 期接受治疗的个体
  treated_group <- my_data %>% filter(G == g & t >= G)
  
  # E[Y_t(g)|G_g=1]：接受治疗个体的均值
  EY_t_g <- mean(treated_group$Y)
  #print(EY_t_g)
  
  # E[Y_t(0)|G_g=1]：与接受治疗组匹配的未治疗个体均值
  #has_treated_variable <- paste0("has_treated_", g)
  untreated_group <- my_data %>% filter(G > g & t >= G)
  EY_t_0 <- mean(untreated_group$Y)
  #print(EY_t_0)
  
  # ATT
  ATT <- EY_t_g - EY_t_0
  return(ATT)
}

ATT_result <- compute_ATT(data4, 2002, 2010)
print(ATT_result)

g_values <- c(1996, 2002, 2008, 2014)
t_values <- 1990:2020

results <- data.frame(g = integer(), t = integer(), ATT = numeric())

for (g in g_values) {
  for (t in t_values) {
    if (t >= g) {  # 确保 t >= g
      ATT_value <- compute_ATT(data4, g, t)
      if (!is.na(ATT_value)) {
        results <- rbind(results, data.frame(g = g, t = t, ATT = ATT_value))
      }
    }
  }
}

print(results)

## Plotting
ggplot(results, aes(x = t, y = ATT, color = as.factor(g))) +
  geom_line(size = 1) +
  labs(title = "ATT Estimates Over Time for Different Treatment Groups",
       x = "Time (t)", 
       y = "ATT Estimates",
       color = "Treatment Group (g)") +
  theme_minimal() +
  theme(legend.position = "right") 


##### Aggregate ATT

# Function to compute weighted aggregated ATT at each t
compute_weighted_ATT <- function(my_data, g_values, t_values) {
  att_results <- data.frame(g = integer(), t = integer(), ATT = numeric(), Weight = numeric())
  
  # Compute ATT for each g and t
  for (g in g_values) {
    for (t in t_values) {
      if (t >= g) {  # Ensure valid t
        ATT_value <- compute_ATT(my_data, g, t)
        
        if (!is.na(ATT_value)) {
          # Weight = size of group g up to time t
          weight <- nrow(my_data %>% filter(G == g & t >= G))
          
          att_results <- rbind(att_results, data.frame(g = g, t = t, ATT = ATT_value, Weight = weight))
        }
      }
    }
  }
  
  # Aggregate ATT across groups at each time t
  weighted_att_results <- att_results %>%
    group_by(t) %>%
    summarise(Aggregated_ATT = sum(ATT * Weight, na.rm = TRUE) / sum(Weight, na.rm = TRUE))
  
  return(weighted_att_results)
}


aggregated_ATT <- compute_weighted_ATT(data4, g_values, t_values)

# Print results
print(aggregated_ATT)

# Plot aggregated ATT over time
library(ggplot2)
ggplot(aggregated_ATT, aes(x = t, y = Aggregated_ATT)) +
  geom_line(size = 1, color = "blue") +
  labs(title = "Aggregated ATT Over Time", x = "Time (t)", y = "Aggregated ATT") +
  theme_minimal()








