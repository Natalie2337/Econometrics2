
#Q1
set.seed(101)
num = 10000
e <- rnorm(num,mean=0,sd=2)
y <- 100 + exp(e)
print(y)
print(mean(y))
print(var(y))
y0 <- y
var_y0 <- var(y)
mean_y0 <- mean(y)

#Q2
hist(y)
hist(y-mean(y))

#Q3

#construct data frame
df_data <- data.frame(y, e)
df_data0 <- data.frame(y, e)

#random 50-50 students
ran_indices <- sample(1:num,100)
print(ran_indices)
treated_indices <- sample(ran_indices, 50)
untreated_indices <- setdiff(ran_indices,treated_indices)
print(treated_indices)
print(untreated_indices)

#add treatment D
df_data$D <- 0
df_data$D[treated_indices] = 1

df_subdata <- df_data[ran_indices,]
df_subdata$new_y <- df_subdata$y + 5*df_subdata$D

#regression
model <- lm(new_y ~ D, data = df_subdata)
summary(model)
print(coef(model)["D"])


#Q4

beta_hat_list <- numeric(10000)

for (i in 1:10000){
  temp_df_data <- df_data0
  
  #random 50-50 students
  temp_ran_indices <- sample(1:num,100)
  #print(temp_ran_indices)
  temp_treated_indices <- sample(temp_ran_indices, 50)
  temp_untreated_indices <- setdiff(temp_ran_indices,temp_treated_indices)
  #print(temp_treated_indices)
  #print(temp_untreated_indices)
  
  #add treatment D
  temp_df_data$D <- 0
  temp_df_data$D[temp_treated_indices] = 1
  
  temp_df_subdata <- temp_df_data[temp_ran_indices,]
  temp_df_subdata$new_y <- temp_df_subdata$y + 5*temp_df_subdata$D
  
  #regression
  temp_model <- lm(new_y ~ D, data = temp_df_subdata)
  #print(coef(temp_model)["D"])
  beta_hat_list[i] = coef(temp_model)["D"]
}

summary(beta_hat_list)
print(mean(beta_hat_list))

# Q5
hist(beta_hat_list)
hist(beta_hat_list-5)
2
# Q6
negative_num <- sum(beta_hat_list<0)
p_negative <- negative_num/length(beta_hat_list)
print(p_negative)

# Q7
alpha = 0.05
t_alpha = qnorm(1-alpha/2)
k = 0.8
t_1_minus_k = qnorm(k)
p = 0.5
sigma_square = var_y0
MDE = 5

N_star = ( (t_1_minus_k+t_alpha)/MDE )^2 * sigma_square/(p*(1-p))
print(N_star)

# Q8

beta_hat_list2 <- numeric(10000)
p_value_list <- numeric(10000)

for (i in 1:10000){
  temp_df_data <- df_data0
  
  #random 50-50 students
  temp_ran_indices <- sample(1:num,N_star)
  #print(temp_ran_indices)
  temp_treated_indices <- sample(temp_ran_indices, N_star/2)
  temp_untreated_indices <- setdiff(temp_ran_indices,temp_treated_indices)
  #print(temp_treated_indices)
  #print(temp_untreated_indices)
  
  #add treatment D
  temp_df_data$D <- 0
  temp_df_data$D[temp_treated_indices] = 1
  
  temp_df_subdata <- temp_df_data[temp_ran_indices,]
  temp_df_subdata$new_y <- temp_df_subdata$y + 5*temp_df_subdata$D
  
  #regression
  temp_model <- lm(new_y ~ D, data = temp_df_subdata)
  #print(coef(temp_model)["D"])
  beta_hat_list2[i] = coef(temp_model)["D"]
  p_value_list[i] <- summary(temp_model)$coefficients["D", "Pr(>|t|)"]
}

#print(beta_hat_list2)
print(mean(beta_hat_list2))

hist(beta_hat_list2)
hist(beta_hat_list2-5)

print(p_value_list)
# rejects H0: beta=0 when p_value < 0.05
rej <- sum(p_value_list<0.05)
p_rej <- rej/10000
print(p_rej)

# Q9
sigma_square_hat <- sum((df_subdata$y - mean(df_subdata$y))^2) / (100 - 1)
print(sigma_square_hat)
new_N_star = ( (t_1_minus_k+t_alpha)/MDE )^2 * sigma_square_hat/(p*(1-p))
print(new_N_star)


beta_hat_list3 <- numeric(10000)
p_value_list2 <- numeric(10000)

for (i in 1:10000){
  temp_df_data <- df_data0
  
  #random 50-50 students
  temp_ran_indices <- sample(1:num,new_N_star)
  #print(temp_ran_indices)
  temp_treated_indices <- sample(temp_ran_indices, new_N_star/2)
  temp_untreated_indices <- setdiff(temp_ran_indices,temp_treated_indices)
  #print(temp_treated_indices)
  #print(temp_untreated_indices)
  
  #add treatment D
  temp_df_data$D <- 0
  temp_df_data$D[temp_treated_indices] = 1
  
  temp_df_subdata <- temp_df_data[temp_ran_indices,]
  temp_df_subdata$new_y <- temp_df_subdata$y + 5*temp_df_subdata$D
  
  #regression
  temp_model <- lm(new_y ~ D, data = temp_df_subdata)
  #print(coef(temp_model)["D"])
  beta_hat_list3[i] = coef(temp_model)["D"]
  p_value_list2[i] <- summary(temp_model)$coefficients["D", "Pr(>|t|)"]
}

print(beta_hat_list3)
print(mean(beta_hat_list3))

hist(beta_hat_list3)
hist(beta_hat_list3-5)

#print(p_value_list2)
# rejects H0: beta=0 when p_value < 0.05
rej2 <- sum(p_value_list2<0.05)
p_rej2 <- rej2/10000
print(p_rej2)


