library(estimatr)
library(haven)
library(sandwich)
library(lmtest)
library(dplyr)
library(ggplot2)
library(AER)

all_sites_data <- read_dta("allsites_2025.dta")
all_covariates_data <- read_dta("allcovariates_2025.dta")
site_covariates_data <- read_dta("sitecovariates_2025.dta")
mile_data <- read_dta("2miledata_2025.dta")

# Q1

#a
#regression1
model1 <- lm_robust(lnmdvalhs0 ~ npl2000 + lnmeanhs8, data = all_sites_data) 
summary(model1, se="hetero")
#coeftest(model1, vcov = vcovHC(model1, type = "HC1"))


#regression2
all_vars_sites_data <- names(all_sites_data)
basic_character_list <- c("firestoveheat80", "noaircond80", "nofullkitchen80", "zerofullbath80",
                          "detach80occ", "mobile80occ")
temp_vars <- grep("^(bedrms|blt)", all_vars_sites_data, value = TRUE)
housing_character_list <- c("npl2000", "lnmeanhs8", basic_character_list, as.list(temp_vars))
# note: need to drop one of the blt* to avoid perfect colinearity!!
housing_character_list <- housing_character_list[housing_character_list != "blt40_yrs80occ"]
housing_character_list <- housing_character_list[housing_character_list != "bedrms5_80occ"]
model_formula <- as.formula(paste("lnmdvalhs0", "~", paste(housing_character_list, collapse = " + ")))
model2 <- lm_robust(model_formula, data = all_sites_data)
summary(model2, se="hetero")

#coeftest(model2, vcov = vcovHC(model2, type = "HC1"))


#regression3
Economic_population_list <- c("pop_den8", "shrblk8", "shrhsp8", "child8", "old8",
                              "shrfor8", "ffh8", "smhse8", "hsdrop8", "unemprt8",
                              "povrat8", "welfare8", "avhhin8", "tothsun8", "ownocc8",
                              "occupied80")
temp_vars_2 <- grep("^(no_hs_dipl|ba_or_b)", all_vars_sites_data, value = TRUE)
combined_list <- c(housing_character_list,Economic_population_list,as.list(temp_vars_2))
model_formula2 <- as.formula(paste("lnmdvalhs0", "~", paste(combined_list, collapse = " + ")))
model3 <- lm_robust(model_formula2, data = all_sites_data)
summary(model3, se="hetero")
#coeftest(model3, vcov = vcovHC(model3, type = "HC1"))


#regression4
model_formula3 <- as.formula(paste("lnmdvalhs0 ~", paste(c(combined_list, "statefips"), collapse = " + ")))
model4 <- lm_robust(model_formula3, data = all_sites_data)
summary(model4, se="hetero")
#coeftest(model4, vcov = vcovHC(model4, type = "HC1"))

#b: compare covariates
covariate_list <- c(basic_character_list,Economic_population_list)
covariate_list <- covariate_list[covariate_list!="old8"]

compare_table <- all_covariates_data %>%
  group_by(npl2000) %>%
  summarise(across(all_of(covariate_list), \(x) mean(x,na.rm=TRUE)))
print(compare_table)
diff_by_npl2000 <- compare_table[2,] - compare_table[1,]
print(diff_by_npl2000)

compare_data <- site_covariates_data %>%
  mutate(
    hrs_group = case_when(
      hrs_82 < 16.5 ~ 0,
      hrs_82 >= 16.5 & hrs_82 <= 28.5 ~ 1,
      hrs_82 >= 28.5 & hrs_82 <= 40.5 ~ 2,
      hrs_82 > 40.5 ~ 3
    )
  )

compare_table2 <- compare_data %>%
  group_by(hrs_group) %>%
  summarise(across(all_of(covariate_list), \(x) mean(x,na.rm=TRUE)))
print(compare_table2)  

diff_by_npl2000_0_1 <- compare_table2[2,] - compare_table2[1,]
diff_by_npl2000_1_2 <- compare_table2[3,] - compare_table2[2,]
diff_by_npl2000_2_3 <- compare_table2[4,] - compare_table2[3,]
print(diff_by_npl2000_0_1)
print(diff_by_npl2000_1_2)
print(diff_by_npl2000_2_3)

# Q2

#b
hrs_bin <- pretty(mile_data$hrs_82, n = 10)  
hist_obj <- hist(mile_data$hrs_82, breaks = hrs_bin, freq = FALSE, 
                 main = "Histogram", xlab = "HRS_82", ylab = "Density")

abline(v = 28.5, col = "red", lwd = 2, lty = 2)

#hrs_mid <- (head(hrs_bin,-1)+tail(hrs_bin,-1)) / 2
bin_data <- data.frame(
  mid = hist_obj$mids,
  density = hist_obj$density
)

left_bin <- bin_data %>% filter(mid<=28.5)
right_bin <- bin_data %>% filter(mid>28.5)

model_left <- lm(density ~ mid, data=left_bin)
model_right <- lm(density ~ mid, data=right_bin)

lines(left_bin$mid, predict(model_left), col = "blue", lwd = 2)
lines(right_bin$mid, predict(model_right), col = "blue", lwd = 2)

# Q3

#a
new_covariates <- c("firestoveheat80_nbr", "noaircond80_nbr", "nofullkitchen80_nbr", "zerofullbath80_nbr",
                    "detach80occ_nbr", "mobile80occ_nbr", "pop_den8_nbr","shrblk8_nbr",
                    "shrhsp8_nbr", "child8_nbr", "shrfor8_nbr", "ffh8_nbr", "smhse8_nbr", "hsdrop8_nbr", 
                    "unemprt8_nbr", "povrat8_nbr", "welfare8_nbr", "avhhin8_nbr", "tothsun8_nbr",
                    "ownocc8_nbr", "occupied80_nbr")
temp_vars3 <- grep("^(bedrms|blt|no_hs_dipl|no_hs_dipl)", mile_data, value = TRUE)
new_cov_list <- c(new_covariates,temp_vars3)
data_RD <- mile_data
data_RD$z <- ifelse(data_RD$hrs_82 > 28.5, 1, 0)

formula_rd_fs <- as.formula(
  paste("npl2000 ~ z +", paste(new_cov_list, collapse = " + "))
)

model_first_stage <- lm(formula_rd_fs, data = data_RD)
summary(model_first_stage, se="hetero")

data_RD_between <- data_RD %>% filter(hrs_82 >= 16.5 & hrs_82 <= 40.5)

model_first_stage_with_bandwidth <- lm(formula_rd_fs, data = data_RD_between)
summary(model_first_stage_with_bandwidth, se="hetero")

#b
data_RD_plot <- data_RD %>%
  mutate(group = ifelse(hrs_82 < 28.5, "l", "r"))

ggplot(data_RD_plot, aes(x = hrs_82, y = npl2000)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(data = subset(data_RD_plot, group == "l"), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = subset(data_RD_plot, group == "r"), method = "lm", se = FALSE, color = "blue") +
  geom_vline(xintercept = 28.5, linetype = "dashed", color = "red") +
  labs( title = "Pr(NPL2000) and HRS_82", x = "HRS", y = "Pr(NPL2000)") +
  theme_minimal()

#c
ggplot(data_RD_plot, aes(x = hrs_82, y = mdvalhs9)) +
  geom_point(alpha = 0.3, size = 1.5) +
  geom_smooth(data = subset(data_RD_plot, group == "l"), method = "lm", se = FALSE, color = "blue") +
  geom_smooth(data = subset(data_RD_plot, group == "r"), method = "lm", se = FALSE, color = "blue") +
  geom_vline(xintercept = 28.5, linetype = "dashed", color = "red") +
  labs( title = "Pr(NPL2000) and HRS_82", x = "HRS", y = "Pr(NPL2000)") +
  theme_minimal()


# Q4
data_RD$pred_npl <- predict(model_first_stage)
formula_rd_ss <- as.formula(paste("mdvalhs0 ~ pred_npl +", paste(new_cov_list, collapse = " + ")))
model_second_stage <- lm(formula_rd_ss, data = data_RD)
summary(model_second_stage, se="hetero")

data_RD_between$pred_npl <- predict(model_first_stage_with_bandwidth)
model_second_stage_with_bandwidth <- lm(formula_rd_ss, data = data_RD_between)
summary(model_second_stage_with_bandwidth, se="hetero")

