###########################糖尿病####################
#######################################################
         #####################################
# 定义一个按照样本权重进行抽样的函数
set.seed(123)
weighted_bootstrap_sample <- function(data) {
  
  # 尝试进行bootstrap抽样，直到每个stratum都至少有两个PSUs
  while(TRUE) {
    sample.indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE, prob = data$WTMEC2YR)
    sampled_data <- data[sample.indices, , drop = FALSE]
    
    all_strata_have_at_least_two_PSUs <- TRUE
    
    stratums <- unique(sampled_data$SDMVSTRA)
    for (stratum in stratums) {
      stratum_data <- subset(sampled_data, SDMVSTRA == stratum)
      psus_in_stratum <- unique(stratum_data$SDMVPSU)
      
      if(length(psus_in_stratum) < 2) {
        all_strata_have_at_least_two_PSUs <- FALSE
        break
      }
    }
    
    if(all_strata_have_at_least_two_PSUs) {
      return(sampled_data)
    }
  }
}

# 定义计算中介效应占比的函数
calculate_proportion_mediated <- function(data, mediation_model) {
  group_NHANES_design_boot <- svydesign(data = data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
  lavaan_fit_bootstrap <- sem(mediation_model, data = data, estimator="MLR")
  survey_fit_bootstrap <- lavaan.survey(lavaan.fit = lavaan_fit_bootstrap, survey.design = group_NHANES_design_boot)
  param_estimates_bootstrap <- parameterEstimates(survey_fit_bootstrap)
  indirect_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "indirect", "est"]
  total_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "total", "est"]
  
  if (total_effect_bootstrap != 0) {
    return(indirect_effect_bootstrap / total_effect_bootstrap)
  } else {
    return(NA)
  }
}
library(ggplot2)
set.seed(123)
# Initialize dataframe to store median and confidence intervals for the diabetes group
results_df_diabetes <- data.frame(DiabetesGroup = character(),
                                  MedianProportion = numeric(),
                                  LowerBound = numeric(),
                                  UpperBound = numeric(),
                                  stringsAsFactors = FALSE)

# Initialize a new dataframe to save each bootstrap result
bootstrap_results_df_diabetes <- data.frame(DiabetesGroup = character(),
                                            ProportionMediated = numeric(),
                                            stringsAsFactors = FALSE)

# Subset data for diabetes group equal to 1
group_data <- subset(paper_data_imputed_diabetes, Diabetes == 1)
bootstraps <- 1000
bootstrap_proportions <- numeric(bootstraps)

for (i in 1:bootstraps) {
  bootstrap_data <- weighted_bootstrap_sample(group_data)
  proportion_mediated <- calculate_proportion_mediated(bootstrap_data, mediation_model)
  bootstrap_proportions[i] <- proportion_mediated
  # Save each bootstrap result
  bootstrap_results_df_diabetes <- rbind(bootstrap_results_df_diabetes, data.frame(DiabetesGroup = "1",
                                                                                   ProportionMediated = proportion_mediated))
}

# Calculate and save summary statistics
alpha <- 0.05
median_proportion <- median(bootstrap_proportions, na.rm = TRUE)
lower_bound <- quantile(bootstrap_proportions, alpha / 2, na.rm = TRUE)
upper_bound <- quantile(bootstrap_proportions, 1 - alpha / 2, na.rm = TRUE)
results_df_diabetes <- rbind(results_df_diabetes, data.frame(DiabetesGroup = "1",
                                                             MedianProportion = median_proportion,
                                                             LowerBound = lower_bound,
                                                             UpperBound = upper_bound))
# 定义y轴的最小值和最大值
y_min_limit <- -0.1
y_max_limit <- 0.5

# 调整LowerBound和UpperBound，如果它们超出了y轴的限制
results_df_diabetes$LowerBound1 <- pmax(results_df_diabetes$LowerBound, y_min_limit)
results_df_diabetes$UpperBound1 <- pmin(results_df_diabetes$UpperBound, y_max_limit)
# 结合中位数、置信区间和散点的可视化
my_plot <- ggplot() +
  geom_jitter(data = bootstrap_results_df_diabetes, aes(x = DiabetesGroup, y = ProportionMediated), 
              width = 0.15, height = 0, alpha = 0.5, color = "black") +
  geom_pointrange(data = results_df_diabetes, aes(x = DiabetesGroup, y = MedianProportion, 
                                                  ymin = LowerBound1, ymax = UpperBound1), 
                  color = "red", size = 1) +
  scale_y_continuous(limits = c(-0.1, 0.5)) +  # 设置x轴的极限
  xlab("Remember Group") +
  ylab("Proportion Mediated") +
  ggtitle("Proportion Mediated: Bootstrap Samples with Median and Confidence Intervals by Remember Group") +
  theme_minimal()

# 输出图像
print(my_plot)
###########################total population#####################
#######################################
# 定义一个按照样本权重进行抽样的函数
weighted_bootstrap_sample <- function(data) {
  
  # 尝试进行bootstrap抽样，直到每个stratum都至少有两个PSUs
  while(TRUE) {
    sample.indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE, prob = data$WTMEC2YR)
    sampled_data <- data[sample.indices, , drop = FALSE]
    
    all_strata_have_at_least_two_PSUs <- TRUE
    
    stratums <- unique(sampled_data$SDMVSTRA)
    for (stratum in stratums) {
      stratum_data <- subset(sampled_data, SDMVSTRA == stratum)
      psus_in_stratum <- unique(stratum_data$SDMVPSU)
      
      if(length(psus_in_stratum) < 2) {
        all_strata_have_at_least_two_PSUs <- FALSE
        break
      }
    }
    
    if(all_strata_have_at_least_two_PSUs) {
      return(sampled_data)
    }
  }
}

# 定义计算中介效应占比的函数
calculate_proportion_mediated <- function(data, mediation_model) {
  group_NHANES_design_boot <- svydesign(data = data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
  lavaan_fit_bootstrap <- sem(mediation_model, data = data, estimator="MLR")
  survey_fit_bootstrap <- lavaan.survey(lavaan.fit = lavaan_fit_bootstrap, survey.design = group_NHANES_design_boot)
  param_estimates_bootstrap <- parameterEstimates(survey_fit_bootstrap)
  indirect_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "indirect", "est"]
  total_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "total", "est"]
  
  if (total_effect_bootstrap != 0) {
    return(indirect_effect_bootstrap / total_effect_bootstrap)
  } else {
    return(NA)
  }
}
library(ggplot2)
set.seed(123)
mediation_model <- paste0('
    # Mediation effect with added covariates for the mediator
    depression.total ~ a * ', var, ' + Stroke + Diabetes + Hypertension +  Heart 
    
    # Direct effect with covariates on the outcome
    overall_cognitive_score ~ c * ', var, ' + b * depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR +  Diabetes + Hypertension + Heart + Marital
    
    # Indirect effect
    indirect := a * b
    
    # Total effect
    total := c + indirect
  ')
# Initialize a data frame to store results
results_df <- data.frame(MedianProportion = numeric(),
                         LowerBound = numeric(),
                         UpperBound = numeric(),
                         stringsAsFactors = FALSE)

# Initialize a new data frame to save each bootstrap's results
bootstrap_results_df <- data.frame(ProportionMediated = numeric(),
                                   stringsAsFactors = FALSE)

# Specify the number of bootstrap iterations
bootstraps <- 1000  # For demonstration, using 1000 iterations; adjust as necessary for your analysis

bootstrap_proportions <- numeric(bootstraps)

for (i in 1:bootstraps) {
  bootstrap_data <- weighted_bootstrap_sample(paper_data_imputed) # Use the entire dataset
  proportion_mediated <- calculate_proportion_mediated(bootstrap_data, mediation_model)
  bootstrap_proportions[i] <- proportion_mediated
  # Save each bootstrap result
  bootstrap_results_df <- rbind(bootstrap_results_df, data.frame(ProportionMediated = proportion_mediated))
}

# After the bootstrap loop, you can calculate median, lower, and upper bounds for the total dataset
median_proportion <- median(bootstrap_proportions, na.rm = TRUE)
conf_interval <- quantile(bootstrap_proportions, c(0.025, 0.975), na.rm = TRUE)
lower_bound <- conf_interval[1]
upper_bound <- conf_interval[2]

# Save the summary results to the results_df
results_df <- rbind(results_df, data.frame(MedianProportion = median_proportion,
                                           LowerBound = lower_bound,
                                           UpperBound = upper_bound))

# 定义y轴的最小值和最大值
y_min_limit <- -0.1
y_max_limit <- 0.3

# 调整LowerBound和UpperBound，如果它们超出了y轴的限制
results_df$LowerBound1 <- pmax(results_df$LowerBound, y_min_limit)
results_df$UpperBound1 <- pmin(results_df$UpperBound, y_max_limit)
# 结合中位数、置信区间和散点的可视化
my_plot <- ggplot() +
  geom_jitter(data = bootstrap_results_df, aes(x = factor(0.5), y = ProportionMediated), 
              width = 0.15, height = 0, alpha = 0.5, color = "black") +
  geom_pointrange(data = results_df, aes(x = factor(0.5), y = MedianProportion, 
                                         ymin = LowerBound1, ymax = UpperBound1), 
                  color = "red", size = 1) +
  scale_y_continuous(limits = c(-0.1, 0.3)) +  # 设置x轴的极限
  xlab("Overall Analysis") +
  ylab("Proportion Mediated") +
  ggtitle("Proportion Mediated: Bootstrap Samples with Median and Confidence Intervals by Remember Group") +
  theme_minimal()

# 输出图像
print(my_plot)
