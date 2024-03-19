#################################
##########################unweihgted中介分析
##########################################################################


library(lavaan.survey)
library(lavaan)

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 对每个变量执行中介分析
for (var in variables) {
  mediation_model <- paste0('
    # Mediation effect
    depression.total ~ a * ', var, '
    
    # Direct effect
    overall_cognitive_score ~ c * ', var, ' + b * depression.total
    
    # Indirect effect
    indirect := a * b
    
    # Total effect
    total := c + indirect
  ')
  
  # 使用不同的估计器重新拟合模型（例如，MLR）
  lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")
  summary(lavaan_fit)
  survey_fit <- lavaan_fit
  
  # 获取参数估计
  param_estimates <- parameterEstimates(survey_fit)
  
  # 提取直接效应的统计数据
  direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
  
  # 提取间接和总效应的统计数据
  indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
  total_effect <- param_estimates[param_estimates$lhs == "total", ]
  
  # 显示保留更多小数位数的估计值
  print(paste0("For variable: ", var))
  
  print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
  
  print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
  
  print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
  
  # 计算中介效应占比
  if (total_effect$est != 0) {
    proportion_mediated <- indirect_effect$est / total_effect$est
    print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
  } else {
    print("Total effect is zero, cannot calculate proportion mediated.")
  }
  # 提取直接效应的置信区间
  direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
  
  # 提取间接效应和总效应的置信区间
  indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
  total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
  
  # 打印置信区间
  print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
}
####################多变量############

library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 对每个变量执行中介分析
for (var in variables) {
  mediation_model <- paste0('
    # Mediation effect with added covariates for the mediator
    depression.total ~ a * ', var, ' + Stroke + Diabetes + Hypertension +  Heart
    
    # Direct effect with covariates on the outcome
    overall_cognitive_score ~ c * ', var, ' + b * depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Heart
    
    # Indirect effect
    indirect := a * b
    
    # Total effect
    total := c + indirect
  ')
  
  # 使用不同的估计器重新拟合模型（例如，MLR）
  lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")
  summary(lavaan_fit)
  survey_fit <- lavaan_fit
  
  # 获取参数估计
  param_estimates <- parameterEstimates(survey_fit)
  
  # 提取直接效应的统计数据
  direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
  
  # 提取间接和总效应的统计数据
  indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
  total_effect <- param_estimates[param_estimates$lhs == "total", ]
  
  # 显示保留更多小数位数的估计值
  print(paste0("For variable: ", var))
  
  print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
  
  print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
  
  print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
  
  # 计算中介效应占比
  if (total_effect$est != 0) {
    proportion_mediated <- indirect_effect$est / total_effect$est
    print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
  } else {
    print("Total effect is zero, cannot calculate proportion mediated.")
  }
  # 提取直接效应的置信区间
  direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
  
  # 提取间接效应和总效应的置信区间
  indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
  total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
  
  # 打印置信区间
  print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
}
#summary(survey_fit , fit.measures=TRUE)
# 获取详细参数估计
#########################单变量亚组###################################
######################################################
#####Age.group
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组
age_groups <- c(0, 1)

# 对每个变量和每个年龄组执行中介分析
for (var in variables) {
  for (age_group in age_groups) {
    
    # 选取当前年龄组的数据
    group_data <- subset(paper_data_imputed, Age.group == age_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in age group: ", age_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
######################多变量
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组
age_groups <- c(0, 1)

# 对每个变量和每个年龄组执行中介分析
for (var in variables) {
  for (age_group in age_groups) {
    
    # 选取当前年龄组的数据
    group_data <- subset(paper_data_imputed, Age.group == age_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '+ Stroke + Diabetes + Hypertension +  Heart 
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total + Drink + Stroke + Education + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Heart
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in age group: ", age_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
###########################################亚组分析 gender
#####gender
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组
# 定义年龄组为 Gender
gender_groups <- unique(paper_data_imputed$Gender)

# 对每个变量和每个性别组进行中介分析
for (var in variables) {
  for (gender_group in gender_groups) {
    
    # 选取当前性别组的数据
    group_data <- subset(paper_data_imputed, Gender == gender_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in gender group: ", gender_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
###############################多变量################################
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组
# 定义年龄组为 Gender
gender_groups <- unique(paper_data_imputed$Gender)

# 对每个变量和每个性别组进行中介分析
for (var in variables) {
  for (gender_group in gender_groups) {
    
    # 选取当前性别组的数据
    group_data <- subset(paper_data_imputed, Gender == gender_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '+ Stroke + Diabetes + Hypertension +  Heart 
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total + Stroke + Drink +  Education + Age.group + INDFMPIR + Marital + Diabetes + Hypertension + Heart
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in gender group: ", gender_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
colnames(paper_data_imputed)
###################################################
###########################################亚组分析 diabete
#####diabetes
library(lavaan.survey)
library(lavaan)
# 创建一个新的数据框来存储修改
paper_data_imputed_diabetes <- paper_data_imputed


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组为 Diabetes
diabetes_groups <- unique(paper_data_imputed_diabetes$Diabetes)

# 对每个变量和每个糖尿病组进行中介分析
for (var in variables) {
  for (diabetes_group in diabetes_groups) {
    
    # 选取当前糖尿病组的数据
    group_data <- subset(paper_data_imputed_diabetes, Diabetes == diabetes_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in gender group: ", diabetes_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
#########################多变量
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组为 Diabetes
diabetes_groups <- unique(paper_data_imputed_diabetes$Diabetes)

# 对每个变量和每个糖尿病组进行中介分析
for (var in variables) {
  for (diabetes_group in diabetes_groups) {
    
    # 选取当前糖尿病组的数据
    group_data <- subset(paper_data_imputed_diabetes, Diabetes == diabetes_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '+ Stroke + Hypertension +  Heart 
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total +   Drink + Stroke + Education + Age.group + Gender + INDFMPIR + Marital + Hypertension + Heart
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in gender group: ", diabetes_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}

###########################################亚组分析 hypertension
#####hypertension
library(lavaan.survey)
library(lavaan)


# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组为 Hypertension
hypertension_groups <- unique(paper_data_imputed$Hypertension)

# 对每个变量和每个高血压组进行中介分析
for (var in variables) {
  for (hypertension_group in hypertension_groups) {
    
    # 选取当前高血压组的数据
    group_data <- subset(paper_data_imputed, Hypertension == hypertension_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust", survey.adjust.domain.lonely=TRUE)
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in hypertension group: ", hypertension_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
################################多变量
library(lavaan.survey)
library(lavaan)

NES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义年龄组为 Hypertension
hypertension_groups <- unique(paper_data_imputed$Hypertension)

# 对每个变量和每个高血压组进行中介分析
for (var in variables) {
  for (hypertension_group in hypertension_groups) {
    
    # 选取当前高血压组的数据
    group_data <- subset(paper_data_imputed, Hypertension == hypertension_group)
    
    mediation_model <- paste0('
      # Mediation effect
      depression.total ~ a * ', var, '+ Stroke + Diabetes +  Heart
      
      # Direct effect
      overall_cognitive_score ~ c * ', var, ' + b * depression.total + Drink + Stroke + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Heart
      
      # Indirect effect
      indirect := a * b
      
      # Total effect
      total := c + indirect
    ')
    
    # 使用不同的估计器重新拟合模型（例如，MLR）
    lavaan_fit <- sem(mediation_model, data = group_data, estimator="MLR")
    summary(lavaan_fit)
    
    # 这里我们需要使用修改后的数据和设计创建一个新的survey设计
    group_NHANES_design <- svydesign(data = group_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust", survey.adjust.domain.lonely=TRUE)
    
    survey_fit <- lavaan_fit
    
    # 获取参数估计
    param_estimates <- parameterEstimates(survey_fit)
    
    # 提取直接效应的统计数据
    direct_effect <- param_estimates[param_estimates$op == "~" & param_estimates$rhs == var & param_estimates$lhs == "overall_cognitive_score", ]
    
    # 提取间接和总效应的统计数据
    indirect_effect <- param_estimates[param_estimates$lhs == "indirect", ]
    total_effect <- param_estimates[param_estimates$lhs == "total", ]
    
    # 显示保留更多小数位数的估计值
    print(paste0("For variable: ", var, " in hypertension group: ", hypertension_group))
    
    print(paste("Direct effect: ", format(round(direct_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(direct_effect$se, 10), nsmall = 10), ", z-value: ", format(round(direct_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(direct_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Indirect effect: ", format(round(indirect_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(indirect_effect$se, 10), nsmall = 10), ", z-value: ", format(round(indirect_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(indirect_effect$pvalue, 10), nsmall = 10)))
    
    print(paste("Total effect: ", format(round(total_effect$est, 10), nsmall = 10), ", Std.Err: ", format(round(total_effect$se, 10), nsmall = 10), ", z-value: ", format(round(total_effect$z, 10), nsmall = 10), ", P(>|z|): ", format(round(total_effect$pvalue, 10), nsmall = 10)))
    
    # 计算中介效应占比
    if (total_effect$est != 0) {
      proportion_mediated <- indirect_effect$est / total_effect$est
      print(paste("Proportion mediated: ", format(round(proportion_mediated, 10), nsmall = 10)))
    } else {
      print("Total effect is zero, cannot calculate proportion mediated.")
    }
    # 提取直接效应的置信区间
    direct_effect_ci <- param_estimates[param_estimates$lhs == "overall_cognitive_score" & param_estimates$rhs == var, c("ci.lower", "ci.upper")]
    
    # 提取间接效应和总效应的置信区间
    indirect_effect_ci <- param_estimates[param_estimates$lhs == "indirect", c("ci.lower", "ci.upper")]
    total_effect_ci <- param_estimates[param_estimates$lhs == "total", c("ci.lower", "ci.upper")]
    
    # 打印置信区间
    print(paste("Direct effect CI: [", format(round(direct_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(direct_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Indirect effect CI: [", format(round(indirect_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(indirect_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
    print(paste("Total effect CI: [", format(round(total_effect_ci[1, "ci.lower"], 10), nsmall = 10), ", ", format(round(total_effect_ci[1, "ci.upper"], 10), nsmall = 10), "]"))
  }
}
