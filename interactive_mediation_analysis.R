library(lavaan)
library(survey)
library(lavaan)
library(survey)

# 假设数据已经载入到paper_data_imputed_diabetes
# 预先计算抑郁与糖尿病的交互项
paper_data_imputed$Depression_Diabetes_Interaction <- paper_data_imputed$depression.total * paper_data_imputed$Diabetes
NHANES_design <- svydesign(data = paper_data_imputed, 
                           ids = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           nest = TRUE, 
                           weights = ~WTMEC2YR, 
                           survey.lonely.psu = "adjust")
variables <- c("LBXVD3MS")
for (var in variables) {
  # 构建模型字符串
  mediation_model <- paste0('
        # 中介模型，包括交互项对中介变量和结果变量的影响
        depression.total ~ a1 * ', var, ' + a2 * Diabetes + a3 * Depression_Diabetes_Interaction + Stroke + Hypertension + Heart
        overall_cognitive_score ~ c1 * ', var, ' + c2 * Diabetes + c3 * Depression_Diabetes_Interaction + b1 * depression.total + Drink + Stroke + Education + Age.group + Gender + INDFMPIR + Hypertension + Heart + Marital
        
        indirect1 := a1 * b1 # 传统的间接效应
        indirect2 := a3 * b1 # 交互项的间接效应
        total_indirect := indirect1 + indirect2 # 总的间接效应
        total := c1 + total_indirect # 总效应，包括直接效应和所有间接效应
    ')
  
  # 拟合SEM模型
  lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator = "MLR")
  summary(lavaan_fit)
  
  # 创建并拟合调查模型
 
  survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
  summary(survey_fit)
}
# 使用parameterEstimates()函数提取参数估计
parameters <- parameterEstimates(survey_fit)
