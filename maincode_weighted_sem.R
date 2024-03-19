rm(list = ls());gc()
#### 0.准备好环境 ####
library(gtsummary)
library(survey)
library(haven)
library(tableone)
library(plyr)
library(dplyr) 
library(tidyverse)
library(arsenal) 

setwd("E:/R2023/nhanes/R_project/nhanes_data/exercise") #需要转换为自己的数据读取路径

#### 一. 定位数据模块和变量，获取源数据 ####
##### 1.1 DEMO-人口学数据提取 #####
demo.g <- read_xpt("2011-2012/Demographics/demo_g.xpt")#参见上述设置默认路径，文件名称后缀不同"g,h"
demo.h <- read_xpt("2013-2014/Demographics/demo_h.xpt")#参见上述设置默认路径，文件名称后缀不同"g,h"
colnames(demo.g)
colnames(demo.h)
### 1.2 提取研究所需要的变量
# 年龄-RIDAGEYR; 性别-RIAGENDR; 种族-RIDRETH3; 教育程度-DMDEDUC2; 贫困程度-INDFMPIR;
#补充了DMDMARTL：婚姻状态，RIDEXMON：季节
demo.data.file <- dplyr::bind_rows(list(demo.g, demo.h))
demo.data <- demo.data.file[,c('SEQN', 'RIDAGEYR', 'RIAGENDR', 'RIDRETH3', 'DMDEDUC2', 'INDFMPIR','RIDEXMON',"DMDMARTL")]

##### 1.2 SMQ-吸烟数据提取 #####
### 1.1 提取 Component文件
smq.g <- read_xpt("2011-2012/Questionnaire/smq_g.xpt")
smq.h <- read_xpt("2013-2014/Questionnaire/smq_h.xpt")

### 2. 提取研究所需要的变量
# 是否吸烟至少100支-SMQ020; 现在是否吸烟-SMQ040; 多久前开始戒烟-SMQ050Q;
# 多久前开始戒烟的时间单位（天、周、月、年）-SMQ050U;
smq.data.file <- dplyr::bind_rows(list(smq.g, smq.h))
smq.data <- smq.data.file[,c('SEQN', 'SMQ020', 'SMQ040', 'SMQ050Q', 'SMQ050U')]

##### 1.3 ALQ-饮酒数据提取 ######
### 1.提取 Component文件
alq.g <- read_xpt("2011-2012/Questionnaire/alq_g.xpt")
alq.h <- read_xpt("2013-2014/Questionnaire/alq_h.xpt")

### 2.提取研究所需要的变量
# 每年至少喝12杯酒-ALQ101
# 一生中至少喝过12次酒-ALQ110；
# 过去12个月多久喝一次酒-ALQ120Q
# 过去12个月饮酒频率的单位（周，月，年）-ALQ120U;

alq.data.file <- dplyr::bind_rows(list(alq.g, alq.h))
alq.data <- alq.data.file[,c('SEQN', 'ALQ101', 'ALQ110', 'ALQ120Q', 'ALQ120U')]

##### 1.4 BMX-BMI、腰围数据提取 #####
### 1.提取 Component文件
bmx.g <- read_xpt("2011-2012/Examination/bmx_g.xpt") #注意是 Examination 的类别
bmx.h <- read_xpt("2013-2014/Examination/bmx_h.xpt") #注意是 Examination 的类别

### 2.提取研究所需要的变量 BMI-BMXBMI; 腰围-BMXWAIST
bmx.data.file <- dplyr::bind_rows(list(bmx.g, bmx.h))
bmx.data <- bmx.data.file[,c('SEQN', 'BMXBMI', 'BMXWAIST')]

##### 1.7 CFQ-认知数据提取 #####
### 1.提取 Component文件
cfq.g <- read_xpt('2011-2012/Questionnaire/cfq_g.xpt')
cfq.h <- read_xpt('2013-2014/Questionnaire/cfq_h.xpt')

### 2.提取研究所需要的变量
#CERAD 词汇第1次即刻记忆测验	CFDCST1
#CERAD 词汇第2次即刻记忆测验	CFDCST2
#CERAD 词汇第3次即刻记忆测验	CFDCST3
#CERAD 3次词汇即刻记忆测验总分	CERAD_1+CERAD_2+CERAD_3
#CERAD 延迟回忆评分	CFDCSR
#语言流畅性评分Animal Fluency	CFDAST
#数字符号替代测试(DSST)	CFDDS

cfq.data.file <- dplyr::bind_rows(list(cfq.g, cfq.h))
cfq.data <- cfq.data.file[,c('SEQN', 'CFDCST1', 'CFDCST2', 'CFDCST3',
                             'CFDCSR', 'CFDAST', 'CFDDS')]

###serum vitamin D数据提取
### 1.提取 Component文件
vig.g <- read_xpt('2011-2012/Laboratory/VID_G.XPT')
vig.h <- read_xpt('2013-2014/Laboratory/VID_H.XPT')
colnames(vig.g)
colnames(vig.h)
### 2.提取研究所需要的变量
#25OHD2+25OHD3 (nmol/L)
#25-羟基维生素D2 (nmol/L)
#25-羟基维生素D3 (nmol/L)
#epi-25OHD3 (nmol/L)

vig.data.file <- dplyr::bind_rows(list(vig.g, vig.h))
colnames(vig.data.file)
vig.data <- vig.data.file[,c('SEQN', 'LBXVIDMS','LBDVIDLC', 'LBXVD2MS','LBDVD2LC', 'LBXVD3MS','LBDVD3LC',
                             'LBXVE3MS',"LBDVE3LC")]

############抑郁评分和overall impairment of the depressive symptoms
### 1.提取 Component文件
dpq.g <- read_xpt('2011-2012/Questionnaire/DPQ_G.XPT')
dpq.h <- read_xpt('2013-2014/Questionnaire/DPQ_H.XPT')
colnames(dpq.g)
colnames(dpq.h)
### 2.提取研究所需要的变量
#PHQ-9 抑郁评分"DPQ010" "DPQ020" "DPQ030" "DPQ040" "DPQ050" 
#"DPQ060" "DPQ070" "DPQ080" "DPQ090"
#the overall impairment of the depressive symptoms "DPQ100"

dpq.data.file <- dplyr::bind_rows(list(dpq.g, dpq.h))
dpq.data <- dpq.data.file[,c("SEQN", "DPQ010", "DPQ020", "DPQ030", "DPQ040", 
                             "DPQ050", "DPQ060", "DPQ070", "DPQ080", "DPQ090", "DPQ100")]
##### 1.4 DIQ-糖尿病数据提取 #####
### 1.4.1 提取 Component文件
# NHANES官网链接：https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/DIQ_D.htm
#https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/DIQ_E.htm
diq.g <- read_xpt('2011-2012/Questionnaire/DIQ_G.XPT')
diq.h <- read_xpt('2013-2014/Questionnaire/DIQ_H.XPT')

colnames(diq.g)
colnames(diq.h)

### 1.4.2 提取研究所需要的变量
# 糖尿病诊断-DIQ010;
diq.data.file <- dplyr::bind_rows(list(diq.g, diq.h))
dim(diq.data.file)
diq.data <- diq.data.file[,c("SEQN", "DIQ010")]
# View(diq.data)#查看数据
##### 1.5 MCQ-慢病数据提取 #####
mcq.g <- read_xpt('2011-2012/Questionnaire/MCQ_G.XPT')
mcq.h <- read_xpt('2013-2014/Questionnaire/MCQ_H.XPT')

colnames(mcq.g)
colnames(mcq.h)

### 1.5.2 提取研究所需要的变量
mcq.data.file <- dplyr::bind_rows(list(mcq.g, mcq.h))
dim(mcq.data.file)
mcq.data <- mcq.data.file[,c("SEQN", "MCQ084","MCQ380","MCQ160B", "MCQ160G", "MCQ160K", "MCQ160C", "MCQ220", "MCQ160F", "MCQ230A", "MCQ230B", "MCQ230C","MCQ160D","MCQ160E")]
# View(mcq.data)#查看数据
##### 1.6 BPQ-血压数据提取 #####
### 1.6.1 提取 Component文件
# NHANES官网链接：https://wwwn.cdc.gov/Nchs/Nhanes/2005-2006/BPQ_D.htm
#https://wwwn.cdc.gov/Nchs/Nhanes/2007-2008/BPQ_E.htm
bpq.g <- read_xpt('2011-2012/Questionnaire/bpq_G.XPT')
bpq.h <- read_xpt('2013-2014/Questionnaire/bpq_H.XPT')

colnames(bpq.g)
colnames(bpq.h)

### 1.6.2 提取研究所需要的变量
# 高血压诊断-BPQ020;
bpq.data.file <- dplyr::bind_rows(list(bpq.g, bpq.h))
dim(bpq.data.file)
bpq.data <- bpq.data.file[,c("SEQN", "BPQ020")]
# View(bpq.data)#查看数据
####################提取睡眠障碍
slq.g <- read_xpt('2011-2012/Questionnaire/SLQ_G.XPT')
slq.h <- read_xpt('2013-2014/Questionnaire/SLQ_H.XPT')

colnames(slq.g)
colnames(slq.h)


### 1.6.2 提取研究所需要的变量
slq.data.file <- dplyr::bind_rows(list(slq.g, slq.h))
dim(slq.data.file)
slq.data <- slq.data.file[,c("SEQN", "SLQ050", "SLQ060")]
# ##################################################################
# #########################血脂异常的提取
##########Hypercholesterolemia#########加入胆固醇相关的问卷
bpq.lipi.g <- read_xpt('2011-2012/Questionnaire/bpq_G.XPT')
bpq.lipi.h <- read_xpt('2013-2014/Questionnaire/bpq_H.XPT')

colnames(bpq.lipi.g)
bpq.lipi.data.file <- dplyr::bind_rows(list(bpq.lipi.g, bpq.lipi.h))
bpq.lipi.data <- bpq.lipi.data.file[,c("SEQN", "BPQ080","BPQ090D")]
glimpse(bpq.lipi.data)
#上面这两个变量取值为1的赋值为1，取值为2的赋值为零，其余的赋值为na
library(tidyverse)

bpq.lipi.data <- bpq.lipi.data %>%
  mutate(dyslipidemia = case_when(
    (BPQ080 == 1 | BPQ090D == 1) ~ 1,
    (BPQ080 != 1 | BPQ090D != 1) & (BPQ080 == 2 | BPQ090D == 2) ~ 0,
    TRUE ~ NA_real_
  ))
# ###########
table(bpq.lipi.data$dyslipidemia, useNA = "always")

lipi.data <- bpq.lipi.data %>% dplyr::select(SEQN,dyslipidemia)

##############################
# 找到权重变量
weight.data <- demo.data.file[,c("SEQN", "WTMEC2YR")]
weight.data$WTMEC2YR <- weight.data$WTMEC2YR/2 #权重的计算方式，需要按 Cycle 进行平均

##### 2.2 复杂抽样的其他变量-DEMO #####
survey.design.data <- demo.data.file[,c("SEQN", "SDMVPSU", "SDMVSTRA")]

#### 3.合并上述所有数据（把列拼接起来）-Output #### 
output <- plyr::join_all(list(demo.data, weight.data, survey.design.data, 
                              bmx.data, diq.data, mcq.data, lipi.data,
                              bpq.data,smq.data, alq.data,cfq.data,dpq.data,vig.data,slq.data), by='SEQN', type='left')


##### 3.2 针对合并后的数据进行质控，确定人数能对得上 #####
# 1.根据 Table1 进行质控
data.age.60 <- output %>%
  filter(RIDAGEYR >= 60)

dim(data.age.60)

# 2. 确认下权重
#比对权重参考已发表文献 PMID: 30326796
weight.non.na.index <- which(!is.na(data.age.60$WTMEC2YR))
sum(data.age.60$WTMEC2YR[weight.non.na.index]) # 计算权重-59659564 ✅

#### 4.根据文章的筛选策略进行初步筛选（仅根据原始数据） ####
paper.data <- output %>%
  filter(RIDAGEYR >= 60,
         !is.na(CFDCSR),    # CERAD 延迟回忆评分
         !is.na(CFDAST),    # 语言流畅性评分Animal Fluency
         !is.na(CFDDS),     # 数字符号替代测试(DSST)
         !is.na(DPQ010), !is.na(DPQ020), !is.na(DPQ030), !is.na(DPQ040),
         !is.na(DPQ050), !is.na(DPQ060), !is.na(DPQ070), !is.na(DPQ080),
         !is.na(DPQ090), !is.na(LBXVD3MS), 
         !is.na(CFDCST1),!is.na(CFDCST2),!is.na(CFDCST3))
dim(paper.data)

# 找到具有缺失值的列
cols_with_na <- paper.data %>%
  summarise_all(function(x) any(is.na(x))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na) %>%
  pull(column)

# 输出具有缺失值的列名
print(cols_with_na)

# 计算每列的缺失值占比并显示
missing_proportions <- paper.data %>%
  summarise_all(function(x) mean(is.na(x))) %>%
  gather(key = "column", value = "proportion")

missing_proportions$proportion <- missing_proportions$proportion * 100

# 输出每列的缺失值占比
print(missing_proportions)

####衍生得到相应的变量，同步衍生 ####
##### 5.1 性别-Sex#####
paper.data$Sex <- ifelse(paper.data$RIAGENDR == 1, 'male', 'female')
##### 5.2 年龄-age.group #####
paper.data$age.group <- ifelse(paper.data$RIDAGEYR >= 60 & paper.data$RIDAGEYR < 69, '60-69 years',
                               ifelse(paper.data$RIDAGEYR >=70 & paper.data$RIDAGEYR < 79, '70-79 years',
                                      '80+ years'))
##### 5.4 复杂转换-饮酒 alq.group #####
# 衍生结果为：Non-drinker, 1-5 drinks/month, 5-10 drinks/month, 10+ drinks/month
# https://wwwn.cdc.gov/Nchs/Nhanes/2011-2012/ALQ_G.htm#ALQ120U （课程使用的paper）
# ALQ120Q, 具体的数值;  777:refused, 999:don't know
# ALQ120U, 1:week, 2:month, 3:year, 7:refused, 9:don't know
# 统一单位为月
# week-month: *4; year-month:/12
ori.alq.unit <- paper.data$ALQ120U
table(ori.alq.unit)
trans.unit.month <- ifelse(ori.alq.unit == 1, 4, 
                           ifelse(ori.alq.unit == 3, 1/12, 
                                  ifelse(ori.alq.unit == 7|ori.alq.unit == 9, NA, 1)))
paper.data$trans.unit.month <- trans.unit.month
# View(paper.data)

# 统一数值为月饮酒量-quantity
ori.alq.quantity <- paper.data$ALQ120Q
trans.quantity.month <- ifelse(ori.alq.quantity >= 0,  
                               ori.alq.quantity * trans.unit.month, NA)
paper.data$trans.quantity.month <- trans.quantity.month
# 将饮酒量的结果划分为4档：Non-drinker, 1-5 drinks/month, 5-10 drinks/month, 10+ drinks/month
# 首先根据转化得到的trans.quantity.month，将饮酒量划分3档
# 利用 paper.data$ALQ101
alq101 <- paper.data$ALQ101
trans.quantity.month.factor <- ifelse(trans.quantity.month >=1 & trans.quantity.month <5, '1-5 drinks/month',
                                      ifelse(trans.quantity.month >=5 & trans.quantity.month <10, '5-10 drinks/month',
                                             ifelse(trans.quantity.month >=10, '10+ drinks/month', 'wait')))

paper.data$trans.quantity.month.factor <- trans.quantity.month.factor

ddply(paper.data, .(ALQ101, trans.quantity.month.factor), summarise, n = length(SEQN))

# ALQ101 回答是1，但是 trans.quantity.month.factor 没有具体的值，则将 trans.quantity.month.factor 取值为1
index.1 <- which((trans.quantity.month.factor=='wait' | is.na(trans.quantity.month.factor)) & alq101 == 1) #844
trans.quantity.month.factor[index.1] <- '1-5 drinks/month'

paper.data$trans.quantity.month.factor <- trans.quantity.month.factor

index.less.1 <- which(alq101 == 2)
trans.quantity.month.factor[index.less.1] <- 'Non-drinker'

table(trans.quantity.month.factor)
# 根据是否饮酒补充 Non-drinker
paper.data$alq.group <- trans.quantity.month.factor
missing_alq_group <- sum(is.na(paper.data$alq.group))
# 使用table函数计算alq.group列的每个分类的频数，包括NA值
alq_group_counts <- table(paper.data$alq.group, useNA = "ifany")

# 打印结果
print(alq_group_counts)


##### 认知评分 #####
#CERAD 词汇第1次即刻记忆测验	CFDCST1
#CERAD 词汇第2次即刻记忆测验	CFDCST2
#CERAD 词汇第3次即刻记忆测验	CFDCST3
#CERAD 3次词汇即刻记忆测验总分	CERAD_1+CERAD_2+CERAD_3

CERAD.total <- apply(paper.data[,c('CFDCST1', 'CFDCST2', 'CFDCST3','CFDCSR')], 1, sum)
paper.data$CERAD.total <- CERAD.total
######################################################
colnames(paper.data)
# 计算每个认知测试的Z分数
paper.data <- paper.data %>%
  mutate(z_CERAD = scale(CERAD.total),
         z_Animal_Fluency = scale(CFDAST),
         z_DSST = scale(CFDDS))

# 计算整体认知分数，即三个Z分数的平均值
paper.data <- paper.data %>%
  mutate(overall_cognitive_score = rowMeans(select(paper.data, starts_with("z_"))))

# 确定整体认知分数在最低四分之一的个体
lowest_quartile_threshold <- quantile(paper.data$overall_cognitive_score, 0.25)
paper.data <- paper.data %>%
  mutate(low_global_cognition = ifelse(overall_cognitive_score <= lowest_quartile_threshold, "Yes", "No"))

##### smoke.group #####
# 本次衍生采取常规简单的定义
# SMQ020:Smoked at least 100 cigarettes in life, 1:Yes, 2:No
# SMQ040:Do you now smoke cigarettes, 1:Every day, 2:Some days	

# 使用 dplyr 的 mutate 函数可以 Create, modify, and delete 列，下面使用 mutate 衍生 smoke group 变量
# 波浪号 ~ 前面是条件，后面是满足这个条件的取值
paper.data <- mutate(paper.data, smoke.group = case_when(
  SMQ020 == 2 ~ 'Never smoker',
  SMQ020 == 1 & SMQ040 == 3 ~ 'Former smoker',
  SMQ020 == 1 & SMQ040 <= 2 ~ 'Current smoker'
))


##### education.attainment #####
# 仅仅是单个变量的转换，可以使用 recode_factor，注意，原先的取值是数值或者中文的结果，要使用``标注
# 要注意 7\9\77\99这类的取值，不处理的话直接是 NA, warning 提示：Unreplaced values treated as NA as `.x` is not compatible.

education.attainment <- recode_factor(paper.data$DMDEDUC2, 
                                      `1` = 'Less Than 9th Grade',
                                      `2` = '9-11th Grade',
                                      `3`= 'High School Grad/GED',
                                      `4`= 'Some College or AA degree',
                                      `5`= 'College Graduate or above')

paper.data$education.attainment <- education.attainment
# 使用table函数计算DMDEDUC2列的每个分类的频数，包括NA值
DMDEDUC2_counts <- table(paper.data$DMDEDUC2, useNA = "ifany")

# 打印结果
print(DMDEDUC2_counts)

######################维生素D
# 找到具有缺失值的列
cols_with_na <- paper.data %>%
  summarise_all(function(x) any(is.na(x))) %>%
  gather(key = "column", value = "has_na") %>%
  filter(has_na) %>%
  pull(column)

# 输出具有缺失值的列名
print(cols_with_na)

# 计算每列的缺失值占比并显示
missing_proportions <- paper.data %>%
  summarise_all(function(x) mean(is.na(x))) %>%
  gather(key = "column", value = "proportion")

missing_proportions$proportion <- missing_proportions$proportion * 100

# 输出每列的缺失值占比
print(missing_proportions)
missing_proportions$column
#######抑郁评分
###这里把7或9的PHQ评分项全部删掉
paper.data <- paper.data %>%
  filter(!(DPQ010 %in% c(7, 9) |
             DPQ020 %in% c(7, 9) |
             DPQ030 %in% c(7, 9) |
             DPQ040 %in% c(7, 9) |
             DPQ050 %in% c(7, 9) |
             DPQ060 %in% c(7, 9) |
             DPQ070 %in% c(7, 9) |
             DPQ080 %in% c(7, 9) |
             DPQ090 %in% c(7, 9)))
####################################
depression.total <- apply(paper.data[,c('DPQ010', 'DPQ020', 'DPQ030', 'DPQ040', 'DPQ050', 'DPQ060', 'DPQ070', 'DPQ080', 'DPQ090')], 1, sum)
paper.data$depression.total <- depression.total

table(paper.data$depression.total)
######################把教育程度和认知功能叠加
colnames(paper.data)
#提取数据
edu.cog.data <- paper.data %>%
  select(education.attainment, CERAD.total, CFDAST, CFDDS)

# 根据教育水平分组来计算各个认知测试的 z-score
edu.cog.data$z_score1 <- ave(edu.cog.data$CERAD.total, edu.cog.data$education.attainment, FUN = function(x) {
  (x - mean(x)) / sd(x)
})
edu.cog.data$z_score2 <- ave(edu.cog.data$CFDAST, edu.cog.data$education.attainment, FUN = function(x) {
  (x - mean(x)) / sd(x)
})
edu.cog.data$z_score3 <- ave(edu.cog.data$CFDDS, edu.cog.data$education.attainment, FUN = function(x) {
  (x - mean(x)) / sd(x)
})

# 计算全局认知度量，这是三个 z-score 的平均值
paper.data$global_cognitive_edu <- rowMeans(edu.cog.data[,c('z_score1', 'z_score2', 'z_score3')])

# 为每个个体标记“低认知性能”
paper.data$low_cognitive_edu <- ifelse(paper.data$global_cognitive_edu < -1, "Yes", "No")

##########################################################################
#### 二、Paper Table ####
#### 1. 分析数据准备 ####
# 生成复杂抽样的对象
NHANES_design <- svydesign(data = paper.data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust") 
library(writexl)
write_xlsx(paper.data, "paper_data.xlsx")
# 修饰表格1-修改分类变量取值出现的顺序

# 按照临床含义排序-1饮酒、吸烟
table(paper.data$alq.group, useNA = "ifany")
levels(paper.data$alq.group)
class(paper.data$alq.group)
paper.data$alq.group <- factor(paper.data$alq.group, 
                               levels = c('1-5 drinks/month', 
                                          '5-10 drinks/month',
                                          '10+ drinks/month',
                                          'Non-drinker'))

#### 2. Table1 ####
# 加权的情况，对连续变量进行离散分类
# https://rdrr.io/cran/survey/man/svyquantile.html
NHANES_design <- svydesign(data = paper.data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust") 
colnames(paper.data)
overall_cognitive_score.quantile.res <- svyquantile(~ overall_cognitive_score, NHANES_design, quantiles = c(0, 0.25, 0.5, 0.75, 1))
paper.data$overall_cognitive_score.quantile.var <- cut(paper.data$overall_cognitive_score,
                                          breaks = overall_cognitive_score.quantile.res$overall_cognitive_score[,'quantile'],
                                          labels = c('Q1', 'Q2', 'Q3', 'Q4'))
                                           
# View(paper.data[,c('Dietary.LZ.quantile.var', 'Dietary.LZ')])
# forcats::fct_count(paper.data$Dietary.LZ.quantile.var)
paper.data$overall_cognitive_score.quantile.var[which(is.na(paper.data$overall_cognitive_score.quantile.var))] <- 'Q1'
#  # 将最小值替换为 Q1，以免为 NA

###################缺失值

# 计算每列的缺失值占比并显示
missing_proportions <- paper.data %>%
  summarise_all(function(x) mean(is.na(x))) %>%
  gather(key = "column", value = "proportion")

missing_proportions$proportion <- missing_proportions$proportion * 100

# 输出每列的缺失值占比
print(missing_proportions)

# 找出缺失值比例大于或等于10%的列
columns_to_remove <- missing_proportions %>%
  filter(proportion >= 10) %>%
  pull(column)

# 删除这些列
paper.data_clean <- paper.data %>%
  select(-one_of(columns_to_remove))

# 输出清理后的数据
print(head(paper.data_clean))
#对于睡眠障碍，进行SLQ050,SLQ060合并
paper.data_clean <- paper.data_clean %>% 
  mutate(SLQ = ifelse(SLQ050 == 1 | SLQ060 == 1, 1, 0))
# 过滤掉所有以"MCQ"开头的列且值为9的记录,改为na，以便后期用随机森林插补
paper.data_clean <- paper.data_clean %>%
  mutate(
    across(c(starts_with("MCQ"), "DIQ010", "BPQ020"), 
           ~ ifelse(. == 9, NA, .)
    )
  )
colnames(paper.data_clean)
glimpse(paper.data_clean)
########对维生素在检测值以下的值进行考察
table(paper.data_clean$LBDVIDLC, useNA = "always")
table(paper.data_clean$LBDVD2LC, useNA = "always")
table(paper.data_clean$LBDVD3LC, useNA = "always")
table(paper.data_clean$LBDVE3LC, useNA = "always")
############
paper.data_clean <- paper.data_clean %>%
  mutate(dyslipidemia = as.factor(dyslipidemia))
paper.data_clean <- paper.data_clean %>%
  select(-SLQ050, -SLQ060,-LBDVIDLC, -LBDVD2LC, -LBDVD3LC, -LBDVE3LC)
paper.data_clean$overall_cognitive_score.quantile.var
library(dplyr)

paper.data_clean <- paper.data_clean %>%
  mutate(overall_cognitive_score.quantile.var = as.character(overall_cognitive_score.quantile.var)) %>%
  mutate(overall_cognitive_score.quantile.var = dplyr::recode(overall_cognitive_score.quantile.var, "Q1" = "1", "Q2" = "0", "Q3" = "0", "Q4" = "0")) %>%
  mutate(overall_cognitive_score.quantile.var = as.numeric(overall_cognitive_score.quantile.var))
table(paper.data_clean$overall_cognitive_score.quantile.var)

#############对心脏病的整理
colnames(paper.data_clean)
# 计算并打印每列的值的计数，包括缺失值
summary_values <- function(data, cols) {
  lapply(data[, cols, drop = FALSE], function(x) {
    table(factor(x, exclude = NULL))
  })
}

# 应用于指定列
result <- summary_values(paper.data_clean, c("MCQ160B", "MCQ160C", "MCQ160D", "MCQ160E"))

# 打印结果
print(result)
#######
library(dplyr)

# 更新paper.data_clean数据框，添加新变量
paper.data_clean <- paper.data_clean %>%
  mutate(new_heart = case_when(
    MCQ160B == 1 | MCQ160C == 1 | MCQ160D == 1 | MCQ160E == 1 ~ 1,
    is.na(MCQ160B) | is.na(MCQ160C) | is.na(MCQ160D) | is.na(MCQ160E) ~ NA_real_,
    TRUE ~ 0
  )) 
table(paper.data_clean$new_heart, useNA = "ifany")
#####
paper.data_clean <- paper.data_clean %>%
  select(-MCQ160B, -MCQ160C,-MCQ160D,MCQ160E) %>%
  rename(MCQ160B = new_heart) %>% 
  mutate(MCQ160B = factor(MCQ160B))
#对婚姻状态进行整理
table(paper.data_clean$DMDMARTL, useNA = "ifany")
paper.data_clean <- paper.data_clean %>%
  mutate(Marital = case_when(
    DMDMARTL == 1 | DMDMARTL == 6 ~ 1,  # “已婚”包括原代码1和6
    DMDMARTL == 2 | DMDMARTL == 3 | DMDMARTL == 4 | DMDMARTL == 5 ~ 0,  # “未婚/其他”包括原代码2, 3, 4, 5
    DMDMARTL == 77 ~ NA_real_,  # 将77归类为NA
    TRUE ~ NA_real_  # 将其他所有情况（包括NA）归类为NA
  )) %>% 
  mutate(Marital = factor(Marital))

# 对糖尿病进行整理
paper.data_clean <- paper.data_clean %>%
  rename(DIQ010 = DIQ010) %>%
  mutate(DIQ010 = as.numeric(DIQ010),  # 先转换为数值型（如果需要的话）
         DIQ010 = case_when(
           DIQ010 == 1 ~ 1,
           DIQ010 == 3 ~ 1,
           DIQ010 == 2 ~ 0,
           TRUE ~ NA_real_  # 其他情况设置为NA
         )) %>% 
  mutate(DIQ010 = factor(DIQ010))
table(paper.data_clean$Marital, useNA = "ifany")
table(paper.data_clean$DIQ010, useNA = "ifany")
glimpse(paper.data_clean)
#################################################################
# #加载必要的库
# glimpse(paper.data_clean)
# colnames(paper.data_clean)
# table(addNA(paper.data_clean$age.group))
# table(paper.data_clean$CFDCST1)
# library(missForest)
# factor_cols <- c("Sex","age.group","low_global_cognition","smoke.group","low_cognitive_edu","alq.group","race",
#                  "DIQ010", "Marital", "MCQ160B", "MCQ160G", "MCQ160K", "MCQ220", "MCQ160F", "dyslipidemia", "BPQ020", "SMQ020", "ALQ101","SLQ",'RIDEXMON')  # 列出需要转换的列名
# paper.data_clean[factor_cols] <- lapply(paper.data_clean[factor_cols], as.factor)
# #选择变量进行插补
# 
# impute_data <- paper.data_clean %>%
#   dplyr::select(
#     RIDAGEYR, INDFMPIR,
#     RIDEXMON, BMXBMI,
#     BMXWAIST, DIQ010, Marital, MCQ160B, MCQ160G, MCQ160K,
#     MCQ220, MCQ160F, dyslipidemia, BPQ020,
#     LBXVD3MS,
#     Sex, alq.group,overall_cognitive_score,
#     smoke.group, education.attainment, depression.total,
#     SLQ
#   )
# set.seed(2024)
# forest_result <- missForest(impute_data, maxiter = 20, ntree = 500, verbose = TRUE)
# paper.data_imputed <- forest_result$ximp
# paper.data_imputed <- paper.data_imputed %>%
#   cbind(paper.data_clean %>% select(SDMVPSU, SDMVSTRA, WTMEC2YR,SEQN))
# colnames(paper.data_imputed)
# 
# ##################
# missing_proportions <- paper.data_imputed %>%
#   summarise_all(function(x) mean(is.na(x))) %>%
#   gather(key = "column", value = "proportion")
# 
# missing_proportions$proportion <- missing_proportions$proportion * 100
# print(missing_proportions)
# library(writexl)
# write_xlsx(paper.data_imputed, "paper.data_imputed.xlsx")
####################################################################################
#################################
#加权相关性分析
glimpse(paper.data_clean)
# 加载survey包
library(survey)
library(jtools)
NHANES_design <- svydesign(data = paper.data_clean,
                           ids = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           nest = TRUE,
                           weights = ~WTMEC2YR,
                           survey.lonely.psu = "adjust")
# 执行加权相关性分析
correlation_matrix <- svycor(~CERAD.total + CFDAST + CFDDS + overall_cognitive_score, NHANES_design, sig.stats = TRUE)

# 打印相关矩阵
print(correlation_matrix)

correlation_matrix$p.values
###################################
###############对连续变量进行加权Wilcoxon秩和检验（和gtsummary的结果比对）
NHANES_design <- svydesign(data = paper.data_clean,
                           ids = ~SDMVPSU,
                           strata = ~SDMVSTRA,
                           nest = TRUE,
                           weights = ~WTMEC2YR,
                           survey.lonely.psu = "adjust")

glimpse(paper.data_clean)

# 定义一个加权Wilcoxon测试函数
run_weighted_wilcoxon <- function(var_name, design) {
  formula <- as.formula(paste(var_name, "~ overall_cognitive_score.quantile.var"))
  test_result <- svyranktest(formula, design = design, test = "wilcoxon")
  p_value <- as.numeric(test_result$p.value)
  return(round(p_value, 3))
}

# 需要测试的变量列表
variables_to_test <- c("RIDAGEYR", "INDFMPIR", "BMXBMI", "BMXWAIST",
                       "LBXVIDMS", "LBXVD2MS", "LBXVD3MS", "LBXVE3MS",
                       "depression.total", "CERAD.total", "CFDAST", "CFDDS")

# 初始化结果dataframe
result_df <- data.frame(variable = character(), p_value = numeric())

# 运行测试并保存结果
for (var in variables_to_test) {
  p_value <- run_weighted_wilcoxon(var, NHANES_design)
  result_df <- rbind(result_df, data.frame(variable = var, p_value = p_value))
}

# 显示结果
print(result_df)
##############################################################################
###################################################################################
########################模型拟合
#####首先是作为连续变量拟合
setwd("E:/R2023/nhanes/R_project")
library(readxl)
paper_data_imputed <- read_excel("paper.data_imputed.xlsx")
glimpse(paper_data_imputed)
table(paper_data_imputed$Marital)
##对于RIDAGEYR 的处理
paper_data_imputed <- paper_data_imputed %>%
  rename(Age = RIDAGEYR) %>%
  mutate(Age.group = ifelse(Age < 70, 0, 1))
##对RIAGENDR的处理
table(paper_data_imputed$Sex)
paper_data_imputed <- paper_data_imputed %>%
  mutate(Gender = recode(Sex, male = 1, female = 0)) %>%
  select(-Sex)
##对DIQ010的处理
paper_data_imputed <- paper_data_imputed %>%
  rename(Diabetes = DIQ010) %>%
  mutate(Diabetes = as.numeric(Diabetes),  # 先转换为数值型（如果需要的话）
         Diabetes = case_when(
           Diabetes == 1 ~ 1,
           Diabetes == 3 ~ 1,
           Diabetes == 2 ~ 0,
           TRUE ~ NA_real_  # 其他情况设置为NA
         ))


#处理BPQ020列
paper_data_imputed <- paper_data_imputed %>%
  rename(Hypertension = BPQ020) %>%
  mutate(Hypertension = as.numeric(Hypertension),  # 先确保该列为数值型
         Hypertension = case_when(
           Hypertension == 1 ~ 1,
           Hypertension == 2 ~ 0,
           TRUE ~ NA_real_  # 其他情况设置为NA
         ))
#########对alq.group，smoke.group，education.attainment 进行转换
# 修改列名和将有序字符列转换为数值列
paper_data_imputed <- paper_data_imputed %>%
  mutate(
    alq.group = factor(alq.group, 
                       levels = c("Non-drinker", "1-5 drinks/month", "5-10 drinks/month", "10+ drinks/month"),
                       ordered = TRUE),
    smoke.group = factor(smoke.group, 
                         levels = c("Never smoker", "Former smoker", "Current smoker"),
                         ordered = TRUE),
    education.attainment = factor(education.attainment, 
                                  levels = c('Less Than 9th Grade',
                                             '9-11th Grade',
                                             'High School Grad/GED',
                                             'Some College or AA degree',
                                             'College Graduate or above'),
                                  ordered = TRUE)
  ) %>%
  mutate(
    Drink = as.numeric(alq.group) - 1,
    Smoke = as.numeric(smoke.group) - 1,
    Education = as.numeric(education.attainment) - 1
  ) %>%
  select(-alq.group, -smoke.group, -education.attainment) 
##################
glimpse(paper_data_imputed)
###################
#对MCQ160F的处理
table(paper_data_imputed$MCQ160F)
paper_data_imputed <- paper_data_imputed %>%
  rename(Stroke = MCQ160F) %>%
  mutate(Stroke = as.numeric(Stroke),  # 先确保该列为数值型
         Stroke = case_when(
           Stroke == 1 ~ 1,
           Stroke == 2 ~ 0,
           TRUE ~ NA_real_  # 其他情况设置为NA
         ))
table(paper_data_imputed$Stroke)
table(paper_data_imputed$MCQ160B)
#######处理MCQ160B
paper_data_imputed <- paper_data_imputed %>%
  rename(Heart = MCQ160B) %>%
  mutate(Heart = as.numeric(Heart),  # 先确保该列为数值型
         Heart = case_when(
           Heart == 1 ~ 1,
           Heart == 0 ~ 0,
           TRUE ~ NA_real_  # 其他情况设置为NA
         ))
table(paper_data_imputed$Heart,useNA = "ifany")
#################################
#对婚姻的处理
glimpse(paper_data_imputed)
paper_data_imputed <- paper_data_imputed %>%
  mutate( Marital= as.numeric(Marital))
table(paper_data_imputed$Marital,useNA = "ifany")
#############################################################
            ##################################
                  #################
                       #####
######################构建模型################################
#################先构建连续型变量depression~vitamin
table(paper_data_imputed$depression.total)
paper_data_imputed$depression.total <- 27 - paper_data_imputed$depression.total

################
####################对各变量进行标准化#######################
paper_data_imputed$LBXVD3MS <- as.vector(scale(paper_data_imputed$LBXVD3MS))
#paper_data_imputed$LBXVE3MS <- as.vector(scale(paper_data_imputed$LBXVE3MS))
paper_data_imputed$depression.total <- as.vector(scale(paper_data_imputed$depression.total))
#################################################################
colnames(paper_data_imputed)
paper_data_imputed$INDFMPIR
paper_data_imputed <- paper_data_imputed %>%
  mutate(INDFMPIR = case_when(
    INDFMPIR >= 0 & INDFMPIR <= 1.30 ~ 0,
    INDFMPIR > 1.30 & INDFMPIR <= 3.50 ~ 1,
    INDFMPIR > 3.50 ~ 2
  ))
table(paper_data_imputed$INDFMPIR)
table(paper_data_imputed$Drink)
# #这里将饮酒的2，3级别合并为2
# paper_data_imputed <- paper_data_imputed %>%
#   mutate(Drink = ifelse(Drink == 3, 2, Drink))
# table(paper_data_imputed$Smoke)

#################################################################
glimpse(paper_data_imputed)
##################################################################
#######################单变量模型depression~vitamin#######################################
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  formula <- as.formula(paste("depression.total ~", var))
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  estimate <- summary_model$coefficients[2, "Estimate"]
  std_error <- summary_model$coefficients[2, "Std. Error"]
  
  results[var, "Estimate"] <- estimate
  results[var, "Std. Error"] <- std_error
  results[var, "t value"] <- summary_model$coefficients[2, "t value"]
  results[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
  
  # 计算置信区间
  results[var, "Lower CI"] <- estimate - 1.96 * std_error
  results[var, "Upper CI"] <- estimate + 1.96 * std_error
}

# 打印结果
print(results)
# #####################################构建多变量模型
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- variables

# 定义协变量列表
covariates <- c("Stroke", "Diabetes", "Hypertension", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 循环遍历变量并进行分析
for (var in variables) {
  # 在公式中添加协变量
  formula <- as.formula(paste("depression.total ~", var, "+", covariates_string))

  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  estimate <- summary_model$coefficients[2, "Estimate"]
  std_error <- summary_model$coefficients[2, "Std. Error"]

  results[var, "Estimate"] <- estimate
  results[var, "Std. Error"] <- std_error
  results[var, "t value"] <- summary_model$coefficients[2, "t value"]
  results[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]

  # 计算置信区间
  results[var, "Lower CI"] <- estimate - 1.96 * std_error
  results[var, "Upper CI"] <- estimate + 1.96 * std_error
}

# 打印结果
print(results)
######################################################################
#################################
#####################维生素D3和认知总评分（直接效应）
#######################单变量模型overall_cognitive_score~vitamin#######################################
colnames(paper_data_imputed)
glimpse(paper_data_imputed)
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  formula <- as.formula(paste("overall_cognitive_score ~", var))
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  estimate <- summary_model$coefficients[2, "Estimate"]
  std_error <- summary_model$coefficients[2, "Std. Error"]
  
  results[var, "Estimate"] <- estimate
  results[var, "Std. Error"] <- std_error
  results[var, "t value"] <- summary_model$coefficients[2, "t value"]
  results[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
  
  # 计算置信区间
  results[var, "Lower CI"] <- estimate - 1.96 * std_error
  results[var, "Upper CI"] <- estimate + 1.96 * std_error
}

# 打印结果
print(results)

#####################################构建多变量模型

colnames(paper_data_imputed)
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- variables

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education","Age.group", "Gender", "INDFMPIR", "Diabetes", "Hypertension", "Heart","Marital")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 循环遍历变量并进行分析
for (var in variables) {
  # 在公式中添加协变量
  formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
  
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  estimate <- summary_model$coefficients[2, "Estimate"]
  std_error <- summary_model$coefficients[2, "Std. Error"]
  
  results[var, "Estimate"] <- estimate
  results[var, "Std. Error"] <- std_error
  results[var, "t value"] <- summary_model$coefficients[2, "t value"]
  results[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
  
  # 计算置信区间
  results[var, "Lower CI"] <- estimate - 1.96 * std_error
  results[var, "Upper CI"] <- estimate + 1.96 * std_error
}

# 打印结果
print(results)
##################################################################
############################认知和维生素D的亚组分析########################################
###确定亚组
glimpse(paper_data_imputed)
#Smoke, Drink,Diabetes, Marital, Heart, Stroke, Hypertension,Gender,Age.group,INDFMPIR的各个占比情况
#柱状图见柱状图code,R文件 



############subgroup 年龄
# 初始化两个结果矩阵，一个用于 Age.group = 0, 另一个用于 Age.group = 1
results_group0 <- matrix(NA, nrow = length(variables), ncol = 6)
results_group1 <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results_group0) <- colnames(results_group1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_group0) <- rownames(results_group1) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (age_group in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Age.group == age_group)
    
    # 构建模型
    formula <- as.formula(paste("overall_cognitive_score ~", var))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (age_group == 0) {
      results_group0[var, "Estimate"] <- estimate
      results_group0[var, "Std. Error"] <- std_error
      results_group0[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_group0[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_group0[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_group0[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_group1[var, "Estimate"] <- estimate
      results_group1[var, "Std. Error"] <- std_error
      results_group1[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_group1[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_group1[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_group1[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Age.group = 0 (Less than 70 years old)")
print(results_group0)

print("Results for Age.group = 1 (70 years old and above)")
print(results_group1)
###############################多变量 年龄亚组####################
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education",  "Gender", "INDFMPIR", "Marital", "Diabetes", "Hypertension", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 初始化结果矩阵
results_group0 <- matrix(NA, nrow = length(variables), ncol = 6)
results_group1 <- matrix(NA, nrow = length(variables), ncol = 6)
colnames(results_group0) <- colnames(results_group1) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_group0) <- rownames(results_group1) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (age_group in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Age.group == age_group)
    
    # 构建模型，在公式中添加协变量
    formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (age_group == 0) {
      results_group0[var, "Estimate"] <- estimate
      results_group0[var, "Std. Error"] <- std_error
      results_group0[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_group0[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_group0[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_group0[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_group1[var, "Estimate"] <- estimate
      results_group1[var, "Std. Error"] <- std_error
      results_group1[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_group1[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_group1[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_group1[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Age.group = 0 (Less than 70 years old)")
print(results_group0)

print("Results for Age.group = 1 (70 years old and above)")
print(results_group1)

#############################################
glimpse(paper_data_imputed)
#############gender作为分组变量
# 初始化两个结果矩阵，一个用于 Gender = 0 (Female), 另一个用于 Gender = 1 (Male)
results_female <- matrix(NA, nrow = length(variables), ncol = 6)
results_male <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_female) <- colnames(results_male) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_female) <- rownames(results_male) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (gender in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Gender == gender)
    
    # 构建模型
    formula <- as.formula(paste("overall_cognitive_score ~", var))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (gender == 0) {
      results_female[var, "Estimate"] <- estimate
      results_female[var, "Std. Error"] <- std_error
      results_female[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_female[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_female[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_female[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_male[var, "Estimate"] <- estimate
      results_male[var, "Std. Error"] <- std_error
      results_male[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_male[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_male[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_male[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Gender = 0 (Female)")
print(results_female)

print("Results for Gender = 1 (Male)")
print(results_male)
##############################多变量 gender
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education","Age.group",  "INDFMPIR", "Marital", "Diabetes", "Hypertension", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 初始化两个结果矩阵
results_female <- matrix(NA, nrow = length(variables), ncol = 6)
results_male <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_female) <- colnames(results_male) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_female) <- rownames(results_male) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (gender in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Gender == gender)
    
    # 构建模型，在公式中添加协变量
    formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (gender == 0) {
      results_female[var, "Estimate"] <- estimate
      results_female[var, "Std. Error"] <- std_error
      results_female[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_female[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_female[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_female[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_male[var, "Estimate"] <- estimate
      results_male[var, "Std. Error"] <- std_error
      results_male[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_male[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_male[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_male[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Gender = 0 (Female)")
print(results_female)

print("Results for Gender = 1 (Male)")
print(results_male)
#######diabetes
# 初始化两个结果矩阵，一个用于 Diabetes = 0 (非糖尿病)，另一个用于 Diabetes = 1 (糖尿病)
results_non_diabetic <- matrix(NA, nrow = length(variables), ncol = 6)
results_diabetic <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_non_diabetic) <- colnames(results_diabetic) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_non_diabetic) <- rownames(results_diabetic) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (diabetes_status in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Diabetes == diabetes_status)
    
    # 构建模型
    formula <- as.formula(paste("overall_cognitive_score ~", var))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (diabetes_status == 0) {
      results_non_diabetic[var, "Estimate"] <- estimate
      results_non_diabetic[var, "Std. Error"] <- std_error
      results_non_diabetic[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_non_diabetic[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_non_diabetic[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_non_diabetic[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_diabetic[var, "Estimate"] <- estimate
      results_diabetic[var, "Std. Error"] <- std_error
      results_diabetic[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_diabetic[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_diabetic[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_diabetic[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Diabetes = 0 (Non-Diabetic)")
print(results_non_diabetic)

print("Results for Diabetes = 1 (Diabetic)")
print(results_diabetic)
###############################多变量
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education","Age.group", "Gender", "INDFMPIR", "Marital",  "Hypertension", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 初始化两个结果矩阵，一个用于 Diabetes = 0 (非糖尿病)，另一个用于 Diabetes = 1 (糖尿病)
results_non_diabetic <- matrix(NA, nrow = length(variables), ncol = 6)
results_diabetic <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_non_diabetic) <- colnames(results_diabetic) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_non_diabetic) <- rownames(results_diabetic) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (diabetes_status in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Diabetes == diabetes_status)
    
    # 构建模型，在公式中添加协变量
    formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (diabetes_status == 0) {
      results_non_diabetic[var, "Estimate"] <- estimate
      results_non_diabetic[var, "Std. Error"] <- std_error
      results_non_diabetic[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_non_diabetic[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_non_diabetic[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_non_diabetic[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_diabetic[var, "Estimate"] <- estimate
      results_diabetic[var, "Std. Error"] <- std_error
      results_diabetic[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_diabetic[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_diabetic[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_diabetic[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Diabetes = 0 (Non-Diabetic)")
print(results_non_diabetic)

print("Results for Diabetes = 1 (Diabetic)")
print(results_diabetic)

#################hypertension
glimpse(paper_data_imputed)
# 初始化两个结果矩阵，一个用于 Hypertension = 0 (非高血压)，另一个用于 Hypertension = 1 (高血压)
results_non_hypertensive <- matrix(NA, nrow = length(variables), ncol = 6)
results_hypertensive <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_non_hypertensive) <- colnames(results_hypertensive) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_non_hypertensive) <- rownames(results_hypertensive) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (hypertension_status in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Hypertension == hypertension_status)
    
    # 构建模型
    formula <- as.formula(paste("overall_cognitive_score ~", var))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (hypertension_status == 0) {
      results_non_hypertensive[var, "Estimate"] <- estimate
      results_non_hypertensive[var, "Std. Error"] <- std_error
      results_non_hypertensive[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_non_hypertensive[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_non_hypertensive[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_non_hypertensive[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_hypertensive[var, "Estimate"] <- estimate
      results_hypertensive[var, "Std. Error"] <- std_error
      results_hypertensive[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_hypertensive[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_hypertensive[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_hypertensive[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Hypertension = 0 (Non-Hypertensive)")
print(results_non_hypertensive)

print("Results for Hypertension = 1 (Hypertensive)")
print(results_hypertensive)
########################subgroup 多变量高血压
# 定义要分析的变量
variables <- c("LBXVD3MS")

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education", "Age.group", "Gender", "INDFMPIR", "Marital", "Diabetes", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 初始化两个结果矩阵，一个用于 Hypertension = 0 (非高血压)，另一个用于 Hypertension = 1 (高血压)
results_non_hypertensive <- matrix(NA, nrow = length(variables), ncol = 6)
results_hypertensive <- matrix(NA, nrow = length(variables), ncol = 6)

colnames(results_non_hypertensive) <- colnames(results_hypertensive) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results_non_hypertensive) <- rownames(results_hypertensive) <- variables

# 循环遍历变量并进行分析
for (var in variables) {
  
  for (hypertension_status in c(0, 1)) {
    # 创建亚组数据
    sub_design <- subset(NHANES_design, Hypertension == hypertension_status)
    
    # 构建模型，在公式中添加协变量
    formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
    model <- svyglm(formula, design = sub_design)
    summary_model <- summary(model)
    estimate <- summary_model$coefficients[2, "Estimate"]
    std_error <- summary_model$coefficients[2, "Std. Error"]
    
    if (hypertension_status == 0) {
      results_non_hypertensive[var, "Estimate"] <- estimate
      results_non_hypertensive[var, "Std. Error"] <- std_error
      results_non_hypertensive[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_non_hypertensive[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_non_hypertensive[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_non_hypertensive[var, "Upper CI"] <- estimate + 1.96 * std_error
    } else {
      results_hypertensive[var, "Estimate"] <- estimate
      results_hypertensive[var, "Std. Error"] <- std_error
      results_hypertensive[var, "t value"] <- summary_model$coefficients[2, "t value"]
      results_hypertensive[var, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
      results_hypertensive[var, "Lower CI"] <- estimate - 1.96 * std_error
      results_hypertensive[var, "Upper CI"] <- estimate + 1.96 * std_error
    }
  }
}

# 打印结果
print("Results for Hypertension = 0 (Non-Hypertensive)")
print(results_non_hypertensive)

print("Results for Hypertension = 1 (Hypertensive)")
print(results_hypertensive)

##############################考察中介变量与因变量的关系
colnames(paper_data_imputed)
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust") 
m1 <- svyglm( overall_cognitive_score~ depression.total, design = NHANES_design) 
summary(m1)
# 获取模型摘要
summary_m1 <- summary(m1)
# 提取估计值和标准误差
estimate <- coef(summary_m1)["depression.total", "Estimate"]
std_error <- coef(summary_m1)["depression.total", "Std. Error"]

# 计算95%置信区间
lower_95_ci <- estimate - (1.96 * std_error)
upper_95_ci <- estimate + (1.96 * std_error)

# 输出结果
cat("95% Confidence Interval for the estimate is [", lower_95_ci, ", ", upper_95_ci, "]\n")
#####################
m2 <- svyglm(overall_cognitive_score ~ depression.total + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Stroke + Heart, design = NHANES_design)
summary(m2)
# 获取模型摘要
summary_m2 <- summary(m2)
# 提取估计值和标准误差
estimate <- coef(summary_m2)["depression.total", "Estimate"]
std_error <- coef(summary_m2)["depression.total", "Std. Error"]

# 计算95%置信区间
lower_95_ci <- estimate - (1.96 * std_error)
upper_95_ci <- estimate + (1.96 * std_error)

# 输出结果
cat("95% Confidence Interval for the estimate is [", lower_95_ci, ", ", upper_95_ci, "]\n")
##############亚组分析
library(survey)

# 创建一个空的数据框来存储所有的结果
results_df <- data.frame()

# 二分类变量列表
binary_variables <- c("Age.group", "Gender", "Hypertension")
build_formula <- function(exclude_var) {
  base_formula <- "overall_cognitive_score ~ depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Diabetes + Hypertension +  Heart + Marital"
  new_formula <- as.formula(gsub(paste0(" + ", exclude_var, " "), " ", base_formula))
  return(new_formula)
}
# 迭代遍历每个二分类变量
# 在循环中使用新的build_formula函数来创建公式
for(binary_var in binary_variables){
  
  # 获取这个变量的每个水平
  levels <- unique(paper_data_imputed[[binary_var]])
  
  # 对每个水平进行分析
  for(level in levels){
    
    # 创建亚数据集
    sub_data <- subset(paper_data_imputed, paper_data_imputed[[binary_var]] == level)
    
    # 更新样本设计
    NHANES_design <- svydesign(data = sub_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
    
    # 单因素分析
    m1 <- svyglm(overall_cognitive_score ~ depression.total, design = NHANES_design)
    
    # 多因素分析
    m2 <- svyglm(build_formula(binary_var), design = NHANES_design)
    
    # 提取结果
    for(model in list(m1, m2)){
      
      # 提取模型摘要
      summary_model <- summary(model)
      
      # 获取结果
      coefs <- coef(summary_model)
      
      # 创建结果数据框
      res_df <- data.frame(
        Variable = binary_var,
        Level = level,
        Model = ifelse(identical(model, m1), "Univariate", "Multivariate"),
        Estimate = coefs["depression.total", "Estimate"],
        `Lower CI` = coefs["depression.total", "Estimate"] - (1.96 * coefs["depression.total", "Std. Error"]),
        `Upper CI` = coefs["depression.total", "Estimate"] + (1.96 * coefs["depression.total", "Std. Error"]),
        `Pr(>|t|)` = coefs["depression.total", "Pr(>|t|)"],
        `Std. Error` = coefs["depression.total", "Std. Error"],
        `t value` = coefs["depression.total", "t value"]
      )
      
      # 添加到总结果数据框
      results_df <- rbind(results_df, res_df)
    }
  }
}

# 显示结果
print(results_df)
############
#对于Diabetes，Heart进行单独的亚组分析
# 对Diabetes变量单独处理
# 对Diabetes变量单独处理
paper_data_imputed_diabetes <- paper_data_imputed
paper_data_imputed_diabetes$SDMVSTRA[paper_data_imputed_diabetes$SDMVSTRA == 103] <- 102

# 获取Diabetes变量的每个水平
levels_diabetes <- unique(paper_data_imputed_diabetes$Diabetes)

# 对每个水平进行分析
for(level in levels_diabetes){
  
  # 创建亚数据集
  sub_data <- subset(paper_data_imputed_diabetes, paper_data_imputed_diabetes$Diabetes == level)
  
  # 创建新的样本设计
  NHANES_design_sub <- svydesign(data = sub_data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
  
  
  # 单因素分析
  m1 <- svyglm(overall_cognitive_score ~ depression.total, design = NHANES_design_sub)
  
  # 多因素分析
  m2 <- svyglm(overall_cognitive_score ~ depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Heart, design = NHANES_design_sub)
  
  # 提取结果
  for(model in list(m1, m2)){
    
    # 提取模型摘要
    summary_model <- summary(model)
    
    # 获取结果
    coefs <- coef(summary_model)
    
    # 创建结果数据框
    res_df <- data.frame(
      Variable = "Diabetes",
      Level = level,
      Model = ifelse(identical(model, m1), "Univariate", "Multivariate"),
      Estimate = coefs["depression.total", "Estimate"],
      `Lower CI` = coefs["depression.total", "Estimate"] - (1.96 * coefs["depression.total", "Std. Error"]),
      `Upper CI` = coefs["depression.total", "Estimate"] + (1.96 * coefs["depression.total", "Std. Error"]),
      `Pr(>|t|)` = coefs["depression.total", "Pr(>|t|)"],
      `Std. Error` = coefs["depression.total", "Std. Error"],
      `t value` = coefs["depression.total", "t value"]
    )
    
    # 添加到总结果数据框
    results_df <- rbind(results_df, res_df)
  }
}

# 显示结果
print(results_df)

#######################做认知

#####################################
# 加载所需库
library(survey)

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵
results <- data.frame(Var_Estimate = numeric(length(variables)), 
                      Var_StdError = numeric(length(variables)), 
                      Var_tValue = numeric(length(variables)), 
                      Var_pValue = numeric(length(variables)), 
                      Dep_Estimate = numeric(length(variables)), 
                      Dep_StdError = numeric(length(variables)), 
                      Dep_tValue = numeric(length(variables)), 
                      Dep_pValue = numeric(length(variables)), 
                      Interaction_Estimate = numeric(length(variables)), 
                      Interaction_StdError = numeric(length(variables)), 
                      Interaction_tValue = numeric(length(variables)), 
                      Interaction_pValue = numeric(length(variables)), 
                      Var_CI_Lower = numeric(length(variables)), 
                      Var_CI_Upper = numeric(length(variables)), 
                      Dep_CI_Lower = numeric(length(variables)), 
                      Dep_CI_Upper = numeric(length(variables)), 
                      Interaction_CI_Lower = numeric(length(variables)), 
                      Interaction_CI_Upper = numeric(length(variables)), 
                      row.names = variables)

rownames(results) <- variables

# 初始化结果矩阵
for (var in variables) {
  # 定义包含交互项的模型公式
  formula <- as.formula(paste("overall_cognitive_score ~", var, "+ depression.total + ", var, ":depression.total"))
  
  # 拟合模型
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  
  # 填充结果矩阵
  results[var, "Var_Estimate"] <- summary_model$coefficients[var, "Estimate"]
  results[var, "Var_StdError"] <- summary_model$coefficients[var, "Std. Error"]
  results[var, "Var_tValue"] <- summary_model$coefficients[var, "t value"]
  results[var, "Var_pValue"] <- summary_model$coefficients[var, "Pr(>|t|)"]
  
  results[var, "Dep_Estimate"] <- summary_model$coefficients["depression.total", "Estimate"]
  results[var, "Dep_StdError"] <- summary_model$coefficients["depression.total", "Std. Error"]
  results[var, "Dep_tValue"] <- summary_model$coefficients["depression.total", "t value"]
  results[var, "Dep_pValue"] <- summary_model$coefficients["depression.total", "Pr(>|t|)"] 
  
  interaction_term <- paste(var, "depression.total", sep = ":")
  results[var, "Interaction_Estimate"] <- summary_model$coefficients[interaction_term, "Estimate"]
  results[var, "Interaction_StdError"] <- summary_model$coefficients[interaction_term, "Std. Error"]
  results[var, "Interaction_tValue"] <- summary_model$coefficients[interaction_term, "t value"]
  results[var, "Interaction_pValue"] <- summary_model$coefficients[interaction_term, "Pr(>|t|)"]
  
  # 添加置信区间
  ci <- confint(model)
  results[var, "Var_CI_Lower"] <- ci[var, 1]
  results[var, "Var_CI_Upper"] <- ci[var, 2]
  results[var, "Dep_CI_Lower"] <- ci["depression.total", 1]
  results[var, "Dep_CI_Upper"] <- ci["depression.total", 2]
  results[var, "Interaction_CI_Lower"] <- ci[interaction_term, 1]
  results[var, "Interaction_CI_Upper"] <- ci[interaction_term, 2]
}

# 打印结果矩阵
print(results)
############################多变量
# 加载所需库
library(survey)

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵
results <- data.frame(Var_Estimate = numeric(length(variables)), 
                      Var_StdError = numeric(length(variables)), 
                      Var_tValue = numeric(length(variables)), 
                      Var_pValue = numeric(length(variables)), 
                      Dep_Estimate = numeric(length(variables)), 
                      Dep_StdError = numeric(length(variables)), 
                      Dep_tValue = numeric(length(variables)), 
                      Dep_pValue = numeric(length(variables)), 
                      Interaction_Estimate = numeric(length(variables)), 
                      Interaction_StdError = numeric(length(variables)), 
                      Interaction_tValue = numeric(length(variables)), 
                      Interaction_pValue = numeric(length(variables)), 
                      Var_CI_Lower = numeric(length(variables)), 
                      Var_CI_Upper = numeric(length(variables)), 
                      Dep_CI_Lower = numeric(length(variables)), 
                      Dep_CI_Upper = numeric(length(variables)), 
                      Interaction_CI_Lower = numeric(length(variables)), 
                      Interaction_CI_Upper = numeric(length(variables)), 
                      row.names = variables)

rownames(results) <- variables

# 初始化结果矩阵
for (var in variables) {
  # 定义包含交互项的模型公式，加入协变量
  formula <- as.formula(paste("overall_cognitive_score ~", var, "+ depression.total + ", var, ":depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension +  Heart"))
  
  # 拟合模型
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  
  # 填充结果矩阵
  results[var, "Var_Estimate"] <- summary_model$coefficients[var, "Estimate"]
  results[var, "Var_StdError"] <- summary_model$coefficients[var, "Std. Error"]
  results[var, "Var_tValue"] <- summary_model$coefficients[var, "t value"]
  results[var, "Var_pValue"] <- summary_model$coefficients[var, "Pr(>|t|)"]
  
  results[var, "Dep_Estimate"] <- summary_model$coefficients["depression.total", "Estimate"]
  results[var, "Dep_StdError"] <- summary_model$coefficients["depression.total", "Std. Error"]
  results[var, "Dep_tValue"] <- summary_model$coefficients["depression.total", "t value"]
  results[var, "Dep_pValue"] <- summary_model$coefficients["depression.total", "Pr(>|t|)"] 
  
  interaction_term <- paste(var, "depression.total", sep = ":")
  results[var, "Interaction_Estimate"] <- summary_model$coefficients[interaction_term, "Estimate"]
  results[var, "Interaction_StdError"] <- summary_model$coefficients[interaction_term, "Std. Error"]
  results[var, "Interaction_tValue"] <- summary_model$coefficients[interaction_term, "t value"]
  results[var, "Interaction_pValue"] <- summary_model$coefficients[interaction_term, "Pr(>|t|)"]
  
  # 添加置信区间
  ci <- confint(model)
  results[var, "Var_CI_Lower"] <- ci[var, 1]
  results[var, "Var_CI_Upper"] <- ci[var, 2]
  results[var, "Dep_CI_Lower"] <- ci["depression.total", 1]
  results[var, "Dep_CI_Upper"] <- ci["depression.total", 2]
  results[var, "Interaction_CI_Lower"] <- ci[interaction_term, 1]
  results[var, "Interaction_CI_Upper"] <- ci[interaction_term, 2]
}

# 打印结果矩阵
print(results)
#########################################
library(ggplot2)

# 创建一个数据框来存放摘要结果，但删除 (Intercept) 行
summary_data <- data.frame(
  Variable = rownames(summary_model$coefficients)[-1], 
  Estimate = summary_model$coefficients[-1, "Estimate"],
  Std.Error = summary_model$coefficients[-1, "Std. Error"],
  tValue = summary_model$coefficients[-1, "t value"],
  pValue = summary_model$coefficients[-1, "Pr(>|t|)"]
)

# 计算置信区间
summary_data$CI_Lower = summary_data$Estimate - 1.96 * summary_data$Std.Error
summary_data$CI_Upper = summary_data$Estimate + 1.96 * summary_data$Std.Error
# 创建一个新的数据框来存放单因素分析的结果
single_factor_data <- data.frame(
  Variable = c("LBXVD3MS0", "Depression.total0", "LBXVD3MS:Depression.total0"),
  Estimate = c(0.07669316, 0.1245689, -0.003046606),
  Std.Error = c(0.02342621, 0.02535098, 0.02778185),
  tValue = c(3.273818, 4.913769, -0.1096617),
  pValue = c(0.002746131, 3.223625e-05, 0.913433),
  CI_Lower = c(0.02878117, 0.07272028, -0.05986687),
  CI_Upper = c(0.1246051, 0.1764174, 0.05377366)
)

# 将单因素分析数据框和多因素分析数据框合并
summary_data <- rbind(
  summary_data,
  single_factor_data
)
summary_data$Variable
# 数据转换
summary_data$Estimate <- ifelse(summary_data$Estimate > -0.4, summary_data$Estimate, summary_data$Estimate * 2 + 0.7)
summary_data$CI_Lower <- ifelse(summary_data$CI_Lower > -0.4, summary_data$CI_Lower, summary_data$CI_Lower * 2 + 0.7)
summary_data$CI_Upper <- ifelse(summary_data$CI_Upper > -0.4, summary_data$CI_Upper, summary_data$CI_Upper * 2 + 0.7)

# 创建森林图
cross_zero <- summary_data %>% filter(CI_Lower * CI_Upper < 0)

my_plot <- ggplot(data = summary_data, aes(y = reorder(Variable, -Estimate), x = Estimate, xmin = CI_Lower, xmax = CI_Upper)) +
  geom_pointrange(color = "blue", size = 2, fatten = 2, lineend = "round", linewidth = 1.5) + 
  geom_pointrange(data = cross_zero, color = "red", size = 2, fatten = 2, lineend = "round", linewidth = 1.5) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "red") +
  xlab("Estimate") +
  ylab("Variable") +
  theme_minimal(base_size = 15) +
  theme(
    axis.title.x = element_text(color = "black", size = 14, face = "bold"),
    axis.title.y = element_text(color = "black", size = 14, face = "bold"),
    axis.text.x = element_text(color = "black", size = 12),
    axis.text.y = element_text(color = "black", size = 12),
    plot.title = element_text(color = "black", size = 16, face = "bold", hjust = 0.5)
  ) +
  ggtitle("Forest Plot") +
  scale_x_continuous(breaks = c(-0.6, -0.2, 0, 0.2), labels = c("-0.6", "-0.2", "0", "0.2"))

# 现在使用ggsave函数保存这个plot为PDF
ggsave(filename = "forest_plot.pdf", plot = my_plot, width = 11, height = 8.5)
#################################
##########################中介分析
##########################################################################
# 计算Diabetes列中每个值的频数
diabetes_counts <- table(paper_data_imputed$Diabetes)
diabetes_counts
library(pwr)
# 假设效应大小为中等（f2 = 0.15），α = 0.05，自变量数 = 9
pwr.f2.test(u = 10, v = 757-10-1, f2 = 0.025, sig.level = 0.05)
table(paper_data_imputed$Diabetes)
table(paper_data_imputed$Heart)
table(paper_data_imputed$INDFMPIR)
pwr.f2.test(u = 10, v = 479-10-1, f2 = 0.025, sig.level = 0.05)

library(lavaan.survey)
library(lavaan)

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
  survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
  summary(survey_fit)
  
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 对每个变量执行中介分析
for (var in variables) {
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
  
  # 使用不同的估计器重新拟合模型（例如，MLR）
  lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")
  summary(lavaan_fit)
  survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
  summary(survey_fit)
  
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 在新数据框中进行修改
paper_data_imputed_diabetes$SDMVSTRA[paper_data_imputed_diabetes$SDMVSTRA == 103] <- 102
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed_diabetes, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed_diabetes, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
      overall_cognitive_score ~ c * ', var, ' + b * depression.total +   Drink + Stroke + Education + Age.group + Gender + INDFMPIR + Hypertension + Heart + Marital
      
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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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
###################

# # 定义一个按照样本权重进行抽样的函数
# weighted_bootstrap_sample <- function(data) {
# 
#   # 尝试进行bootstrap抽样，直到每个stratum都至少有两个PSUs
#   while(TRUE) {
#     sample.indices <- sample(1:nrow(data), size = nrow(data), replace = TRUE, prob = data$WTMEC2YR)
#     sampled_data <- data[sample.indices, , drop = FALSE]
# 
#     all_strata_have_at_least_two_PSUs <- TRUE
# 
#     stratums <- unique(sampled_data$SDMVSTRA)
#     for (stratum in stratums) {
#       stratum_data <- subset(sampled_data, SDMVSTRA == stratum)
#       psus_in_stratum <- unique(stratum_data$SDMVPSU)
# 
#       if(length(psus_in_stratum) < 2) {
#         all_strata_have_at_least_two_PSUs <- FALSE
#         break
#       }
#     }
# 
#     if(all_strata_have_at_least_two_PSUs) {
#       return(sampled_data)
#     }
#   }
# }
# 
# # 定义计算中介效应占比的函数
# calculate_proportion_mediated <- function(data, mediation_model) {
#   group_NHANES_design_boot <- svydesign(data = data, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")
#   lavaan_fit_bootstrap <- sem(mediation_model, data = data, estimator="MLR")
#   survey_fit_bootstrap <- lavaan.survey(lavaan.fit = lavaan_fit_bootstrap, survey.design = group_NHANES_design_boot)
#   param_estimates_bootstrap <- parameterEstimates(survey_fit_bootstrap)
#   indirect_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "indirect", "est"]
#   total_effect_bootstrap <- param_estimates_bootstrap[param_estimates_bootstrap$lhs == "total", "est"]
# 
#   if (total_effect_bootstrap != 0) {
#     return(indirect_effect_bootstrap / total_effect_bootstrap)
#   } else {
#     return(NA)
#   }
# }
# 
# # 对每个糖尿病组进行中介分析
# for (diabetes_group in diabetes_groups) {
#   # 选取当前糖尿病组的数据
#   group_data <- subset(paper_data_imputed_diabetes, Diabetes == diabetes_group)
# 
#   # 进行加权的bootstrap
#   bootstraps <- 1000
#   bootstrap_proportions <- numeric(bootstraps)
# 
#   for (i in 1:bootstraps) {
#     bootstrap_data <- weighted_bootstrap_sample(group_data)
#     bootstrap_proportions[i] <- calculate_proportion_mediated(bootstrap_data, mediation_model)
#   }
# 
#   # 计算置信区间
#   alpha <- 0.05
#   lower_bound <- quantile(bootstrap_proportions, alpha / 2, na.rm = TRUE)
#   upper_bound <- quantile(bootstrap_proportions, 1 - alpha / 2, na.rm = TRUE)
#   print(paste0("For diabetes group: ", diabetes_group))
#   print(paste("Proportion mediated CI: [", format(round(lower_bound, 10), nsmall = 10), ", ", format(round(upper_bound, 10), nsmall = 10), "]"))
# }



###########################################亚组分析 hypertension
#####hypertension
library(lavaan.survey)
library(lavaan)

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

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
    
    survey_fit <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = group_NHANES_design)
    summary(survey_fit)
    
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


#################考察自变量和中介变量是否独立影响因素
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables)*2, ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- c("LBXVD3MS", "LBXVD3MS_depression")

# 循环遍历变量并进行分析
for (i in seq_along(variables)) {
  var <- variables[i]
  formula <- as.formula(paste("overall_cognitive_score ~", var, "+ depression.total"))
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  
  results[2*i-1, "Estimate"] <- summary_model$coefficients[2, "Estimate"]
  results[2*i-1, "Std. Error"] <- summary_model$coefficients[2, "Std. Error"]
  results[2*i-1, "t value"] <- summary_model$coefficients[2, "t value"]
  results[2*i-1, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
  results[2*i-1, "Lower CI"] <- results[2*i-1, "Estimate"] - 1.96 * results[2*i-1, "Std. Error"]
  results[2*i-1, "Upper CI"] <- results[2*i-1, "Estimate"] + 1.96 * results[2*i-1, "Std. Error"]
  
  results[2*i, "Estimate"] <- summary_model$coefficients[3, "Estimate"]
  results[2*i, "Std. Error"] <- summary_model$coefficients[3, "Std. Error"]
  results[2*i, "t value"] <- summary_model$coefficients[3, "t value"]
  results[2*i, "Pr(>|t|)"] <- summary_model$coefficients[3, "Pr(>|t|)"]
  results[2*i, "Lower CI"] <- results[2*i, "Estimate"] - 1.96 * results[2*i, "Std. Error"]
  results[2*i, "Upper CI"] <- results[2*i, "Estimate"] + 1.96 * results[2*i, "Std. Error"]
}

# 打印结果
print(results)
#################多因素
# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, ids = ~SDMVPSU, strata = ~SDMVSTRA, nest = TRUE, weights = ~WTMEC2YR, survey.lonely.psu = "adjust")

# 定义要分析的变量
variables <- c("LBXVD3MS")

# 初始化结果矩阵，添加置信区间的列
results <- matrix(NA, nrow = length(variables)*2, ncol = 6)
colnames(results) <- c("Estimate", "Std. Error", "t value", "Pr(>|t|)", "Lower CI", "Upper CI")
rownames(results) <- c(paste(variables, "_main", sep = ""), paste(variables, "_depression.total", sep = ""))

# 定义协变量列表
covariates <- c("Drink", "Stroke", "Education", "Age.group", "Gender", "INDFMPIR", "Marital", "Diabetes", "Hypertension", "depression.total", "Heart")

# 将协变量列表转换为一个字符串，用'+'符号连接
covariates_string <- paste(covariates, collapse = " + ")

# 循环遍历变量并进行分析
for (i in seq_along(variables)) {
  var <- variables[i]
  # 在公式中添加协变量
  formula <- as.formula(paste("overall_cognitive_score ~", var, "+", covariates_string))
  
  model <- svyglm(formula, design = NHANES_design)
  summary_model <- summary(model)
  
  # 提取主要变量的统计信息
  results[2*i-1, "Estimate"] <- summary_model$coefficients[2, "Estimate"]
  results[2*i-1, "Std. Error"] <- summary_model$coefficients[2, "Std. Error"]
  results[2*i-1, "t value"] <- summary_model$coefficients[2, "t value"]
  results[2*i-1, "Pr(>|t|)"] <- summary_model$coefficients[2, "Pr(>|t|)"]
  results[2*i-1, "Lower CI"] <- results[2*i-1, "Estimate"] - 1.96 * results[2*i-1, "Std. Error"]
  results[2*i-1, "Upper CI"] <- results[2*i-1, "Estimate"] + 1.96 * results[2*i-1, "Std. Error"]
  
  # 提取depression.total的统计信息
  results[2*i, "Estimate"] <- summary_model$coefficients[which(rownames(summary_model$coefficients) == "depression.total"), "Estimate"]
  results[2*i, "Std. Error"] <- summary_model$coefficients[which(rownames(summary_model$coefficients) == "depression.total"), "Std. Error"]
  results[2*i, "t value"] <- summary_model$coefficients[which(rownames(summary_model$coefficients) == "depression.total"), "t value"]
  results[2*i, "Pr(>|t|)"] <- summary_model$coefficients[which(rownames(summary_model$coefficients) == "depression.total"), "Pr(>|t|)"]
  results[2*i, "Lower CI"] <- results[2*i, "Estimate"] - 1.96 * results[2*i, "Std. Error"]
  results[2*i, "Upper CI"] <- results[2*i, "Estimate"] + 1.96 * results[2*i, "Std. Error"]
}

# 初始化结果矩阵，添加置信区间的列

# 打印结果
print(results)
#################################################################
#################不同的协变量的中介分析拟合优度比较########
##################################
#中介路径包含慢病，直接路径所有变量
library(lavaan.survey)
library(lavaan)

# 定义复杂样本设计
NHANES_design <- svydesign(data = paper_data_imputed, 
                           ids = ~SDMVPSU, 
                           strata = ~SDMVSTRA, 
                           nest = TRUE, 
                           weights = ~WTMEC2YR, 
                           survey.lonely.psu = "adjust")

# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS + Stroke + Diabetes + Hypertension + Heart 
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Heart
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'
# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")

# 应用NHANES设计加权
survey_fit1 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
##############
############################
#中介路径包含慢病，直接路径只有慢病
# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS + Stroke + Diabetes + Hypertension + Heart 
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Stroke +  Diabetes +  Heart + Hypertension
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'

# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")
# 应用NHANES设计加权
survey_fit2 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
###############
#################################
#中介路径包含慢病，直接路径没有慢病
# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS + Stroke + Diabetes + Hypertension + Heart 
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Drink + Education + Age.group + Gender + INDFMPIR + Marital
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'
# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")

# 应用NHANES设计加权
survey_fit3 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
###############
####################
#中介路径没有慢病，直接路径包含所有
# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Stroke + Drink + Education + Age.group + Gender + INDFMPIR + Marital + Diabetes + Hypertension + Heart
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'
# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")

# 应用NHANES设计加权
survey_fit4 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
##########
##################
#中介路径没有慢病，直接路径只有慢病
# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Stroke +  Diabetes + Hypertension + Heart
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'
# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")

# 应用NHANES设计加权
survey_fit5 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)
summary(survey_fit5,fit.measures=TRUE)
############
########################
#中介路径没有慢病，直接路径没有慢病
# 定义中介分析模型
mediation_model <- '
  # Mediation effect with added covariates for the mediator
  depression.total ~ a * LBXVD3MS 
  
  # Direct effect with covariates on the outcome
  overall_cognitive_score ~ c * LBXVD3MS + b * depression.total + Drink + Education + Age.group + Gender + INDFMPIR + Marital
  
  # Indirect effect
  indirect := a * b
  
  # Total effect
  total := c + indirect
'
# 拟合模型使用MLR估计器
lavaan_fit <- sem(mediation_model, data = paper_data_imputed, estimator="MLR")

# 应用NHANES设计加权
survey_fit6 <- lavaan.survey(lavaan.fit = lavaan_fit, survey.design = NHANES_design)

################

####################
# 加载必要的库
library(lavaan)

# 假设 survey_fit1 到 survey_fit6 已经定义并拟合好了
# 创建模型列表
model_list <- list(survey_fit1, survey_fit2, survey_fit3, survey_fit4, survey_fit5, survey_fit6)

# 初始化一个空的data.frame来存储所有模型的拟合指标
fit_measures_df <- data.frame(model = character(), cfi = numeric(), tli = numeric(), aic = numeric(), bic = numeric(), rmsea = numeric(), srmr = numeric(), sabic = numeric(),stringsAsFactors = FALSE)

# 遍历模型列表，提取拟合指标，并将它们添加到data.frame中
for (i in 1:length(model_list)) {
  # 获取模型汇总
  a <- summary(model_list[[i]], fit.measures=TRUE)
  
  # 提取指标
  cfi <- a$fit['cfi']
  tli <- a$fit['tli']
  aic <- a$fit['aic']
  bic <- a$fit['bic']
  rmsea <- a$fit['rmsea']
  srmr <- a$fit['srmr']
  sabic <- a$fit['bic2']
  
  # 将提取的指标添加到data.frame
  fit_measures_df <- rbind(fit_measures_df, data.frame(model = paste("Model", i), cfi = cfi, tli = tli, aic = aic, bic = bic, rmsea = rmsea, srmr = srmr, sabic = sabic))
}

# 查看汇总的拟合指标
print(fit_measures_df)
