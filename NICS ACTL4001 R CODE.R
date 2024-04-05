########################################################################################################################
############################################## ACTL4001 Group Assignment ############################################### 
####################################### SOA Case Study - SuperLife Saving Lives ######################################## 
########################################################################################################################

####################################################### Settings ####################################################### 
options(scipen=999)

library(tidyverse)
library(readxl)
library(caret)
library(pROC)

################################################## Importing Datasets ################################################## 
INFORCE = read_csv("SuperLife Inforce Dataset.csv", 
                   col_names = c("PolicyNumber", "PolicyType", "IssueYear", "IssueAge", "Sex", "FaceAmount", 
                                 "SmokerStatus", "UnderwritingClass", "UrbanVsRural", "Region", "DistributionChannel", 
                                 "DeathIndicator", "YearOfDeath", "LapseIndicator", "YearOfLapse", "CauseOfDeath"), 
                   skip = 4)

ECONDATA = read_xlsx("Lumaria Economic Data.xlsx", 
                     col_names = c("Year", "Inflation", "OvernightRate", "1YearRiskFreeSpotRate", "10YearRiskFreeSpotRate"), 
                     range = "A13:E74")
MORTDATA = read_xlsx("Lumaria Mortality Table.xlsx", 
                     col_names = c("Age", "MortalityRate"), 
                     range = "A15:B134")

#################################################### Data Cleaning #####################################################
#
INFORCE$LapseIndicator = as.integer(replace(INFORCE$LapseIndicator, INFORCE$LapseIndicator == "Y", 1))
INFORCE = replace_na(INFORCE, list(DeathIndicator = 0, LapseIndicator = 0))
INFORCE$PolicyType = as.factor(INFORCE$PolicyType)
INFORCE$Sex = as.factor(INFORCE$Sex)
#INFORCE$FaceAmount = as.factor(INFORCE$FaceAmount)
INFORCE$SmokerStatus = as.factor(INFORCE$SmokerStatus)
INFORCE$UnderwritingClass = as.factor(INFORCE$UnderwritingClass)
INFORCE$UrbanVsRural = as.factor(INFORCE$UrbanVsRural)
INFORCE$Region = as.factor(INFORCE$Region)
INFORCE$DistributionChannel = as.factor(INFORCE$DistributionChannel)
INFORCE$DeathIndicator = as.factor(INFORCE$DeathIndicator)
INFORCE$LapseIndicator = as.factor(INFORCE$LapseIndicator)
INFORCE$CauseOfDeath = as.factor(INFORCE$CauseOfDeath)

levels(INFORCE$DeathIndicator) = c("F", "T")
levels(INFORCE$LapseIndicator) = c("F", "T")

# Creating New Variable: Current Age
INFORCE$CurrentAge = rep(2023, dim(INFORCE)[1]) - INFORCE$IssueYear + INFORCE$IssueAge
temp = drop_na(data.frame(INFORCE$YearOfDeath - INFORCE$IssueYear + INFORCE$IssueAge))
colnames(temp) = "DeathAge"
INFORCE$CurrentAge[INFORCE$DeathIndicator == 1] = temp$DeathAge

# Summary
summary(INFORCE)
summary(ECONDATA)
summary(MORTDATA)

############################################## Exploratory Data Analysis ############################################### 
INFORCE %>%
ggplot(aes(x = IssueYear)) + 
  geom_histogram(aes(y = after_stat(count)), binwidth = 1, color = "#36B598", fill = "white") + 
  labs(x = "Issue Year", y = "Density", title = "Histogram of Issue Year") + 
  scale_x_continuous(breaks = seq(2000, 2024, 1)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1.2, colour = "grey80"),
        text = element_text(family = "CMU Serif"))

INFORCE %>%
  ggplot(aes(x = IssueAge)) + 
  geom_histogram(aes(y = after_stat(density)), binwidth = 2, color = "#36B598", fill = "white") + 
  labs(x = "Issue Age", y = "Density", title = "Histogram of Issue Age") + 
  scale_x_continuous(breaks = seq(26, 66, 2)) + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1.2, colour = "grey80"),
        text = element_text(family = "CMU Serif"))

INFORCE %>%
  ggplot(aes(x = FaceAmount)) + 
  geom_bar(aes(y = after_stat(count)), color = "#36B598", fill = "white") + 
  labs(x = "Face Amount", y = "Density", title = "Histogram of Face Amount") + 
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1.2, colour = "grey80"),
        text = element_text(family = "CMU Serif"))

SmokerTable = table(INFORCE$SmokerStatus, INFORCE$DeathIndicator)
SmokerTable = cbind(SmokerTable, SmokerTable[,2] / (SmokerTable[,1] + SmokerTable[,2]))
colnames(SmokerTable) = c("Alive", "Dead", "Proportion of Deaths")
SmokerTable

UnderwritingClassTable = table(INFORCE$UnderwritingClass, INFORCE$DeathIndicator)
UnderwritingClassTable = cbind(UnderwritingClassTable, UnderwritingClassTable[,2] / (UnderwritingClassTable[,1] + UnderwritingClassTable[,2]))
temp = UnderwritingClassTable[2, ]
UnderwritingClassTable[2,] = UnderwritingClassTable[3,]
UnderwritingClassTable[3,] = temp
colnames(UnderwritingClassTable) = c("Alive", "Dead", "Proportion of Deaths")
rownames(UnderwritingClassTable) = c("high risk", "moderate risk", "low risk", "very low risk")
UnderwritingClassTable

UrbanRuralTable = table(INFORCE$UrbanVsRural, INFORCE$DeathIndicator)
UrbanRuralTable = cbind(UrbanRuralTable, UrbanRuralTable[,2] / (UrbanRuralTable[,1] + UrbanRuralTable[,2]))
colnames(UrbanRuralTable) = c("Alive", "Dead", "Proportion of Deaths")
UrbanRuralTable

RegionTable = table(INFORCE$Region, INFORCE$DeathIndicator)
RegionTable = cbind(RegionTable, RegionTable[,2] / (RegionTable[,1] + RegionTable[,2]))
colnames(RegionTable) = c("Alive", "Dead", "Proportion of Deaths")
RegionTable

SexTable = table(INFORCE$Sex, INFORCE$DeathIndicator)
SexTable = cbind(SexTable, SexTable[,2] / (SexTable[,1] + SexTable[,2]))
colnames(SexTable) = c("Alive", "Dead", "Proportion of Deaths")
SexTable

SexUnderwritingTable = table(INFORCE$UnderwritingClass, INFORCE$Sex)
SexUnderwritingTable = cbind(SexUnderwritingTable, SexUnderwritingTable[,2] / (SexUnderwritingTable[,1] + SexUnderwritingTable[,2]))
colnames(SexUnderwritingTable) = c("F", "M", "Proportion of Males")
SexUnderwritingTable

DistChannelTable = table(INFORCE$DistributionChannel, INFORCE$DeathIndicator)
DistChannelTable = cbind(DistChannelTable, DistChannelTable[,2] / (DistChannelTable[,1] + DistChannelTable[,2]))
colnames(DistChannelTable) = c("Alive", "Dead", "Proportion of Deaths")
DistChannelTable

SmokerDeathCauseTable = table(INFORCE$SmokerStatus, INFORCE$CauseOfDeath)
SmokerDeathCauseTable


# Graphs
# Inforce Growth
attach(INFORCE)
data = INFORCE %>%
  mutate(YearOfLapse = ifelse(is.na(YearOfLapse), 10000, YearOfLapse)) %>%
  mutate(YearOfDeath= ifelse(is.na(YearOfDeath), 10000, YearOfDeath))


growth = data.frame('year' = c(min(IssueYear):max(IssueYear)), 
                      'count' = c(0)
)



proj = data.frame(2024:2043, c(899394,
                               952382,
                               1006554,
                               1061910,
                               1118450,
                               1176174,
                               1235082,
                               1295174,
                               1356450,
                               1418910,
                               1482554,
                               1547382,
                               1613394,
                               1680590,
                               1748970,
                               1818534,
                               1889282,
                               1961214,
                               2034330,
                               2108630))
colnames(proj) = c("year", "count")
growth = rbind(growth, proj)

for (year in 2001:2023) {
  count_policies = data %>%
    filter(IssueYear <= year) %>%
    filter(YearOfLapse > year) %>%
    filter(YearOfDeath > year) 
  
  growth$count[year-2000] <- nrow(count_policies)
  
}
coef = c(2320074658.2794, -2344071.2976, 592.0127)
x = seq(2001, 2043, by = 0.1)
y = coef[1] + coef[2]*x + coef[3]*x^2  
poly = data.frame(x = x, y = y)

InforceGrowth = ggplot() +
  geom_bar(data = growth, aes(x = year, y = count), stat = "identity", fill = "skyblue") + 
  geom_line(data = poly, aes(x = x, y = y), size = 2, colour = "#000D53") +
  labs(y = "Number of Inforce Policies", x = "Year", title = "Inforce Policy Growth") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "CMU Serif"))
ggsave("Inforce Growth.jpg", width = 16, height = 12, units = "cm", InforceGrowth)

InforceGrowth



#Proportions
data = INFORCE %>%
  mutate(YearOfLapse = ifelse(is.na(YearOfLapse), 10000, YearOfLapse)) %>%
  mutate(YearOfDeath= ifelse(is.na(YearOfDeath), 10000, YearOfDeath))

proportion_WL = c()

for (i in 1:23) {
  count <- data %>% 
    filter(IssueYear <= i+2000) %>%
    filter(YearOfLapse > i+2000) %>%
    filter(YearOfDeath > i+2000) %>%
    
    group_by(PolicyType) %>%
    count(PolicyType)
  
  wlcount <- count %>%
    filter(PolicyType == "SPWL")
  
  proportion_WL[i] <- wlcount$n/(sum(count$n))
  
}

proportion_T = c()
for (i in 1:23) {
  count <- data %>% 
    filter(IssueYear <= i+2000) %>%
    filter(YearOfLapse > i+2000) %>%
    filter(YearOfDeath > i+2000) %>%
    
    group_by(PolicyType) %>%
    count(PolicyType)
  
  tcount <- count %>%
    filter(PolicyType == "T20")
  
  proportion_T[i] <- tcount$n/(sum(count$n))
  
}

year = 2001:2043

proportion_WL = c(proportion_WL, c(0.46642, 0.50076, 0.52978, 0.55493, 0.57711, 0.59695, 0.61490, 0.63128, 0.64636, 0.66031, 0.67331, 0.68546, 0.69688, 0.70764, 0.71782, 0.72748, 0.73667, 0.74543, 0.75380, 0.76181))
proportion_T = 1 - proportion_WL
prop = data_frame(year, proportion_WL, proportion_T)

prop_long = prop %>%
  pivot_longer(cols = c("proportion_WL", "proportion_T"), names_to = "Policy_Type", values_to = "Proportion")

# Plot stacked bar chart
Policy_Proportion = ggplot(prop_long, aes(x = year, y = Proportion, fill = Policy_Type)) +
  geom_col() +
  labs(y = "Proportion", x = "Year", title = "Proportion of Policy Types", fill = "Policy Type") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "CMU Serif")) +
  scale_fill_manual(values = c("proportion_WL" = "skyblue", "proportion_T" = "coral"), labels = c("Whole Life", "20 Year Term"))
ggsave("Policy Proportion.jpg", width = 16, height = 12, units = "cm", Policy_Proportion)
Policy_Proportion















################################################# Modelling Mortality ################################################## 
# Model: The expected cost for each policyholder using the mortality data
# Do the term life first
# Find the probability of death within 20 years
# INFORCE$SurvProb20Year = (1-MORTDATA$MortalityRate[INFORCE$IssueAge + 20]) / (1-MORTDATA$MortalityRate[INFORCE$IssueAge])
# INFORCE$DeathProb20Year = 1 - INFORCE$SurvProb20Year
# INFORCE$ExpectedPayout = INFORCE$DeathProb20Year * INFORCE$FaceAmount


# GLM Modelling for Predicting Death
glmControl = trainControl(method = "none",  classProbs = TRUE, 
                          summaryFunction = defaultSummary)
fitglm = train(DeathIndicator ~ PolicyType + IssueYear + IssueAge + Sex + FaceAmount + SmokerStatus + UnderwritingClass 
               + UrbanVsRural + Region + DistributionChannel + LapseIndicator,
               data = INFORCE, method = "glm", family = binomial(), 
               metric = "AUC", trControl = glmControl)
summary(fitglm)
fitglm
response = predict(fitglm, newdata = INFORCE, type = "prob", list = F)
prediction = predict(fitglm, newdata = INFORCE)
CM = confusionMatrix(prediction, INFORCE$DeathIndicator, positive = "T")
F1 = CM$byClass[7]
test_roc = roc(INFORCE$DeathIndicator ~ response$T, plot = TRUE, print.auc = TRUE)
CM
F1
as.numeric(test_roc$auc)

# Lifetable Calculations - lx
lx = c(10000)
for (i in 2:120){
  lx[i] = (1-MORTDATA$MortalityRate[i-1]) * lx[i-1]
}
MORTDATA$lx = round(lx, 4)

# Lifetable Calculations - dx
dx = c(0)
for (i in 1:120) {
  dx[i] = lx[i] - lx[i+1]
}
MORTDATA$dx = round(dx, 4)

# Lifetable Calculations - px
MORTDATA$px = round(1-MORTDATA$MortalityRate, 6)


# Empirical 10 Year Death Probabilities
qx_10yr = rep(0, 120)
for (i in 26:120) {
  qx_10yr[i] = unlist(count(filter(INFORCE, IssueAge == i, IssueYear <= 2013, DeathIndicator == "T", YearOfDeath - IssueYear <= 10)), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i, IssueYear<= 2013, DeathIndicator == "F")), use.names = F) + 
        unlist(count(filter(INFORCE, IssueAge == i, IssueYear <= 2013, DeathIndicator == "T", YearOfDeath - IssueYear <= 10)), use.names = F) )
}
round(qx_10yr, 4)

# Theoretical 10 Year Death Probabilities
qx_lt_10yr = rep(0, 120)
for (i in 26:120) {
  qx_lt_10yr[i] = (MORTDATA$lx[i] - MORTDATA$lx[i+10])/(MORTDATA$lx[i])
}
round(qx_lt_10yr, 4)

# Lifetable from INFORCE
qx_lt = rep(0, 120)
for (i in 26:64) {
  qx_lt[i] = unlist(count(filter(INFORCE, IssueAge == i, DeathIndicator == "T", YearOfDeath - IssueYear == 1)), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i)), use.names = F) )
}
round(qx_lt, 8)

MORTDATA$qx_inforce = qx_lt

# Checking the INFORCE Morality by Underwriting Class
qx_lt_verylow = rep(0, 120)
for (i in 26:64) {
  qx_lt_verylow[i] = unlist(count(filter(INFORCE, IssueAge == i, DeathIndicator == "T", YearOfDeath - IssueYear == 1, UnderwritingClass == "very low risk")), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i, UnderwritingClass == "very low risk")), use.names = F) )
}
round(qx_lt_verylow, 8)

qx_lt_low = rep(0, 120)
for (i in 26:64) {
  qx_lt_low[i] = unlist(count(filter(INFORCE, IssueAge == i, DeathIndicator == "T", YearOfDeath - IssueYear == 1, UnderwritingClass == "low risk")), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i, UnderwritingClass == "low risk")), use.names = F) )
}
round(qx_lt_low, 8)

qx_lt_moderate = rep(0, 120)
for (i in 26:64) {
  qx_lt_moderate[i] = unlist(count(filter(INFORCE, IssueAge == i, DeathIndicator == "T", YearOfDeath - IssueYear == 1, UnderwritingClass == "moderate risk")), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i, UnderwritingClass == "moderate risk")), use.names = F) )
}
round(qx_lt_moderate, 8)

qx_lt_high = rep(0, 120)
for (i in 26:64) {
  qx_lt_high[i] = unlist(count(filter(INFORCE, IssueAge == i, DeathIndicator == "T", YearOfDeath - IssueYear == 1, UnderwritingClass == "high risk")), use.names = F) / 
    ( unlist(count(filter(INFORCE, IssueAge == i, UnderwritingClass == "high risk")), use.names = F) )
}
round(qx_lt_high, 8)

mean(qx_lt_verylow / qx_lt, na.rm = T)
mean(qx_lt_low / qx_lt, na.rm = T)
mean(qx_lt_moderate / qx_lt, na.rm = T)
mean(qx_lt_high / qx_lt, na.rm = T)

qx_lt_verylow[26:65] / MORTDATA$MortalityRate[Age = 26:65]
qx_lt_low[26:65] / MORTDATA$MortalityRate[Age = 26:65]
qx_lt_moderate[26:65] / MORTDATA$MortalityRate[Age = 26:65]
qx_lt_high[26:65] / MORTDATA$MortalityRate[Age = 26:65]

mean(qx_lt_verylow[26:65] / MORTDATA$MortalityRate[Age = 26:65], na.rm = T)
mean(qx_lt_low[26:65] / MORTDATA$MortalityRate[Age = 26:65], na.rm = T)
mean(qx_lt_moderate[26:65] / MORTDATA$MortalityRate[Age = 26:65], na.rm = T)
mean(qx_lt_high[26:65] / MORTDATA$MortalityRate[Age = 26:65], na.rm = T)

# New Mortality Tables for Underwriting Classes
MORTDATA$qx_verylowrisk = MORTDATA$MortalityRate * c(rep(1, 18), rep(0.6, 120-18))
MORTDATA$qx_lowrisk = MORTDATA$MortalityRate * c(rep(1, 18), rep(0.8, 120-18))
MORTDATA$qx_moderaterisk = MORTDATA$MortalityRate * c(rep(1, 18), rep(1.1, 120-18))
MORTDATA$qx_highrisk = MORTDATA$MortalityRate * c(rep(1, 18), rep(1.4, 120-18))

lx_verylow = c(10000)
for (i in 2:120){
  lx_verylow[i] = (1-MORTDATA$qx_verylowrisk[i-1]) * lx_verylow[i-1]
}
MORTDATA$lx_verylow = round(lx_verylow, 4)

lx_low = c(10000)
for (i in 2:120){
  lx_low[i] = (1-MORTDATA$qx_lowrisk[i-1]) * lx_low[i-1]
}

MORTDATA$lx_low = round(lx_low, 4)

lx_moderate = c(10000)
for (i in 2:120){
  lx_moderate[i] = (1-MORTDATA$qx_moderaterisk[i-1]) * lx_moderate[i-1]
}
MORTDATA$lx_moderate = round(lx_moderate, 4)

lx_high = c(10000)
for (i in 2:120){
  lx_high[i] = (1-MORTDATA$qx_highrisk[i-1]) * lx_high[i-1]
}
MORTDATA$lx_high = round(lx_high, 4)

# Data Visualisation
MortalityRatesGG = MORTDATA[26:64,] %>%
  ggplot(aes(x = Age)) +
  geom_line(aes(y = MortalityRate, colour = "MORTDATA"), size = 1) +
  geom_line(aes(y = qx_inforce, colour = "INFORCE")) +
  geom_smooth(aes(y = qx_inforce, colour = "Spline")) +
  scale_colour_manual(values=c(MORTDATA = "#6571B6", INFORCE = "#E0681A", Spline = "#E12B7B")) + 
  labs(y = "Mortality Rate", x = "Age", colour = "Source", 
       title = "Mortality Rates From INFORCE vs MORTDATA") +
  theme(plot.title = element_text(hjust = 0.5, size = 14),
        axis.line = element_line(linewidth = 1, colour = "grey80"),
        text = element_text(family = "CMU Serif"))
ggsave("Mortality Rates Analysis.jpg", width = 16, height = 12, units = "cm", MortalityRatesGG)

################################################# Premium Calculations ################################################# 
# Basic APV Premium Calculation
v = 1 / (1.067/1.0295)

TermPremium = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    TermPremium[i] = TermPremium[i] + v^k * MORTDATA$lx[i + k - 1] / MORTDATA$lx[i] * MORTDATA$MortalityRate[i + k - 1]
  }
}
TermPremium

WLPremium = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WLPremium[i] = WLPremium[i] + v^k * MORTDATA$lx[i + k - 1] / MORTDATA$lx[i] * MORTDATA$MortalityRate[i + k - 1]
  }
}
WLPremium

# By underwriting classes - Whole Life
MORTDATA_base = read_xlsx("Modelling.xlsx", 
                     col_names = c("Age", "qx", "LF_vl", "qx_vl", "lx_vl", 
                                   "LF_l", "qx_l", "lx_l", 
                                   "LF_m", "qx_m", "lx_m", 
                                   "LF_h", "qx_h", "lx_h"), 
                     sheet = "Mortality (No Interventions)",
                     range = "B10:O129")

MORTDATA_base = as.data.frame(sapply(MORTDATA_base, as.numeric))

wl_vl_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    wl_vl_prem[i] = wl_vl_prem[i] + v^k * MORTDATA_base$lx_vl[i + k - 1] / MORTDATA_base$lx_vl[i] * MORTDATA_base$qx_vl[i + k - 1]
  }
}
wl_vl_prem

wl_l_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    wl_l_prem[i] = wl_l_prem[i] + v^k * MORTDATA_base$lx_l[i + k - 1] / MORTDATA_base$lx_l[i] * MORTDATA_base$qx_l[i + k - 1]
  }
}
wl_l_prem

wl_m_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    wl_m_prem[i] = wl_m_prem[i] + v^k * MORTDATA_base$lx_m[i + k - 1] / MORTDATA_base$lx_m[i] * MORTDATA_base$qx_m[i + k - 1]
  }
}
wl_m_prem

wl_h_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    wl_h_prem[i] = wl_h_prem[i] + v^k * MORTDATA_base$lx_h[i + k - 1] / MORTDATA_base$lx_h[i] * MORTDATA_base$qx_h[i + k - 1]
  }
}
wl_h_prem

wl_base = data.frame(wl_vl_prem, wl_l_prem, wl_m_prem, wl_h_prem)
colnames(wl_base) = c("vl", "l", 'm', "h")

write.csv(as.matrix(wl_base), "wl_premium.csv")

# Term Insurance
# Need annuity present values
ax_vl = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl[i] = ax_vl[i] + v^k * MORTDATA_base$lx_vl[i + k - 1] / MORTDATA_base$lx_vl[i]
  }
}
ax_vl

ax_l = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l[i] = ax_l[i] + v^k * MORTDATA_base$lx_l[i + k - 1] /MORTDATA_base$lx_l[i]
  }
}
ax_l

ax_m = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m[i] = ax_m[i] + v^k * MORTDATA_base$lx_m[i + k - 1] / MORTDATA_base$lx_m[i]
  }
}
ax_m

ax_h = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h[i] = ax_h[i] + v^k * MORTDATA_base$lx_h[i + k - 1] / MORTDATA_base$lx_h[i]
  }
}
ax_h


# Very Low
t20_vl_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem[i] = t20_vl_prem[i] + v^k * MORTDATA_base$lx_vl[i + k - 1] / MORTDATA_base$lx_vl[i] * MORTDATA_base$qx_vl[i + k - 1]
  }
}
t20_vl_prem = t20_vl_prem / ax_vl

# Low
t20_l_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem[i] = t20_l_prem[i] + v^k * MORTDATA_base$lx_l[i + k - 1] / MORTDATA_base$lx_l[i] * MORTDATA_base$qx_l[i + k - 1]
  }
}
t20_l_prem = t20_l_prem / ax_l

# Moderate
t20_m_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem[i] = t20_m_prem[i] + v^k * MORTDATA_base$lx_m[i + k - 1] / MORTDATA_base$lx_m[i] * MORTDATA_base$qx_m[i + k - 1]
  }
}
t20_m_prem = t20_m_prem / ax_m

# High
t20_h_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem[i] = t20_h_prem[i] + v^k * MORTDATA_base$lx_h[i + k - 1] / MORTDATA_base$lx_h[i] * MORTDATA_base$qx_h[i + k - 1]
  }
}
t20_h_prem = t20_h_prem / ax_h

t20_base = data.frame(t20_vl_prem, t20_l_prem, t20_m_prem, t20_h_prem)
colnames(t20_base) = c("vl", "l", 'm', "h")

write.csv(as.matrix(t20_base), "t20premium.csv")

######################################## Intervention Premium Calculations - vl ######################################## 
# By underwriting classes - Whole Life
MORTDATA_verylow = read_xlsx("Modelling.xlsx", 
                         col_names = c("Age", "Mortality", "LF1", "qx1", "lx1",
                                       "LF2", "qx2", "lx2",
                                       "LF3", "qx3", "lx3",
                                       "LF4", "qx4", "lx4",
                                       "LF12", "qx12", "lx12",
                                       "LF13", "qx13", "lx13",
                                       "LF14", "qx14", "lx14",
                                       "LF23", "qx23", "lx23",
                                       "LF24", "qx24", "lx24",
                                       "LF34", "qx34", "lx34",
                                       "LF123", "qx123", "lx123",
                                       "LF124", "qx124", "lx124",
                                       "LF134", "qx134", "lx134",
                                       "LF234", "qx234", "lx234",
                                       "LF1234", "qx1234", "lx1234"), 
                         sheet = "Mortality (Interventions)",
                         range = "B9:AV128")

WL_vl_1_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_1_prem[i] = WL_vl_1_prem[i] + v^k * MORTDATA_verylow$lx1[i + k - 1] / MORTDATA_verylow$lx1[i] * MORTDATA_verylow$qx1[i + k - 1]
  }
}
WL_vl_1_prem

WL_vl_2_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_2_prem[i] = WL_vl_2_prem[i] + v^k * MORTDATA_verylow$lx2[i + k - 1] / MORTDATA_verylow$lx2[i] * MORTDATA_verylow$qx2[i + k - 1]
  }
}
WL_vl_2_prem

WL_vl_3_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_3_prem[i] = WL_vl_3_prem[i] + v^k * MORTDATA_verylow$lx3[i + k - 1] / MORTDATA_verylow$lx3[i] * MORTDATA_verylow$qx3[i + k - 1]
  }
}
WL_vl_3_prem

WL_vl_4_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_4_prem[i] = WL_vl_4_prem[i] + v^k * MORTDATA_verylow$lx4[i + k - 1] / MORTDATA_verylow$lx4[i] * MORTDATA_verylow$qx4[i + k - 1]
  }
}
WL_vl_4_prem

WL_vl_12_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_12_prem[i] = WL_vl_12_prem[i] + v^k * MORTDATA_verylow$lx12[i + k - 1] / MORTDATA_verylow$lx12[i] * MORTDATA_verylow$qx12[i + k - 1]
  }
}
WL_vl_12_prem

WL_vl_13_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_13_prem[i] = WL_vl_13_prem[i] + v^k * MORTDATA_verylow$lx13[i + k - 1] / MORTDATA_verylow$lx13[i] * MORTDATA_verylow$qx13[i + k - 1]
  }
}
WL_vl_13_prem

WL_vl_14_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_14_prem[i] = WL_vl_14_prem[i] + v^k * MORTDATA_verylow$lx14[i + k - 1] / MORTDATA_verylow$lx14[i] * MORTDATA_verylow$qx14[i + k - 1]
  }
}
WL_vl_14_prem

WL_vl_23_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_23_prem[i] = WL_vl_23_prem[i] + v^k * MORTDATA_verylow$lx23[i + k - 1] / MORTDATA_verylow$lx23[i] * MORTDATA_verylow$qx23[i + k - 1]
  }
}
WL_vl_23_prem

WL_vl_24_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_24_prem[i] = WL_vl_24_prem[i] + v^k * MORTDATA_verylow$lx24[i + k - 1] / MORTDATA_verylow$lx24[i] * MORTDATA_verylow$qx24[i + k - 1]
  }
}
WL_vl_24_prem

WL_vl_34_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_34_prem[i] = WL_vl_34_prem[i] + v^k * MORTDATA_verylow$lx34[i + k - 1] / MORTDATA_verylow$lx34[i] * MORTDATA_verylow$qx34[i + k - 1]
  }
}
WL_vl_34_prem

WL_vl_123_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_123_prem[i] = WL_vl_123_prem[i] + v^k * MORTDATA_verylow$lx123[i + k - 1] / MORTDATA_verylow$lx123[i] * MORTDATA_verylow$qx123[i + k - 1]
  }
}
WL_vl_123_prem

WL_vl_124_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_124_prem[i] = WL_vl_124_prem[i] + v^k * MORTDATA_verylow$lx124[i + k - 1] / MORTDATA_verylow$lx124[i] * MORTDATA_verylow$qx124[i + k - 1]
  }
}
WL_vl_124_prem

WL_vl_134_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_134_prem[i] = WL_vl_134_prem[i] + v^k * MORTDATA_verylow$lx134[i + k - 1] / MORTDATA_verylow$lx134[i] * MORTDATA_verylow$qx134[i + k - 1]
  }
}
WL_vl_134_prem

WL_vl_234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_234_prem[i] = WL_vl_234_prem[i] + v^k * MORTDATA_verylow$lx234[i + k - 1] / MORTDATA_verylow$lx234[i] * MORTDATA_verylow$qx234[i + k - 1]
  }
}
WL_vl_234_prem

WL_vl_1234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_vl_1234_prem[i] = WL_vl_1234_prem[i] + v^k * MORTDATA_verylow$lx1234[i + k - 1] / MORTDATA_verylow$lx1234[i] * MORTDATA_verylow$qx1234[i + k - 1]
  }
}
WL_vl_1234_prem

wl_vl = data_frame( WL_vl_1_prem, WL_vl_2_prem, WL_vl_3_prem, WL_vl_4_prem, 
                          WL_vl_12_prem, WL_vl_13_prem, WL_vl_14_prem, WL_vl_23_prem, WL_vl_24_prem, WL_vl_34_prem, 
                          WL_vl_123_prem, WL_vl_124_prem, WL_vl_134_prem, WL_vl_234_prem, WL_vl_1234_prem)
colnames(wl_vl) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(wl_vl), "wholelife_verylow.csv")


# 20 Year Term - Annuities
ax_vl_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_1[i] = ax_vl_1[i] + v^k * MORTDATA_verylow$lx1[i + k - 1] / MORTDATA_verylow$lx1[i]
  }
}
ax_vl_1

ax_vl_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_2[i] = ax_vl_2[i] + v^k * MORTDATA_verylow$lx2[i + k - 1] / MORTDATA_verylow$lx2[i]
  }
}
ax_vl_2

ax_vl_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_3[i] = ax_vl_3[i] + v^k * MORTDATA_verylow$lx3[i + k - 1] / MORTDATA_verylow$lx3[i]
  }
}
ax_vl_3

ax_vl_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_4[i] = ax_vl_4[i] + v^k * MORTDATA_verylow$lx4[i + k - 1] / MORTDATA_verylow$lx4[i]
  }
}
ax_vl_4

ax_vl_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_12[i] = ax_vl_12[i] + v^k * MORTDATA_verylow$lx12[i + k - 1] / MORTDATA_verylow$lx12[i]
  }
}
ax_vl_12

ax_vl_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_13[i] = ax_vl_13[i] + v^k * MORTDATA_verylow$lx13[i + k - 1] / MORTDATA_verylow$lx13[i]
  }
}
ax_vl_13

ax_vl_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_14[i] = ax_vl_14[i] + v^k * MORTDATA_verylow$lx14[i + k - 1] / MORTDATA_verylow$lx14[i]
  }
}
ax_vl_14

ax_vl_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_23[i] = ax_vl_23[i] + v^k * MORTDATA_verylow$lx23[i + k - 1] / MORTDATA_verylow$lx23[i]
  }
}
ax_vl_23

ax_vl_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_24[i] = ax_vl_24[i] + v^k * MORTDATA_verylow$lx24[i + k - 1] / MORTDATA_verylow$lx24[i]
  }
}
ax_vl_24

ax_vl_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_34[i] = ax_vl_34[i] + v^k * MORTDATA_verylow$lx34[i + k - 1] / MORTDATA_verylow$lx34[i]
  }
}
ax_vl_34

ax_vl_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_123[i] = ax_vl_123[i] + v^k * MORTDATA_verylow$lx123[i + k - 1] / MORTDATA_verylow$lx123[i]
  }
}
ax_vl_123

ax_vl_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_124[i] = ax_vl_124[i] + v^k * MORTDATA_verylow$lx124[i + k - 1] / MORTDATA_verylow$lx124[i]
  }
}
ax_vl_124

ax_vl_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_134[i] = ax_vl_134[i] + v^k * MORTDATA_verylow$lx134[i + k - 1] / MORTDATA_verylow$lx134[i]
  }
}
ax_vl_134

ax_vl_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_234[i] = ax_vl_234[i] + v^k * MORTDATA_verylow$lx234[i + k - 1] / MORTDATA_verylow$lx234[i]
  }
}
ax_vl_234

ax_vl_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_vl_1234[i] = ax_vl_1234[i] + v^k * MORTDATA_verylow$lx1234[i + k - 1] / MORTDATA_verylow$lx1234[i]
  }
}
ax_vl_1234


# 20 Year Term - Benefits
t20_vl_prem_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_1[i] = t20_vl_prem_1[i] + v^k * MORTDATA_verylow$lx1[i + k - 1] / MORTDATA_verylow$lx1[i] * MORTDATA_verylow$qx1[i + k - 1]
  }
}
t20_vl_prem_1 = t20_vl_prem_1 / ax_vl_1
t20_vl_prem_1

t20_vl_prem_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_2[i] = t20_vl_prem_2[i] + v^k * MORTDATA_verylow$lx2[i + k - 1] / MORTDATA_verylow$lx2[i] * MORTDATA_verylow$qx2[i + k - 1]
  }
}
t20_vl_prem_2 = t20_vl_prem_2 / ax_vl_2
t20_vl_prem_2

t20_vl_prem_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_3[i] = t20_vl_prem_3[i] + v^k * MORTDATA_verylow$lx3[i + k - 1] / MORTDATA_verylow$lx3[i] * MORTDATA_verylow$qx3[i + k - 1]
  }
}
t20_vl_prem_3 = t20_vl_prem_3 / ax_vl_3
t20_vl_prem_3

t20_vl_prem_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_4[i] = t20_vl_prem_4[i] + v^k * MORTDATA_verylow$lx4[i + k - 1] / MORTDATA_verylow$lx4[i] * MORTDATA_verylow$qx4[i + k - 1]
  }
}
t20_vl_prem_4 = t20_vl_prem_4 / ax_vl_4
t20_vl_prem_4

t20_vl_prem_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_12[i] = t20_vl_prem_12[i] + v^k * MORTDATA_verylow$lx12[i + k - 1] / MORTDATA_verylow$lx12[i] * MORTDATA_verylow$qx12[i + k - 1]
  }
}
t20_vl_prem_12 = t20_vl_prem_12 / ax_vl_12
t20_vl_prem_12

t20_vl_prem_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_13[i] = t20_vl_prem_13[i] + v^k * MORTDATA_verylow$lx13[i + k - 1] / MORTDATA_verylow$lx13[i] * MORTDATA_verylow$qx13[i + k - 1]
  }
}
t20_vl_prem_13 = t20_vl_prem_13 / ax_vl_13
t20_vl_prem_13

t20_vl_prem_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_14[i] = t20_vl_prem_14[i] + v^k * MORTDATA_verylow$lx14[i + k - 1] / MORTDATA_verylow$lx14[i] * MORTDATA_verylow$qx14[i + k - 1]
  }
}
t20_vl_prem_14 = t20_vl_prem_14 / ax_vl_14
t20_vl_prem_14

t20_vl_prem_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_23[i] = t20_vl_prem_23[i] + v^k * MORTDATA_verylow$lx23[i + k - 1] / MORTDATA_verylow$lx23[i] * MORTDATA_verylow$qx23[i + k - 1]
  }
}
t20_vl_prem_23 = t20_vl_prem_23 / ax_vl_23
t20_vl_prem_23

t20_vl_prem_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_24[i] = t20_vl_prem_24[i] + v^k * MORTDATA_verylow$lx24[i + k - 1] / MORTDATA_verylow$lx24[i] * MORTDATA_verylow$qx24[i + k - 1]
  }
}
t20_vl_prem_24 = t20_vl_prem_24 / ax_vl_24
t20_vl_prem_24

t20_vl_prem_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_34[i] = t20_vl_prem_34[i] + v^k * MORTDATA_verylow$lx34[i + k - 1] / MORTDATA_verylow$lx34[i] * MORTDATA_verylow$qx34[i + k - 1]
  }
}
t20_vl_prem_34 = t20_vl_prem_34 / ax_vl_34
t20_vl_prem_34

t20_vl_prem_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_123[i] = t20_vl_prem_123[i] + v^k * MORTDATA_verylow$lx123[i + k - 1] / MORTDATA_verylow$lx123[i] * MORTDATA_verylow$qx123[i + k - 1]
  }
}
t20_vl_prem_123 = t20_vl_prem_123 / ax_vl_123
t20_vl_prem_123

t20_vl_prem_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_124[i] = t20_vl_prem_124[i] + v^k * MORTDATA_verylow$lx124[i + k - 1] / MORTDATA_verylow$lx124[i] * MORTDATA_verylow$qx124[i + k - 1]
  }
}
t20_vl_prem_124 = t20_vl_prem_124 / ax_vl_124
t20_vl_prem_124

t20_vl_prem_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_134[i] = t20_vl_prem_134[i] + v^k * MORTDATA_verylow$lx134[i + k - 1] / MORTDATA_verylow$lx134[i] * MORTDATA_verylow$qx134[i + k - 1]
  }
}
t20_vl_prem_134 = t20_vl_prem_134 / ax_vl_134
t20_vl_prem_134

t20_vl_prem_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_234[i] = t20_vl_prem_234[i] + v^k * MORTDATA_verylow$lx234[i + k - 1] / MORTDATA_verylow$lx234[i] * MORTDATA_verylow$qx234[i + k - 1]
  }
}
t20_vl_prem_234 = t20_vl_prem_234 / ax_vl_234
t20_vl_prem_234

t20_vl_prem_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_vl_prem_1234[i] = t20_vl_prem_1234[i] + v^k * MORTDATA_verylow$lx1234[i + k - 1] / MORTDATA_verylow$lx1234[i] * MORTDATA_verylow$qx1234[i + k - 1]
  }
}
t20_vl_prem_1234 = t20_vl_prem_1234 / ax_vl_1234
t20_vl_prem_1234


t20_vl = data.frame(t20_vl_prem_1, t20_vl_prem_2, t20_vl_prem_3, t20_vl_prem_4, 
                           t20_vl_prem_12, t20_vl_prem_13, t20_vl_prem_14, t20_vl_prem_23, t20_vl_prem_24, t20_vl_prem_34, 
                           t20_vl_prem_123, t20_vl_prem_124, t20_vl_prem_134, t20_vl_prem_234, t20_vl_prem_1234 )
colnames(t20_vl) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(t20_vl), "t20_verylow.csv")

######################################## Intervention Premium Calculations - l ######################################### 
# By underwriting classes - Whole Life
MORTDATA_low = read_xlsx("Modelling.xlsx", 
                             col_names = c("Age", "Mortality", "LF1", "qx1", "lx1",
                                           "LF2", "qx2", "lx2",
                                           "LF3", "qx3", "lx3",
                                           "LF4", "qx4", "lx4",
                                           "LF12", "qx12", "lx12",
                                           "LF13", "qx13", "lx13",
                                           "LF14", "qx14", "lx14",
                                           "LF23", "qx23", "lx23",
                                           "LF24", "qx24", "lx24",
                                           "LF34", "qx34", "lx34",
                                           "LF123", "qx123", "lx123",
                                           "LF124", "qx124", "lx124",
                                           "LF134", "qx134", "lx134",
                                           "LF234", "qx234", "lx234",
                                           "LF1234", "qx1234", "lx1234"), 
                             sheet = "Mortality (Interventions)",
                             range = "B137:AV256")

WL_l_1_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_1_prem[i] = WL_l_1_prem[i] + v^k * MORTDATA_low$lx1[i + k - 1] / MORTDATA_low$lx1[i] * MORTDATA_low$qx1[i + k - 1]
  }
}
WL_l_1_prem

WL_l_2_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_2_prem[i] = WL_l_2_prem[i] + v^k * MORTDATA_low$lx2[i + k - 1] / MORTDATA_low$lx2[i] * MORTDATA_low$qx2[i + k - 1]
  }
}
WL_l_2_prem

WL_l_3_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_3_prem[i] = WL_l_3_prem[i] + v^k * MORTDATA_low$lx3[i + k - 1] / MORTDATA_low$lx3[i] * MORTDATA_low$qx3[i + k - 1]
  }
}
WL_l_3_prem

WL_l_4_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_4_prem[i] = WL_l_4_prem[i] + v^k * MORTDATA_low$lx4[i + k - 1] / MORTDATA_low$lx4[i] * MORTDATA_low$qx4[i + k - 1]
  }
}
WL_l_4_prem

WL_l_12_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_12_prem[i] = WL_l_12_prem[i] + v^k * MORTDATA_low$lx12[i + k - 1] / MORTDATA_low$lx12[i] * MORTDATA_low$qx12[i + k - 1]
  }
}
WL_l_12_prem

WL_l_13_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_13_prem[i] = WL_l_13_prem[i] + v^k * MORTDATA_low$lx13[i + k - 1] / MORTDATA_low$lx13[i] * MORTDATA_low$qx13[i + k - 1]
  }
}
WL_l_13_prem

WL_l_14_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_14_prem[i] = WL_l_14_prem[i] + v^k * MORTDATA_low$lx14[i + k - 1] / MORTDATA_low$lx14[i] * MORTDATA_low$qx14[i + k - 1]
  }
}
WL_l_14_prem

WL_l_23_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_23_prem[i] = WL_l_23_prem[i] + v^k * MORTDATA_low$lx23[i + k - 1] / MORTDATA_low$lx23[i] * MORTDATA_low$qx23[i + k - 1]
  }
}
WL_l_23_prem

WL_l_24_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_24_prem[i] = WL_l_24_prem[i] + v^k * MORTDATA_low$lx24[i + k - 1] / MORTDATA_low$lx24[i] * MORTDATA_low$qx24[i + k - 1]
  }
}
WL_l_24_prem

WL_l_34_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_34_prem[i] = WL_l_34_prem[i] + v^k * MORTDATA_low$lx34[i + k - 1] / MORTDATA_low$lx34[i] * MORTDATA_low$qx34[i + k - 1]
  }
}
WL_l_34_prem

WL_l_123_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_123_prem[i] = WL_l_123_prem[i] + v^k * MORTDATA_low$lx123[i + k - 1] / MORTDATA_low$lx123[i] * MORTDATA_low$qx123[i + k - 1]
  }
}
WL_l_123_prem

WL_l_124_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_124_prem[i] = WL_l_124_prem[i] + v^k * MORTDATA_low$lx124[i + k - 1] / MORTDATA_low$lx124[i] * MORTDATA_low$qx124[i + k - 1]
  }
}
WL_l_124_prem

WL_l_134_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_134_prem[i] = WL_l_134_prem[i] + v^k * MORTDATA_low$lx134[i + k - 1] / MORTDATA_low$lx134[i] * MORTDATA_low$qx134[i + k - 1]
  }
}
WL_l_134_prem

WL_l_234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_234_prem[i] = WL_l_234_prem[i] + v^k * MORTDATA_low$lx234[i + k - 1] / MORTDATA_low$lx234[i] * MORTDATA_low$qx234[i + k - 1]
  }
}
WL_l_234_prem

WL_l_1234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_l_1234_prem[i] = WL_l_1234_prem[i] + v^k * MORTDATA_low$lx1234[i + k - 1] / MORTDATA_low$lx1234[i] * MORTDATA_low$qx1234[i + k - 1]
  }
}
WL_l_1234_prem

wl_l = data_frame(WL_l_1_prem, WL_l_2_prem, WL_l_3_prem, WL_l_4_prem, 
                          WL_l_12_prem, WL_l_13_prem, WL_l_14_prem, WL_l_23_prem, WL_l_24_prem, WL_l_34_prem, 
                          WL_l_123_prem, WL_l_124_prem, WL_l_134_prem, WL_l_234_prem, WL_l_1234_prem)
colnames(wl_l) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(wl_l), "wholelife_low.csv")




# 20 Year Term - Annuities
ax_l_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_1[i] = ax_l_1[i] + v^k * MORTDATA_low$lx1[i + k - 1] / MORTDATA_low$lx1[i]
  }
}
ax_l_1

ax_l_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_2[i] = ax_l_2[i] + v^k * MORTDATA_low$lx2[i + k - 1] / MORTDATA_low$lx2[i]
  }
}
ax_l_2

ax_l_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_3[i] = ax_l_3[i] + v^k * MORTDATA_low$lx3[i + k - 1] / MORTDATA_low$lx3[i]
  }
}
ax_l_3

ax_l_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_4[i] = ax_l_4[i] + v^k * MORTDATA_low$lx4[i + k - 1] / MORTDATA_low$lx4[i]
  }
}
ax_l_4

ax_l_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_12[i] = ax_l_12[i] + v^k * MORTDATA_low$lx12[i + k - 1] / MORTDATA_low$lx12[i]
  }
}
ax_l_12

ax_l_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_13[i] = ax_l_13[i] + v^k * MORTDATA_low$lx13[i + k - 1] / MORTDATA_low$lx13[i]
  }
}
ax_l_13

ax_l_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_14[i] = ax_l_14[i] + v^k * MORTDATA_low$lx14[i + k - 1] / MORTDATA_low$lx14[i]
  }
}
ax_l_14

ax_l_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_23[i] = ax_l_23[i] + v^k * MORTDATA_low$lx23[i + k - 1] / MORTDATA_low$lx23[i]
  }
}
ax_l_23

ax_l_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_24[i] = ax_l_24[i] + v^k * MORTDATA_low$lx24[i + k - 1] / MORTDATA_low$lx24[i]
  }
}
ax_l_24

ax_l_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_34[i] = ax_l_34[i] + v^k * MORTDATA_low$lx34[i + k - 1] / MORTDATA_low$lx34[i]
  }
}
ax_l_34

ax_l_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_123[i] = ax_l_123[i] + v^k * MORTDATA_low$lx123[i + k - 1] / MORTDATA_low$lx123[i]
  }
}
ax_l_123

ax_l_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_124[i] = ax_l_124[i] + v^k * MORTDATA_low$lx124[i + k - 1] / MORTDATA_low$lx124[i]
  }
}
ax_l_124

ax_l_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_134[i] = ax_l_134[i] + v^k * MORTDATA_low$lx134[i + k - 1] / MORTDATA_low$lx134[i]
  }
}
ax_l_134

ax_l_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_234[i] = ax_l_234[i] + v^k * MORTDATA_low$lx234[i + k - 1] / MORTDATA_low$lx234[i]
  }
}
ax_l_234

ax_l_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_l_1234[i] = ax_l_1234[i] + v^k * MORTDATA_low$lx1234[i + k - 1] / MORTDATA_low$lx1234[i]
  }
}
ax_l_1234


# 20 Year Term - Benefits
t20_l_prem_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_1[i] = t20_l_prem_1[i] + v^k * MORTDATA_low$lx1[i + k - 1] / MORTDATA_low$lx1[i] * MORTDATA_low$qx1[i + k - 1]
  }
}
t20_l_prem_1 = t20_l_prem_1 / ax_l_1
t20_l_prem_1

t20_l_prem_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_2[i] = t20_l_prem_2[i] + v^k * MORTDATA_low$lx2[i + k - 1] / MORTDATA_low$lx2[i] * MORTDATA_low$qx2[i + k - 1]
  }
}
t20_l_prem_2 = t20_l_prem_2 / ax_l_2
t20_l_prem_2

t20_l_prem_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_3[i] = t20_l_prem_3[i] + v^k * MORTDATA_low$lx3[i + k - 1] / MORTDATA_low$lx3[i] * MORTDATA_low$qx3[i + k - 1]
  }
}
t20_l_prem_3 = t20_l_prem_3 / ax_l_3
t20_l_prem_3

t20_l_prem_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_4[i] = t20_l_prem_4[i] + v^k * MORTDATA_low$lx4[i + k - 1] / MORTDATA_low$lx4[i] * MORTDATA_low$qx4[i + k - 1]
  }
}
t20_l_prem_4 = t20_l_prem_4 / ax_l_4
t20_l_prem_4

t20_l_prem_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_12[i] = t20_l_prem_12[i] + v^k * MORTDATA_low$lx12[i + k - 1] / MORTDATA_low$lx12[i] * MORTDATA_low$qx12[i + k - 1]
  }
}
t20_l_prem_12 = t20_l_prem_12 / ax_l_12
t20_l_prem_12

t20_l_prem_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_13[i] = t20_l_prem_13[i] + v^k * MORTDATA_low$lx13[i + k - 1] / MORTDATA_low$lx13[i] * MORTDATA_low$qx13[i + k - 1]
  }
}
t20_l_prem_13 = t20_l_prem_13 / ax_l_13
t20_l_prem_13

t20_l_prem_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_14[i] = t20_l_prem_14[i] + v^k * MORTDATA_low$lx14[i + k - 1] / MORTDATA_low$lx14[i] * MORTDATA_low$qx14[i + k - 1]
  }
}
t20_l_prem_14 = t20_l_prem_14 / ax_l_14
t20_l_prem_14

t20_l_prem_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_23[i] = t20_l_prem_23[i] + v^k * MORTDATA_low$lx23[i + k - 1] / MORTDATA_low$lx23[i] * MORTDATA_low$qx23[i + k - 1]
  }
}
t20_l_prem_23 = t20_l_prem_23 / ax_l_23
t20_l_prem_23

t20_l_prem_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_24[i] = t20_l_prem_24[i] + v^k * MORTDATA_low$lx24[i + k - 1] / MORTDATA_low$lx24[i] * MORTDATA_low$qx24[i + k - 1]
  }
}
t20_l_prem_24 = t20_l_prem_24 / ax_l_24
t20_l_prem_24

t20_l_prem_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_34[i] = t20_l_prem_34[i] + v^k * MORTDATA_low$lx34[i + k - 1] / MORTDATA_low$lx34[i] * MORTDATA_low$qx34[i + k - 1]
  }
}
t20_l_prem_34 = t20_l_prem_34 / ax_l_34
t20_l_prem_34

t20_l_prem_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_123[i] = t20_l_prem_123[i] + v^k * MORTDATA_low$lx123[i + k - 1] / MORTDATA_low$lx123[i] * MORTDATA_low$qx123[i + k - 1]
  }
}
t20_l_prem_123 = t20_l_prem_123 / ax_l_123
t20_l_prem_123

t20_l_prem_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_124[i] = t20_l_prem_124[i] + v^k * MORTDATA_low$lx124[i + k - 1] / MORTDATA_low$lx124[i] * MORTDATA_low$qx124[i + k - 1]
  }
}
t20_l_prem_124 = t20_l_prem_124 / ax_l_124
t20_l_prem_124

t20_l_prem_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_134[i] = t20_l_prem_134[i] + v^k * MORTDATA_low$lx134[i + k - 1] / MORTDATA_low$lx134[i] * MORTDATA_low$qx134[i + k - 1]
  }
}
t20_l_prem_134 = t20_l_prem_134 / ax_l_134
t20_l_prem_134

t20_l_prem_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_234[i] = t20_l_prem_234[i] + v^k * MORTDATA_low$lx234[i + k - 1] / MORTDATA_low$lx234[i] * MORTDATA_low$qx234[i + k - 1]
  }
}
t20_l_prem_234 = t20_l_prem_234 / ax_l_234
t20_l_prem_234

t20_l_prem_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_l_prem_1234[i] = t20_l_prem_1234[i] + v^k * MORTDATA_low$lx1234[i + k - 1] / MORTDATA_low$lx1234[i] * MORTDATA_low$qx1234[i + k - 1]
  }
}
t20_l_prem_1234 = t20_l_prem_1234 / ax_l_1234
t20_l_prem_1234


t20_l = data_frame(t20_l_prem_1, t20_l_prem_2, t20_l_prem_3, t20_l_prem_4, 
                   t20_l_prem_12, t20_l_prem_13, t20_l_prem_14, t20_l_prem_23, t20_l_prem_24, t20_l_prem_34, 
                   t20_l_prem_123, t20_l_prem_124, t20_l_prem_134, t20_l_prem_234, t20_l_prem_1234 )
write.csv(as.matrix(t20_l), "t20_low.csv")
colnames(t20_l) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')

######################################## Intervention Premium Calculations - m ######################################### 
# By underwriting classes - Whole Life
MORTDATA_moderate = read_xlsx("Modelling.xlsx", 
                         col_names = c("Age", "Mortality", "LF1", "qx1", "lx1",
                                       "LF2", "qx2", "lx2",
                                       "LF3", "qx3", "lx3",
                                       "LF4", "qx4", "lx4",
                                       "LF12", "qx12", "lx12",
                                       "LF13", "qx13", "lx13",
                                       "LF14", "qx14", "lx14",
                                       "LF23", "qx23", "lx23",
                                       "LF24", "qx24", "lx24",
                                       "LF34", "qx34", "lx34",
                                       "LF123", "qx123", "lx123",
                                       "LF124", "qx124", "lx124",
                                       "LF134", "qx134", "lx134",
                                       "LF234", "qx234", "lx234",
                                       "LF1234", "qx1234", "lx1234"), 
                         sheet = "Mortality (Interventions)",
                         range = "B265:AV384")

WL_m_1_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_1_prem[i] = WL_m_1_prem[i] + v^k * MORTDATA_moderate$lx1[i + k - 1] / MORTDATA_moderate$lx1[i] * MORTDATA_moderate$qx1[i + k - 1]
  }
}
WL_m_1_prem

WL_m_2_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_2_prem[i] = WL_m_2_prem[i] + v^k * MORTDATA_moderate$lx2[i + k - 1] / MORTDATA_moderate$lx2[i] * MORTDATA_moderate$qx2[i + k - 1]
  }
}
WL_m_2_prem

WL_m_3_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_3_prem[i] = WL_m_3_prem[i] + v^k * MORTDATA_moderate$lx3[i + k - 1] / MORTDATA_moderate$lx3[i] * MORTDATA_moderate$qx3[i + k - 1]
  }
}
WL_m_3_prem

WL_m_4_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_4_prem[i] = WL_m_4_prem[i] + v^k * MORTDATA_moderate$lx4[i + k - 1] / MORTDATA_moderate$lx4[i] * MORTDATA_moderate$qx4[i + k - 1]
  }
}
WL_m_4_prem

WL_m_12_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_12_prem[i] = WL_m_12_prem[i] + v^k * MORTDATA_moderate$lx12[i + k - 1] / MORTDATA_moderate$lx12[i] * MORTDATA_moderate$qx12[i + k - 1]
  }
}
WL_m_12_prem

WL_m_13_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_13_prem[i] = WL_m_13_prem[i] + v^k * MORTDATA_moderate$lx13[i + k - 1] / MORTDATA_moderate$lx13[i] * MORTDATA_moderate$qx13[i + k - 1]
  }
}
WL_m_13_prem

WL_m_14_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_14_prem[i] = WL_m_14_prem[i] + v^k * MORTDATA_moderate$lx14[i + k - 1] / MORTDATA_moderate$lx14[i] * MORTDATA_moderate$qx14[i + k - 1]
  }
}
WL_m_14_prem

WL_m_23_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_23_prem[i] = WL_m_23_prem[i] + v^k * MORTDATA_moderate$lx23[i + k - 1] / MORTDATA_moderate$lx23[i] * MORTDATA_moderate$qx23[i + k - 1]
  }
}
WL_m_23_prem

WL_m_24_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_24_prem[i] = WL_m_24_prem[i] + v^k * MORTDATA_moderate$lx24[i + k - 1] / MORTDATA_moderate$lx24[i] * MORTDATA_moderate$qx24[i + k - 1]
  }
}
WL_m_24_prem

WL_m_34_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_34_prem[i] = WL_m_34_prem[i] + v^k * MORTDATA_moderate$lx34[i + k - 1] / MORTDATA_moderate$lx34[i] * MORTDATA_moderate$qx34[i + k - 1]
  }
}
WL_m_34_prem

WL_m_123_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_123_prem[i] = WL_m_123_prem[i] + v^k * MORTDATA_moderate$lx123[i + k - 1] / MORTDATA_moderate$lx123[i] * MORTDATA_moderate$qx123[i + k - 1]
  }
}
WL_m_123_prem

WL_m_124_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_124_prem[i] = WL_m_124_prem[i] + v^k * MORTDATA_moderate$lx124[i + k - 1] / MORTDATA_moderate$lx124[i] * MORTDATA_moderate$qx124[i + k - 1]
  }
}
WL_m_124_prem

WL_m_134_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_134_prem[i] = WL_m_134_prem[i] + v^k * MORTDATA_moderate$lx134[i + k - 1] / MORTDATA_moderate$lx134[i] * MORTDATA_moderate$qx134[i + k - 1]
  }
}
WL_m_134_prem

WL_m_234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_234_prem[i] = WL_m_234_prem[i] + v^k * MORTDATA_moderate$lx234[i + k - 1] / MORTDATA_moderate$lx234[i] * MORTDATA_moderate$qx234[i + k - 1]
  }
}
WL_m_234_prem

WL_m_1234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_m_1234_prem[i] = WL_m_1234_prem[i] + v^k * MORTDATA_moderate$lx1234[i + k - 1] / MORTDATA_moderate$lx1234[i] * MORTDATA_moderate$qx1234[i + k - 1]
  }
}
WL_m_1234_prem

wl_m = data_frame( WL_m_1_prem, WL_m_2_prem, WL_m_3_prem, WL_m_4_prem, 
                   WL_m_12_prem, WL_m_13_prem, WL_m_14_prem, WL_m_23_prem, WL_m_24_prem, WL_m_34_prem, 
                   WL_m_123_prem, WL_m_124_prem, WL_m_134_prem, WL_m_234_prem, WL_m_1234_prem)
colnames(wl_m) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(wl_m), "wholelife_moderate.csv")




# 20 Year Term - Annuities
ax_m_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_1[i] = ax_m_1[i] + v^k * MORTDATA_moderate$lx1[i + k - 1] / MORTDATA_moderate$lx1[i]
  }
}
ax_m_1

ax_m_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_2[i] = ax_m_2[i] + v^k * MORTDATA_moderate$lx2[i + k - 1] / MORTDATA_moderate$lx2[i]
  }
}
ax_m_2

ax_m_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_3[i] = ax_m_3[i] + v^k * MORTDATA_moderate$lx3[i + k - 1] / MORTDATA_moderate$lx3[i]
  }
}
ax_m_3

ax_m_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_4[i] = ax_m_4[i] + v^k * MORTDATA_moderate$lx4[i + k - 1] / MORTDATA_moderate$lx4[i]
  }
}
ax_m_4

ax_m_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_12[i] = ax_m_12[i] + v^k * MORTDATA_moderate$lx12[i + k - 1] / MORTDATA_moderate$lx12[i]
  }
}
ax_m_12

ax_m_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_13[i] = ax_m_13[i] + v^k * MORTDATA_moderate$lx13[i + k - 1] / MORTDATA_moderate$lx13[i]
  }
}
ax_m_13

ax_m_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_14[i] = ax_m_14[i] + v^k * MORTDATA_moderate$lx14[i + k - 1] / MORTDATA_moderate$lx14[i]
  }
}
ax_m_14

ax_m_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_23[i] = ax_m_23[i] + v^k * MORTDATA_moderate$lx23[i + k - 1] / MORTDATA_moderate$lx23[i]
  }
}
ax_m_23

ax_m_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_24[i] = ax_m_24[i] + v^k * MORTDATA_moderate$lx24[i + k - 1] / MORTDATA_moderate$lx24[i]
  }
}
ax_m_24

ax_m_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_34[i] = ax_m_34[i] + v^k * MORTDATA_moderate$lx34[i + k - 1] / MORTDATA_moderate$lx34[i]
  }
}
ax_m_34

ax_m_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_123[i] = ax_m_123[i] + v^k * MORTDATA_moderate$lx123[i + k - 1] / MORTDATA_moderate$lx123[i]
  }
}
ax_m_123

ax_m_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_124[i] = ax_m_124[i] + v^k * MORTDATA_moderate$lx124[i + k - 1] / MORTDATA_moderate$lx124[i]
  }
}
ax_m_124

ax_m_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_134[i] = ax_m_134[i] + v^k * MORTDATA_moderate$lx134[i + k - 1] / MORTDATA_moderate$lx134[i]
  }
}
ax_m_134

ax_m_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_234[i] = ax_m_234[i] + v^k * MORTDATA_moderate$lx234[i + k - 1] / MORTDATA_moderate$lx234[i]
  }
}
ax_m_234

ax_m_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_m_1234[i] = ax_m_1234[i] + v^k * MORTDATA_moderate$lx1234[i + k - 1] / MORTDATA_moderate$lx1234[i]
  }
}
ax_m_1234


# 20 Year Term - Benefits
t20_m_prem_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_1[i] = t20_m_prem_1[i] + v^k * MORTDATA_moderate$lx1[i + k - 1] / MORTDATA_moderate$lx1[i] * MORTDATA_moderate$qx1[i + k - 1]
  }
}
t20_m_prem_1 = t20_m_prem_1 / ax_m_1
t20_m_prem_1

t20_m_prem_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_2[i] = t20_m_prem_2[i] + v^k * MORTDATA_moderate$lx2[i + k - 1] / MORTDATA_moderate$lx2[i] * MORTDATA_moderate$qx2[i + k - 1]
  }
}
t20_m_prem_2 = t20_m_prem_2 / ax_m_2
t20_m_prem_2

t20_m_prem_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_3[i] = t20_m_prem_3[i] + v^k * MORTDATA_moderate$lx3[i + k - 1] / MORTDATA_moderate$lx3[i] * MORTDATA_moderate$qx3[i + k - 1]
  }
}
t20_m_prem_3 = t20_m_prem_3 / ax_m_3
t20_m_prem_3

t20_m_prem_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_4[i] = t20_m_prem_4[i] + v^k * MORTDATA_moderate$lx4[i + k - 1] / MORTDATA_moderate$lx4[i] * MORTDATA_moderate$qx4[i + k - 1]
  }
}
t20_m_prem_4 = t20_m_prem_4 / ax_m_4
t20_m_prem_4

t20_m_prem_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_12[i] = t20_m_prem_12[i] + v^k * MORTDATA_moderate$lx12[i + k - 1] / MORTDATA_moderate$lx12[i] * MORTDATA_moderate$qx12[i + k - 1]
  }
}
t20_m_prem_12 = t20_m_prem_12 / ax_m_12
t20_m_prem_12

t20_m_prem_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_13[i] = t20_m_prem_13[i] + v^k * MORTDATA_moderate$lx13[i + k - 1] / MORTDATA_moderate$lx13[i] * MORTDATA_moderate$qx13[i + k - 1]
  }
}
t20_m_prem_13 = t20_m_prem_13 / ax_m_13
t20_m_prem_13

t20_m_prem_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_14[i] = t20_m_prem_14[i] + v^k * MORTDATA_moderate$lx14[i + k - 1] / MORTDATA_moderate$lx14[i] * MORTDATA_moderate$qx14[i + k - 1]
  }
}
t20_m_prem_14 = t20_m_prem_14 / ax_m_14
t20_m_prem_14

t20_m_prem_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_23[i] = t20_m_prem_23[i] + v^k * MORTDATA_moderate$lx23[i + k - 1] / MORTDATA_moderate$lx23[i] * MORTDATA_moderate$qx23[i + k - 1]
  }
}
t20_m_prem_23 = t20_m_prem_23 / ax_m_23
t20_m_prem_23

t20_m_prem_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_24[i] = t20_m_prem_24[i] + v^k * MORTDATA_moderate$lx24[i + k - 1] / MORTDATA_moderate$lx24[i] * MORTDATA_moderate$qx24[i + k - 1]
  }
}
t20_m_prem_24 = t20_m_prem_24 / ax_m_24
t20_m_prem_24

t20_m_prem_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_34[i] = t20_m_prem_34[i] + v^k * MORTDATA_moderate$lx34[i + k - 1] / MORTDATA_moderate$lx34[i] * MORTDATA_moderate$qx34[i + k - 1]
  }
}
t20_m_prem_34 = t20_m_prem_34 / ax_m_34
t20_m_prem_34

t20_m_prem_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_123[i] = t20_m_prem_123[i] + v^k * MORTDATA_moderate$lx123[i + k - 1] / MORTDATA_moderate$lx123[i] * MORTDATA_moderate$qx123[i + k - 1]
  }
}
t20_m_prem_123 = t20_m_prem_123 / ax_m_123
t20_m_prem_123

t20_m_prem_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_124[i] = t20_m_prem_124[i] + v^k * MORTDATA_moderate$lx124[i + k - 1] / MORTDATA_moderate$lx124[i] * MORTDATA_moderate$qx124[i + k - 1]
  }
}
t20_m_prem_124 = t20_m_prem_124 / ax_m_124
t20_m_prem_124

t20_m_prem_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_134[i] = t20_m_prem_134[i] + v^k * MORTDATA_moderate$lx134[i + k - 1] / MORTDATA_moderate$lx134[i] * MORTDATA_moderate$qx134[i + k - 1]
  }
}
t20_m_prem_134 = t20_m_prem_134 / ax_m_134
t20_m_prem_134

t20_m_prem_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_234[i] = t20_m_prem_234[i] + v^k * MORTDATA_moderate$lx234[i + k - 1] / MORTDATA_moderate$lx234[i] * MORTDATA_moderate$qx234[i + k - 1]
  }
}
t20_m_prem_234 = t20_m_prem_234 / ax_m_234
t20_m_prem_234

t20_m_prem_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_m_prem_1234[i] = t20_m_prem_1234[i] + v^k * MORTDATA_moderate$lx1234[i + k - 1] / MORTDATA_moderate$lx1234[i] * MORTDATA_moderate$qx1234[i + k - 1]
  }
}
t20_m_prem_1234 = t20_m_prem_1234 / ax_m_1234
t20_m_prem_1234


t20_m = data_frame(t20_m_prem_1, t20_m_prem_2, t20_m_prem_3, t20_m_prem_4, 
                   t20_m_prem_12, t20_m_prem_13, t20_m_prem_14, t20_m_prem_23, t20_m_prem_24, t20_m_prem_34, 
                   t20_m_prem_123, t20_m_prem_124, t20_m_prem_134, t20_m_prem_234, t20_m_prem_1234)
colnames(t20_m) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(t20_m), "t20_moderate.csv")

######################################## Intervention Premium Calculations - h ######################################### 
# By underwriting classes - Whole Life
MORTDATA_high = read_xlsx("Modelling.xlsx", 
                              col_names = c("Age", "Mortality", "LF1", "qx1", "lx1",
                                            "LF2", "qx2", "lx2",
                                            "LF3", "qx3", "lx3",
                                            "LF4", "qx4", "lx4",
                                            "LF12", "qx12", "lx12",
                                            "LF13", "qx13", "lx13",
                                            "LF14", "qx14", "lx14",
                                            "LF23", "qx23", "lx23",
                                            "LF24", "qx24", "lx24",
                                            "LF34", "qx34", "lx34",
                                            "LF123", "qx123", "lx123",
                                            "LF124", "qx124", "lx124",
                                            "LF134", "qx134", "lx134",
                                            "LF234", "qx234", "lx234",
                                            "LF1234", "qx1234", "lx1234"), 
                              sheet = "Mortality (Interventions)",
                              range = "B393:AV512")

WL_h_1_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_1_prem[i] = WL_h_1_prem[i] + v^k * MORTDATA_high$lx1[i + k - 1] / MORTDATA_high$lx1[i] * MORTDATA_high$qx1[i + k - 1]
  }
}
WL_h_1_prem

WL_h_2_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_2_prem[i] = WL_h_2_prem[i] + v^k * MORTDATA_high$lx2[i + k - 1] / MORTDATA_high$lx2[i] * MORTDATA_high$qx2[i + k - 1]
  }
}
WL_h_2_prem

WL_h_3_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_3_prem[i] = WL_h_3_prem[i] + v^k * MORTDATA_high$lx3[i + k - 1] / MORTDATA_high$lx3[i] * MORTDATA_high$qx3[i + k - 1]
  }
}
WL_h_3_prem

WL_h_4_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_4_prem[i] = WL_h_4_prem[i] + v^k * MORTDATA_high$lx4[i + k - 1] / MORTDATA_high$lx4[i] * MORTDATA_high$qx4[i + k - 1]
  }
}
WL_h_4_prem

WL_h_12_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_12_prem[i] = WL_h_12_prem[i] + v^k * MORTDATA_high$lx12[i + k - 1] / MORTDATA_high$lx12[i] * MORTDATA_high$qx12[i + k - 1]
  }
}
WL_h_12_prem

WL_h_13_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_13_prem[i] = WL_h_13_prem[i] + v^k * MORTDATA_high$lx13[i + k - 1] / MORTDATA_high$lx13[i] * MORTDATA_high$qx13[i + k - 1]
  }
}
WL_h_13_prem

WL_h_14_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_14_prem[i] = WL_h_14_prem[i] + v^k * MORTDATA_high$lx14[i + k - 1] / MORTDATA_high$lx14[i] * MORTDATA_high$qx14[i + k - 1]
  }
}
WL_h_14_prem

WL_h_23_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_23_prem[i] = WL_h_23_prem[i] + v^k * MORTDATA_high$lx23[i + k - 1] / MORTDATA_high$lx23[i] * MORTDATA_high$qx23[i + k - 1]
  }
}
WL_h_23_prem

WL_h_24_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_24_prem[i] = WL_h_24_prem[i] + v^k * MORTDATA_high$lx24[i + k - 1] / MORTDATA_high$lx24[i] * MORTDATA_high$qx24[i + k - 1]
  }
}
WL_h_24_prem

WL_h_34_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_34_prem[i] = WL_h_34_prem[i] + v^k * MORTDATA_high$lx34[i + k - 1] / MORTDATA_high$lx34[i] * MORTDATA_high$qx34[i + k - 1]
  }
}
WL_h_34_prem

WL_h_123_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_123_prem[i] = WL_h_123_prem[i] + v^k * MORTDATA_high$lx123[i + k - 1] / MORTDATA_high$lx123[i] * MORTDATA_high$qx123[i + k - 1]
  }
}
WL_h_123_prem

WL_h_124_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_124_prem[i] = WL_h_124_prem[i] + v^k * MORTDATA_high$lx124[i + k - 1] / MORTDATA_high$lx124[i] * MORTDATA_high$qx124[i + k - 1]
  }
}
WL_h_124_prem

WL_h_134_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_134_prem[i] = WL_h_134_prem[i] + v^k * MORTDATA_high$lx134[i + k - 1] / MORTDATA_high$lx134[i] * MORTDATA_high$qx134[i + k - 1]
  }
}
WL_h_134_prem

WL_h_234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_234_prem[i] = WL_h_234_prem[i] + v^k * MORTDATA_high$lx234[i + k - 1] / MORTDATA_high$lx234[i] * MORTDATA_high$qx234[i + k - 1]
  }
}
WL_h_234_prem

WL_h_1234_prem = rep(0, 120)
for (i in 1:120) {
  for (k in 1:(120-i)) {
    WL_h_1234_prem[i] = WL_h_1234_prem[i] + v^k * MORTDATA_high$lx1234[i + k - 1] / MORTDATA_high$lx1234[i] * MORTDATA_high$qx1234[i + k - 1]
  }
}
WL_h_1234_prem

wl_h = data_frame(WL_h_1_prem, WL_h_2_prem, WL_h_3_prem, WL_h_4_prem, 
                  WL_h_12_prem, WL_h_13_prem, WL_h_14_prem, WL_h_23_prem, WL_h_24_prem, WL_h_34_prem, 
                  WL_h_123_prem, WL_h_124_prem, WL_h_134_prem, WL_h_234_prem, WL_h_1234_prem)
colnames(wl_h) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(wl_h), "wholelife_high.csv")




# 20 Year Term - Annuities
ax_h_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_1[i] = ax_h_1[i] + v^k * MORTDATA_high$lx1[i + k - 1] / MORTDATA_high$lx1[i]
  }
}
ax_h_1

ax_h_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_2[i] = ax_h_2[i] + v^k * MORTDATA_high$lx2[i + k - 1] / MORTDATA_high$lx2[i]
  }
}
ax_h_2

ax_h_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_3[i] = ax_h_3[i] + v^k * MORTDATA_high$lx3[i + k - 1] / MORTDATA_high$lx3[i]
  }
}
ax_h_3

ax_h_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_4[i] = ax_h_4[i] + v^k * MORTDATA_high$lx4[i + k - 1] / MORTDATA_high$lx4[i]
  }
}
ax_h_4

ax_h_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_12[i] = ax_h_12[i] + v^k * MORTDATA_high$lx12[i + k - 1] / MORTDATA_high$lx12[i]
  }
}
ax_h_12

ax_h_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_13[i] = ax_h_13[i] + v^k * MORTDATA_high$lx13[i + k - 1] / MORTDATA_high$lx13[i]
  }
}
ax_h_13

ax_h_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_14[i] = ax_h_14[i] + v^k * MORTDATA_high$lx14[i + k - 1] / MORTDATA_high$lx14[i]
  }
}
ax_h_14

ax_h_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_23[i] = ax_h_23[i] + v^k * MORTDATA_high$lx23[i + k - 1] / MORTDATA_high$lx23[i]
  }
}
ax_h_23

ax_h_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_24[i] = ax_h_24[i] + v^k * MORTDATA_high$lx24[i + k - 1] / MORTDATA_high$lx24[i]
  }
}
ax_h_24

ax_h_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_34[i] = ax_h_34[i] + v^k * MORTDATA_high$lx34[i + k - 1] / MORTDATA_high$lx34[i]
  }
}
ax_h_34

ax_h_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_123[i] = ax_h_123[i] + v^k * MORTDATA_high$lx123[i + k - 1] / MORTDATA_high$lx123[i]
  }
}
ax_h_123

ax_h_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_124[i] = ax_h_124[i] + v^k * MORTDATA_high$lx124[i + k - 1] / MORTDATA_high$lx124[i]
  }
}
ax_h_124

ax_h_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_134[i] = ax_h_134[i] + v^k * MORTDATA_high$lx134[i + k - 1] / MORTDATA_high$lx134[i]
  }
}
ax_h_134

ax_h_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_234[i] = ax_h_234[i] + v^k * MORTDATA_high$lx234[i + k - 1] / MORTDATA_high$lx234[i]
  }
}
ax_h_234

ax_h_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    ax_h_1234[i] = ax_h_1234[i] + v^k * MORTDATA_high$lx1234[i + k - 1] / MORTDATA_high$lx1234[i]
  }
}
ax_h_1234


# 20 Year Term - Benefits
t20_h_prem_1 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_1[i] = t20_h_prem_1[i] + v^k * MORTDATA_high$lx1[i + k - 1] / MORTDATA_high$lx1[i] * MORTDATA_high$qx1[i + k - 1]
  }
}
t20_h_prem_1 = t20_h_prem_1 / ax_h_1
t20_h_prem_1

t20_h_prem_2 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_2[i] = t20_h_prem_2[i] + v^k * MORTDATA_high$lx2[i + k - 1] / MORTDATA_high$lx2[i] * MORTDATA_high$qx2[i + k - 1]
  }
}
t20_h_prem_2 = t20_h_prem_2 / ax_h_2
t20_h_prem_2

t20_h_prem_3 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_3[i] = t20_h_prem_3[i] + v^k * MORTDATA_high$lx3[i + k - 1] / MORTDATA_high$lx3[i] * MORTDATA_high$qx3[i + k - 1]
  }
}
t20_h_prem_3 = t20_h_prem_3 / ax_h_3
t20_h_prem_3

t20_h_prem_4 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_4[i] = t20_h_prem_4[i] + v^k * MORTDATA_high$lx4[i + k - 1] / MORTDATA_high$lx4[i] * MORTDATA_high$qx4[i + k - 1]
  }
}
t20_h_prem_4 = t20_h_prem_4 / ax_h_4
t20_h_prem_4

t20_h_prem_12 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_12[i] = t20_h_prem_12[i] + v^k * MORTDATA_high$lx12[i + k - 1] / MORTDATA_high$lx12[i] * MORTDATA_high$qx12[i + k - 1]
  }
}
t20_h_prem_12 = t20_h_prem_12 / ax_h_12
t20_h_prem_12

t20_h_prem_13 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_13[i] = t20_h_prem_13[i] + v^k * MORTDATA_high$lx13[i + k - 1] / MORTDATA_high$lx13[i] * MORTDATA_high$qx13[i + k - 1]
  }
}
t20_h_prem_13 = t20_h_prem_13 / ax_h_13
t20_h_prem_13

t20_h_prem_14 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_14[i] = t20_h_prem_14[i] + v^k * MORTDATA_high$lx14[i + k - 1] / MORTDATA_high$lx14[i] * MORTDATA_high$qx14[i + k - 1]
  }
}
t20_h_prem_14 = t20_h_prem_14 / ax_h_14
t20_h_prem_14

t20_h_prem_23 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_23[i] = t20_h_prem_23[i] + v^k * MORTDATA_high$lx23[i + k - 1] / MORTDATA_high$lx23[i] * MORTDATA_high$qx23[i + k - 1]
  }
}
t20_h_prem_23 = t20_h_prem_23 / ax_h_23
t20_h_prem_23

t20_h_prem_24 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_24[i] = t20_h_prem_24[i] + v^k * MORTDATA_high$lx24[i + k - 1] / MORTDATA_high$lx24[i] * MORTDATA_high$qx24[i + k - 1]
  }
}
t20_h_prem_24 = t20_h_prem_24 / ax_h_24
t20_h_prem_24

t20_h_prem_34 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_34[i] = t20_h_prem_34[i] + v^k * MORTDATA_high$lx34[i + k - 1] / MORTDATA_high$lx34[i] * MORTDATA_high$qx34[i + k - 1]
  }
}
t20_h_prem_34 = t20_h_prem_34 / ax_h_34
t20_h_prem_34

t20_h_prem_123 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_123[i] = t20_h_prem_123[i] + v^k * MORTDATA_high$lx123[i + k - 1] / MORTDATA_high$lx123[i] * MORTDATA_high$qx123[i + k - 1]
  }
}
t20_h_prem_123 = t20_h_prem_123 / ax_h_123
t20_h_prem_123

t20_h_prem_124 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_124[i] = t20_h_prem_124[i] + v^k * MORTDATA_high$lx124[i + k - 1] / MORTDATA_high$lx124[i] * MORTDATA_high$qx124[i + k - 1]
  }
}
t20_h_prem_124 = t20_h_prem_124 / ax_h_124
t20_h_prem_124

t20_h_prem_134 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_134[i] = t20_h_prem_134[i] + v^k * MORTDATA_high$lx134[i + k - 1] / MORTDATA_high$lx134[i] * MORTDATA_high$qx134[i + k - 1]
  }
}
t20_h_prem_134 = t20_h_prem_134 / ax_h_134
t20_h_prem_134

t20_h_prem_234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_234[i] = t20_h_prem_234[i] + v^k * MORTDATA_high$lx234[i + k - 1] / MORTDATA_high$lx234[i] * MORTDATA_high$qx234[i + k - 1]
  }
}
t20_h_prem_234 = t20_h_prem_234 / ax_h_234
t20_h_prem_234

t20_h_prem_1234 = rep(0, 120)
for (i in 1:120) {
  for (k in 1:20) {
    t20_h_prem_1234[i] = t20_h_prem_1234[i] + v^k * MORTDATA_high$lx1234[i + k - 1] / MORTDATA_high$lx1234[i] * MORTDATA_high$qx1234[i + k - 1]
  }
}
t20_h_prem_1234 = t20_h_prem_1234 / ax_h_1234
t20_h_prem_1234


t20_h = data_frame(t20_h_prem_1, t20_h_prem_2, t20_h_prem_3, t20_h_prem_4, 
                   t20_h_prem_12, t20_h_prem_13, t20_h_prem_14, t20_h_prem_23, t20_h_prem_24, t20_h_prem_34, 
                   t20_h_prem_123, t20_h_prem_124, t20_h_prem_134, t20_h_prem_234, t20_h_prem_1234 )
colnames(t20_h) = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour')
write.csv(as.matrix(t20_h), "t20_high.csv")

################################################ Profitability Analysis ################################################ 
# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base = bind_rows(cashflow_base, as.tibble(t(row)))
}

cashflow_base = cashflow_base[, 5:8]
names(cashflow_base) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base

# With the Interventions and new mortality
INF = INFORCE
INF$ProjYearOfDeath = rep(0, dim(INF)[1])


# Calculating Life Expectancy - Base
ex_vl = rep(0, 120)
for (age in 1:120) {
  for (k in 1:(120-age)) {
    ex_vl[age] = ex_vl[age] + MORTDATA_base$lx_vl[age + k] / MORTDATA_base$lx_vl[age]
  }
}
MORTDATA_base$ex_vl = ex_vl

ex_l = rep(0, 120)
for (age in 1:120) {
  for (k in 1:(120-age)) {
    ex_l[age] = ex_l[age] + MORTDATA_base$lx_l[age + k] / MORTDATA_base$lx_l[age]
  }
}
MORTDATA_base$ex_l = ex_l

ex_m = rep(0, 120)
for (age in 1:120) {
  for (k in 1:(120-age)) {
    ex_m[age] = ex_m[age] + MORTDATA_base$lx_m[age + k] / MORTDATA_base$lx_m[age]
  }
}
MORTDATA_base$ex_m = ex_m

ex_h = rep(0, 120)
for (age in 1:120) {
  for (k in 1:(120-age)) {
    ex_h[age] = ex_h[age] + MORTDATA_base$lx_h[age + k] / MORTDATA_base$lx_h[age]
  }
}
MORTDATA_base$ex_h = ex_h

# Life Expectancy - Interventions
life_ex = function (data) {
  ex1 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex1[age] = ex1[age] + data$lx1[age + k] / data$lx1[age]
    }
  }
  data$ex1 = ex1
  
  ex2 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex2[age] = ex2[age] + data$lx2[age + k] / data$lx2[age]
    }
  }
  data$ex2 = ex2
  
  ex3 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex3[age] = ex3[age] + data$lx3[age + k] / data$lx3[age]
    }
  }
  data$ex3 = ex3
  
  ex4 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex4[age] = ex4[age] + data$lx4[age + k] / data$lx4[age]
    }
  }
  data$ex4 = ex4
  
  ex12 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex12[age] = ex12[age] + data$lx12[age + k] / data$lx12[age]
    }
  }
  data$ex12 = ex12
  
  ex13 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex13[age] = ex13[age] + data$lx13[age + k] / data$lx13[age]
    }
  }
  data$ex13 = ex13
  
  ex14 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex14[age] = ex14[age] + data$lx14[age + k] / data$lx14[age]
    }
  }
  data$ex14 = ex14
  
  ex23 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex23[age] = ex23[age] + data$lx23[age + k] / data$lx23[age]
    }
  }
  data$ex23 = ex23
  
  ex24 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex24[age] = ex24[age] + data$lx24[age + k] / data$lx24[age]
    }
  }
  data$ex24 = ex24
  
  ex34 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex34[age] = ex34[age] + data$lx34[age + k] / data$lx34[age]
    }
  }
  data$ex34 = ex34
  
  ex123 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex123[age] = ex123[age] + data$lx123[age + k] / data$lx123[age]
    }
  }
  data$ex123 = ex123
  
  ex124 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex124[age] = ex124[age] + data$lx124[age + k] / data$lx124[age]
    }
  }
  data$ex124 = ex124
  
  ex134 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex134[age] = ex134[age] + data$lx134[age + k] / data$lx134[age]
    }
  }
  data$ex134 = ex134
  
  ex234 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex234[age] = ex234[age] + data$lx234[age + k] / data$lx234[age]
    }
  }
  data$ex234 = ex234
  
  ex1234 = rep(0, 120)
  for (age in 1:120) {
    for (k in 1:(120-age)) {
      ex1234[age] = ex1234[age] + data$lx1234[age + k] / data$lx1234[age]
    }
  }
  data$ex1234 = ex1234
  
  return(data)
}

MORTDATA_verylow = life_ex(MORTDATA_verylow)
MORTDATA_low = life_ex(MORTDATA_low)
MORTDATA_moderate = life_ex(MORTDATA_moderate)
MORTDATA_high = life_ex(MORTDATA_high)

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
            
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
      
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
        
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
        
        }
  
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
        
        
      }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))


profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year

  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear <= year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions = bind_rows(cashflow_interventions, as.tibble(t(row)))
}

cashflow_interventions = cashflow_interventions[, 5:8]
names(cashflow_interventions) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions


############################################### Profitability Analysis II ##############################################
# Importing Expense Loaded Premiums
wl_base_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                             col_names = c("vl", "l", "m", "h"),
                             sheet = "WL Aggregate",
                             range = "B2:E120") / 769177

t20_base_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                       col_names = c("vl", "l", "m", "h"),
                       sheet = "T20 Aggregate",
                       range = "B2:E102") / 602229

# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base_el = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base_el = bind_rows(cashflow_base_el, as.tibble(t(row)))
}

cashflow_base_el = cashflow_base_el[, 5:8]
names(cashflow_base_el) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base_el

# With the Interventions and new mortality
INF$ProjYearOfDeath = rep(0, dim(INF)[1])

# Importing Expense Loaded Premiums
wl_vl_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                       col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                     'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                       sheet = "WL VeryLow",
                       range = "C4:Q122") / 769177

wl_l_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "WL Low",
                     range = "C4:Q122") / 769177

wl_m_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Moderate",
                    range = "C4:Q122") / 769177

wl_h_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL High",
                    range = "C4:Q122") / 769177


t20_vl_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 VeryLow",
                     range = "C4:Q122") / 602229

t20_l_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "T20 Low",
                    range = "C4:Q122") / 602229

t20_m_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "T20 Moderate",
                    range = "C4:Q122") / 602229

t20_h_el = read_xlsx("Premiums Expense Loaded.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "T20 High",
                    range = "C4:Q122") / 602229

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
          
        }
        
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
        }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))

profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear <= year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions_el = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions_el = bind_rows(cashflow_interventions_el, as.tibble(t(row)))
}

cashflow_interventions_el = cashflow_interventions_el[, 5:8]
names(cashflow_interventions_el) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions_el 




############################################### Sensitivity Analysis 1.6% ##############################################
# Importing Expense Loaded Premiums
wl_base_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                       col_names = c("vl", "l", "m", "h"),
                       sheet = "WL Aggregate",
                       range = "B2:E120") / 769177

t20_base_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                        col_names = c("vl", "l", "m", "h"),
                        sheet = "T20 Aggregate",
                        range = "B2:E102") / 602229

# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 100000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
    
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base_el_1.6 = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base_el_1.6 = bind_rows(cashflow_base_el_1.6, as.tibble(t(row)))
}

cashflow_base_el_1.6 = cashflow_base_el_1.6[, 5:8]
names(cashflow_base_el_1.6) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base_el_1.6


# With the Interventions and new mortality
INF$ProjYearOfDeath = rep(0, dim(INF)[1])

# Importing Expense Loaded Premiums
wl_vl_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "WL VeryLow",
                     range = "C4:Q122") / 769177

wl_l_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Low",
                    range = "C4:Q122") / 769177

wl_m_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Moderate",
                    range = "C4:Q122") / 769177

wl_h_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL High",
                    range = "C4:Q122") / 769177


t20_vl_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                      col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                      sheet = "T20 VeryLow",
                      range = "C4:Q122") / 602229

t20_l_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Low",
                     range = "C4:Q122") / 602229

t20_m_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Moderate",
                     range = "C4:Q122") / 602229

t20_h_el = read_xlsx("Premiums Expense Loaded 1.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 High",
                     range = "C4:Q122") / 602229

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
          
        }
        
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
        }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))

profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 100000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions_el_1.6 = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions_el_1.6 = bind_rows(cashflow_interventions_el_1.6, as.tibble(t(row)))
}

cashflow_interventions_el_1.6 = cashflow_interventions_el_1.6[, 5:8]
names(cashflow_interventions_el_1.6) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions_el_1.6








############################################### Sensitivity Analysis 5.6% ##############################################
# Importing Expense Loaded Premiums
wl_base_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                       col_names = c("vl", "l", "m", "h"),
                       sheet = "WL Aggregate",
                       range = "B2:E120") / 769177

t20_base_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                        col_names = c("vl", "l", "m", "h"),
                        sheet = "T20 Aggregate",
                        range = "B2:E102") / 602229

# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount)
  cost = cost + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base_el_5.6 = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base_el_5.6 = bind_rows(cashflow_base_el_5.6, as.tibble(t(row)))
}

cashflow_base_el_5.6 = cashflow_base_el_5.6[, 5:8]
names(cashflow_base_el_5.6) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base_el_5.6

# With the Interventions and new mortality
INF$ProjYearOfDeath = rep(0, dim(INF)[1])

# Importing Expense Loaded Premiums
wl_vl_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "WL VeryLow",
                     range = "C4:Q122") / 769177

wl_l_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Low",
                    range = "C4:Q122") / 769177

wl_m_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Moderate",
                    range = "C4:Q122") / 769177

wl_h_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL High",
                    range = "C4:Q122") / 769177


t20_vl_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                      col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                      sheet = "T20 VeryLow",
                      range = "C4:Q122") / 602229

t20_l_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Low",
                     range = "C4:Q122") / 602229

t20_m_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Moderate",
                     range = "C4:Q122") / 602229

t20_h_el = read_xlsx("Premiums Expense Loaded 5.6.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 High",
                     range = "C4:Q122") / 602229

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
          
        }
        
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
        }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))

profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions_el_5.6 = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions_el_5.6 = bind_rows(cashflow_interventions_el_5.6, as.tibble(t(row)))
}

cashflow_interventions_el_5.6 = cashflow_interventions_el_5.6[, 5:8]
names(cashflow_interventions_el_5.6) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions_el_5.6

############################################### Sensitivity Analysis MorL ##############################################
# Importing Expense Loaded Premiums
wl_base_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                       col_names = c("vl", "l", "m", "h"),
                       sheet = "WL Aggregate",
                       range = "B2:E66") / 769177

t20_base_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                        col_names = c("vl", "l", "m", "h"),
                        sheet = "T20 Aggregate",
                        range = "B2:E102") / 602229

# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount)
  cost = cost + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base_el_morl = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base_el_morl = bind_rows(cashflow_base_el_morl, as.tibble(t(row)))
}

cashflow_base_el_morl = cashflow_base_el_morl[, 5:8]
names(cashflow_base_el_morl) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base_el_morl

# With the Interventions and new mortality
INF$ProjYearOfDeath = rep(0, dim(INF)[1])

# Importing Expense Loaded Premiums
wl_vl_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "WL VeryLow",
                     range = "C4:Q122") / 769177

wl_l_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Low",
                    range = "C4:Q122") / 769177

wl_m_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Moderate",
                    range = "C4:Q122") / 769177

wl_h_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL High",
                    range = "C4:Q122") / 769177


t20_vl_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                      col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                      sheet = "T20 VeryLow",
                      range = "C4:Q122") / 602229

t20_l_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Low",
                     range = "C4:Q122") / 602229

t20_m_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Moderate",
                     range = "C4:Q122") / 602229

t20_h_el = read_xlsx("Premiums Expense Loaded Low Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 High",
                     range = "C4:Q122") / 602229

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
          
        }
        
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
        }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))

profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions_el_morl = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions_el_morl = bind_rows(cashflow_interventions_el_morl, as.tibble(t(row)))
}

cashflow_interventions_el_morl = cashflow_interventions_el_morl[, 5:8]
names(cashflow_interventions_el_morl) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions_el_morl


############################################### Sensitivity Analysis MorH ##############################################
# Importing Expense Loaded Premiums
wl_base_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                       col_names = c("vl", "l", "m", "h"),
                       sheet = "WL Aggregate",
                       range = "B2:E66") / 769177

t20_base_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                        col_names = c("vl", "l", "m", "h"),
                        sheet = "T20 Aggregate",
                        range = "B2:E102") / 602229

# Calculating Premiums without Interventions
INFORCE$Premiums = rep(0, dim(INFORCE)[1])

for (class in levels(INFORCE$UnderwritingClass)) {
  for (type in levels(INFORCE$PolicyType)) {
    for (age in 1:120) {
      if (class == "very low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
      } else if (class == "low risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
      } else if (class == "moderate risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
      } else if (class == "high risk" & type == "SPWL") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
      }
      
      
      if (class == "very low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
      } else if (class == "low risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
      } else if (class == "moderate risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
      } else if (class == "high risk" & type == "T20") {
        INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$Premiums = INFORCE[INFORCE$PolicyType == type & INFORCE$IssueAge == age & INFORCE$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
      }
      
    }
  }
}

profit = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$YearOfDeath > year
  is_alive[is.na(is_alive)] = T
  
  just_died = data$YearOfDeath == year
  just_died[is.na(just_died)] = F
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount)
  cost = cost + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_base_el_morh = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit(INFORCE, year)
  row = c(year, row)
  cashflow_base_el_morh = bind_rows(cashflow_base_el_morh, as.tibble(t(row)))
}

cashflow_base_el_morh = cashflow_base_el_morh[, 5:8]
names(cashflow_base_el_morh) = c("Year", "Revenue", "Cost", "Profit")

cashflow_base_el_morh

# With the Interventions and new mortality
INF$ProjYearOfDeath = rep(0, dim(INF)[1])

# Importing Expense Loaded Premiums
wl_vl_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "WL VeryLow",
                     range = "C4:Q122") / 769177

wl_l_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Low",
                    range = "C4:Q122") / 769177

wl_m_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL Moderate",
                    range = "C4:Q122") / 769177

wl_h_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                    col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                  'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                    sheet = "WL High",
                    range = "C4:Q122") / 769177


t20_vl_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                      col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                    'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                      sheet = "T20 VeryLow",
                      range = "C4:Q122") / 602229

t20_l_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Low",
                     range = "C4:Q122") / 602229

t20_m_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 Moderate",
                     range = "C4:Q122") / 602229

t20_h_el = read_xlsx("Premiums Expense Loaded High Mortality.xlsx", 
                     col_names = c("one", "two", 'three', 'four', 'onetwo', 'onethree', 'onefour', 'twothree', 'twofour', 'threefour', 
                                   'onetwothree', 'onetwofour', 'onethreefour', 'twothreefour', 'onetwothreefour'),
                     sheet = "T20 High",
                     range = "C4:Q122") / 602229

set.seed(100)
for (smoker in levels(INF$SmokerStatus)) {
  for (class in levels(INF$UnderwritingClass)) {
    for (type in levels(INF$PolicyType)) {
      for (age in 1:120) {
        
        # Interventions
        if (class == "very low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "SPWL") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * wl_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1234)[age]
          }
          
          
          
          
        }
        
        if (class == "very low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.30)
          i3 = rbinom(1, 1, age / 240 + 0.25)
          i4 = rbinom(1, 1, age/600 + 0.2)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$vl[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_vl)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_vl_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
          
          
        } else if (class == "low risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.35)
          i3 = rbinom(1, 1, age / 240 + 0.35)
          i4 = rbinom(1, 1, age/600 + 0.25)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$l[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_l)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_l_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_low$ex1234)[age]
          }
          
          
          
          
        } else if (class == "moderate risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.40)
          i3 = rbinom(1, 1, age / 240 + 0.45)
          i4 = rbinom(1, 1, age/600 + 0.4)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$m[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_m)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_m_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_moderate$ex1234)[age]
          }
          
          
          
          
        } else if (class == "high risk" & type == "T20") {
          i1 = 0
          if (smoker == "S") {
            i1 = rbinom(1, 1, 0.75)
          }
          
          i2 = rbinom(1, 1, 0.50)
          i3 = rbinom(1, 1, age / 240 + 0.5)
          i4 = rbinom(1, 1, age/600 + 0.5)
          
          if (i1==0 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_base_el$h[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_base$ex_h)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$one[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex1)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$two[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex2)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$three[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex3)[age]
          } else if (i1==0 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$four[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex4)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwo[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex12)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex13)[age]
          } else if (i1==1 & i2==0 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex14)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex23)[age]
          } else if (i1==0 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex24)[age]
          } else if (i1==0 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$threefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex34)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==0) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothree[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex123)[age]
          } else if (i1==1 & i2==1 & i3==0 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwofour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex124)[age]
          } else if (i1==1 & i2==0 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onethreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex134)[age]
          } else if (i1==0 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$twothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_high$ex234)[age]
          } else if (i1==1 & i2==1 & i3==1 & i4==1) {
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$Premiums = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$FaceAmount * t20_h_el$onetwothreefour[age]
            INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$ProjYearOfDeath = INF[INF$PolicyType == type & INF$IssueAge == age & INF$UnderwritingClass == class, ]$IssueYear + round(MORTDATA_verylow$ex1234)[age]
          }
          
          
        }
      }
    }
  }
}

INF$ProjYearOfDeath = round(rnorm(n=dim(INF)[1], mean = INF$ProjYearOfDeath, sd = 8))

profit_interventions = function (data, year) {
  # check the issue year, if it is less or equal than the input year, AND the policy hasnt died, collect a premium
  # check the deaths in this year, and payout the costs
  
  revenue = 0
  cost = 0
  expenses = 60000
  
  is_alive = data$ProjYearOfDeath > year
  just_died = data$ProjYearOfDeath == year
  
  revenue_wl = sum(data[data$PolicyType == "SPWL" & data$IssueYear == year & is_alive, ]$Premiums)
  revenue_t20 = sum(data[data$PolicyType == "T20" & data$IssueYear <= year & is_alive, ]$Premiums)
  
  cost = sum(data[data$IssueYear <= year & just_died, ]$FaceAmount) + count(data[data$IssueYear == year & just_died, ]) * expenses
  
  revenue = revenue_t20 + revenue_wl
  profit = revenue - cost
  
  return (c(revenue, cost, profit))
}

cashflow_interventions_el_morh = tibble(year = numeric(), revenue = numeric(), cost = numeric(), profit = numeric())

for (year in 2001:2023) {
  row = profit_interventions(INF, year)
  row = c(year, row)
  cashflow_interventions_el_morh = bind_rows(cashflow_interventions_el_morh, as.tibble(t(row)))
}

cashflow_interventions_el_morh = cashflow_interventions_el_morh[, 5:8]
names(cashflow_interventions_el_morh) = c("Year", "Revenue", "Cost", "Profit")

cashflow_interventions_el_morh


# Formatting
formatting = function (data, v) {
  i_vec = rep(0, 23)
  for (year in 1:23) {
    i_vec[year] = v^(1-year)
  }
  
  result = data %>%
    mutate(
      Revenue = map(Revenue, ~ round(.x / 1000000)),
      Cost = map(Cost, ~ round(.x / 1000000)), 
      Profit = map(Profit, ~ round(.x / 1000000))
    )
  pv = rep(0,3)
  pv[1] = sum(i_vec * as.numeric(result$Revenue))
  pv[2] = sum(i_vec * as.numeric(result$Cost))
  pv[3] = sum(i_vec * as.numeric(result$Profit))
  
  print(pv)
  return(result)
  
}

cf_base = formatting(cashflow_base, v = 1.0295 / 1.064)
cf_base_el = formatting(cashflow_base_el, v = 1.0295 / 1.064)
cf_base_el_1.6 = formatting(cashflow_base_el_1.6, v = 1/((1.064/1.0295)-0.02))
cf_base_el_5.6 = formatting(cashflow_base_el_5.6, v = 1/((1.064/1.0295)+0.02))
cf_base_el_morh = formatting(cashflow_base_el_morh, v = 1.0295 / 1.064)
cf_base_el_morl = formatting(cashflow_base_el_morl, v = 1.0295 / 1.064)

cf_int = formatting(cashflow_interventions, v = 1.0295 / 1.064)
cf_int_el = formatting(cashflow_interventions_el, v = 1.0295 / 1.064)
cf_int_el_1.6 = formatting(cashflow_interventions_el_1.6, v = 1/((1.064/1.0295)-0.02))
cf_int_el_5.6 = formatting(cashflow_interventions_el_5.6, v = 1/((1.064/1.0295)+0.02))
cf_int_el_morh = formatting(cashflow_interventions_el_morh, v = 1.0295 / 1.064)
cf_int_el_morl = formatting(cashflow_interventions_el_morl, v = 1.0295 / 1.064)






