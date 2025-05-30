rm(list=ls())
pacman::p_load(
  tidyverse,
  magrittr,
  stringr,
  broom,
  cowplot,
  ggrepel,
  kableExtra,
  gtsummary,
  devtools,
  tableone,
  pROC
)
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/plot_util.R")

path_to_data<-"C:/repos/r61-r33_vccc_kumc/data"
path_to_res<-"C:/repos/r61-r33_vccc_kumc/res"

baseline_aset<-readRDS(file.path(path_to_data,'baseline_aset.rda'))
# unenrl_aset<-readRDS(file.path(path_to_data,'unenrol_aset.rda'))

#==== CKD testing ====
var_ckd<-c(
  
)



#==== SDH on BP and MEDS ==== 
var_sel<-c(
  "AGE","SEX_STR","RACE_STR","SITE","ETHN_STR",
  "RUCA_PRIMARY_NONMETRO_IND","ADI_STATERANK","ADI_NATRANK",
  "AV_CNT","TH_CNT","ED_CNT","IP_CNT","HT","WT","BMI",
  "ANE","ARTH","AFIB","BLID","CANC","CKD","CP","CHF","CAD","DEM","DEP","DIA",
  "DIZ", "DYS","FALL","FRA","HEAR","HYPTN","SYNCO","LEUK","MLD","MSLD","MEL",
  "MI","OST","PUD","PVD","PULM","SU","STROKE","TD","UI","USD","VALV","WTLOS",
  "AIDS","CTD","DKD","ADPKD","GN","TIN","URO",
  "BASE_HR","BASE_SBP","BASE_DBP"
)
facvar_sel<-c(
  "SEX","RACE","SITE","ETHNICITY",
  "RUCA_PRIMARY_NONMETRO_IND",
  "ANE","ARTH","AFIB","BLID","CANC","CKD","CP","CHF","CAD","DEM","DEP","DIA",
  "DIZ", "DYS","FALL","FRA","HEAR","HYPTN","SYNCO","LEUK","MLD","MSLD","MEL",
  "MI","OST","PUD","PVD","PULM","SU","STROKE","TD","UI","USD","VALV","WTLOS",
  "AIDS","CTD","DKD","ADPKD","GN","TIN","URO" 
)

CreateTableOne(
  vars =var_sel,
  data = baseline_aset,
  factorVars = facvar_sel
)

varmod_sel<-c(
  "AGE","SEX_STR","RACE_STR","SITE","ETHN_STR",
  "RUCA_PRIMARY_NONMETRO_IND","ADI_STATERANK","ADI_NATRANK",
  "AV_CNT","TH_CNT","ED_CNT","IP_CNT","BMI",
  "ANE","ARTH","AFIB","BLID","CANC","CKD","CP","CHF","CAD","DEM","DEP","DIA",
  "DIZ", "DYS","FALL","FRA","HEAR","HYPTN","SYNCO","LEUK","MLD","MSLD","MEL",
  "MI","OST","PUD","PVD","PULM","SU","STROKE","TD","UI","USD","VALV","WTLOS",
  "AIDS","CTD","DKD","ADPKD","GN","TIN","URO"
)

baseline_aset %<>% filter(!is.na(BMI))

# Specify a null model with no predictors
null_model <- glm(
  BASE_SBP ~ 1,
  data = baseline_aset, 
  family = gaussian()
)

# Specify the full model using all of the potential predictors
full_model <- glm(
  formula(paste0("BASE_SBP ~ ",paste(varmod_sel,collapse = "+"))), 
  data = baseline_aset, 
  family = gaussian()
)

# Use a bi-directional stepwise algorithm to build a parsimonious model
step_model <- step(
  null_model,
  scope = list(lower = null_model, upper = full_model),
  direction = "both"
)

modsumm<-summary(step_model)
modsumm

baseline_aset %<>% 
  mutate(
    BASE_SBP_BIN = as.numeric((BASE_SBP>140))
  )

odds_ratio<-data.frame(
  var1=as.character(),
  var2=as.character(),
  odds_ratio=as.numeric(),
  ci_lower=as.numeric(),
  ci_upper=as.numeric(),
  p_value=as.numeric()
)

for (vari in var){
  cat("test variable:",vari,"\n")
  
  fit<-glm(
    as.formula(paste0("BASE_SBP_BIN ~",vari)),
    data=baseline_aset,
    family=binomial()
  )
  summ.fit<-summary(fit)
  ci.fit<-confint(fit)
  
  odds_ratio<-odds_ratio %>%
    add_row(
      var1 = vari,
      var2 = attr(summ.fit$coefficients,"dimnames")[[1]][-1],
      odds_ratio=exp(summ.fit$coefficients[-1,1]), # note that the coefficient of logistic regression is the log-odds
      ci_lower = exp(ci.fit[-1,1]),
      ci_upper = exp(ci.fit[-1,2]),
      p_value=summ.fit$coefficients[-1,4])
  
}
odds_ratio<-odds_ratio %>% 
  filter(p_value<0.1) %>%
  arrange(p_value)

odds_ratio
