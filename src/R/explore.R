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
  # "CKD_TS_NO",
  # "CKD_TS_EITHER",
  # "CKD_TS_BOTH",
  # "CKD_TS_EGFR",
  # "CKD_TS_ACRPCR",
  # "CKD_TS_GRP",
  "MED_CNT_IN",
  "AGE",
  "SEX_STR",
  "RACE_STR",
  "ETHN_STR",
  "ADI_NATRANK",
  "ADI_STATERANK",
  "RUCA_PRIMARY_GRP",
  "RUCA_PRIMARY_NONMETRO_IND",
  "SMOKER_IND",
  "BASE_SBP",
  "BASE_DBP",
  "BMI",
  "CCI_TOTAL",
  "CCI_TOTAL_GRP",
  "EFI_DIA",
  "EGFR_MULTI_IND",
  "ACRPCR_MULTI_IND"
)
facvar_ckd<-var_ckd[!var_ckd %in% c(
  "MED_CNT_IN",
  "AGE",
  "ADI_NATRANK",
  "ADI_STATERANK",
  "BASE_SBP",
  "BASE_DBP",
  "BMI",
  "CCI_TOTAL"
)]

CreateTableOne(
  data = baseline_aset,
  vars = var_ckd,
  factorVars = facvar_ckd,
  strata = "CKD_TS_EITHER"
)

CreateTableOne(
  data = baseline_aset,
  vars = var_ckd,
  factorVars = facvar_ckd,
  strata = "CKD_TS_GRP"
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


