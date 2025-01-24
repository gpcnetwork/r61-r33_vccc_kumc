#load packages
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
  pROC,
  corrplot,
  glmnet,
  boot
)

#load custom functions
source_url("https://raw.githubusercontent.com/sxinger/utils/master/analysis_util.R")
source_url("https://raw.githubusercontent.com/sxinger/utils/master/plot_util.R")

#specify abs paths
path_to_data<-"C:/repos/r61-r33_vccc_kumc/data"
path_to_res<-"C:/repos/r61-r33_vccc_kumc/res"
path_to_ref<-"C:/repos/r61-r33_vccc_kumc/ref"

#load data
dat<-readRDS(file.path(path_to_data,"tcog_sdh.rda")) %>%
  mutate(
    SEX_FAC = relevel(factor(SEX),ref='1'),
    RACE_FAC = relevel(factor(RACE),ref='5'),
    ETHN_FAC = relevel(factor(ETHNICITY),ref='2')
  )
dd<-read.csv(file.path(path_to_ref,'data_dict.csv'),stringsAsFactors = F)

var_sdh<-dd %>% filter(DATA_SOURCE %in% c('ACS')) %>% select(VAR) %>% unlist
var_base_lbl<-c("AGE","SEX_STR","RACE_STR","ETHN_STR","BSBP_GROUP","STATE","COUNTY","SITE")
var_tcog<-dd %>% filter(DOMAIN == 'Tcog'&!grepl("DATE",VAR)) %>% select(VAR) %>% unlist

var_all<-c(var_base_lbl,var_tcog,var_sdh)
facvar_all<-c("SEX_STR","RACE_STR","ETHN_STR","BSBP_GROUP","STATE","COUNTY","SITE")

#quick summary
var_lbl_df <- dd %>% mutate(var=VAR,var_lbl=VARIABLE_LABEL) %>% select(var,var_lbl)
cohort_summ<-univar_analysis_mixed(
  df = dat,
  id_col ="STUDY_ID",
  var_lst = var_all,
  facvar_lst  = facvar_all,
  pretty = T,
  var_lbl_df = var_lbl_df
)
cohort_summ %>% 
  save_kable(
    file.path(path_to_res,'tcog_sdh_allvar.html')
  ) 

#exclude variables with high missing rate
miss_rt_tbl<-data.frame(
  sdh_var=as.character(),
  miss_rt = as.numeric()
)
for(x in var_sdh){
  miss_rt<-mean(as.numeric(is.na(dat[,x])))
  miss_rt_tbl %<>%
    add_row(
      sdh_var=x,
      miss_rt = miss_rt
    )
}

var_base_mod<-c("AGE","SEX_FAC","RACE_FAC","ETHN_FAC","BSBP_GROUP","SITE")
var_tcog_disc<-var_tcog[!grepl("N",var_tcog)]
var_tcog_cont<-var_tcog[grepl("N",var_tcog)&!grepl("NE",var_tcog)]
var_sdh_sel<-var_sdh[var_sdh %in% (miss_rt_tbl %>% filter(miss_rt<0.2) %>% select(sdh_var) %>% unlist)]

#univariate selection with fixed confounding set
odds_ratio<-data.frame(
  y=as.character(),
  sdh_var=as.character(),
  var=as.character(),
  odds_ratio=as.numeric(),
  ci_lower=as.numeric(),
  ci_upper=as.numeric(),
  p_value=as.numeric()
)

for(y in c(var_tcog_disc,var_tcog_cont)){
  cat("outcome:",y,"\n")
  
  for(x in var_sdh_sel){
    cat("outcome:",y,"; sdh variable:",x,"\n")
    
    dat_filter<-dat[!is.na(dat[,x]),]
    if(y %in% var_tcog_disc){
      fit<-glm(
        as.formula(paste0(y," ~ ",paste(c(var_base_mod,x),collapse = "+"))),
        data = dat_filter,
        family = poisson(link = "log")
      )
    }else{
      fit<-glm(
        as.formula(paste0(y," ~ ",paste(c(var_base_mod,x),collapse = "+"))),
        data = dat_filter,
        family = Gamma(link = "log")
      )
    }
    summ.fit<-summary(fit)
    ci.fit<-confint(fit)
    
    odds_ratio<-odds_ratio %>%
      add_row(
        y = y,
        sdh_var = x,
        var = row.names(summ.fit$coefficients)[-1],
        odds_ratio = exp(summ.fit$coefficients[-1,1]),
        ci_lower = exp(ci.fit[-1,1]),
        ci_upper = exp(ci.fit[-1,2]),
        p_value=summ.fit$coefficients[-1,4]
      )
  }
}
write.csv(odds_ratio,file.path(path_to_res,'sdh_univar_sel.csv'),row.names = FALSE)


#multiple LASSO regression selection by domain
facvar_lst<-c("SEX_FAC","RACE_FAC","ETHN_FAC","BSBP_GROUP","SITE")
datfac_ohe<-dat %>% select(all_of(c("STUDY_ID",facvar_lst))) %>%
  pivot_longer(cols = - STUDY_ID,names_to = "facvar",values_to = "val") %>%
  mutate(ind = 1) %>% unite("var",c(facvar,val),sep="") %>%
  select(STUDY_ID,var,ind) %>%
  pivot_wider(names_from = var,values_from = ind,values_fill = list(ind=0)) %>%
  select(-SEX_FAC1,-RACE_FAC5,-ETHN_FAC2,-BSBP_GROUPbsbp0,-SITEKUMC) 

dat2<-dat %>% select(-all_of(facvar_lst)) %>%
  left_join(datfac_ohe,by="STUDY_ID")

var_base_mod2<-c("AGE",colnames(datfac_ohe)[!colnames(datfac_ohe) %in% "STUDY_ID"])
domain_lst<-c("Social context","Economic context","Education","Physical infrastructure","Healthcare context" )
odds_ratio_lasso<-data.frame(
  y=as.character(),
  domain=as.character(),
  topic=as.character(),
  var=as.character(),
  coef=as.numeric(),
  coef_lower=as.numeric(),
  coef_upper=as.numeric(),
  p_value=as.numeric()
)
for(s in c(var_tcog_disc,var_tcog_cont)){
  # s<-c(var_tcog_disc,var_tcog_cont)[1]
  cat("outcome:",s,"\n")
  
  for(d in domain_lst){
    # d<-"Social context"
    cat("outcome:",s,";domain:",d,"\n")
    var_domain<-dd %>% filter(DOMAIN==d) %>% select(TOPIC) %>% unique %>% unlist
    
    for(t in var_domain){
      # t<-var_domain[1]
      cat("outcome:",s,";domain:",d,";topic:",t,"\n")
      var_sdh_selt<-var_sdh_sel[var_sdh_sel %in% (dd %>% filter(DOMAIN==d&TOPIC==t) %>% select(VAR) %>% unlist)]
      
      dat2_filter<-dat2[!is.na(dat[,s]),] %>%
        mutate(across(where(is.numeric), ~ ifelse(is.na(.), median(., na.rm = TRUE), .)))
      X<-as.matrix(dat2_filter[,c(var_base_mod2,var_sdh_selt)])
      y<-unlist(dat2_filter[,s])
      
      if(s %in% var_tcog_disc){
        lasso_model <- cv.glmnet(x = X,y = y,family="poisson",alpha = 1)  # alpha = 1 for Lasso
      }else{
        lasso_model <- cv.glmnet(x = X,y = y,alpha = 1)  # alpha = 1 for Lasso
      }
      
      best_lambda <- lasso_model$lambda.min      # Best lambda from cross-validation
      
      # Fit the final Lasso model with the best lambda
      final_model <- glmnet(X, y, alpha = 1, lambda = best_lambda)
      coef(final_model)  # View coefficients
      
      # Function to fit Lasso and return coefficients
      lasso_coef <- function(data, indices) {
        sample_data <- data[indices, ]
        X_sample <- as.matrix(sample_data[,c(var_base_mod2,var_sdh_selt)])
        y_sample <- unlist(sample_data[,s])
        model <- glmnet(X_sample, y_sample, alpha = 1, lambda = best_lambda)
        return(as.vector(coef(model)))
      }
      
      # Perform bootstrapping
      boot_results <- boot(data = dat2_filter, statistic = lasso_coef, R = 100)
      
      # Extract confidence intervals
      conf_intervals <- t(sapply(1:ncol(boot_results$t), function(i) {
        quantile(boot_results$t[, i], c(0.025, 0.975))
      }))
      
      # Estimate p-values (proportion of bootstrap samples where coefficient is zero)
      p_values <- sapply(1:ncol(boot_results$t), function(i) {
        mean(boot_results$t[, i] == 0)
      })
      
      # Combine results into a data frame
      odds_ratio_lasso %<>%
        bind_rows(
          data.frame(
            y = s,
            domain = d,
            topic = t,
            var = colnames(X),
            coef = coef(final_model)[-1],  # Exclude intercept
            coef_lower = conf_intervals[, 1][-1],
            coef_upper = conf_intervals[, 2][-1],
            p_value = p_values[-1]
          )
        )
    }
  }
}
odds_ratio_lasso %<>%
  mutate(
    OR = exp(coef),
    OR_lower = exp(coef_lower),
    OR_upper = exp(coef_upper)
  )
write.csv(odds_ratio_lasso,file.path(path_to_res,'sdh_group_sel.csv'),row.names = FALSE)


#multiple regression selection by dimension-reduction per domain



# write.csv(odds_ratio,file.path(path_to_res,'sdh_group_sel.csv'),row.names = FALSE)

