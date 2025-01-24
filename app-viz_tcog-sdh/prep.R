pacman::p_load(
  tidyverse,
  magrittr
)

path_to_res<-"C:/repos/r61-r33_vccc_kumc/res"
path_to_ref<-"C:/repos/r61-r33_vccc_kumc/ref"
rslt_uni<-read.csv(file.path(path_to_res,'sdh_univar_sel.csv'),stringsAsFactors = F)
rslt_lasso<-read.csv(file.path(path_to_res,'sdh_group_sel.csv'),stringsAsFactors = F)
dd<-read.csv(file.path(path_to_ref,'data_dict.csv'),stringsAsFactors = F)

dt_plt<-rslt_lasso %>%
  mutate(
    coef_sign = sign(coef),
    OR_lower = pmin(OR_lower,2),
    OR = pmax(pmin(OR,2),OR_lower),
    OR_upper = pmax(pmin(OR_upper,2),OR_lower)) %>%
  select(y,domain,topic,coef_sign,OR,OR_lower,OR_upper,p_value,var) %>%
  inner_join(
    rslt_uni %>%
      filter(sdh_var==var) %>%
      mutate(p_value = round(p_value,4)) %>%
      select(y,odds_ratio,ci_lower,ci_upper,p_value,var) %>%
      rename(pval=p_value),
    by=c("y","var")
  ) %>%
  left_join(
    dd %>% select(VAR,VARIABLE_LABEL),
    by=c("var"="VAR")
  ) %>%
  left_join(
    dd %>% select(VAR,VARIABLE_LABEL) %>% rename(y_lbl = VARIABLE_LABEL),
    by=c("y"="VAR")
  )

saveRDS(dt_plt,file="C:/repos/r61-r33_vccc_kumc/app-viz_tcog-sdh/data/rslt_tbl.rda")
