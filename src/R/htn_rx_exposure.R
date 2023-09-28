rm(list=ls()); gc()

dat<-read.csv("./private/medications.csv", stringsAsFactors = F)

rxnorm_lst<-dat %>%
  select(rxnorm_cui,raw_rx_med_name) %>% unique


# 