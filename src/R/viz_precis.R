rm(list=ls()); gc()

pacman::p_load(
  tidyverse,
  magrittr,
  readxl,
  fmsb
)

# The row 1 must contain the maximum values for each variable
# The row 2 must contain the minimum values for each variable
dat<-read_excel("./res/precis_raw.xlsx") %>%
  mutate(Domain = gsub(" ","\n",Domain)) %>%
  select(Domain,Score) 

dat2<-dat %>% mutate(id = 'vccc') %>%
  bind_rows(dat %>% mutate(id = 'max',Score = 5)) %>%
  bind_rows(dat %>% mutate(id = 'min', Score = 1)) %>%
  pivot_wider(
    names_from = Domain,
    values_from = Score,
    id_cols = 'id'
  ) %>%
  arrange(id) %>%
  select(-id)

color = "#E7B800"
png('./res/vccc_precis.png',
    height = 600, width = 600)
radarchart(
  df = dat2,
  axistype = 1,
  # Customize the polygon
  pcol = color, 
  pfcol = scales::alpha(color, 0.5), 
  plwd = 2, 
  plty = 1,
  # Customize the grid
  cglcol = "grey", 
  cglty = 1, 
  cglwd = 0.8,
  # Customize the axis
  axislabcol = "black",
  # Variable labels
  caxislabels = 1:5
)
dev.off()
