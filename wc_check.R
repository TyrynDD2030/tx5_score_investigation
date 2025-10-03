pacman::p_load(
  dplyr,
  cem,
  ggplot2,
  tidyr,
  lubridate,
  stringr,
  haven,
  scales,
  lme4,
  purrr,
  lmerTest,
  glmmTMB,
  flextable,
  leaflet,
  elomtools,
  survey,
  skimr,
  WeightIt,
  cobalt
)


tb5_2024 <- read_dta("data/2024tb5_all_forms_inter_data.dta") 

check <- tb5_2024 %>% 
  select(contains("ot"))

# Check why the outcomes are really low for children in Q2 and Q3 in the Western Cape
# Check if similar finding in Gauteng

prop_on_track <- tb5_2024 %>% 
  group_by(elom_prov, n_pri_fee_group7) %>% 
  summarise(n_elom_total_cuts_3 = mean(n_elom_total_cuts_3),
            n = n(),.groups="drop")

