# Check that the table Zenzi produced is correct. Shows the planned/allocated sample sizes. Use the number realised

pacman::p_load(
  dplyr,
  tidyr,
  haven
)

# Number of allocated children -------------------------------------------------
tb5_2024 <- read_dta("data/2024tb5_all_forms_inter_data.dta")

realised_elps <- tb5_2024 %>% 
  distinct(id_ecd, .keep_all = TRUE) %>% 
  # group_by(elom_prov) %>% 
  count(elom_prov) # Correct

sum(realised_elps$n) # Correct


# Number of realised children --------------------------------------------------
tb5_2024 <- read_dta("data/2024tb5_all_forms_inter_data.dta")

realised_child <- tb5_2024 %>% 
  distinct(id_child, .keep_all = TRUE) %>% 
  # group_by(elom_prov) %>% 
  count(elom_prov) # Correct

sum(realised_child$n) # Correct


# Double check Gunilla's planned vs allocated sizes

