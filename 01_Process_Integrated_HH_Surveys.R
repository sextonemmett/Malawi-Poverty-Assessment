rm(list = ls())

library(tidyverse)
library(vvauditor)

setwd('SET YOUR WD')

### LOAD DATA ### 

# Data Sources:
# World Bank Malawi Integrated Household Survey 2019-20, https://microdata.worldbank.org/index.php/catalog/3818
# World Bank Malawi Integrated Household Survey 2016-17,https://microdata.worldbank.org/index.php/catalog/2936

  # Load consumption dfs
  consumption_2016_raw = read_csv('./01_Data/MWI_2016_IHS/consumption_aggregate/ihs4 consumption aggregate.csv')
  consumption_2019_raw = read_csv('./01_Data/MWI_2019_IHS/ihs5_consumption_aggregate.csv')
  
  # Load household rosters
  hh_2016_raw = read_csv('./01_Data/MWI_2016_IHS/household/hh_mod_b.csv')
  hh_2019_raw = read_csv('./01_Data/MWI_2019_IHS/HH_MOD_B.csv')
  
  # Load education dfs
  edu_2016_raw = read_csv('./01_Data/MWI_2016_IHS/household/hh_mod_c.csv')
  edu_2019_raw = read_csv('./01_Data/MWI_2019_IHS/HH_MOD_C.csv')
  
  # Load house structure dfs
  struc_2016_raw = read_csv('./01_Data/MWI_2016_IHS/household/hh_mod_f.csv')
  struc_2019_raw = read_csv('./01_Data/MWI_2019_IHS/HH_MOD_f.csv')
  
  # Load business dfs
  busi_2016_raw = read_csv('./01_Data/MWI_2016_IHS/household/hh_mod_n1.csv')
  busi_2019_raw = read_csv('./01_Data/MWI_2019_IHS/HH_MOD_N1.csv')

### PROCESS CONSUMPTION DATA ###
    
  # Unique on household ID
  unique_id(consumption_2016_raw, case_id)
  unique_id(consumption_2019_raw, case_id)
  
  # Calculate poverty groups in 2016
  consumption_2016 = consumption_2016_raw %>% 
    mutate(c_pc_consumption = expagg/hhsize) %>% 
    mutate(c_poverty_group = case_when(c_pc_consumption < 85259 ~ 'Ultra-poor',
                                       c_pc_consumption >= 85259 & c_pc_consumption < 137425 ~ 'Poor',
                                       c_pc_consumption >= 137425 ~ 'Non-poor', 
                                       TRUE ~ 'Error'))
  
  consumption_2016 %>% count(c_poverty_group)
  
  # Calculate poverty groups in 2019
  consumption_2019 = consumption_2019_raw %>% 
    mutate(c_pc_consumption = expagg/hhsize) %>% 
    mutate(c_poverty_group = case_when(c_pc_consumption < 101293 ~ 'Ultra-poor',
                                       c_pc_consumption >= 101293 & c_pc_consumption < 165879 ~ 'Poor',
                                       c_pc_consumption >= 165879 ~ 'Non-poor', 
                                       TRUE ~ 'Error'))
  
  consumption_2019 %>% count(c_poverty_group)
  
  consumption_2016 = consumption_2016 %>% 
    mutate(c_food_share = rexp_cat011/rexpagg,
           c_rural = case_when(urban == 2 ~ 1, TRUE ~ 0),
           c_south = case_when(region == 3 ~ 1, TRUE ~ 0),
           c_central = case_when(region == 2 ~ 1, TRUE ~ 0),
           c_north = case_when(region == 1 ~ 1, TRUE~ 0)) %>% 
    select(case_id, expagg, c_pc_consumption, hhsize, c_poverty_group, urban, region, rexp_cat011, c_food_share, c_rural, c_south, c_central, c_north) 
  
  consumption_2019 = consumption_2019 %>% 
    mutate(c_food_share = rexp_cat011/rexpagg,
           c_rural = case_when(urban == 2 ~ 1, TRUE ~ 0),
           c_south = case_when(region == 3 ~ 1, TRUE ~ 0),
           c_central = case_when(region == 2 ~ 1, TRUE ~ 0),
           c_north = case_when(region == 1 ~ 1, TRUE~ 0)) %>% 
    select(case_id, expagg, c_pc_consumption, hhsize, c_poverty_group, urban, region, rexp_cat011, c_food_share, c_rural, c_south, c_central, c_north) 

### PROCESS HOUSEHOLD ROSTERS ###
  
  heads_2016 = hh_2016_raw %>% 
    filter(hh_b04 == 1) %>% 
    mutate(c_female_head = case_when(hh_b03 == 2 ~ 1, TRUE ~ 0)) %>% 
    select(case_id, PID, c_female_head)
  
  heads_2016 %>% count(PID)

  heads_2019 = hh_2019_raw %>% 
    filter(hh_b04 == 'HEAD') %>% 
    mutate(c_female_head = case_when(hh_b03 == 'FEMALE' ~ 1, TRUE ~ 0)) %>% 
    select(case_id, PID, c_female_head)
  
  heads_2019 %>% count(PID)

### PROCESS EDUCATION DATA ###
  
  edu_head_2016 = left_join(heads_2016, edu_2016_raw, by = c('case_id', 'PID'))
  edu_head_2016 = edu_head_2016 %>% 
    mutate(c_no_prim_edu = case_when(hh_c09 == 1 ~ 1, TRUE ~ 0),
           c_edu_grp = case_when(hh_c09 == 1 | hh_c09 == 2 ~ 'At most primary school', TRUE ~ 'More than primary school')) %>% 
    select(case_id, c_female_head, c_no_prim_edu, c_edu_grp)
  
  edu_head_2019 = left_join(heads_2019, edu_2019_raw, by = c('case_id', 'PID'))
  edu_head_2019 = edu_head_2019 %>% 
    mutate(c_no_prim_edu = case_when(hh_c09 == 'NONE' ~ 1, TRUE ~ 0),
           c_edu_grp = case_when(hh_c09 == 'NONE' | hh_c09 == 'PSLC' ~ 'At most primary school', TRUE ~ 'More than primary school')) %>% 
    select(case_id, c_female_head, c_no_prim_edu, c_edu_grp)

### PROCESS BUSINESS OWNERSHIP DATA ###
  
  busi_2016 = busi_2016_raw %>% 
    mutate(c_owned_busi = case_when(hh_n01 == 1 | 
                                    hh_n02 == 1 | 
                                    hh_n03 == 1 | 
                                    hh_n04 == 1 | 
                                    hh_n05 == 1 | 
                                    hh_n06 == 1 | 
                                    hh_n07 == 1 |
                                    hh_n08 == 1 ~ 1, TRUE ~ 0)) %>% 
    select(case_id, c_owned_busi)
  busi_2016 %>% count(c_owned_busi)
  
  busi_2019 = busi_2019_raw %>% 
    mutate(c_owned_busi = case_when(hh_n01 == 'YES' | 
                                    hh_n02 == 'YES' | 
                                    hh_n03 == 'YES' | 
                                    hh_n04 == 'YES' | 
                                    hh_n05 == 'YES' | 
                                    hh_n06 == 'YES' | 
                                    hh_n07 == 'YES' |
                                    hh_n08 == 'YES' ~ 1, TRUE ~ 0)) %>% 
    select(case_id, c_owned_busi)
  busi_2019 %>% count(c_owned_busi)
  
### PROCESS STRUCTURE DATA ### 
  
  struc_2016 = struc_2016_raw %>% 
    mutate(c_wtr_in_dwell = case_when(hh_f36 == 1 ~ 1, TRUE ~ 0)) %>% 
    select(case_id, c_wtr_in_dwell, hh_f10)

  struc_2019 = struc_2019_raw %>% 
    mutate(c_wtr_in_dwell = case_when(hh_f36 == 'PIPED INTO DWELLING' ~ 1, TRUE ~ 0)) %>% 
    select(case_id, c_wtr_in_dwell, hh_f10)
  
### MERGE DATA ###
  
  df_2016 = reduce(list(consumption_2016, edu_head_2016, busi_2016, struc_2016), inner_join, by = 'case_id')
  df_2019 = reduce(list(consumption_2019, edu_head_2019, busi_2019, struc_2019), inner_join, by = 'case_id')
  
  # Add persons-per-room variable
  df_2016 = df_2016 %>% 
    mutate(c_person_per_rm = hhsize/hh_f10) %>% 
    mutate(c_person_per_rm = case_when(is.infinite(c_person_per_rm) ~ NA, TRUE ~ c_person_per_rm))
  
  df_2019 = df_2019 %>% 
    mutate(c_person_per_rm = hhsize/hh_f10) %>% 
    mutate(c_person_per_rm = case_when(is.infinite(c_person_per_rm) ~ NA, TRUE ~ c_person_per_rm))
  
### SAVE ###
  
  save(df_2016, file = './01_Data/processed_2016_data.rds')
  save(df_2019, file = './01_Data/processed_2019_data.rds')
