rm(list = ls())

library(tidyverse)
library(ggplot2)
library(ineq)
library(writexl)

setwd('SET YOUR WD')

### LOAD PROCESSED DATA ###
  
  load(file = './01_Data/processed_2016_data.rds')
  load(file = './01_Data/processed_2019_data.rds')

### CORRELATES OF POVERTY ###
  
  # 2016/2017 survey
  corr_2016 = df_2016 %>% 
    group_by(c_poverty_group) %>% 
      summarize(across(c('c_female_head', 
                         'c_no_prim_edu', 
                         'hhsize', 
                         'c_rural', 
                         'c_south', 
                         'c_central', 
                         'c_north',
                         'c_owned_busi',
                         'c_wtr_in_dwell',
                         'c_person_per_rm',
                         'c_pc_consumption',
                         'c_food_share'), mean, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_longer(cols = -c_poverty_group)  %>% 
    pivot_wider(id_cols = name, values_from = value, names_from = c_poverty_group) %>% 
    mutate(`Poor - Non-poor` = `Poor`-`Non-poor`,
           `Ultra-poor - Poor` = `Ultra-poor`-`Poor`) %>% 
    select(`Variable` = name,
           `Ultra-poor`,
           `Poor`,
           `Non-poor`,
           `Ultra-poor - Poor`,
           `Poor - Non-poor`)
  
  # 2019/2020 survey
  corr_2019 = df_2019 %>% 
    group_by(c_poverty_group) %>% 
    summarize(across(c('c_female_head', 
                       'c_no_prim_edu', 
                       'hhsize', 
                       'c_rural', 
                       'c_south', 
                       'c_central', 
                       'c_north',
                       'c_owned_busi',
                       'c_wtr_in_dwell',
                       'c_person_per_rm',
                       'c_pc_consumption',
                       'c_food_share'), mean, na.rm = TRUE)) %>% 
    ungroup() %>% 
    pivot_longer(cols = -c_poverty_group)  %>% 
    pivot_wider(id_cols = name, values_from = value, names_from = c_poverty_group) %>% 
    mutate(`Poor - Non-poor` = `Poor`-`Non-poor`,
           `Ultra-poor - Poor` = `Ultra-poor`-`Poor`) %>% 
    select(`Variable` = name,
           `Ultra-poor`,
           `Poor`,
           `Non-poor`,
           `Ultra-poor - Poor`,
           `Poor - Non-poor`)
  
  write_xlsx(corr_2016, './03_Output/correlation_table_2016.xlsx')
  write_xlsx(corr_2019, './03_Output/correlation_table_2019.xlsx')
  
  # Test for statistical differences in means between poverty groups
  # T-test for continuous variables and Chi-sqr for proportions
  continuous_variables = c('hhsize', 
                           'c_person_per_rm', 
                           'c_pc_consumption', 
                           'c_food_share')
  
  binary_variables = c('c_female_head', 
                       'c_no_prim_edu',
                       'c_rural',
                       'c_south',
                       'c_central',
                       'c_north',
                       'c_owned_busi',
                       'c_wtr_in_dwell')
  
  # Function to test statistical difference of means between poverty groups for binary variables
  store_chi_tests = function(df, binary_variables){
    results = list()
    for(binary_var in binary_variables) {
      print(binary_var)
      results[[binary_var]] = pairwise.prop.test(x = table(df[['c_poverty_group']], df[[binary_var]]), p.adjust.method = "holm")
    }
    return(results)
  }
    
  chi_results_2016 = store_chi_tests(df_2016, binary_variables)
  chi_results_2019 = store_chi_tests(df_2019, binary_variables)
  
  # Function to test statistical difference of means between poverty groups for continuous variables
  store_t_tests = function(df, continuous_variables) {
    results = list()
    for(contin_var in continuous_variables) {
      print(contin_var)
      results[[contin_var]] = pairwise.t.test(df[[contin_var]], df[['c_poverty_group']], p.adjust.method = "holm")
    }
    return(results)
  }
  
  t_results_2016 = store_t_tests(df_2016, continuous_variables)
  t_results_2019 = store_t_tests(df_2019, continuous_variables)
  
### CONSUMPTION PROFILE ###
  
  # Prepare the data
  df_2016 = df_2016 %>% 
    mutate(year = '2016',
           c_log_pc_con = log(c_pc_consumption))
  df_2019 = df_2019 %>% 
    mutate(year = '2019',
           c_log_pc_con = log(c_pc_consumption))

  threshold_2016 = quantile(df_2016$c_log_pc_con, probs = 0.99)
  threshold_2019 = quantile(df_2019$c_log_pc_con, probs = 0.99)
  
  # Filter out the data that's above the 99th percentile to avoid large right tail in consumption profile chart
  df_2016_filtered = df_2016 %>% filter(c_log_pc_con <= threshold_2016)
  df_2019_filtered = df_2019 %>% filter(c_log_pc_con <= threshold_2019)
  
  combined_df = rbind(df_2016_filtered %>% select(c_log_pc_con, year), 
                      df_2019_filtered %>% select(c_log_pc_con, year))
  
  # Calculate the CDF
  combined_df = combined_df %>%
    group_by(year) %>%
    arrange(c_log_pc_con) %>%
    mutate(cdf = cumsum(c_log_pc_con) / sum(c_log_pc_con))
  
  # Plotting the CDF
  income_prof = ggplot(combined_df, aes(x = c_log_pc_con, y = cdf, color = year)) +
      geom_line() +
      geom_vline(xintercept = log(137425), linetype = "dashed", color = "orangered3", linewidth = 0.8) +
      geom_vline(xintercept = log(165879), linetype = "dashed", color = "green4", linewidth = 0.8) +
      labs(x     = 'Log per-capita consumption', 
           y     = 'CDF',
           color = 'Survey Year')+
           # caption = "Note: Top 1% of incomes were dropped to avoid long right tail in profile.") +
      theme_minimal() +
      theme(text = element_text(family = "Times New Roman"), 
            axis.text = element_text(color = "black", size = 14),
            axis.title = element_text(color = "black", size = 14),
            legend.title = element_text(size = 14),
            axis.ticks = element_line(color = "black", linewidth = .6)) + 
      scale_color_manual(values = c("2016" = "orangered3", "2019" = "green4"))
    
    income_prof
    
   ggsave(file = './03_Output/income_profiles.jpeg', plot = income_prof, width = 10, height = 5)
   
### POVERTY INDICATORS ###
  
  df_2016 = df_2016 %>% 
    mutate(c_poor_cnt = ifelse(c_poverty_group %in% c('Ultra-poor', 'Poor'), 1*hhsize, 0),
           c_hh_pov_gap = ifelse(c_poverty_group %in% c('Ultra-poor', 'Poor'), (137425 - c_pc_consumption)/137425, 0)) %>% 
    mutate(c_hh_pov_gap_sqrd = c_hh_pov_gap^2)

  pov_indic_2016 = df_2016 %>% 
    summarize(
      'Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize),
      'Poverty gap (P1)'  = sum(c_hh_pov_gap*hhsize)/sum(hhsize),
      'Poverty severity (P2)' = sum(c_hh_pov_gap_sqrd*hhsize)/sum(hhsize)
    )
  
  df_2019 = df_2019 %>% 
    mutate(c_poor_cnt = ifelse(c_poverty_group %in% c('Ultra-poor', 'Poor'), 1*hhsize, 0),
           c_hh_pov_gap = ifelse(c_poverty_group %in% c('Ultra-poor', 'Poor'), (165879 - c_pc_consumption)/165879, 0)) %>% 
    mutate(c_hh_pov_gap_sqrd = c_hh_pov_gap^2)
  
  pov_indic_2019 = df_2019 %>% 
    summarize(
      'Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize),
      'Poverty gap (P1)'  = sum(c_hh_pov_gap*hhsize)/sum(hhsize),
      'Poverty severity (P2)' = sum(c_hh_pov_gap_sqrd*hhsize)/sum(hhsize)
    )

  # Poverty indicators for sub-populations
  # Urban vs rural
  pov_urb_rur_2016 = df_2016 %>% 
    group_by(c_rural) %>% 
      summarize('Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize)) %>% 
    ungroup()
  pov_urb_rur_2019 = df_2019 %>% 
    group_by(c_rural) %>% 
      summarize('Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize)) %>% 
    ungroup()
  # Educational attainment
  pov_prim_edu_2016 = df_2016 %>% 
    group_by(c_edu_grp) %>% 
      summarize('Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize)) %>% 
    ungroup()
  pov_prim_edu_2019 = df_2019 %>% 
    group_by(c_edu_grp) %>% 
      summarize('Poverty rate (P0)' = sum(c_poor_cnt)/sum(hhsize)) %>% 
    ungroup()
  
### SECTION 4: INEQUALITY INDICATORS ###
  ## 4.1: Theil and Gini ##
  # Calculate theil and gini for 2016 urban pop
  urban_2016 = df_2016 %>% filter(c_rural == 0)

  theil_index_urb_2016 = Theil(urban_2016$c_pc_consumption)
  gini_urb_2016 = Gini(urban_2016$c_pc_consumption)
  
  urban_ineq_2016 = data.frame(
    'Theil_index' = theil_index_urb_2016,
    'Gini_coefficient' = gini_urb_2016,
    'Subset' = 'Urban'
  )
  
  # Calculate theil and gini for 2016 rural pop
  rural_2016 = df_2016 %>% filter(c_rural == 1)
  
  theil_index_rur_2016 = Theil(rural_2016$c_pc_consumption)
  gini_rur_2016 = Gini(rural_2016$c_pc_consumption)
  
  rural_ineq_2016 = data.frame(
    'Theil_index' = theil_index_rur_2016,
    'Gini_coefficient' = gini_rur_2016,
    'Subset' = 'Rural'
  )
  
  # Calculate theil and gini for 2019 urban pop
  urban_2019 = df_2019 %>% filter(c_rural == 0)
  
  theil_index_urb_2019 = Theil(urban_2019$c_pc_consumption)
  gini_urb_2019 = Gini(urban_2019$c_pc_consumption)
  
  urban_ineq_2019 = data.frame(
    'Theil_index' = theil_index_urb_2019,
    'Gini_coefficient' = gini_urb_2019,
    'Subset' = 'Urban'
  )
  
  # Calculate theil and gini for 2019 rural pop
  rural_2019 = df_2019 %>% filter(c_rural == 1)
  
  theil_index_rur_2019 = Theil(rural_2019$c_pc_consumption)
  gini_rur_2019 = Gini(rural_2019$c_pc_consumption)
  
  rural_ineq_2019 = data.frame(
    'Theil_index' = theil_index_rur_2019,
    'Gini_coefficient' = gini_rur_2019,
    'Subset' = 'Rural'
  )

  ## Lorenz Curves ##
    
     # Compute Lorenz curve coordinates for each vector
     lc_rur_2016 = Lc(rural_2016$c_pc_consumption)
     lc_rur_2019 = Lc(rural_2019$c_pc_consumption)
     lc_urb_2016 = Lc(urban_2016$c_pc_consumption)
     lc_urb_2019 = Lc(urban_2019$c_pc_consumption)
     
     # Convert Lorenz curve coordinates to data frames
     df_rur_2016 = data.frame(Share.of.Total = lc_rur_2016$p, Cumulative.Share = lc_rur_2016$L, Group = 'Rural 2016')
     df_rur_2019 = data.frame(Share.of.Total = lc_rur_2019$p, Cumulative.Share = lc_rur_2019$L, Group = 'Rural 2019')
     df_urb_2016 = data.frame(Share.of.Total = lc_urb_2016$p, Cumulative.Share = lc_urb_2016$L, Group = 'Urban 2016')
     df_urb_2019 = data.frame(Share.of.Total = lc_urb_2019$p, Cumulative.Share = lc_urb_2019$L, Group = 'Urban 2019')
     
     # Combine data frames
     lorenz_data = rbind(df_rur_2016, df_rur_2019, df_urb_2016, df_urb_2019)
     
     # Plot Lorenz curves
     lorenz_curve = ggplot(lorenz_data, aes(x = Share.of.Total, y = Cumulative.Share, color = Group)) +
       geom_line(linewidth = 1.2) +
       geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "gray") +
       labs(x = "Cumulative share of population",
            y = "Cumulative share of income",
            color = "Group") +
       theme_minimal() +
       theme(text = element_text(family = "Times New Roman"), 
             axis.text = element_text(color = "black", size = 14),
             axis.title = element_text(color = "black", size = 14),
             legend.title = element_text(size = 14),
             axis.ticks = element_line(color = "black", linewidth = .6)) + 
       scale_color_manual(values = c("orangered3", "orangered4", "green3", "green4"))

     ggsave(filename = './03_Output/Lorenz_curves.jpeg', plot = lorenz_curve, width = 10, height = 5)     
    
    
    
    
  
  
  
