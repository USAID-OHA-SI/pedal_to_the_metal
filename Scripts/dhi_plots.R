# PROJECT:  pedal_to_the_metal
# PURPOSE:  DHI viz
# AUTHOR:   A.Chafetz | USAID
# REF ID:   22955bf0 
# LICENSE:  MIT
# DATE:     2024-11-05
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(vroom)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "22955bf0"  #a reference to be places in viz captions 
  
  path_dhi <-  si_path() %>% return_latest("DHI.*Detailed")
  path_dhi_sum <-  si_path() %>% return_latest("DHI.*Summary")
  
  
# IMPORT ------------------------------------------------------------------
  
  df_dhi <- vroom(path_dhi)
  df_dhi_sum <- vroom(path_dhi_sum)
  

# MUNGE -------------------------------------------------------------------

  unique(df_dhi$dhi_question_code)
  unique(df_dhi$primary_system_category)
  
    tibble(primary_system_category = unique(df_dhi$primary_system_category)) %>% 
      mutate(new = ifelse(str_detect(primary_system_category, "\\)$"),
                          str_extract(primary_system_category, "(?<=\\().*(?=\\))"),
                          "Other")) %>% 
      arrange(desc(new)) %>% 
      prinf()
  
  
  df_sys <- df_dhi %>% 
    bind_rows(df_dhi %>% 
                mutate(funding_agency = "PEPFAR")) %>% 
    filter(country == "Tanzania") %>% 
    rename(fiscal_year = dhi_submission_fiscal_year) %>% 
    clean_agency() %>% 
    filter(fiscal_year == max(fiscal_year),
           dhi_question_code == "estimated_budget") %>% 
    mutate(estimated_budget = as.numeric(response_option_desc)) %>% 
    group_by(fiscal_year, country, funding_agency) %>% 
    summarise(estimated_budget = sum(estimated_budget, na.rm = TRUE),
              n_systems = n_distinct(discrete_system_id),
              n_activities = n(),
              .groups = "drop") %>% 
    mutate(fill_color = ifelse(funding_agency == "USAID", si_palettes$hunter_t[3], "gray80"),
           funding_agency = fct_relevel(funding_agency, "USAID") %>% fct_rev(),
           fill_color = fct_rev(fill_color))
  
  df_sys %>% 
    filter(country == "Tanzania",
           funding_agency != "PEFPAR") %>% 
    ggplot(aes(estimated_budget, country, fill = fill_color)) +
    geom_col() +
    scale_fill_identity() +
    labs(x = NULL, y = NULL) +
    si_style_nolines() 
  

  df_sys_cat <- df_dhi %>% 
    rename(fiscal_year = dhi_submission_fiscal_year) %>% 
    clean_agency() %>% 
    filter(fiscal_year == max(fiscal_year),
           dhi_question_code == "estimated_budget") %>% 
    mutate(estimated_budget = as.numeric(response_option_desc)) %>% 
    mutate(primary_system_category = ifelse(str_detect(primary_system_category, "\\)$"),
                        str_extract(primary_system_category, "(?<=\\().*(?=\\))"),
                        "Other")) %>% 
    group_by(fiscal_year, country, funding_agency, primary_system_category) %>% 
    summarise(estimated_budget = sum(estimated_budget, na.rm = TRUE),
              n_systems = n_distinct(discrete_system_id),
              n_activities = n(),
              .groups = "drop") %>% 
    group_by(fiscal_year, country, funding_agency) %>%
    mutate(share = estimated_budget /sum(estimated_budget)) %>% 
    ungroup() %>% 
    filter(funding_agency %in% c("USAID", "CDC"))

  df_sys_cat %>% 
    filter(country == "Tanzania") %>% 
    mutate(primary_system_category = fct_reorder(primary_system_category, estimated_budget, sum),
           primary_system_category = fct_relevel(primary_system_category, "Other", after = 0)) %>% 
    ggplot(aes(share, funding_agency, fill = primary_system_category)) +
    geom_col() +
    facet_grid(~fct_rev(primary_system_category)) +
    labs(x = NULL, y = NULL) +
    si_style_nolines() +
    theme(axis.text.x = element_blank(),
          legend.position = "none")
  
  
