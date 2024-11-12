# PROJECT:  pedal_to_the_metal
# PURPOSE:  DHI viz
# AUTHOR:   A.Chafetz | USAID
# REF ID:   22955bf0 
# LICENSE:  MIT
# DATE:     2024-11-05
# UPDATED:  2024-11-12

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

  source("Scripts/save_png.R")
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "22955bf0"  #a reference to be places in viz captions 
  
  path_dhi <-  si_path() %>% return_latest("DHI.*Detailed")
  
  v_countries <- pepfar_country_list %>%
    filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
    pull(country)
  
# IMPORT ------------------------------------------------------------------
  
  df_dhi <- vroom(path_dhi)
  

# EXPLORE -----------------------------------------------------------------

  # unique(df_dhi$dhi_question_code)
  # unique(df_dhi$primary_system_category)
  # 
  #   tibble(primary_system_category = unique(df_dhi$primary_system_category)) %>% 
  #     mutate(new = ifelse(str_detect(primary_system_category, "\\)$"),
  #                         str_extract(primary_system_category, "(?<=\\().*(?=\\))"),
  #                         "Other")) %>% 
  #     arrange(desc(new)) %>% 
  #     prinf()
  # 
  

# DHI OVERVIEW ------------------------------------------------------------

  prep_dhi_overview <- function(df){
    df_sys <- df %>% 
      bind_rows(df %>% 
                  mutate(funding_agency = "PEPFAR")) %>% 
      rename(fiscal_year = dhi_submission_fiscal_year) %>% 
      clean_agency() %>% 
      filter(fiscal_year == max(fiscal_year),
             dhi_question_code == "estimated_budget") %>% 
      mutate(estimated_budget = as.numeric(response_option_desc)) %>% 
      group_by(fiscal_year, country, funding_agency) %>% 
      summarise(estimated_budget = sum(estimated_budget, na.rm = TRUE),
                n_systems = n_distinct(discrete_system_id),
                n_activities = n(),
                .groups = "drop") 
    
    #expand grid to avoid empty plots
    df_sys <- df_sys %>% 
      right_join(expand_grid(fiscal_year = max(df_sys$fiscal_year),
                country = pepfar_country_list$country,
                funding_agency = c("PEPFAR","USAID")),
                by = join_by(fiscal_year, country, funding_agency))
    
    
    df_sys <- df_sys %>% 
      mutate(budget_share = case_when(funding_agency != "PEPFAR" ~ estimated_budget)) %>% 
      group_by(country) %>% 
      mutate(budget_share = budget_share/ sum(budget_share, na.rm = TRUE),
             budget_share = case_when(funding_agency == "USAID" ~ budget_share)) %>% 
      ungroup()
    
    df_sys <- df_sys %>%
      mutate(fill_color = ifelse(funding_agency == "USAID", si_palettes$hunter_t[1], "gray80"),
             funding_agency = fct_relevel(funding_agency, "USAID") %>% fct_rev(),
             fill_color = fct_rev(fill_color))
    
    return(df_sys)
  }
 
  
  
  
  plot_dhi_overview <- function(df, cntry, export = TRUE){
    
    df_cntry <- df %>%  
      filter(country == cntry) 
    
    #subititle
    v_subt <- df_cntry %>% 
      filter(funding_agency %in% c("USAID", "PEPFAR")) %>% 
      mutate(agency_anno = glue("**<span style = 'color:{fill_color};'>{funding_agency}</span>** - Budget: {label_currency(.1, scale_cut = cut_si(''))(estimated_budget)} | Systems: {label_number(1)(n_systems)} | Activities: {label_number(1)(n_activities)}")) %>% 
      arrange(funding_agency) %>% 
      pull() %>% 
      paste(collapse = "<br>")
    
    v <- df_cntry %>% 
      filter(funding_agency != "PEPFAR") %>% 
      ggplot(aes(estimated_budget, country, fill = fill_color)) +
      geom_col(na.rm = TRUE) +
      geom_text(aes(label = label_percent()(budget_share)), na.rm = TRUE,
                family = "Source Sans Pro", hjust = -.3, color = matterhorn) +
      scale_fill_identity() +
      labs(x = NULL, y = NULL,
           subtitle = v_subt) +
      si_style_nolines() +
      theme(axis.text = element_blank(),
            plot.subtitle = element_markdown())
    
    
    if(export)
      save_png(cntry, "dhi", "overview")
    
    return(v)
  }
    
  
  #test
  # df_sys <- prep_dhi_overview(df_dhi)
  # 
  # map(v_countries[20],
  #     ~plot_dhi_overview(df_sys, .x, FALSE))
  

# DHI CATEGORY BREAKDOWN --------------------------------------------------

  
  prep_dhi_cat <- function(df){
    
    df_sys_cat <- df %>% 
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
      filter(funding_agency %in% c("USAID", "CDC")) %>% 
      mutate(fill_color = ifelse(funding_agency == "USAID", si_palettes$hunter_t[1], "gray80"))
    
    
    #expand grid to avoid empty plots
    df_sys_cat <- df_sys_cat %>% 
      right_join(expand_grid(fiscal_year = max(df_sys_cat$fiscal_year),
                             country = pepfar_country_list$country,
                             funding_agency = c("USAID", "CDC"),
                             primary_system_category = unique(df_sys_cat$primary_system_category)),
                 by = join_by(fiscal_year, country, funding_agency, primary_system_category)) %>% 
      mutate(estimated_budget = ifelse(is.na(estimated_budget), 0, estimated_budget))
    
    return(df_sys_cat)
    
  }
  
    plot_dhi_cat <- function(df, cntry, export){
      
      df_cntry <- df %>% 
        filter(country == cntry) %>% 
        mutate(primary_system_category = fct_reorder(primary_system_category, estimated_budget, sum),
               primary_system_category = fct_relevel(primary_system_category, "Other", after = 0)) %>% 
        filter(estimated_budget > 0)
        
      v <-  df_cntry %>% 
        ggplot(aes(share, funding_agency, fill = fill_color)) +
        geom_col(na.rm = TRUE) +
        geom_text(aes(label = label_percent(1)(share)),
                  na.rm = TRUE, hjust = -.3,
                  family = "Source Sans Pro", color = matterhorn) +
        facet_grid(~fct_rev(primary_system_category)) +
        labs(x = NULL, y = NULL,
             title = "Agency share of investments by HIS category" %>% toupper) +
        scale_fill_identity() +
        coord_cartesian(clip = "off") +
        si_style_nolines() +
        theme(axis.text.x = element_blank(),
              legend.position = "none")
      
      if(export)
        save_png(cntry, "dhi", "cat")
      
      return(v)
      
    }
  

  # test
  df_sys_cat <- prep_dhi_cat(df_dhi)

  map(v_countries[20],
      ~plot_dhi_cat(df_sys_cat, .x, FALSE))
  