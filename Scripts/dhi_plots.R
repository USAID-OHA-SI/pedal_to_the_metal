# PROJECT:  pedal_to_the_metal
# PURPOSE:  DHI viz
# AUTHOR:   A.Chafetz | USAID
# REF ID:   22955bf0 
# LICENSE:  MIT
# DATE:     2024-11-05
# UPDATED:  2024-11-12

# DEPENDENCIES ------------------------------------------------------------
  
  # #general
  # library(tidyverse)
  # library(vroom)
  # library(glue)
  # #oha
  # library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  # #viz extensions
  # library(scales, warn.conflicts = FALSE)
  # library(systemfonts)
  # library(tidytext)
  # library(patchwork)
  # library(ggtext)
  # 
  # source("Scripts/save_png.R")
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # ref_id <- "22955bf0"  #a reference to be places in viz captions 
  # 
  # path_dhi <-  si_path() %>% return_latest("DHI.*Detailed")
  # 
  # v_countries <- pepfar_country_list %>%
  #   filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
  #   pull(country)
  
# IMPORT ------------------------------------------------------------------
  
  # df_dhi <- vroom(path_dhi)
  

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
      full_join(expand_grid(fiscal_year = max(df_sys$fiscal_year),
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
      mutate(fill_color = ifelse(funding_agency == "USAID", si_palettes$hunter_t[1], slate),
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
      scale_y_discrete(expand = expansion(mult = 0))+
      theme(axis.text = element_blank(),
            plot.subtitle = element_markdown(),
            plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
            )
    
    
    if(export)
      save_png(cntry, "dhi", "overview", height = .75, width = 5, scale = 1.05)
    
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
      mutate(fill_color = ifelse(funding_agency == "USAID", si_palettes$hunter_t[1], slate))
    
    
    #expand grid to avoid empty plots
    df_sys_cat <- df_sys_cat %>% 
      full_join(expand_grid(fiscal_year = max(df_sys_cat$fiscal_year),
                             country = pepfar_country_list$country,
                             funding_agency = c("USAID", "CDC"),
                             primary_system_category = unique(df_sys_cat$primary_system_category)),
                 by = join_by(fiscal_year, country, funding_agency, primary_system_category)) %>% 
      mutate(estimated_budget = ifelse(is.na(estimated_budget), 0, estimated_budget))
    
    return(df_sys_cat)
    
  }
  
    plot_dhi_cat <- function(df, cntry, export = TRUE){
      
      df_cntry <- df %>% 
        filter(country == cntry) %>% 
        mutate(primary_system_category = fct_reorder(primary_system_category, estimated_budget, sum),
               primary_system_category = fct_relevel(primary_system_category, "Other", after = 0)) %>% 
        filter(estimated_budget > 0)
      
      #ensure USAID and CDC have lines
      df_cntry <- df_cntry %>% 
        full_join(expand_grid(funding_agency = c("USAID", "CDC")),
                   by = join_by(funding_agency))
        
      v <-  df_cntry %>% 
        ggplot(aes(share, funding_agency, fill = fill_color)) +
        geom_blank(aes(share*1.2))+
        geom_col(na.rm = TRUE, width = 0.25) +
        geom_text(aes(label = label_percent(1)(share)),
                  na.rm = TRUE, hjust = -0.05,
                  family = "Source Sans Pro", color = matterhorn) +
        #facet_grid(~fct_rev(primary_system_category)) +
        facet_wrap(~fct_rev(primary_system_category), nrow = 1) +
        labs(x = NULL, y = NULL) +
             # title = "Agency share of investments by HIS category" %>% toupper) +
        scale_fill_identity() +
        coord_cartesian(clip = "off") +
        si_style_nolines() +
        #scale_x_continuous(expand = c(0,0)) +
        theme(
          axis.text.x = element_blank(),
          # axis.text.y = element_text(margin = margin(r = -5)), # Reduce margin on the right to bring y labels closer
          # axis.ticks.y = element_blank(),                      # Remove y-axis ticks to eliminate extra space
          legend.position = "none",
          panel.spacing = unit(0.2, "lines"),                 # Reduce space between panels further
          plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt"),
          strip.text = element_text(margin = margin(t = 0, r = 0, b = 0, l = 0))
        ) 
      
      if(export)
        save_png(cntry, "dhi", "cat", scale = 1.15, height = 0.5, width = 5)
      
      return(v)
      
    }
  

  # test
  # df_sys_cat <- prep_dhi_cat(df_dhi)
  # 
  # map(v_countries[20],
  #     ~plot_dhi_cat(df_sys_cat, .x, FALSE))
  # 
    
    
    make_dhi_cat_table <- function(df, cntry, export = TRUE){
      
      custom_palette <- colorRampPalette(c(si_palettes$viking_t[1], si_palettes$viking_t[5]))(20)
      
      df_cntry <- df %>% 
        filter(country == cntry) %>% 
        mutate(primary_system_category = fct_reorder(primary_system_category, estimated_budget, sum),
               primary_system_category = fct_relevel(primary_system_category, "Other", after = 0)) %>% 
        filter(estimated_budget > 0)
      
      #ensure USAID and CDC have lines
      df_cntry <- df_cntry %>% 
        full_join(expand_grid(funding_agency = c("USAID", "CDC")),
                  by = join_by(funding_agency))  
      
      df_cntry %>% 
        select(country, funding_agency, primary_system_category, share) %>% 
        pivot_wider(names_from = primary_system_category,
                    values_from = share) %>% 
        mutate(agency = fct_relevel(funding_agency, c("USAID", "CDC"))) %>% 
        arrange(agency) %>% 
        select(-country, -funding_agency) %>% 
        select(agency, everything()) %>% 
        gt() %>% 
        fmt_percent(columns = where(is.double),
                    decimal = 0) %>% 
        sub_missing(columns = where(is.double),
                    missing_text = "-") %>% 
        cols_label(agency = "") %>% 
        gtExtras::gt_color_rows(
          columns = where(is.double),  # Specify columns that need coloring
          domain = c(0, 1),            # Domain of the data to scale the colors
          palette = c(si_palettes$viking_t[1], si_palettes$viking_t[5]),  # Custom color palette
          alpha = 0.7,   
          reverse = TRUE, # Transparency level
          na.color = grey10k # No fill for missing values
        ) %>% 
        tab_style(
          style = cell_borders(
            sides = c("bottom", "left", "right"),  # Specify all sides for stroke
            color = "white",                              # Set border color to white
            weight = px(1)                                # Thickness of the border
          ),
          locations = cells_body(columns = where(is.numeric))  # Apply to entire table body
        ) %>% 
        cols_width(
          agency ~ px(50),    # Set the column 'mpg' to have a width of 100 pixels
          everything() ~ px(80)  # Set all other columns to have a width of 150 pixels
        ) %>% 
        tab_style(
          style = cell_text(align = "left"),  # Align text to the right
          locations = cells_body(
            columns = agency)  # Target specific columns (e.g., mpg and cyl)
          ) %>% 
        compress_rows(font_size = 14) 
      
      if(export)
        save_gt(t, cntry, "dhi", "cat-tbl")
      
      }

    