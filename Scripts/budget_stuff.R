# PROJECT:  pedal_to_the_metal
# PURPOSE:  budget visuals
# AUTHOR:   A.Chafetz | USAID
# REF ID:   9c6a789d 
# LICENSE:  MIT
# DATE:     2024-10-31
# UPDATED:  2024-11-13

# DEPENDENCIES ------------------------------------------------------------
  
  # #general
  # library(tidyverse)
  # library(glue)
  # #oha
  # library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  # #viz extensions
  # library(scales, warn.conflicts = FALSE)
  # library(systemfonts)
  # library(tidytext)
  # library(patchwork)
  # library(ggtext)
  # library(gt)
  # library(fontawesome)
  # 
  # source("Scripts/save_png.R")
  # source("Scripts/save_gt.R")

# GLOBAL VARIABLES --------------------------------------------------------
  
  # ref_id <- "9c6a789d"  #a reference to be places in viz captions 
  # 
  # path_fsd <-  si_path() %>% return_latest("Financial")
  # 
  # meta <- get_metadata(path_fsd)  #extract MSD metadata
  # 
  # v_countries <- pepfar_country_list %>%
  #   filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
  #   pull(country)

# IMPORT ------------------------------------------------------------------
  
  # df_fsd <- read_psd(path_fsd)
  

# BUDGET TREND ------------------------------------------------------------

  #budget trend
  prep_bdgt_trend <- function(df, cntry){
    
    df_budget_trend <- df %>%
      bind_rows(df %>% mutate(fundingagency = "PEPFAR")) %>%
      mutate(fundingagency = ifelse(str_detect(fundingagency, "USAID"), "USAID", fundingagency)) %>%
      filter(fundingagency %in% c("USAID", "PEPFAR"),
             fiscal_year >= 2021) %>% 
      group_by(fiscal_year, country, fundingagency) %>% 
      summarise(cop_budget_total = sum(cop_budget_total, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(pt_label = case_when(fundingagency == "USAID" ~ 
                                    label_currency(1, scale = 1e-6, suffix = "m")(cop_budget_total)))
    
    #TODO - should this exclude SCH?
    
    return(df_budget_trend)
  }
  
  #plot budget trend
  plot_bdgt_trend <- function(df, cntry, export = TRUE){
    
    df_cntry <- df %>% 
      filter(country == {cntry})
    
    v <- df_cntry %>% 
      ggplot(aes(fiscal_year, cop_budget_total, fill = fundingagency)) +
      geom_col(position = "identity", width = 0.5) +
      geom_errorbar(aes(ymin = cop_budget_total, ymax = cop_budget_total), 
                    size = 0.5, width = 0.5, colour = grey50k, position = "identity") +
      geom_text(aes(label = pt_label), na.rm = TRUE, color = si_palettes$hunter_t[1],
                family = "Source Sans Pro", vjust = -.4, size = 12/.pt) +
      scale_y_continuous(label = label_currency(scale = 1e-6, suffix = "m")) +
      scale_fill_manual(values = c("PEPFAR" = grey20k,
                                   "USAID" = si_palettes$hunter_t[1])) +
      
      labs(x = NULL, y = NULL,
           title = glue("<span style = 'color:{si_palettes$hunter_t[1]};'>USAID'S</span> SHARE OF THE TOTAL PEPFAR BUDGET")) +
      si_style_ygrid() +
      theme(axis.text.y = element_blank(),
            legend.position ="none",
            plot.title = element_markdown(),
            plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
      )
    
    if(export)
      save_png(cntry, "budget", "trend", scale = 0.5)
    
    return(v)
  }
  

 #test 
  # df_bdgt_trnd <- prep_bdgt_trend(df_fsd)
  # 
  # map(v_countries[20],
  #     ~plot_bdgt_trend(df_bdgt_trnd, .x, FALSE))


# BUDGET TABLE ------------------------------------------------------------
  
  prep_bdgt_tbl <- function(df){
    
    df_budget_tbl <- df %>% 
      select(-pt_label) %>% 
      pivot_wider(names_from = fundingagency,
                  values_from = cop_budget_total) %>% 
      mutate(`USAID share` = USAID/PEPFAR) %>%
      pivot_longer(-c(fiscal_year, country),
                   names_to = "type") %>% 
      group_by(country, type) %>% 
      mutate(direction = case_when(value > lag(value,  order_by = fiscal_year) ~ "increase",
                                   value < lag(value,  order_by = fiscal_year) ~ "decrease",
                                   TRUE ~ "flat")) %>% 
      ungroup() %>% 
      filter(fiscal_year == max(fiscal_year)) %>% 
      mutate(val_fmt = ifelse(type == "USAID share",
                              percent_format(1)(value),
                              label_currency(1, scale = 1e-6, suffix = "m")(value))
             
      ) %>% 
      select(fiscal_year, country, type, val_fmt, direction)
    
    return(df_budget_tbl)
  }
  
  #from create_epi_control_summary_table.R
  compress_rows <- function(gt_object, font_size = 12) {
    gt_object %>%
      tab_options(
        data_row.padding = px(1),
        column_labels.padding = px(1),
        summary_row.padding = px(1),
        table.font.size = px(font_size),
        table_body.hlines.width = 0
      )
  }  
  
  plot_budget_tbl <- function(df, cntry, export = TRUE){
    
    df_cntry <- df %>% 
      filter(country == {cntry})
    
    t <- df_cntry %>% 
      gt() %>%
      cols_hide(c(fiscal_year, country)) %>% 
      # Transform the 'direction' column to include icons
      text_transform(
        locations = cells_body(columns = direction),
        fn = function(x) {
          case_when(
            x == "increase" ~ fa("chevron-up", fill = si_palettes$hunter_t[3]) %>% as.character,
            x == "decrease" ~ fa("chevron-down", fill = orchid_bloom) %>% as.character,
            x == "flat"     ~ fa("chevron-right", fill = grey20k) %>% as.character
          )
        }
      ) %>% 
      cols_align(align = "right",
                 columns = "val_fmt") %>% 
      cols_align(align = "center",
                 columns = direction) %>% 
      tab_options(column_labels.hidden = TRUE) %>% 
      # tab_header(title = glue("BUDGET FY{unique(df$fiscal_year) %>% str_sub(-2)}")) %>% 
      opt_align_table_header(align = "left") %>% 
      compress_rows(font_size = 11) 
    
    if(export)
      save_gt(t, "budget", "tbl")
    
    return(t)
    
  }
  

  #test 
  # df_bdgt_trend_tbl <- df_bdgt_trnd %>% 
  #   prep_bdgt_tbl()
  # 
  # map(v_countries[20],
  #     ~plot_budget_tbl(df_bdgt_trend_tbl, .x, FALSE))
  
   
# LOCAL PARTNER SHARE -----------------------------------------------------

  #TODO - should this just be USAID?
  #TODO - should this exclude SCH?
  
  prep_lp_share <- function(df){
    
    ptnr_type <- c("Local", "International", "TBD") #"Unknown" [M&O]
    
    df_lp <- df %>% 
      filter(fiscal_year == max(fiscal_year),
             fundingagency %in% c("USAID", "USAID/WCF")) %>%
      remove_mo() %>% 
      mutate(local_prime_partner = ifelse(mech_name == "TBD" | prime_partner_name == "TBD", "TBD", local_prime_partner),
             local_prime_partner = factor(local_prime_partner, ptnr_type) %>% fct_rev()) %>% 
      count(country, fiscal_year, local_prime_partner,
            wt = cop_budget_total, name = "cop_budget_total")
    
    #expand grid to avoid empty plots
    df_lp <- df_lp %>% 
      full_join(expand_grid(fiscal_year = max(df_lp$fiscal_year),
                            country = pepfar_country_list$country,
                            local_prime_partner = ptnr_type),
                by = join_by(fiscal_year, country, local_prime_partner)) %>% 
      mutate(cop_budget_total = ifelse(is.na(cop_budget_total), 0, cop_budget_total))
    
    #generate local partner share
    df_lp <- df_lp %>% 
      group_by(country) %>% 
      mutate(share = cop_budget_total / sum(cop_budget_total)) %>% 
      ungroup() %>% 
      mutate(pt_label = case_when(local_prime_partner == "Local" ~ 
                                    label_percent(1)(share)))
    
    return(df_lp)
  }
 
  plot_lp_share <- function(df, cntry, export = TRUE){
    
    df_cntry <- df %>% 
      filter(country == {cntry})
    
    tbd_share <- label_percent(1)(df_cntry[df_cntry$local_prime_partner == "TBD",]$share)
    
    v <- df_cntry %>% 
      ggplot(aes(cop_budget_total, country, fill = local_prime_partner)) +
      geom_col(width = 0.05) +
      scale_fill_manual(values = c("Local" = si_palettes$hunter_t[1], 
                                   "International" = si_palettes$hunter_t[5], #grey20k, 
                                   # "Unknown" = si_palettes$hunter_t[5], #grey30k
                                   "TBD" = si_palettes$hunter_t[5] #grey30k
                                   )) +
      geom_text(aes(label = pt_label), na.rm = TRUE, color = si_palettes$hunter_t[1],
                family = "Source Sans Pro", hjust = -.25, size = 11/.pt) +
      labs(x = NULL, y = NULL,
           title = glue("USAID BUDGET TO <span style = 'color:{si_palettes$hunter_t[1]};'>LOCAL PARTNERS</span>"),
           subtitle = glue("FY{str_sub(df_cntry$fiscal_year, -2)} Budget [{tbd_share} classified as 'TBD']")) +
           #caption = "Note: Includes SCH") +
      si_style_nolines() +
      scale_y_discrete(expand = expansion(mult = 0)) +
      theme(axis.text = element_blank(),
            plot.title = element_markdown(),
            legend.position = "none",
            plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
            )
    
    if(export)
      save_png(cntry, "budget", "lp-share", height = .75, width = 5, scale = 1.05)
    
    return(v)
  }
  
  #test 
   # df_lp_share <- prep_lp_share(df_fsd)
   # 
   # map(v_countries[20],
   #     ~plot_lp_share(df_lp_share, .x, FALSE))
   

  
  