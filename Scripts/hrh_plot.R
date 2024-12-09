# PROJECT:  pedal_to_the_metal
# PURPOSE:  HRH viz
# AUTHOR:   A.Chafetz | USAID
# REF ID:   05e50315 
# LICENSE:  MIT
# DATE:     2024-11-04
# UPDATED:  2024-11-12

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
  # 
  # source("Scripts/save_png.R")
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  # ref_id <- "05e50315"  #a reference to be places in viz captions 
  # 
  # path_hrh <-  si_path() %>% return_latest("HRH.*not_redacted.*txt")
  # 
  # meta <- get_metadata(path_hrh)  #extract MSD metadata
  # 
  # v_countries <- pepfar_country_list %>%
  #   filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
  #   pull(country)
  
# IMPORT ------------------------------------------------------------------
  
  # df_hrh <- read_psd(path_hrh)
  

# MUNGE -------------------------------------------------------------------

  
  prep_hrh <- function(df){
    
    #ensure the non redacted dataset is being used (has annual spend needed)
    if (!"actual_annual_spend" %in% names(df))
    cli::cli_abort(c("{.var actual_annual_spend} is missing from the dataset",
                     "x" = "Ensure you are using the not redacted version from the HRH branch"))
    
    #aggregate staffing counts and expediture for the latest year
    df_hrh_v <- df %>% 
      clean_agency() %>% 
      filter(funding_agency %in% c("USAID", "CDC"),
             fiscal_year == max(fiscal_year)) %>% 
      group_by(fiscal_year, country, funding_agency, er_category) %>% 
      summarise(across(c(individual_count, actual_annual_spend), \(x) sum(x, na.rm = TRUE)),
                .groups = "drop")
    
    #expand grid to avoid empty plots
    df_hrh_v <- df_hrh_v %>% 
      full_join(expand_grid(fiscal_year = max(df_hrh_v$fiscal_year),
                             country = pepfar_country_list$country,
                             funding_agency = c("CDC","USAID"),
                             er_category = unique(df_hrh_v$er_category)),
                 by = join_by(fiscal_year, country, funding_agency, er_category))
    
    #pivot longer for plotting and clean up values
    df_hrh_v <- df_hrh_v %>% 
      pivot_longer(c(individual_count, actual_annual_spend),
                   names_to = "type") %>% 
      mutate(er_category = ifelse(er_category == "Implementing Mechanism Program Management Staff",
                                  "IP Staff", er_category),
             type = case_match(type,
                               "individual_count" ~ "Staff (individuals)",
                               "actual_annual_spend" ~ "Expenditure"),
             val_fmt = ifelse(type == "Expenditure", 
                              label_currency(.1, scale_cut = cut_si(''))(value),
                              label_number(.1, scale_cut = cut_si(''))(value)),
             val_fmt = ifelse(value < 1e3, as.character(value), val_fmt),
             fill_color = ifelse(funding_agency ==  "USAID", 
                                 si_palettes$hunter_t[1], slate))
    
    return(df_hrh_v)
  }
  
  
  plot_hrh <- function(df, cntry, export = TRUE){
    
    #subset to selected country (add variable for ordering plot on staff)
    df_hrh_v_cntry <- df %>% 
      filter(country == cntry) %>% 
      mutate(ordering = ifelse(type == 'Staff (individuals)', value, 0))
    
    
    #extract agency totals (staff + exp) for plot subtitle
    v_subt <- df_hrh_v_cntry %>% 
      count(funding_agency, type, fill_color, wt = value, name = "value") %>% 
      mutate(type = str_extract(type, "Staff|Expenditure")) %>% 
      pivot_wider(names_from = type) %>% 
      group_by(funding_agency) %>% 
      mutate(agency_anno = glue("**<span style = 'color:{fill_color};'>{funding_agency}</span>** - Staff: {label_number(.1, scale_cut = cut_si(''))(sum(Staff))} | Exp: {label_currency(.1, scale_cut = cut_si(''))(sum(Expenditure))}")) %>% 
      ungroup() %>% 
      arrange(desc(funding_agency)) %>% 
      pull() %>% 
      paste(collapse = "<br>")
    
    #plot 
    v <- df_hrh_v_cntry %>% 
      ggplot(aes(value, fct_reorder(er_category, ordering, .fun = sum, .na_rm = TRUE), fill = fill_color)) +
      geom_blank(aes(x = value*1.25)) +
      geom_col(width = 0.7, na.rm = TRUE) +
      geom_text(aes(label = val_fmt), hjust = -.10, na.rm = TRUE,
                family = "Source Sans Pro", color = matterhorn) +
      facet_grid(fct_rev(funding_agency) ~ fct_rev(type), scales = "free_x", switch = "y") +
      scale_x_continuous(labels = label_number(.1, scale_cut = cut_si(''))) +
      scale_fill_identity() +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL,
           subtitle = v_subt) +
      si_style_xgrid() +
      scale_y_discrete(expand = expansion(mult = 0.03)) +
      theme(strip.text = element_text(hjust = .5),
            strip.placement = "outside",
            axis.text.x = element_blank(),
            plot.subtitle = element_markdown(hjust = 1),
            plot.title = element_text(hjust = 0.09),
            panel.spacing = unit(0.3, "line"),
            plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
            )
    
    if(export)
      save_png(cntry, "hrh", scale = 1.15, width = 5.4431, height = 2.36)
    
    return(v)
  }
  
  
  
  #test
  # df_hrh_plot <- prep_hrh(df_hrh)
  # 
  # map(v_countries[17],
  #      ~plot_hrh(df_hrh_plot, .x, F))

