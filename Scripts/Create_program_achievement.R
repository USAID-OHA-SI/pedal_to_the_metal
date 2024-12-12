# PROJECT:  C:/Users/tessam/Documents/Github/pedal_to_the_metal
# PURPOSE:  Create programmatic evaluation summary based on 0-1 achievements
# AUTHOR:   T. Essam | USAID
# REF ID:   f73560ca
# LICENSE:  MIT
# DATE:     2024-11-17
# UPDATED:  derived from create_programmatic_eval.r

# DEPENDENCIES ------------------------------------------------------------

  #general
  library(tidyverse)
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

  ref_id <- "f1a0e12f"  #a reference to be places in viz captions
  
  #path_msd <- si_path() %>% return_latest("PSNU_IM")
  #path_genie <- si_path() %>% return_latest("PSNUByIMs-Global")
  meta <- get_metadata("C:\\Users\\atowey\\Downloads\\MER_Structured_Datasets_PSNU_IM_FY22-25_20240913_v2_1.zip")
  cntry <- "Zambia"

# IMPORT ------------------------------------------------------------------
  #df <- read_psd(path_msd)
  df <- read_psd("C:\\Users\\atowey\\Downloads\\MER_Structured_Datasets_PSNU_IM_FY22-25_20240913_v2_1.zip")
  # Pull the list of OUs for which visuals need to be generated
  cop_ous <- glamr::pepfar_country_list %>%
    filter(str_detect(operatingunit, "Region", negate = T)) %>%
    pull(operatingunit)

  df <- df %>%
    filter(country %in% cop_ous)
    

# MUNGE FUNCTIONS -------------------------------------------------------------------

  ## IIT and VLC for TOTAL, PEDS, AGWY, and MALES ----  
# Prepare iit and VLC achievements for total, peds, agyw and males
  prep_iit_vlc_age <- function(.data){
    
    df_main <- .data %>%
      filter(indicator %in% c("TX_CURR_Lag2", "TX_PVLS", "TX_CURR", 
                              "TX_NEW", "TX_CURR_Lag1",
                              "TX_ML_IIT_less_three_mo",
                              "TX_ML_IIT_six_more_mo", 
                              "TX_ML_IIT_three_five_mo"),
             use_for_age == "Y",
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "HHS/CDC")) %>%
      clean_agency() %>%
      mutate(type = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Peds",
        sex == "Female" & ageasentered %in% c("10-14","15-19", "20-24")  ~ "AGYW",
        sex == "Male" & trendscoarse == "15+" ~ "Males (15+)",
        TRUE ~ "All Else"
      )) %>%
      gophr::clean_indicator() %>%
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator, type) %>%
      summarise(cumulative = sum(cumulative, na.rm = TRUE),
                .groups = "drop") %>%
      pivot_wider(names_from = indicator, # Pivot wide to perform calculations
                  values_from = cumulative,
                  names_glue = "{tolower(indicator)}")
    
    df_tot <- df_main %>% 
      mutate(type = "Total") %>% 
      group_by(country, psnu, psnuuid, funding_agency, type) %>% 
      summarize(across(where(is.double), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    
    df_main_tot <- df_main %>% 
      filter(type != "All Else") %>% 
      bind_rows(df_tot) %>% 
      arrange(country, psnu, psnuuid, type) %>% 
      # Add all three IIT categories together and divide by TX CURR
      rowwise() %>% 
      mutate(iit = (sum(tx_ml_iit_less_three_mo, tx_ml_iit_six_more_mo, tx_ml_iit_three_five_mo, na.rm = T) / sum(tx_curr_lag1, tx_new, na.rm = T)),
             vlc = tx_pvls_d / tx_curr_lag2) %>% 
      ungroup() %>% 
      #vls = tx_pvls / tx_pvls_d)
      filter(!is.na(type)) %>%
      select(country, psnu, psnuuid, funding_agency, type, vlc, iit) %>% 
      pivot_longer(cols = c(vlc, iit), 
                   names_to = "indicator",
                   values_to = "achievement") %>% 
      filter(!is.na(achievement), !is.nan(achievement)) %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) 
    
    return(df_main_tot)
  
  }
  
  ## PrEP Achievement for TOTAL, PEDS, AGWY and MALES -----
  
  prep_prep_achv <- function(.data){
    
   df_main <- .data %>% 
      # filter(country == "Zambia") %>% 
      filter(indicator %in% c("PrEP_NEW"), 
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "HHS/CDC"),
             use_for_age == "Y") %>%
      clean_agency() %>%
      mutate(type = case_when(
        ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Peds",
        # Targets are not set at this granular of a level
        sex == "Female" & target_age_2024 == "15-24"  ~ "AGYW",
        sex == "Male" & trendscoarse == "15+" ~ "Males (15+)",
        TRUE ~ "All Else"
      )) %>%
      gophr::clean_indicator() %>%
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator, type) %>%
      summarise(across(c(targets, cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop")
   
   df_tot <-  df_main %>% 
     mutate(type = "Total") %>% 
     group_by(country, psnu, psnuuid, funding_agency, type, indicator) %>% 
     summarize(across(where(is.double), \(x) sum(x, na.rm = TRUE)), .groups = "drop")
    
   df_main_tot <- df_main %>% 
     filter(type != "All Else") %>% 
     bind_rows(df_tot) %>% 
     arrange(country, psnu, psnuuid, type) %>% 
     calc_achievement() %>% 
     select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
     filter(!is.na(achievement))
   
   return(df_main_tot)
    
  }
  #df_prep <- prep_prep_achv(df)

#df_iit_vlc_age <- prep_iit_vlc_age(df)

  ## KP VLC ----  
  # Create KP VLC & VLS
  prep_kp_vl <- function(.data){
    
    kp_vl_disag <- "KeyPop/HIVStatus"
    
    df_kp_vlc <- .data %>% 
      # filter(country == "Zambia") %>% 
      filter(indicator %in% c("TX_PVLS", "TX_CURR_Lag2"), 
             standardizeddisaggregate %in% c(kp_vl_disag), 
             fiscal_year == meta$curr_fy,
             safe_for_vlc == "Y",
             funding_agency %in% c("USAID", "HHS/CDC")) %>% 
      clean_indicator() %>% 
      clean_agency() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>%
      summarise(across(c(cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>% 
      pivot_wider(names_from = indicator, # Pivot wide to perform calculations
                  values_from = cumulative,
                  names_glue = "{tolower(indicator)}") %>% 
      mutate(vlc = tx_pvls_d / tx_curr_lag2) %>% 
      filter(!is.na(vlc)) %>% 
      mutate(type = "KeyPop", indicator = "vlc") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement = vlc, type)
    
    return(df_kp_vlc)
  }
  
  #df_kp_vlc <- prep_kp_vl(df)

  ## KP PrEP Achievement ----  
  # Calculate KP prep achievement
  prep_kp_prep <- function(.data){
    
    kp_prep_disag <- c("KeyPopAbr") 
   
    df_kp <- .data %>%  
  # filter(country == "Zambia") %>% 
    filter(indicator %in% c("PrEP_NEW"), 
           standardizeddisaggregate %in% c(kp_prep_disag), 
           fiscal_year == meta$curr_fy,
           funding_agency %in% c("USAID", "HHS/CDC")) %>% 
      clean_indicator() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator, standardizeddisaggregate) %>%
      summarise(across(c(targets, cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>%
      gophr::clean_agency() %>%
      gophr::adorn_achievement(qtr = meta$curr_qtr) %>% 
      filter(targets > 0) %>% 
      mutate(type = "KeyPop") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) 
  
    return(df_kp)
  }
  
  #df_kp_prep <- prep_kp_prep(df)
  
  # Total Num PMTCT_EID and OVC_SERV
  prep_ovc <- function(.data){
    
    df_ovc <-  .data %>% 
      # filter(country == "Zambia") %>% 
      filter(indicator %in% c("OVC_SERV_UNDER_18"),
             standardizeddisaggregate == "Total Numerator",
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "HHS/CDC")) %>%
      clean_agency() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>%
      summarise(across(c(targets, cumulative), ~ sum(.x, na.rm = TRUE)),
                .groups = "drop") %>% 
      calc_achievement() %>% 
      mutate(type = "Total") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
    filter(!is.na(achievement))
    
    return(df_ovc)
  }
  
  prep_eid <- function(.data){
   
    df_eid <-  .data %>% 
      #filter(country == "Zambia") %>% 
      clean_indicator() %>% 
      filter(indicator %in% c("PMTCT_EID_D", "PMTCT_EID_Less_Equal_Two_Months"),
             standardizeddisaggregate %in% c("Total Numerator", "Total Denominator"), 
             fiscal_year == meta$curr_fy,
             funding_agency %in% c("USAID", "HHS/CDC")
      ) %>%
      clean_agency() %>% 
      group_by(fiscal_year, country, psnu, psnuuid, funding_agency, indicator) %>%
      summarize(cumulative = sum(cumulative, na.rm = T), .groups = "drop") %>% 
      pivot_wider(names_from = indicator, 
                  values_from = cumulative) %>% 
      mutate(achievement = PMTCT_EID_Less_Equal_Two_Months/PMTCT_EID_D) %>% 
      mutate(type = "Total", indicator = "PMTCT_EID_Less_Equal_Two_Months") %>% 
      select(country, psnu, psnuuid, funding_agency, indicator, achievement, type) %>% 
      filter(!is.na(achievement))
    
  return(df_eid)
    
  }
  
  
  
  #prep_pmtct_ovc(df)
  

  
  
  # PLOT FUNCTIONS ----------------------------------------------------------

  ## Custom Jittering ----
  # Allows for up/down jittering of dots in strip
  custom_jitter <- function(.data, jitter_factor = 0.05) {
    
    set.seed(42)
    .data %>%
      group_by(indicator, type) %>%  # Group by indicator to normalize each indicator separately
      mutate(
        # Jitter for visualization
        y_jitter = case_when(
          funding_agency == "USAID" ~ runif(n(), .01, jitter_factor), # Could adjust these values as needed
          funding_agency == "CDC" ~ runif(n(), -jitter_factor, -.01),
          TRUE ~ 0
        )
      ) %>%
      ungroup()  # Remove grouping after mutation
  }
  
  
  
  ## Create strip plot ----
  plot_program_achv_other <- function(.data, meta, cntry, jitter_factor, export = T) {
    
    options(warn = -1)
    
    # Set axis points
    baseline_pt_1 <- 0
    baseline_pt_2 <- .25
    baseline_pt_3 <- .5
    baseline_pt_4 <- .75
    baseline_pt_5 <- 1
    
    df <- .data %>%
      filter(country == cntry) %>% 
      # Adjust df for plot
      custom_jitter(0.05) %>%
      mutate(
        funding_agency = factor(funding_agency, levels = c("USAID", "CDC")),
        indicator = recode(indicator, "vlc" = "VLC", "iit" = "IIT", 
                           "PMTCT_EID_Less_Equal_Two_Months" = "EID_COV\n<=2 mo", 
                           "OVC_SERV_UNDER_18" = "OVC_SERV\n< 18") # For display purposes
      ) 
    
    p <- df %>% 
      # Plot setup
      ggplot(aes(achievement, y_jitter, color = funding_agency)) +
      geom_blank(aes(y = -y_jitter)) +
      geom_point(na.rm = TRUE, alpha = .5, size = 2) + # Adjust size as needed
      scale_color_manual(values = c("USAID" = hunter, "CDC" = slate),
                         name = "Funding Agency") +
      facet_grid(rows = vars(indicator), cols = vars(type), scales = "free", switch = "y") +
      theme(strip.text = element_markdown()) +
      scale_x_continuous(
        limits = c(0, 1),
        breaks = c(0, 0.5, 1),
        labels = c("0%", "50%", "100%"),
        oob = scales::squish,
        sec.axis = dup_axis()
      ) +
      
      # Line range with ticks
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 0), color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_1, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_2, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_3, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_4, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_5, y = 0), shape = 3, color = "#D3D3D3") +
      
      #Formatting
      si_style_nolines() +
      theme(strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1),
            axis.text.x = element_markdown(),
            axis.text.y = element_blank(),
            title = element_blank(),
            panel.spacing.y = unit(1, "lines"),
            strip.placement = "outside",
            strip.text = element_text(face = "bold", hjust = 0.5),
            plot.margin = unit(c(0,0,0,0), "in"),
            legend.position = "none") +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      guides(size = "none", shape = guide_legend())  
    
    
    if(export)
      #save_png(cntry, "program", scale = 1.25, height = 3.5, width = 8.22)
    
    return(p)
    
  }
  
  ## Create strip plot ----
  plot_program_achv_iit <- function(.data, meta, cntry, jitter_factor, export = T) {
    
    options(warn = -1)
    
    # Set axis points
    baseline_pt_1 <- 0
    baseline_pt_2 <- .25
    baseline_pt_3 <- .5
    baseline_pt_4 <- .75
    baseline_pt_5 <- 1
    
    df <- .data %>%
      filter(country == cntry) %>% 
      # Adjust df for plot
      custom_jitter(0.05) %>%
      mutate(
        funding_agency = factor(funding_agency, levels = c("USAID", "CDC")),
        indicator = recode(indicator, "vlc" = "VLC", "iit" = "IIT", 
                           "PMTCT_EID_Less_Equal_Two_Months" = "EID_COV\n<=2 mo", 
                           "OVC_SERV_UNDER_18" = "OVC_SERV\n< 18") # For display purposes
      ) 
    
    p <- df %>% 
      # Plot setup
      ggplot(aes(achievement, y_jitter, color = funding_agency)) +
      geom_blank(aes(y = -y_jitter)) +
      geom_point(na.rm = TRUE, alpha = .5, size = 2) + # Adjust size as needed
      scale_color_manual(values = c("USAID" = hunter, "CDC" = slate),
                         name = "Funding Agency") +
      facet_grid(rows = vars(indicator), cols = vars(type), scales = "free", switch = "y") +
      theme(strip.text = element_markdown()) +
      scale_x_continuous(
        limits = c(0, .1),
        breaks = c(0, 0.05, .1),
        labels = c("0%", "5%", "10%"),
        oob = scales::squish
      ) +
      
      # Line range with ticks
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 0), color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_1, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_2, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_3, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_4, y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pt_5, y = 0), shape = 3, color = "#D3D3D3") +
      
      #Formatting
      si_style_nolines() +
      theme(strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1),
            axis.text.x = element_markdown(),
            axis.text.y = element_blank(),
            title = element_blank(),
            panel.spacing.y = unit(1, "lines"),
            strip.placement = "outside",
            strip.text.x = element_text(color="#00000000", face = "bold", hjust = 0.5),
            strip.text.y = element_text(face = "bold", hjust = 0.5),
            plot.margin = unit(c(0,0,0,0), "in"),
            legend.position = "none") +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      guides(size = "none", shape = guide_legend())  
    
    
    if(export)
      #save_png(cntry, "program", scale = 1.25, height = 3.5, width = 8.22)
    
    return(p)
    
  }
  
  


# COMBINE DATA ------------------------------------------------------------

  # df_iit_vlc_age  <- prep_iit_vlc_age(df)
  # df_prep_achv    <- prep_prep_achv(df)
  # df_kp_prep      <- prep_kp_prep(df)
  # df_kp_vl        <- prep_kp_vl(df)
  # df_pmtct        <- prep_pmtct_ovc(df)

# Combine all data into a single dataframe

  # map(grid_dfs, ~.x %>% count(type))  
  # map(grid_dfs, ~.x %>% names())  
  
  prep_program_data <- function(.data){
    
    # Prep each data frame for combining
    df_iit_vlc_age  <- prep_iit_vlc_age(.data)
    df_prep_achv    <- prep_prep_achv(.data)
    df_kp_prep      <- prep_kp_prep(.data)
    df_kp_vl        <- prep_kp_vl(.data)
    df_eid          <- prep_eid(.data)
    df_ovc          <- prep_ovc(.data)
    
    # move into a list for reducing into a single dataframe
    grid_dfs <- list(df_iit_vlc_age, df_prep_achv, df_kp_prep, df_kp_vl, df_eid, df_ovc)
    
    # Create final data frame for return in render script
    df_combo <- reduce(grid_dfs, bind_rows) %>% 
      mutate(type = factor(type, levels = c("Total", "KeyPop", "Peds", "AGYW", "Males (15+)")),
             indicator = factor(indicator, levels = c("OVC_SERV_UNDER_18",
                                                      "PrEP_NEW",
                                                      "PMTCT_EID_Less_Equal_Two_Months",
                                                      "iit", "vlc"))) %>% 
      custom_jitter()
    
    return(df_combo)
    
  }
  

  df_combo <- prep_program_data(df)
  
  plot_program_achv(df_combo, meta, cntry = "Zambia", .05)
  
  #walk(v_countries, 
  #     .f = ~ plot_program_acvh(df_combo, meta, cntry = .x, jitter_factor = 0.05))
  

  # Split data into IIT and non-IIT and plot
  df_iit <- df_combo %>% filter(indicator == "iit")
  df_other <- df_combo %>% filter(indicator != "iit")
  
  country_select = "South Africa"
  # Create the blank row with the specified columns (may need to update!)
  blank_row <- data.frame(
    country = country_select,
    psnu = NA,
    psnuuid = NA,
    funding_agency = NA,
    indicator = "iit",  # Set the indicator value to 'iit'
    achievement = NA,
    type = "KeyPop",
    y_jitter = NA,
    stringsAsFactors = FALSE
  )
  
  iit_with_blank_row <- rbind(df_iit, blank_row)
  
  plot_other <- plot_program_achv_other(df_other, meta, cntry = country_select, .05)
  plot_iit <- plot_program_achv_iit(iit_with_blank_row, meta, cntry = country_select, .05)
  
  combined_plot <- plot_other / plot_iit + plot_layout(heights = c(4, 1))
  
  print(combined_plot)
  
  

# # EXPORT ----------------------------------------------------------------

  

