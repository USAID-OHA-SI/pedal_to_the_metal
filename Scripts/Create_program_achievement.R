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
  
  path_msd <- si_path() %>% return_latest("PSNU_IM")
  #path_genie <- si_path() %>% return_latest("PSNUByIMs-Global")
  #meta <- get_metadata("C:\\Users\\atowey\\Downloads\\MER_Structured_Datasets_PSNU_IM_FY22-25_20240913_v2_1.zip")
  cntry <- "Zambia"

# IMPORT ------------------------------------------------------------------
  df <- read_psd(path_msd)
  #df <- read_psd("C:\\Users\\atowey\\Downloads\\MER_Structured_Datasets_PSNU_IM_FY22-25_20240913_v2_1.zip")
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
  
  
  
  # PLOT FUNCTIONS ----------------------------------------------------------

  ## Custom Jittering ----
  # Allows for up/down jittering of dots in strip
  custom_jitter <- function(.data, jitter_factor = 0.05) {
    
    set.seed(42)
    .data %>%
      group_by(indicator, type) %>%  # Group by indicator to normalize each indicator separately
      mutate(
        y_jitter = case_when(
          funding_agency == "USAID" ~ runif(n(), .01, jitter_factor), # Could adjust these values as needed
          funding_agency == "CDC" ~ runif(n(), -jitter_factor, -.01),
          TRUE ~ 0
        )
      ) %>%
      ungroup()
  }
  
  
  ## Create strip plot ----
  plot_program_achv_other <- function(.data, meta, cntry, jitter_factor) {
    options(warn = -1)
    
    # Set axis points on 25s
    baseline_pts <- c(0, .25, .5, .75, 1)
    
    # Organize and name columns and rows
    df <- .data %>%
      filter(country == cntry) %>% 
      custom_jitter(0.05) %>% # Apply jitter
      mutate(
        funding_agency = factor(funding_agency, levels = c("USAID", "CDC")),
        indicator = recode(indicator,# Rename
                           "PrEP_NEW" = "PrEP_NEW\nresults : target",
                           "vlc" = "VLC", 
                           "PMTCT_EID_Less_Equal_Two_Months" = "EID_COV\n<=2 mo", 
                           "OVC_SERV_UNDER_18" = "OVC_SERV\n< 18\nresults : target")
      ) %>%
      mutate(
        indicator = factor(indicator, # Reorder
                           levels = c("VLC", 
                                      "PrEP_NEW\nresults : target", 
                                      "OVC_SERV\n< 18\nresults : target", 
                                      "EID_COV\n<=2 mo"))
      )
    
    # TO DO: If data are completely missing then make blank (theme_void)
    
    p <- df %>% 
      #Plot points
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
        oob = scales::squish, # Squish > 100%s
        sec.axis = dup_axis()
      ) +
      
      # Plot line range with ticks at 25s
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 0), color = "#D3D3D3") + # Line on each facet
      geom_point(aes(x = baseline_pts[1], y = 0), shape = 3, color = "#D3D3D3") + # Use plus signs as the ticks on the line
      geom_point(aes(x = baseline_pts[2], y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pts[3], y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pts[4], y = 0), shape = 3, color = "#D3D3D3") +
      geom_point(aes(x = baseline_pts[5], y = 0), shape = 3, color = "#D3D3D3") +
      
      # Format plot
      si_style_nolines() +
      theme(strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1),
            axis.text.x = element_markdown(), # Keep x labels (0, 25%, etc)
            axis.text.y = element_blank(), # Remove y axis points since are just random jitter
            title = element_blank(), # Title to be added in AI
            panel.spacing.y = unit(1, "lines"),
            strip.placement = "outside",
            strip.text = element_text(face = "bold", hjust = 0.5), # Bold indicator and population names
            plot.margin = unit(c(0,0,0,0), "in"),
            legend.position = "none") + # Legend to be added in AI
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      guides(size = "none", shape = guide_legend())  
    
    return(p)
  }
  
  plot_program_achv_iit <- function(.data, meta, cntry, jitter_factor = 0.05, missing_types) {
    options(warn = -1)
    
    # Set baseline points 0 5% 10%
    baseline_pts <- c(0, 0.05, 0.1)
    
    # Same as previous; organize data
    df <- .data %>%
      filter(country == cntry) %>% 
      custom_jitter(jitter_factor) %>%
      mutate(
        funding_agency = factor(funding_agency, levels = c("USAID", "CDC")),
        indicator = recode(indicator, "iit" = "% IIT"),
        # If there are missing types that will be a blank in the row (i.e. KeyPop having no IIT data) 
        # then make the line and ticks transparent color
        grid_color = if (length(missing_types) != 0) { 
          ifelse(type %in% missing_types, "#00000000", "#D3D3D3")
        } else {
          "#D3D3D3"
        }
      )
    
        
    # TO DO: If data are completely missing then make blank (theme_void)
    
    # Create the plot
    p <- ggplot(df, aes(achievement, y_jitter)) +
      geom_blank(aes(y = -y_jitter)) +
      geom_point(aes(color = funding_agency), na.rm = TRUE, alpha = 0.5, size = 2) +
      scale_color_manual(values = c("USAID" = hunter, "CDC" = slate),
                         name = "Funding Agency") +
      facet_grid(rows = vars(indicator), cols = vars(type), scales = "free", switch = "y") +
      scale_x_continuous(
        limits = c(0, 0.1),
        breaks = baseline_pts,
        labels = c("0%", "5%", ">=10%"),
        oob = scales::squish # Squish >=10%s
      ) +
      # Dynamic line range with per-facet grid color
      geom_linerange(aes(xmin = 0, xmax = 1.1, y = 0, color = I(grid_color)), show.legend = FALSE) + # Line
      geom_point(aes(x = baseline_pts[1], y = 0), color = I(df$grid_color), shape = 3, show.legend = FALSE) + #Ticks
      geom_point(aes(x = baseline_pts[2], y = 0), color = I(df$grid_color), shape = 3, show.legend = FALSE) +
      geom_point(aes(x = baseline_pts[3], y = 0), color = I(df$grid_color), shape = 3, show.legend = FALSE) +
      
      # Format plot same as previous
      si_style_nolines() +
      theme(
        strip.text = element_markdown(),
        # Color column labels transparent to keep the sizing consistent BUT remove the text
        strip.text.x = element_text(color = "#00000000", face = "bold", hjust = 0.5), 
        strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1, face = "bold"),
        axis.text.x = element_text(),
        axis.text.y = element_blank(),
        title = element_blank(),
        panel.spacing.y = unit(1, "lines"),
        strip.placement = "outside",
        plot.margin = unit(c(0, 0, 0, 0), "in"),
        legend.position = "none"
      ) +
      coord_cartesian(clip = "off") +
      labs(x = NULL, y = NULL) +
      guides(size = "none", shape = guide_legend())
    
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
      mutate(type = factor(type, levels = c("Total", "AGYW", "Males (15+)", "KeyPop", "Peds")), # Reorder
             indicator = factor(indicator, levels = c("OVC_SERV_UNDER_18",
                                                      "PrEP_NEW",
                                                      "PMTCT_EID_Less_Equal_Two_Months",
                                                      "iit", "vlc"))) %>% 
      custom_jitter()
    
    return(df_combo)
    
  }
  
# PLOT ----------------------------------------------------------------------
  
  plot_program <- function(df, meta, country_select) {
    # Prepare the data
    df_combo <- prep_program_data(df)
    
<<<<<<< HEAD
    # Filter data for IIT and the selected country
=======
    # Filter data for the selected country
>>>>>>> efe2b7c4e7d9a022f3ab20efaa433a65f6bb015a
    df_iit <- df_combo %>% filter(indicator == "iit", country == country_select)
    df_other <- df_combo %>% filter(indicator != "iit", country == country_select)
    
    # Identify populations the country has data for
    required_types <- unique(df_other$type)
    count_indicators <- length(unique(df_other$indicator))
    
    # Check for missing types in IIT data
    missing_types <- setdiff(required_types, df_iit$type)
    
    # Add blank rows for missing types in the IIT data
    if (length(missing_types) > 0) {
      blank_rows <- lapply(missing_types, function(type) {
        data.frame(
          country = country_select,
          psnu = NA,
          psnuuid = NA,
          funding_agency = NA,
          indicator = "iit",  # Set the indicator value to 'iit'
          achievement = NA,
          type = type,        # Use the missing type
          y_jitter = NA,
          stringsAsFactors = FALSE
        )
      })
      
      # Combine all blank rows into a single dataframe
      blank_rows_df <- do.call(rbind, blank_rows)
      
      # Append blank rows to the original dataframe
      df_iit <- rbind(df_iit, blank_rows_df)
    }
    
    # Plot other indicators
    plot_other <- plot_program_achv_other(df_other, meta, cntry = country_select, jitter_factor = 0.05)
    
    # Plot IIT
    plot_iit <- plot_program_achv_iit(df_iit, meta, cntry = country_select, jitter_factor = 0.05, missing_types = missing_types)
    
    # Combine the plots
    combined_plot <- plot_other / plot_iit + plot_layout(heights = c(count_indicators, 1))
    
<<<<<<< HEAD
    return(combined_plot)
  }
  
  print(plot_program(df, meta, "Tanzania"))
=======
    # Print the combined plot
    print(combined_plot)
    
    return(combined_plot)
  }
  
  plot_program(df, meta, "Tanzania")
>>>>>>> efe2b7c4e7d9a022f3ab20efaa433a65f6bb015a
  
  #walk(v_countries, 
  #     .f = ~ plot_program_acvh(df_combo, meta, cntry = .x, jitter_factor = 0.05))
  

  ##### TO DO ##############
  # geom_blank / theme_void if we are missing all data or some data
  # Test
  # Set up exporting

