# PROJECT: COP/ROP Sprint
# PURPOSE: Munge and Analysis of Policy Lab Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  1b6b1dd7
# LICENSE: MIT
# DATE:   2024-11-01
# NOTES: adapted from https://github.com/USAID-OHA-SI/hardapoart/blob/main/Scripts/16_10s_barriers.R

# LOCALS & SETUP ============================================================================

  # Libraries
    library(gagglr)
    library(tidyverse)
    library(scales)
    library(sf)
    library(systemfonts)
    library(tidytext)
    library(patchwork)
    library(ggtext)
    library(glue)
    library(gt)
    library(gtExtras)
    library(googlesheets4)
    library(readxl)
    library(fontawesome)
    
    
  # SI specific paths/functions  
    #load_secrets("email")
    
    #pol_lab_id <- "1zVaQa2lYZqy3U7hdYLmK3spIxIL4aM3N"
      
  # Grab metadata
    metadata_pol_lab <- list(caption = "Source: HIV Policy Lab [2024-07-18]")
  
  # REF ID for plots
    ref_id <- "1b6b1dd7"
    
  # Functions  
    

# LOAD DATA ============================================================================  

  #Read in HIV Policy Lab data export
      #df <- googlesheets4::read_sheet(ss = "1zVaQa2lYZqy3U7hdYLmK3spIxIL4aM3N")
    #df_tens <- googlesheets4::range_speedread(as_sheets_id(pol_lab_id), 
                                              #"Policy adoption data",
                                              #skip = 6, col_types = "c") %>% 
      #janitor::clean_names()
    
    df_tens <- read_excel("Data/HIV Policy Lab - Data Export 2024-10-31.xlsx",
                             sheet = "Policy adoption data", 
                             skip = 6, col_types = "text") %>% 
      janitor::clean_names()
    
    View(df_tens)

# MUNGE ============================================================================
  
  #Prep data 
    prep_10s_barriers <- function(df, cntry) {
      
      if(cntry %ni% unique(df$country))
        return(NULL)
      
      #select just the policy structural indicators
      ind_sel <- c(paste0("S", 1:6), "S9")
      
      #limit to just relevant indicators & rename
      df_struct <- df %>% 
        dplyr::rename(indicator = indicator_subindicator_name) %>% 
        dplyr::filter(indicator %in% ind_sel) %>% 
        dplyr::mutate(indicator_name = dplyr::recode(indicator,
                                                     "S1" = "Same-sex sex non-criminalization",
                                                     "S2" = "Sex work non-criminalization",
                                                     "S3" = "Drug use non-criminalization",
                                                     "S4" = "HIV exposure non-criminalization",
                                                     "S5" = "Non-discrimination protections",
                                                     "S6" = "National human rights institutions",
                                                     "S9" = "Gender based violence"))
      
      #rename countries and limit to just PEPFAR
      df_struct <- df_struct %>% 
        dplyr::mutate(country = ifelse(stringr::str_detect(country, "Ivoire"), "Cote d'Ivoire", country),
                      country = dplyr::recode(country,
                                              "Myanmar" = "Burma",
                                              "Lao People's Democratic Republic" = "Laos",
                                              "Tanzania (United Republic of)" = "Tanzania",
                                              "Viet Nam" = "Vietnam"
                      )) %>% 
        dplyr::filter(country %in% glamr::pepfar_country_list$country)
      
      #aggregate adoption across PEPFAR countries
      df_viz <- df_struct %>% 
        dplyr::filter(year == "Most recent",
                      country == cntry) %>% 
        dplyr::count(country, adoption_level, indicator_name) %>% 
        dplyr::filter(!is.na(adoption_level))
      
      # add viz components
      df_viz <- df_viz %>% 
        dplyr::mutate(adoption_level = factor(adoption_level, c("Adopted", "Partial", "Not adopted")),
                      indicator_order = dplyr::case_when(adoption_level == "Not adopted" ~ 3,
                                                         adoption_level == "Partial" ~ 2, 
                                                         TRUE ~ 1),
                      fill_color = dplyr::case_when(adoption_level == "Not adopted" ~ "#F8A27E",
                                                    adoption_level == "Partial" ~ "#FBDC99",
                                                    adoption_level == "Adopted" ~ "#5BB5D5")) 
      
      return(df_viz)
      
    }
    
      
      df_sa <- prep_10s_barriers(df_tens, "South Africa")
      df_bw <- prep_10s_barriers(df_tens, "Botswana")
    
  
# VIZ ============================================================================

  #Bar chart 
    viz_10s_barriers <- function(df) {
      
      q <- glue::glue("What gaps exists in adopting structural laws/policies towards UNAIDS' 10-10-10 goals?") %>% toupper
      
      if(is.null(df) || nrow(df) == 0)
        return(dummy_plot(q))
      
      ref_id <- "1b6b1dd7" #update plot identification
      
      df %>% 
        ggplot2::ggplot(ggplot2::aes(n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
        ggplot2::geom_col(ggplot2::aes(fill = fill_color)) +
        ggplot2::geom_vline(xintercept = 0) +
        ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
        ggplot2::scale_fill_identity() +
        ggplot2::scale_x_continuous(position = "top") +
        ggplot2::labs(x = NULL, y = NULL,
                      title = {q},
                      subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
                      caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
        glitr::si_style_nolines() +
        ggplot2::theme(strip.placement = "outside",
                       axis.text.x = ggplot2::element_blank())
      
    }
      
        viz_10s_barriers(df_sa)
        viz_10s_barriers(df_bw)
      
# Icon Version  ------------------------------------------------------------------
        #use shapes instead of bars
        
        plot_viz_10s <- function(df) {
          
          q <- glue::glue("What gaps exists in adopting structural laws/policies towards UNAIDS' 10-10-10 goals?") %>% toupper
          
          if(is.null(df) || nrow(df) == 0)
            return(dummy_plot(q))
          
          ref_id <- "1b6b1dd7" #update plot identification
          
          df %>% 
            ggplot2::ggplot(ggplot2::aes(x = n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
            ggplot2::geom_point(aes(color = fill_color), shape = 19, size = 10) + 
            ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
            ggplot2::scale_color_identity() +
            ggplot2::scale_x_continuous(position = "top") +
            ggplot2::labs(x = NULL, y = NULL,
                          title = {q},
                          subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
                          caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
            glitr::si_style_nolines() +
            ggplot2::theme(strip.placement = "outside",
                           strip.text.x = ggplot2::element_text(hjust = .5), 
                           axis.text.x = ggplot2::element_blank())
          
        }
        
        plot_viz_10s(df_sa)
        plot_viz_10s(df_bw)
     
        

# SPINDOWN ============================================================================

