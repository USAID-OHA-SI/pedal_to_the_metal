# PROJECT: COP/ROP Sprint
# PURPOSE: Munge and Analysis of Policy Lab Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  1b6b1dd7
# LICENSE: MIT
# DATE:   2024-12-13
# NOTES: adapted from https://github.com/USAID-OHA-SI/hardapoart/blob/main/Scripts/16_10s_barriers.R

# LOCALS & SETUP ============================================================================

  # Libraries
    # library(gagglr)
    # library(tidyverse)
    # library(scales)
    # library(sf)
    # library(systemfonts)
    # library(tidytext)
    # library(patchwork)
    # library(ggtext)
    # library(glue)
    # library(gt)
    # library(gtExtras)
    # library(googlesheets4)
    # library(readxl)
    # library(fontawesome)
    # 
    
  # SI specific paths/functions  
  #   load_secrets()
  #   
  #   pol_lab_id <- "1cm4sLwtAC52U82dceu-Nuts08gJoV_QOy_V-kxEGSdg"
  #     
  # # Grab metadata
  #   metadata_pol_lab <- list(caption = "Source: HIV Policy Lab [2024-07-18]")
  # 
  # # REF ID for plots
  #   ref_id <- "1b6b1dd7"
    
  # Functions  
     #cop_ous <- glamr::pepfar_country_list %>% 
       #filter(country !="Ukraine") %>% 
     #filter(str_detect(operatingunit, "Region", negate = T)) %>% 
      # pull(country)

# LOAD DATA ============================================================================  

  #Read in HIV Policy Lab data export
    # df_tens <- googlesheets4::range_speedread(as_sheets_id(pol_lab_id), 
    #                                           "Policy adoption data",
    #                                           skip = 6, col_types = c(.default = "c")) %>% 
    #   janitor::clean_names()
    
  
  # Automates the loading of the 10s data
    load_tens <- function(){
      
      # Authentical with google server to use drive
      glamr::load_secrets()
      
      # Google ID to load
      pol_lab_id <- "1cm4sLwtAC52U82dceu-Nuts08gJoV_QOy_V-kxEGSdg"
      
      df_tens <- googlesheets4::range_speedread(as_sheets_id(pol_lab_id), 
                                                "Policy adoption data", 
                                                skip = 6, col_types = c(.default = "c")) %>% 
        janitor::clean_names()
      
       return(df_tens)
    }
    
     
    
    
    #df_tens <- read_excel("Data/HIV Policy Lab - Data Export 2024-10-31.xlsx",
     #                        sheet = "Policy adoption data", 
      #                       skip = 6, col_types = "text") %>% 
      #janitor::clean_names()
    
    #View(df_tens)

# MUNGE ============================================================================
  
  #Prep data 
    prep_10s_barriers <- function(df) {
      
      # if(cntry %ni% unique(df$country))
      #   return(NULL)
      
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
        dplyr::mutate(country = dplyr::recode(country,
                                              "Côte d'Ivoire" = "Cote d'Ivoire",
                                              "Tanzania (United Republic of)" = "Tanzania",
                                              "Viet Nam" = "Vietnam", 
                                              "Lao People's Democratic Republic" = "Laos", #add ROP country names 
                                              "Myanmar" = "Burma"
                      )) %>% 
        dplyr::filter(country %in% glamr::pepfar_country_list$country)
      
      #aggregate adoption across PEPFAR countries
      df_viz <- df_struct %>% 
        dplyr::filter(year == "Most recent") %>% 
        dplyr::count(country, adoption_level, indicator_name) #%>% 
        #dplyr::filter(!is.na(adoption_level))
      
      #identify missing countries 
      missing_countries <- setdiff(glamr::pepfar_country_list$country, unique(df_viz$country))
      
      #create rows for missing countries 
      missing_rows <- expand_grid(
        country = missing_countries,
        indicator_name = unique(df_struct$indicator_name)
        ) %>% 
        mutate(n = NA,
               #indicator_order = NA_real_, 
               fill_color = NA_character_)
      
      #add in missing combos 
      df_viz <- dplyr::bind_rows(df_viz, missing_rows) 
        
      # add viz components
      df_viz <- df_viz %>% 
        dplyr::mutate(adoption_level = factor(adoption_level, c("Adopted", "Partial", "Not adopted")),
                      indicator_order = dplyr::case_when(adoption_level == "Not adopted" ~ 3,
                                                         adoption_level == "Partial" ~ 2, 
                                                         #adoption_level == "Adopted" ~ 1,
                                                         TRUE ~ 1),
                      fill_color = dplyr::case_when(adoption_level == "Not adopted" ~ "#E571B0", 
                                                    adoption_level == "Partial" ~ "#FBDC99",
                                                    adoption_level == "Adopted" ~ "#5BB5D5")) 
      
      return(df_viz)
      
    }
    
      
      # df_tens_viz <- prep_10s_barriers(df_tens)
      # df_bw <- prep_10s_barriers(df_tens, "Botswana")
      
      
  
# VIZ ============================================================================

  #Bar chart 
    viz_10s_barriers <- function(df, cntry, export = T) {
      
      df <- df %>% 
        filter(country == cntry)
      
      # q <- glue::glue("What gaps exists in adopting structural laws/policies towards UNAIDS' 10-10-10 goals?") %>% toupper
      
      if(is.null(df) || nrow(df) == 0)
        return(dummy_plot(q))
      
      # ref_id <- "1b6b1dd7" #update plot identification
      # 
      # df %>% 
      #   ggplot2::ggplot(ggplot2::aes(n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
      #   ggplot2::geom_col(ggplot2::aes(fill = fill_color, x = 1)) +
      #   ggplot2::geom_vline(xintercept = 0) +
      #   ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
      #   ggplot2::scale_fill_identity() +
      #   ggplot2::scale_x_continuous(position = "top") +
      #   ggplot2::labs(x = NULL, y = NULL,
      #                 title = {q},
      #                 subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
      #                 caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
      #   glitr::si_style_nolines() +
      #   ggplot2::theme(strip.placement = "outside",
      #                  axis.text.x = ggplot2::element_blank())
      
      v <- df %>% 
      complete(adoption_level, indicator_name) %>% 
        mutate(adoption_level = fct_relevel(adoption_level, c("Not adopted", "Partial", "Adopted")),
               fill_color = ifelse(is.na(fill_color), grey10k, fill_color),
               indicator_order = fct_reorder(indicator_name, indicator_order, .na_rm = T)) %>% 
        filter(adoption_level != "NA") %>% 
        ggplot(aes(x = adoption_level, y = indicator_order)) +
        geom_tile(aes(fill = fill_color), color = "white") +
        scale_fill_identity() +
        si_style_nolines() +
        scale_x_discrete(position = "top", labels = function(x) str_wrap(x, width = 10)) +
        labs(x = NULL, y = NULL,
             subtitle = toupper("UNAIDS' 10-10-10 goals: adoption of laws/policies")) +
        coord_fixed(ratio = .66) 
      
      if(export)
        save_png(cntry, "kp", "policy", scale = 0.65)
      
      return(v)
      
    }
      
      #viz_10s_barriers(df_tens_viz, "Zambia")


          
      
# Icon Version  ------------------------------------------------------------------
        #use shapes instead of bars
        dotplot_viz_10s <- function(df, cntry, export = TRUE) {
          
          df <- df %>% 
            filter(country == cntry)
          
          if(is.null(df) || nrow(df) == 0 || all(is.na(df$adoption_level))) {
            v <- ggplot() + 
                geom_blank() + 
                theme_void() 
            } else {
          
          v <- df %>% 
            ggplot2::ggplot(ggplot2::aes(x = 0, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
            ggplot2::geom_point(aes(color = fill_color), size = 5) + 
            ggplot2::geom_point(size = 5, stroke = 0.5, shape = 1, color = grey90k)+
            #ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
            ggplot2::scale_color_identity() +
            ggplot2::scale_x_discrete(, position = "top") +
            ggplot2::scale_y_discrete(position = "right") +
            ggplot2::labs(x = "Status", y = NULL,
                          # title = "GAPS IN THE 10-10-10 GOALS",
                          subtitle = glue::glue("<span style = 'font-weight: bold; color:{orchid_bloom}'>Not Adopted </span> |
                                 <span style = 'font-weight: bold;color:#f6af15'>Partially </span> |
                                                <span style = 'font-weight: bold;color:#2e92b5'>Adopted</span>")) +
                          #subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
                          #caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
            glitr::si_style_nolines() +
            ggplot2::theme(axis.text.x = ggplot2::element_blank(),
                           plot.subtitle = element_markdown(),
                           plot.margin = ggplot2::margin(0, 0, 0, 0, unit = "pt")
                           )}
          
          if(export){
            save_png(cntry, "kp", "policy", width = 2.5, height = 2.1)}
          
          return(v)
          
        }
        
        #dotplot_viz_10s(df_tens_viz, "Zambia")
        
        
      
      
        

# SPINDOWN ============================================================================

        
        #plot_viz_10s <- function(df) {
        
        #q <- glue::glue("What gaps exists in adopting structural laws/policies towards UNAIDS' 10-10-10 goals?") %>% toupper
        
        #if(is.null(df) || nrow(df) == 0)
         # return(dummy_plot(q))
        
        #ref_id <- "1b6b1dd7" #update plot identification
        
        #df %>% 
         # ggplot2::ggplot(ggplot2::aes(x = n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
          #ggplot2::geom_point(aes(color = fill_color), shape = 19, size = 10) + 
          #ggplot2::facet_wrap(~forcats::fct_rev(adoption_level)) +
          #ggplot2::scale_color_identity() +
          #ggplot2::scale_x_continuous(position = "top") +
          #ggplot2::labs(x = NULL, y = NULL,
           #             title = {q},
            #            subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
             #           caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
          #glitr::si_style_nolines() +
          #ggplot2::theme(strip.placement = "outside",
           #              strip.text.x = ggplot2::element_text(hjust = .5), 
            #             axis.text.x = ggplot2::element_blank())
        
        #}
