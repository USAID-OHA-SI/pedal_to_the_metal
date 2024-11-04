# PROJECT: COP/ROP Sprint 
# PURPOSE: Munge and Analysis of PLHIV Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  fde3ca9c
# LICENSE: MIT
# DATE:   2024-10-31
# NOTES:   

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
    
    
  # SI specific paths/functions  
    load_secrets()
    merdata <- file.path(glamr::si_path("path_msd"))
    
    subnat_filepath <- si_path() %>% return_latest("NAT_SUBNAT")
      
  # Grab metadata
   meta <- get_metadata(subnat_filepath)
  
  # REF ID for plots
    ref_id <- "fde3ca9c"
    
  # Functions
    #Loop over list of countries  
    cop_ous <- glamr::pepfar_country_list %>% 
      filter(str_detect(operatingunit, "Region", negate = T)) %>% 
      pull(operatingunit)

# LOAD DATA ============================================================================  
  df_subnat <- read_psd(subnat_filepath)
    
  #test_df <- df_subnat %>% 
   # filter(fiscal_year == 2023, 
    #       operatingunit == "South Africa")
    
  psnu_sf <- st_read("GIS/VcPepfarPolygons.shp")

# MUNGE ============================================================================
  
  #Investigate
    #names(test_df)
    #str(test_df)
    #glimpse(test_df)
    #view(test_df)
  
    #unique(test_df$indicator) #contains PLHIV indicator 
    #unique(test_df$indicatortype) #contains National, Sub-national, Not applicable
    #nique(test_df$standardizeddisaggregate) #need Total Numerator
    
  #Subset subnat data
    #plhiv_df <- df_subnat %>% 
     # filter(indicator %in% "PLHIV", 
      #       standardizeddisaggregate == "Total Numerator") %>% 
      #group_by(fiscal_year, operatingunit,snu1, snu1uid,psnu, psnuuid, indicator) %>% 
      #summarise(across(c(targets), 
        #               \(x) sum(x, na.rm = TRUE)),
       #         .groups = "drop") %>% 
    #  mutate(fy = as.character(fiscal_year))
  
  #Join the sf df to the plhiv df
    #names(psnu_sf)
    
    #uid_df <- psnu_sf %>% 
     # rename(psnuuid = uid)
      #rename(snu1uid = uid)
    
    #df_psnu_plhiv_geo <- uid_df %>% 
     # left_join(., plhiv_df) %>%
      #filter(operatingunit == "South Africa") %>% 
      #arrange(desc(targets))
    
    
    #Function to prep PLHIV data
    prep_plhiv_tbl <- function(df1, df2) {
      # Subset and summarize the subnational data
      plhiv_df <- df1 %>% 
        filter(indicator == "PLHIV", 
               standardizeddisaggregate == "Total Numerator") %>% 
        group_by(fiscal_year, operatingunit, snu1, snu1uid, psnu, psnuuid, indicator) %>% 
        summarise(across(c(targets), 
                         \(x) sum(x, na.rm = TRUE)),
                  .groups = "drop") %>% 
        mutate(fy = as.character(fiscal_year))
      
      # Prepare the PSNU shapefile data
      uid_df <- df2 %>% 
        rename(psnuuid = uid)
      
      # Join the spatial data with the summarized PLHIV data
      df_psnu_plhiv_geo <- uid_df %>% 
        left_join(plhiv_df, by = "psnuuid") %>% 
        arrange(desc(targets))
      
      return(df_psnu_plhiv_geo)
    }
    
    prep_plhiv_tbl(df_subnat, psnu_sf)
   
  
# VIZ ============================================================================
    
    #2023 PLHIV targets
      #South Africa example
    df_psnu_plhiv_geo %>%
      filter(operatingunit == "South Africa",
             fiscal_year == "2023") %>% #range 3k-750k
      ggplot() + 
      geom_sf(aes(fill = targets), color = grey80k, size = .1) +
      scale_fill_si(palette = "hunter_c", discrete = F, reverse = T,
                    #labels = ~label_number(scales_cut = cut_short_scale())(abs(.))
                    labels = label_number(scale = 1, big.mark = ",")
                                          ) + 
      si_legend_fill() + 
      si_style_map() + 
      labs(title = "PSNU MAP OF PLHIV TARGETS", 
           caption = glue("{meta$caption}")) +
      theme(legend.text = element_text(size = 7)) 
    
    
    #Define the function to plot for a specific country
    plot_plhiv_map <- function(df, country) {
      df %>%
        filter(operatingunit == country, fiscal_year == "2023") %>%
        ggplot() + 
        geom_sf(aes(fill = targets), color = grey80k, size = .1) +
        scale_fill_si(
          palette = "hunter_c", discrete = FALSE, reverse = TRUE,
          labels = label_number(scale = 1, big.mark = ",")
        ) + 
        si_legend_fill() + 
        si_style_map() + 
        labs(title = str_to_upper(glue("psnu map of plhiv targets in {country}")), 
             caption = glue("{meta$caption}")) +
        theme(legend.text = element_text(size = 7))
    }
    
    plot_plhiv_map(df_psnu_plhiv_geo, "South Africa")
    
    

# SPINDOWN ============================================================================

