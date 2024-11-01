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
      #list of countries to loop over
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
  names(test_df)
  str(test_df)
  glimpse(test_df)
  view(test_df)
  
    unique(test_df$indicator) #contains PLHIV indicator 
    unique(test_df$indicatortype) #contains National, Sub-national, Not applicable
    unique(test_df$standardizeddisaggregate) #need Total Numerator
    
  #Subset subnat data
    plhiv_df <- df_subnat %>% 
      filter(indicator %in% "PLHIV", 
             standardizeddisaggregate == "Total Numerator") %>% 
      group_by(fiscal_year, operatingunit,snu1, snu1uid, psnuuid, indicator) %>% 
      summarise(across(c(targets), 
                       \(x) sum(x, na.rm = TRUE)),
                .groups = "drop") %>% 
      mutate(fy = as.character(fiscal_year))
  
  #Join the sf df to the plhiv df
    names(psnu_sf)
    
    uid_df <- psnu_sf %>% 
      rename(psnuuid = uid)
      #rename(snu1uid = uid)
    
    df_psnu_plhiv_geo <- uid_df %>% 
      left_join(., plhiv_df) %>%
      #filter(operatingunit == "South Africa") %>% 
      arrange(desc(targets))
    
    
   
  
# VIZ ============================================================================

  #PLHIV Targets Map
    #df_psnu_plhiv_geo %>% 
     # ggplot() + 
      #geom_sf(aes(fill = targets )) + 
      #labs(title = "PLHIV Targets")
    
    
    #2023 PLHIV targets
    df_psnu_plhiv_geo %>%
      filter(operatingunit == "South Africa",
             fiscal_year == "2023") %>% #range 3k-750k
      ggplot() + 
      geom_sf(aes(fill = targets), color = grey90k, size = .1) +
      scale_fill_si(palette = "hunter_c", discrete = F) + 
      labs(title = "PSNU MAP OF PLHIV TARGETS") + 
      si_style_map() + 
      theme(legend.text = element_text(size = 7))
    

# SPINDOWN ============================================================================

