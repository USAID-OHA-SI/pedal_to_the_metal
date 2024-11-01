# PROJECT: COP/ROP Sprint
# PURPOSE: Munge and Analysis of UNAIDS Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  06c95022
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
      
  # Grab metadata
   get_metadata(file_path)
  
  # REF ID for plots
    ref_id <- "06c95022"
    
  # Functions  
  

# LOAD DATA ============================================================================  

  #install UNAIDS package from dev_edms branch
    remotes::install_github("USAID-OHA-SI/mindthegap", ref = "dev_edms")
    
    df_tt <- mindthegap::load_unaids(pepfar_only = T) %>% 
      dplyr::filter(indicator_type == "Percent")
    
    View(df_tt)

# MUNGE ============================================================================
  
  #Key Epi Gaps
    #Limit Test and Treat data
      #note: percent indicators - sex disagg only for adults (15+), not children (<15)
    df_rel_lim <- 
      df_tt %>% 
      filter(year == 2023,
             indicator %in% c("Percent Known Status of PLHIV", #using rel base indicators
                              "Percent on ART with Known Status",
                              "Percent VLS on ART"), 
             sex != "All") %>% 
      select(year, country, indicator, estimate, lower_bound, upper_bound, 
             age, sex)
  
# VIZ ============================================================================

  #Lollipop Plot 
      #3 stacked plots of Known Status, On ART, VLS
      #broken out by age (0-14, 15+) and sex (male, female)
    
    df_viz <- df_rel_lim %>% 
      filter(country == "South Africa") %>% 
      mutate(share = estimate/100,
             share_low = lower_bound/100, share_up = upper_bound/100,
             flag = share <= 0.95) %>% 
      arrange(desc(share))
    
    #Version 1- Male/Female on the same x-axis
    df_viz %>% 
      mutate(population = if_else(sex == "Female", -share, share),
             indicator = factor(indicator, levels = c("Percent VLS on ART",
                                                      "Percent on ART with Known Status",
                                                      "Percent Known Status of PLHIV")),
             sex_color = ifelse(sex == "Male", glitr::hunter, glitr::orchid_bloom)) %>% 
      ggplot(aes(x = share, y = indicator)) + 
      geom_segment(aes(x = share_low, xend = share_up, yend = indicator, color = glitr::whisper), size = 2) + #lollipop line
      geom_point(aes(x = share, color = sex_color),size = 3) + #lollipop points
      geom_vline(xintercept = 0.95, linetype = "dashed", color = slate)+
      scale_y_discrete(label = c("VLS",
                                 "On ART",
                                 "Known Status")) + 
      scale_x_continuous(labels = percent, limits = c(.5,1)) + 
      scale_color_identity()+ 
      si_style_xgrid()+
      theme(plot.subtitle = element_markdown()) + 
      labs(x = NULL, y = NULL,
           title = "KEY EPI GAPS",
           caption = glue("{mindthegap::source_note}"))
    
    #Version 2 - split axis by Male vs Female 
    df_viz %>% 
      mutate(population = if_else(sex == "Female", -share, share),
             indicator = factor(indicator, levels = c("Percent VLS on ART",
                                                      "Percent on ART with Known Status",
                                                      "Percent Known Status of PLHIV")),
             sex_color = ifelse(sex == "Male", glitr::hunter, glitr::orchid_bloom)) %>% 
      ggplot(aes(x = population, y = indicator)) + 
      geom_segment(data = df_viz %>% filter(sex == "Female"), 
                   aes(x = -share_up, xend = -share_low, 
                       yend = indicator, color = glitr::whisper), size = 4) + # female segments
      geom_segment(data = df_viz %>% filter(sex == "Male"), 
                   aes(x = share_low, xend = share_up, 
                       yend = indicator, color = glitr::whisper), size = 4) + # male segments
      #geom_segment(aes(x = -share_up, xend = -share_low, 
       #                yend = indicator, color = glitr::whisper), size = 2) + #lollipop line
      geom_point(aes(x = population, color = sex_color),size = 4) + #lollipop points
      geom_vline(xintercept = 0, linetype = "dashed", color = slate)+ #vertical line at 0
      scale_y_discrete(label = c("VLS",
                                 "On ART",
                                 "Known Status")) + 
      scale_x_continuous(labels = percent,
                         limits = c(-1,1)
                         #breaks = seq(-1,1,0.75)
                         ) + 
      scale_color_identity()+ 
      si_style_xgrid()+
      theme(plot.subtitle = element_markdown()) + 
      labs(x = NULL, y = NULL,
           title = "KEY EPI GAPS",
           subtitle = glue("<span style= 'color:{hunter};'>Male</span> and
                           <span style= 'color:{orchid_bloom};'>Female</span>
                           Adults (15+) progress towards 95-95-95s"),
           caption = glue("{mindthegap::source_note} | Ref ID: {ref_id}"))
    
      

# SPINDOWN ============================================================================

