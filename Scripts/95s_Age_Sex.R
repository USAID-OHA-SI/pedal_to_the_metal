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
  #remotes::install_github("USAID-OHA-SI/mindthegap", ref = "dev_edms") #install UNAIDS package from dev_edms branch
  library(mindthegap)
    
  
  # REF ID for plots
    ref_id <- "06c95022"
    
  # Functions  
    #list of countries  
    cop_ous <- glamr::pepfar_country_list %>% 
      filter(str_detect(operatingunit, "Region", negate = T)) %>% 
      pull(operatingunit)

# LOAD DATA ============================================================================  

    df_tt <- mindthegap::load_unaids(pepfar_only = T) %>% 
      dplyr::filter(indicator_type == "Percent", 
                    year == 2023)
    

# MUNGE ============================================================================
  
    prep_tt_tbl <- function(df) {
      
      #Limit the data
    df_rel_lim <- df %>% 
      filter(#country == cntry,
             indicator %in% c("Percent Known Status of PLHIV", 
                              "Percent on ART with Known Status",
                              "Percent VLS on ART"), 
             !(sex == "All" & age == "All"),
             ! (sex == "All" & age == "15+")) %>% 
      select(country, indicator,age, sex, estimate) %>% 
      
      mutate(share = estimate/100,
             flag = share >= 0.95) 
    
    df_viz <- df_rel_lim%>% 
      mutate(stroke_color = ifelse(flag == FALSE, glitr::hw_orchid_bloom, glitr::hw_hunter),
            sex = str_sub(sex, end = 1),
             age_sex = glue("{age} {sex}"),
             age_sex = str_remove(age_sex, " A"),
             indic_age_sex = glue("{indicator} {age_sex}"),
             indic_age_sex = fct_relevel(indic_age_sex,
                                         c("Percent VLS on ART 15+ F", "Percent VLS on ART 15+ M",
                                           "Percent VLS on ART 0-14",
                                           "Percent on ART with Known Status 15+ F", "Percent on ART with Known Status 15+ M",
                                           "Percent on ART with Known Status 0-14",
                                           "Percent Known Status of PLHIV 15+ F", "Percent Known Status of PLHIV 15+ M", 
                                           "Percent Known Status of PLHIV 0-14"))) %>% 
      mutate(indicator = case_when(str_detect(indicator, "PLHIV") ~ "Known Status",
                                   str_detect(indicator, "on ART with Known Status") ~ "On ART",
                                   str_detect(indicator, "VLS") ~ "VLS", 
                                   TRUE ~ "")) 
    
    return(df_viz)
    }
    
    df_viz <- prep_tt_tbl(df_tt)
    
    df_sa <- prep_tt_tbl(df_tt) %>% 
      filter(country == "South Africa")
    
    #purrr::map(cop_ous, ~prep_tt_tbl(df_tt, .))
    
  
# VIZ ============================================================================

  #Lollipop Plot 
      #3 stacked plots of Known Status, On ART, VLS
      #broken out by age (0-14, 15+) and sex (male, female)
      #note: sex disagg only for adults (15+), not children (<15)
    
    plot_epi_gaps <- function(df, cntry) {
      
      df <- df_viz %>% 
        filter(country == cntry)
      
      ggplot(df,
             aes(x = share, y = indic_age_sex, color = stroke_color)) + 
      geom_vline(xintercept = 0.95, linetype = "dashed", color = slate) +
      geom_segment(aes( x = .95, xend = share, yend = indic_age_sex),
                   linewidth = 1, alpha = .6) + 
      geom_point(size = 3.5) + 
      scale_x_continuous(labels = percent, expand = c(0.05, .05)) + 
      scale_y_discrete(labels = setNames(df$age_sex, df$indic_age_sex)) + 
      # geom_text(data = df %>% group_by(indicator) %>% 
      #             slice(1),
      #           aes(label=indicator, x = .98, color = "black"),
      #           family = "Source Sans Pro", size = 4, vjust = -.6) + 
      facet_grid(indicator ~ .,  scales = "free_y", switch = "y")+
      scale_color_identity() + 
      si_style_xgrid() +
      theme(strip.text = element_text(hjust = .5, size = 20/.pt),
            strip.placement = "outside", 
            panel.spacing = unit(.25, "line"),
            plot.title = element_text(hjust = 0.1)) +
      labs(x = NULL, y = NULL,
           title = glue("KEY EPIDEMIOLOGICAL GAPS")
      )
           # caption = glue("{mindthegap::source_note} | Ref ID: {ref_id}"))
    }
      

    plot_epi_gaps(df_viz, "South Africa")
    si_preview(scale = 0.5)

# SPINDOWN ============================================================================

    