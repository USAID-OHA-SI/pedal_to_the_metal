# PROJECT: COP/ROP Sprint
# PURPOSE: Munge and Analysis of Policy Lab Data
# AUTHOR:  Lemlem Baraki | SI
# REF ID:  1b6b1dd7
# LICENSE: MIT
# DATE:   2024-11-13
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
load_secrets()

pol_lab_id <- "1cm4sLwtAC52U82dceu-Nuts08gJoV_QOy_V-kxEGSdg"

# Grab metadata
metadata_pol_lab <- list(caption = "Source: HIV Policy Lab [2024-07-18]")

# REF ID for plots
ref_id <- "1b6b1dd7"

# Functions  
cop_ous <- glamr::pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Region", negate = T)) %>% 
  pull(operatingunit)

# LOAD DATA ============================================================================  

#Read in HIV Policy Lab data export
df_tens <- googlesheets4::range_speedread(as_sheets_id(pol_lab_id), 
                                          "Policy adoption data",
                                          skip = 6, col_types = c(.default = "c")) %>% 
  janitor::clean_names()

#df_tens <- read_excel("Data/HIV Policy Lab - Data Export 2024-10-31.xlsx",
#                        sheet = "Policy adoption data", 
#                       skip = 6, col_types = "text") %>% 
#janitor::clean_names()

#View(df_tens)

# MUNGE ============================================================================

#Prep data 
prep_10s_barriers <- function(df) {
  
  #if(cntry %ni% unique(df$country))
  # return(NULL)
  
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
                                          "Viet Nam" = "Vietnam"
    )) %>% 
    dplyr::filter(country %in% glamr::pepfar_country_list$country)
  
  #aggregate adoption across PEPFAR countries
  df_viz <- df_struct %>% 
    dplyr::filter(year == "Most recent",
                  #country == cntry
    ) %>% 
    dplyr::count(country, adoption_level, indicator_name) #%>% 
  #dplyr::filter(!is.na(adoption_level))
  
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


df_viz <- prep_10s_barriers(df_tens)


# VIZ ============================================================================

#Bar chart 
viz_10s_barriers <- function(df, cntry) {
  
  q <- glue::glue("The largest gaps in the 10-10-10 goals"
    #What gaps exists in adopting structural laws/policies towards UNAIDS' 10-10-10 goals?"
    ) %>% toupper
  
  if(is.null(df) || nrow(df) == 0)
    return(dummy_plot(q))
  
  #Filter the dataframe by the specified country
  df <- df %>% dplyr::filter(country == cntry)
  
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
                  subtitle = glue::glue("<span style = 'font-weight: bold;color:#5BB5D5'>Adopted</span> |
                                 <span style = 'font-weight: bold;color:#FBDC99'>Partially </span> |
                                                <span style = 'font-weight: bold; color:#F8A27E'>Not Adopted </span>"),
                  #subtitle = glue::glue("{unique(df$country)}'s progress towards adopting structural laws/policies towards UNAIDS' 10-10-10 goals"),
                  caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
    glitr::si_style_nolines() +
    ggplot2::theme(strip.placement = "outside",
                   axis.text.x = ggplot2::element_blank(),
                   plot.subtitle = element_markdown(),
                   plot.margin = margin(0,0,0,0, unit = "pt"))
  
}

viz_10s_barriers(df_viz, "South Africa")

# Icon Version  ------------------------------------------------------------------
#use shapes instead of bars
#plot_viz_10s <- function(df, cntry) {
  
  #if(is.null(df) || nrow(df) == 0)
   # return(dummy_plot(q))
  
  #Filter the dataframe by the specified country
  #df <- df %>% dplyr::filter(country == cntry)
  
  #q <- glue::glue("THE LARGEST GAPS IN THE 10-10-10 GOALS IN {df$country}") %>% toupper
  
  #ref_id <- "1b6b1dd7" #update plot identification
  
  #df %>% 
   # ggplot2::ggplot(ggplot2::aes(x = n, forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE))) +
    #ggplot2::geom_point(aes(color = fill_color), shape = 19, size = 16) + 
    #ggplot2::scale_color_identity() +
    #ggplot2::scale_x_continuous(position = "top") +
    #ggplot2::labs(x = "Status", y = NULL,
                #  title = {q},
                 # subtitle = glue::glue("<span style = 'font-weight: bold; color:#F8A27E'>Not Adopted </span> |
                  #               <span style = 'font-weight: bold;color:#FBDC99'>Partially </span> |
                   #                             <span style = 'font-weight: bold;color:#5BB5D5'>Adopted</span>"),
                  # caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
    #glitr::si_style_nolines() +
    #ggplot2::theme(strip.placement = "outside",
     #              axis.text.x = ggplot2::element_blank(),
      #             plot.subtitle = element_markdown(),
       #            plot.margin = margin(0,0,0,0, unit = "pt"))
  
#}


#plot_viz_10s(df_viz, "South Africa")


#plot_viz_10s <- function(df, cntry) {
  
 # if(is.null(df) || nrow(df) == 0)
  #  return(dummy_plot(q))
  
  #Filter the dataframe by the specified country
  #df <- df %>% dplyr::filter(country == cntry)
  
  #q <- glue::glue("THE LARGEST GAPS IN THE 10-10-10 GOALS IN {df$country}") %>% toupper
  
  #ref_id <- "1b6b1dd7" #update plot identification
  
  #Set all points to a fixed position on the x-axis
  #df$adoption_level <- "Adopted" 
  
  #df %>% 
   # ggplot2::ggplot(ggplot2::aes(x = adoption_level, y = n, color = fill_color)) +
    #ggplot2::geom_point(shape = 19, size = 16) + 
    #ggplot2::facet_wrap(~forcats::fct_reorder(indicator_name, indicator_order, na.rm = TRUE),
     #                   nrow = 1,
      #                  labeller = label_wrap_gen(width = 20)) +
    #ggplot2::scale_color_identity() +
    #ggplot2::labs(x = NULL, y = NULL,
     #             title = {q},
      #            subtitle = glue::glue("<span style = 'font-weight: bold;color:#5BB5D5'>Adopted</span> |
       #                          <span style = 'font-weight: bold;color:#FBDC99'>Partially </span> |
        #                                        <span style = 'font-weight: bold; color:#F8A27E'>Not Adopted </span>"),
                  #caption = glue::glue("{metadata_pol_lab$caption} |  Ref id: {ref_id}")) +
    #glitr::si_style_nolines() +
    #ggplot2::theme(strip.placement = "outside",
                #strip.text = element_text(hjust = 0.5),
                 #  axis.text = element_blank(),
                  # plot.subtitle = element_markdown(),
                  # plot.margin = margin(0,0,0,0, unit = "pt")
                   #)
  
#}

#plot_viz_10s(df_viz, "Botswana")

# SPINDOWN ============================================================================