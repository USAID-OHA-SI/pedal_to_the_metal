# PROJECT:  pedal_to_the_metal
# PURPOSE:  Programmatic Viz
# AUTHOR:   A. Towey | USAID
# REF ID:   f1a0e12f 
# LICENSE:  MIT
# DATE:     2024-10-31
# UPDATED: 

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

path_msd <- si_path()

meta <- get_metadata(path_msd)

cntry <- "Zambia"

# IMPORT ------------------------------------------------------------------

df <- read_psd(path_msd)

# Pull the list of OUs for which visuals need to be generated
cop_ous <- glamr::pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Region", negate = T)) %>% 
  pull(operatingunit)

df_filtered <- df %>%
  filter(country %in% cop_ous)


# MUNGE -------------------------------------------------------------------

process_achievement_data <- function(df, cntry, meta) {
  df %>%
    filter(country == cntry,
           indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D", 
                            "PMTCT_EID", "OVC_SERV_UNDER_18", "AGYW_PREV",
                            "PrEP_NEW", "TX_NET_NEW", "TX_ML_IIT_less_three_mo",
                            "TX_ML_IIT_six_more_mo", "TX_ML_IIT_three_five_mo")) %>%
    mutate(type = case_when(
      sex == "Female" & ageasentered %in% c("10-14","15-19", "20-24")  ~ "AGYW",
      ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Peds",
      sex == "Male" & ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "Males (15+)",
      TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")
    )) %>%
    group_by(fiscal_year, country, psnu, funding_agency, indicator, type) %>%
    summarise(across(c(targets, cumulative), ~ sum(.x, na.rm = TRUE)),
              .groups = "drop") %>%
    #Filter out rows where specific indicators have targets == 0
    filter(!(indicator %in% c("AGYW_PREV", "OVC_SERV_UNDER_18", "PMTCT_EID", "PrEP_NEW") & targets == 0)) %>%
    gophr::clean_agency() %>%
    gophr::adorn_achievement(qtr = meta$curr_qtr) %>%
    filter(!is.na(type))
}

process_viral_load_data <- function(df, cntry, meta) {
  df %>%
    filter(indicator %in% c("TX_CURR_Lag2", "TX_PVLS", "TX_CURR","TX_ML_IIT_less_three_mo",
                            "TX_ML_IIT_six_more_mo", "TX_ML_IIT_three_five_mo"),
           fiscal_year == meta$curr_fy,
           country == cntry) %>%
    clean_agency() %>%
    mutate(type = case_when(
      sex == "Female" & ageasentered %in% c("10-14","15-19", "20-24")  ~ "AGYW",
      ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Peds",
      sex == "Male" & ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "Males (15+)",
      TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")
    )) %>%
    gophr::clean_indicator() %>%
    group_by(fiscal_year, country, psnu, funding_agency, indicator, type) %>%
    summarise(cumulative = sum(cumulative, na.rm = TRUE),
              .groups = "drop") %>%
    pivot_wider(names_from = indicator, # Pivot wide to perform calculations
                values_from = cumulative,
                names_glue = "{tolower(indicator)}") %>%
    
    # Add all three IIT categories together and divide by TX CURR
    # Calculate VLS and VLC
    mutate(iit = (tx_ml_iit_less_three_mo + tx_ml_iit_six_more_mo + tx_ml_iit_three_five_mo)/tx_curr,
           vlc = tx_pvls_d / tx_curr_lag2,
           vls = tx_pvls / tx_pvls_d) %>%
    filter(!is.na(type)) %>%
    pivot_longer(cols = c(vlc, vls, tx_pvls_d, iit),
                 names_to = "indicator",
                 values_to = "value",
                 values_drop_na = TRUE) %>%
    mutate(
      cumulative = case_when(
        indicator == "iit" ~ tx_curr,
        indicator %in% c("vls", "vlc") ~ tx_pvls,
        indicator == "tx_pvls_d" ~ value,
        TRUE ~ NA_real_
      ),
      achievement = if_else(indicator %in% c("vlc", "vls", "iit"), value, NA_real_)
    ) %>%
    # mutate(
    #   achv_color = case_when(
    #     indicator == "vls" & achievement > 0.95 ~ "#697EBC",
    #     indicator == "vls" & achievement <= 0.95 ~ "#F8A27E",
    #     indicator == "vlc" & achievement > 0.90 ~ "#697EBC",
    #     indicator == "vlc" & achievement <= 0.90 ~ "#F8A27E",
    #     TRUE ~ NA_character_
    #   )
    # ) %>%
    select(fiscal_year, country, psnu, funding_agency, indicator, type, cumulative, achievement)
}

combine_data <- function(df_achv, df_vl_vs) {
  bind_rows(df_achv, df_vl_vs)
}

# VIZ ---------------------------------------------------------------------

custom_jitter <- function(df, jitter_factor = 0.05) {
  df %>%
    group_by(indicator, type) %>%  # Group by indicator to normalize each indicator separately
    filter(funding_agency %in% c("USAID", "CDC", "DEFAULT")) %>%
    mutate(
      # Jitter for visualization
      y_jitter = case_when(
        funding_agency == "USAID" ~ runif(n(), .01, jitter_factor), # Could adjust these values as needed
        funding_agency == "CDC" ~ runif(n(), -jitter_factor, -.01),
        funding_agency == "DEFAULT" ~ runif(n(), -jitter_factor/2, jitter_factor/2),
        TRUE ~ 0
      )
    ) %>%
    ungroup()  # Remove grouping after mutation
}

# Function to process and visualize achievement data for a specific country
process_achievement_viz <- function(df_combined, cntry, jitter_factor) {
  df_achv_viz <- df_combined %>%
    filter(country == cntry) %>%
    filter(indicator %in% c("vlc", "vls", "PrEP_NEW", "OVC_SERV_UNDER_18", "AGYW_PREV", "TX_NET_NEW", "PMTCT_EID", "iit")) %>%
    filter(!(indicator %in% c('OVC_SERV_UNDER_18', "AGYW_PREV") & type != "Total"))%>% # Only report OVC and AGYW as Total
    mutate(type = factor(type, levels = c("Total", "KeyPop", "Peds", "AGYW", "Males (15+)")),
           indicator = factor(indicator, levels = c("vlc", "vls", "PrEP_NEW", "TX_NET_NEW", "iit", "PMTCT_EID", "OVC_SERV_UNDER_18", "AGYW_PREV"))) %>%
    custom_jitter(jitter_factor) %>%  # Apply the custom jitter function
    
    
    return(df_achv_viz)
}

generate_plot <- function(df_achv_viz, meta, cntry) {
  # Baseline point for line with ticks
  baseline_pt_1 <- 0
  baseline_pt_2 <- .25
  baseline_pt_3 <- .5
  baseline_pt_4 <- .75
  baseline_pt_5 <- 1
  
  df_achv_viz %>%
    # Adjust df for plot
    mutate(
      funding_agency = recode(funding_agency, "DEFAULT" = "PEPFAR"), # Display DEFAULT (i.e. No Agency) as PEPFAR
      funding_agency = factor(funding_agency, levels = c("USAID", "CDC", "PEPFAR")),
      indicator = recode(indicator, "vls" = "VLS", "vlc" = "VLC", "iit" = "IIT", "OVC_SERV_UNDER_18" = "OVC_SERV (<18)") # For display purposes
    ) %>%
    # Plot setup
    ggplot(aes(achievement, y_jitter, color = funding_agency)) +
    geom_point(na.rm = TRUE, alpha = .4, size = 2, shape = 16) + # Adjust size as needed
    scale_color_manual(values = c("USAID" = midnight_blue, "CDC" = viking, "PEPFAR" = slate),
                       name = "Funding Agency",
                       labels = c("USAID", "CDC", "PEPFAR")) +
    facet_grid(rows = vars(indicator), cols = vars(type), scales = "free_y", switch = "y") +
    theme(strip.text = element_markdown()) +
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),
      labels = c("0%", "50%", "100%"),
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
    theme(strip.text = element_markdown(hjust = .5),
          strip.text.y.left = element_text(angle = 0, vjust = 0.5, hjust = 1),
          axis.text.x = element_blank(),
          axis.text.y = element_blank()) +
    coord_cartesian(clip = "off") +
    labs(x = NULL, y = NULL,
         # Change this as needed for the PDF
         caption = glue("Target achievement by PSNU (capped at 110%).")) +
    theme(axis.text.x = element_text(),
          axis.text.y = element_blank(),
          strip.text = element_markdown(),
          panel.spacing.y = unit(2, "lines")) + # Adjust for spacing
    guides(size = "none", shape = guide_legend())
}

# RUN FUNCTION ---------------------------------------------------------------

run_achievement_analysis <- function(df, cntry, meta, jitter_factor) {
  # Get achievement data
  df_achv <- process_achievement_data(df, cntry, meta)
  # Get mutated VLS/VLC data
  df_vl_vs <- process_viral_load_data(df, cntry, meta)
  # Combine them
  df_combined <- combine_data(df_achv, df_vl_vs)
  # Set data up to be visualized
  df_achv_viz <- process_achievement_viz(df_combined, cntry, jitter_factor)
  # Plot
  plot <- generate_plot(df_achv_viz, meta, cntry)
  return(plot)
}


# Example usage:
plot <- run_achievement_analysis(df_filtered, cntry, meta, .05)
print(plot)


################## TO DO #################################################
# Fix any labeling (i.e. OVC_SERV_UNDER_18)
# Formatting to explain some are true percentages and some are target achievement
# What to do about TX_NET_NEW?
# Cut Top 80%?
# Adjust x scales?
# What to do about jitter for AGYW? Currently its randomly distributed.


