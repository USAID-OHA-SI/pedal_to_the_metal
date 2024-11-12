# PROJECT:  pedal_to_the_metal
# PURPOSE:  Programmatic Viz #1 Percentage Dot Plots
# AUTHOR:   A. Towey | USAID
# REF ID:    
# LICENSE:  MIT
# DATE:     2024-11-04
# UPDATED: 

# DEPENDENCIES ------------------------------------------------------------

#general
library(tidyverse)
library(glue)
#oha
library(gagglr) 
library(gophr)
library(glitr)
#viz extensions
library(scales, warn.conflicts = FALSE)
library(systemfonts)
library(tidytext)
library(patchwork)
library(ggtext)

# GLOBAL VARIABLES --------------------------------------------------------

ref_id <- ""  #a reference to be places in viz captions 

path_msd <- si_path()

meta <- get_metadata(path_msd)

cntry <- "Tanzania"

# IMPORT ------------------------------------------------------------------

df <- read_psd(path_msd)


# MUNGE -------------------------------------------------------------------
prep_viral_load_kp_agyw <- function(df, cntry){
  
  # Filter to select indicators + country
  df_vl <- df %>%
    clean_indicator() %>%
    filter(indicator %in% c("TX_CURR", "TX_PVLS", "TX_PVLS_D"),
           country == cntry)

  
  # Define groups - Total, KP, AGYW, Peds, Men 15+
  df_vl <- df_vl %>% 
    mutate(type = case_when(
      sex == "Female" & ageasentered %in%  c("10-14","15-19", "20-24")  ~ "AGYW",
      ageasentered %in% c("<01", "01-04", "05-09", "10-14") ~ "Peds",
      sex == "Male" & ageasentered %in% c("15-19", "20-24", "25-29", "30-34", "35-39", "40-44", "45-49", "50+") ~ "Males (15+)",
      TRUE ~ str_extract(standardizeddisaggregate, "Total|KeyPop")
    ))
  
  print(table(df_vl$type))
  
  #aggregate & reshape long
  df_vl <- df_vl %>% 
    group_by(fiscal_year, funding_agency, country, psnu, indicator, type) %>% 
    summarise(across(starts_with("qtr"), \(x) sum(x, na.rm = TRUE)),
              .groups = "drop") %>% 
    reshape_msd(include_type = FALSE)
  
  #pivot wide in order to subtract KP from GP (Total)
  df_vl <- df_vl %>% 
    pivot_wider(names_from = type,
                values_fill = 0) 
  
  #create KeyPop if missing
  if(!"KeyPop" %in% names(df_vl))
    df_vl <- mutate(df_vl, KeyPop = 0)
  
  View(df_vl)
  #subtract KP from GP (Total)
  df_vl <- df_vl %>%
    mutate(GenPop = Total - KeyPop,
           GenPop = ifelse(GenPop < 0, 0, GenPop)) %>% 
    pivot_longer(-where(is.character),
                 names_to = "type") %>% 
    mutate(group = case_when( type %in% c("Total") ~ "Total",
                              type %in% c("KeyPop", "GenPop") ~ "KP-GP",
                              type %in% c("AGYW", "Non-AGYW") ~ "AGYW"),
           .before = type)
  
  #reshape wider by indicator and create lag for VLC
  df_vl <- df_vl %>% 
    pivot_wider(names_from = indicator,
                names_glue = "{tolower(indicator)}") %>% 
    group_by(country, psnu, funding_agency, group, type) %>% 
    mutate(tx_curr_lag2 = lag(tx_curr, n = 2, order_by = period)) %>% 
    ungroup()
  
  #calculate VLC/S
  df_vl <- df_vl %>%
    mutate(vlc = tx_pvls_d/tx_curr_lag2,
           vls = tx_pvls/tx_pvls_d) %>% 
    filter(!is.nan(vlc))
  
  #limit to latest period
  df_vl <- filter(df_vl, period == max(period))
  
  return(df_vl)
}

df_viz <- prep_viral_load_kp_agyw(df, "Minoria")



# VIZ ------------------------------------------------------------------------

# Percents for VLC, VLS, PMCTCT
create_percents_plot <- function(df_viz, indicators, jitter_factor = 0.5, tx_size_range = c(2, 12)) {
  
  baseline_pt_1 <- 0
  baseline_pt_2 <- .25
  baseline_pt_3 <- .5
  baseline_pt_4 <- .75
  baseline_pt_5 <- 1
  
  # Custom jitter function to control above or below line based on agency
  custom_jitter <- function(df, jitter_factor) {
    df %>%
      mutate(
        y_jitter = case_when(
          funding_agency == "USAID" ~ runif(n(), 0, jitter_factor),  # Jitter upwards for USAID
          funding_agency == "HHS/CDC" ~ runif(n(), -jitter_factor, 0),  # Jitter downwards for CDC
          TRUE ~ 0  # No jitter for other agencies
        )
      )
  }
  
  # Restructure the data to include all indicators in long format
  df_long <- df_viz %>%
    filter(!type %in% c("NA", "GenPop")) %>%
    mutate(type = factor(type, levels = c("Total", "KeyPop", "AGYW", "Males (15+)", "Peds"))) %>%
    pivot_longer(cols = all_of(indicators), names_to = "indicator", values_to = "indicator_value") %>%
    custom_jitter(jitter_factor = jitter_factor)  # Apply jitter function
  
  # Define the plot
  v <- ggplot(df_long, aes(x = indicator_value, y = y_jitter, color = funding_agency)) + 
    geom_linerange(aes(xmin = 0, xmax = 1), y = 0, color = "#D3D3D3", size = 15) +
    geom_point(aes(x = baseline_pt_1, y = 0), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x = baseline_pt_2, y = 0), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x = baseline_pt_3, y = 0), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x = baseline_pt_4, y = 0), shape = 3, color = "#D3D3D3") +
    geom_point(aes(x = baseline_pt_5, y = 0), shape = 3, color = "#D3D3D3") +
    geom_point(aes(size = tx_curr, shape = funding_agency), position = position_dodge(width=0), na.rm = TRUE, alpha = 0.6) +  # Apply jitter for dots
    scale_size_continuous(range = tx_size_range) +  # Control dot size range
    scale_color_manual(values = c("USAID" = midnight_blue, "HHS/CDC" = viking, "DOD" = "#42f551")) + 
    scale_shape_manual(values = c("USAID" = 16, "HHS/CDC" = 18)) + 
    facet_grid(rows = vars(indicator), cols = vars(type), scales = "free_x", 
               switch = "y",
               labeller = labeller(indicator = c("vlc" = "VLC", "vls" = "VLS"), 
                                   type = c("Total" = "Total", "KeyPop" = "KP", "AGYW" = "AGYW", "Males (15+)" = "Males (15+)", "Peds"="Peds (<15)")))  # Nested faceting
  
  # Squish x-axis
  v <- v + 
    scale_x_continuous(
      limits = c(0, 1),
      breaks = c(0, 0.5, 1),  # Specify ticks
      labels = c("0%", "50%", "100%"),
      oob = scales::squish
    )
  
  # Adjust theme
  v <- v +
    si_style_nolines() +
    theme(strip.text = element_text(hjust = 0.5),
          axis.text.x = element_text(),
          axis.text.y = element_blank(),
          legend.position = "none",
          axis.title.x = element_blank(),
          axis.title.y = element_blank())
  
  # Adjust titles and captions
  v <- v +
    theme(
      plot.subtitle = element_markdown(),
      strip.text = element_markdown(),
      panel.spacing.y = unit(2, "lines")
    )
  
  # Adjust points getting clipped
  v <- v + coord_cartesian(clip = "off")
  
  # Return the plot
  return(v)
}

# Create a jitter plot for 'vlc' and 'vls' indicators
# Need to add IIT & PMTCT_EID
create_jitter_plot(df_viz, c("vlc", "vls"), jitter_factor = 1)


# WRAPPER FUNCTION ------------------------------------------------------------

plot_dotpercent <- function(df, cntry) {
  df_viz <- prep_viral_load_kp_agyw(df, "Tanzania")
  plot <- create_jitter_plot(df_viz, c("vlc", "vls"), jitter_factor = 1)
  return(plot)
}



################################ TO DO #####################################

# THIS SCRIPT
  # Add color label for CDC vs USAID
  # Add PMTCT_EID (Total only no subpops)
  # Add IIT for all groups

# OTHER DOT PLOTS (NOT PERCENTAGE). Achievement of Targets Coloring
  # PReP_NEW (not for Peds)
  # OVC_ SERV (OVC_HIVSTAT pos + neg + test not req/ ovc ovc hiv stat_d) (<18)
  # AGYW_PREV

# LINE/DUMBELL CHARTS
  # TX_NET_NEW


#NOTES
# Filtered out DOD
# Squished > 100%
