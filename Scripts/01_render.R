# PROJECT:  pedal_to_the_metal
# PURPOSE:  render plots
# AUTHOR:   A.Chafetz | T.Essam USAID
# REF ID:   7cf8b162 
# LICENSE:  MIT
# DATE:     2024-11-12
# UPDATED:  2024-11-14

# DEPENDENCIES ------------------------------------------------------------
  
  #general
  library(tidyverse)
  library(vroom)
  library(glue)
  #oha
  library(gagglr) ##install.packages('gagglr', repos = c('https://usaid-oha-si.r-universe.dev', 'https://cloud.r-project.org'))
  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gt)
  library(fontawesome)
  library(googlesheets4)
  library(mindthegap)
  
  source("Scripts/save_png.R")
  source("Scripts/save_gt.R")
  source("Scripts/budget_stuff.R")
  source("Scripts/hrh_plot.R")
  source("Scripts/dhi_plots.R")  
  source("Scripts/10-10-10s.R")
  source("Scripts/create_epi_control_summary_table.R")
  source("Scripts/95s_Age_Sex.R")

  load_secrets()

# GLOBAL VARIABLES --------------------------------------------------------
  
  #reference id
  ref_id <- "7cf8b162"  #a reference to be places in viz captions 
  
  #data paths
  path_msd <-  si_path() %>% return_latest("OU_IM")
  path_fsd <-  si_path() %>% return_latest("Financial")
  path_hrh <-  si_path() %>% return_latest("HRH.*not_redacted.*txt")
  path_dhi <-  si_path() %>% return_latest("DHI.*Detailed")
  
  #MSD metadata
  meta <- get_metadata(path_msd)  #extract MSD metadata
  
  #full country list
  v_countries <- pepfar_country_list %>%
    filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
    pull(country)
  
  #full country iso list
  v_iso <- pepfar_country_list %>%
    filter(str_detect(operatingunit, "Region", negate = TRUE)) %>%
    pull(country_iso)
  
  #country to test for plots
  test_cntry <- "Zambia"
  
# IMPORT ------------------------------------------------------------------
  
  #import all datasets
  df_msd <- read_psd(path_msd)
  
  df_fsd <- read_psd(path_fsd)
  
  df_hrh <- read_psd(path_hrh)
  
  df_dhi <- read_psd(path_dhi)
  
  #import UNAIDS Data
  df_unaids  <- load_unaids(pepfar_only = TRUE)

  
# Ordering based on order they appear in document
  
# EPI SUMMARY TABLE -------------------------------------------------------
  
  #Create data
  #Can use df_unaids, but need appropriate filters
  df_epi <- load_and_filter_data() #calls load_unaids() within
  df_epi_stats <- prepare_epi_stats(df_epi)
  df_epi_cntrl <- prepare_epi_control(df_epi)
  df_95s <- prepare_95s_summary(df_epi)
  
  remove(ALL_AGE, ALL_SEX, GOAL, TARGET_YEAR)
  
  #Test
  create_epi_tbl(test_cntry) 
  
  #iterate
  walk(v_countries, .f = ~ create_epi_tbl(.x) %>% 
         save_gt(.x, "epi", subtopic = "tbl"))  
  
  #check
  list.files("Images", "epi-tbl") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## CLEAR UNAIDS DATA ----
  rm(df_95s, df_epi, df_epi_cntrl, df_epi_stats)
  
# EPI GAP LOLLIPOPS -------------------------------------------------------
  
  df_tt <- df_unaids %>% 
    dplyr::filter(indicator_type == "Percent", 
                  year == 2023)
  
  #Create data
  df_epi_gaps <- prep_tt_tbl(df_tt)
  
  #Test
  plot_epi_gaps(df_epi_gaps, "Zambia")
  si_preview()
  
  #iterate
  walk(v_countries, .f = ~ plot_epi_gaps(df_epi_gaps, .x))  
  
  #check
  list.files("Images", "epi-gaps") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## CLEAR EPI GAPS DATA ----
  rm(df_epi_gaps, df_tt)
  
    
# 10s Plots ---------------------------------------------------------------
  
  # Chunks for loading and working with 10s
  df_tens <- load_tens()
  
  df_tens_viz <- prep_10s_barriers(df_tens)
  dotplot_viz_10s(df_tens_viz, "Zambia")
  
  #iterate
  walk(v_countries,
       ~dotplot_viz_10s(df_tens_viz, .x))
  
  #check
  list.files("Images", "kp-policy") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## CLEAR 10s data ----
  remove(df_tens, df_tens_viz)  
  
  

# BUDGET SECTION ----------------------------------------------------------


  ## BUDGET TREND BAR CHART -----
  
  #munge data
  df_bdgt_trnd <- prep_bdgt_trend(df_fsd)
  
  #test
  plot_bdgt_trend(df_bdgt_trnd, test_cntry)
  si_preview()
  
  #iterate
  walk(v_countries,
      ~plot_bdgt_trend(df_bdgt_trnd, .x))
  
  #check
  list.files("Images", "budget-trend") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## BUDGET TABLE -----
  
  #munge data
  df_bdgt_tbl <- prep_bdgt_tbl(df_bdgt_trnd)
  
  #test
  plot_budget_tbl(df_bdgt_tbl, test_cntry)
  # plot_budget_tbl(df_bdgt_tbl, test_cntry)
  # si_preview()
  
  #iterate
  walk(v_countries,
       ~plot_budget_tbl(df_bdgt_tbl, .x))
  
  ## LOCAL PARTNER SHARE -----
  
  #mundge data
  df_lp_share <- prep_lp_share(df_fsd)
  
  #test
  plot_lp_share(df_lp_share, test_cntry)
  si_preview()
  
  #iterate
  walk(v_countries,
      ~plot_lp_share(df_lp_share, .x))
  
  #check
  list.files("Images", "budget-lp-share") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## CLEAR BUDGET DATA ----
  
  rm(df_bdgt_trnd, df_bdgt_tbl, df_lp_share)
  si_clear_preview()
  

# HRH SECTION -------------------------------------------------------------

  ## HRH PLOT -----
  
  #munge
  df_hrh_plot <- prep_hrh(df_hrh)
  
  #test
  plot_hrh(df_hrh_plot, test_cntry)
   si_preview()
  
  #iterate
  walk(v_countries,
       ~plot_hrh(df_hrh_plot, .x))
  
  #check
  list.files("Images", "hrh") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  
  ## CLEAR HRH data -----
  rm(df_hrh_plot)
  si_clear_preview()
  

# DHI SECTION -------------------------------------------------------------

  ## DHI Overview Plot ----

  #munge
  df_sys <- prep_dhi_overview(df_dhi)

  #test
  plot_dhi_overview(df_sys, test_cntry)
  si_preview()
  
  #iterate
  walk(v_countries,
      ~plot_dhi_overview(df_sys, .x))
  
  #check
  list.files("Images", "dhi-overview") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  
  ## DHI Categorical Table -----
  
  #munge
  df_sys_cat <- prep_dhi_cat(df_dhi)
  
  #test
  # plot_dhi_cat(df_sys_cat, test_cntry)
  # si_preview()
  make_dhi_cat_table(df_sys_cat, test_cntry) 
  
  #iterate
  # walk(v_countries,
  #     ~plot_dhi_cat(df_sys_cat, .x))
  walk(v_countries,
      ~make_dhi_cat_table(df_sys_cat, .x))

  #check
  # list.files("Images", "dhi-cat") %>% 
  #   str_sub(end = 3) %>% 
  #   setdiff(v_iso)
  list.files("Images", "dhi-cat-tbl") %>% 
    str_sub(end = 3) %>% 
    setdiff(v_iso)
  
  ## CLEAR DHI data -----
  rm(df_sys, df_sys_cat)
  si_clear_preview()
  

# TABLE SUMARY ----------------------------------------------------------------
  
  # Source Creation of dynamic text
  source("Scripts/Create_main_text_table.R") 
  
 df_final <-  df_final %>% 
    mutate(age_dep_d = str_remove_all(age_dep_d, " Burden"),
            nurses_d = str_remove_all(nurses_d, " Adequate"))
 
  
  # Create Image paths inside table dataframe for Images1:N
  tbl_list <-  list("epi-tbl", "epi-gaps", "kp-policy","budget-trend", "budget-tbl", 
       "budget-lp-share", "hrh", "dhi-overview", "dhi-cat-tbl")
  
  




  
  