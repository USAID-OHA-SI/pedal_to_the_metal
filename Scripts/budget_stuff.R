# PROJECT:  pedal_to_the_metal
# PURPOSE:  budget visuals
# AUTHOR:   A.Chafetz | USAID
# REF ID:   9c6a789d 
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
  library(gt)
  library(fontawesome)
  

# GLOBAL VARIABLES --------------------------------------------------------
  
  ref_id <- "9c6a789d"  #a reference to be places in viz captions 
  
  path_fsd <-  si_path() %>% return_latest("Fin")
  
  meta <- get_metadata(path_fsd)  #extract MSD metadata
  
  # cntry <- "Tanzania"

# IMPORT ------------------------------------------------------------------
  
  df_fsd <- read_psd(path_fsd)
  

# MUNGE -------------------------------------------------------------------

  # df_budget <- df_fsd %>% 
  #   filter(country == cntry)
  

# BUDGET TREND ------------------------------------------------------------

  create_budget_trend <- function(df, cntry, export = TRUE){
    v <- df %>% 
      prep_bdgt_trend(cntry) %>% 
      plot_bdgt_trend()
    
    if(export)
      si_save(glue("{cntry}_budget_trend.png"), path = "Images")
    
    return(v)
  }
  
  #budget trend
  prep_bdgt_trend <- function(df, cntry){
    
    df_budget_trend <- df %>% 
      filter(country == cntry)
    
    df_budget_trend <- df_budget_trend %>%
      bind_rows(df_budget_trend %>% mutate(fundingagency = "PEPFAR")) %>%
      mutate(fundingagency = ifelse(str_detect(fundingagency, "USAID"), "USAID", fundingagency)) %>%
      filter(fundingagency %in% c("USAID", "PEPFAR"),
             fiscal_year >= 2021) %>% 
      group_by(fiscal_year, fundingagency) %>% 
      summarise(cop_budget_total = sum(cop_budget_total, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(pt_label = case_when(fundingagency == "USAID" ~ 
                                    label_currency(1, scale = 1e-6, suffix = "m")(cop_budget_total)))
    
    #TODO - should this exclude SCH?
    
    return(df_budget_trend)
  }
  
  #plot budget trend
  plot_bdgt_trend <- function(df){
    df %>% 
      ggplot(aes(fiscal_year, cop_budget_total, fill = fundingagency)) +
      geom_col(position = "identity", width = 0.75) +
      geom_text(aes(label = pt_label), na.rm = TRUE, color = si_palettes$hunter_t[1],
                family = "Source Sans Pro", vjust = -.4, size = 12/.pt) +
      scale_y_continuous(label = label_currency(scale = 1e-6, suffix = "m")) +
      scale_fill_manual(values = c("PEPFAR" = grey20k,
                                   "USAID" = si_palettes$hunter_t[1])) +
      labs(x = NULL, y = NULL,
           title = glue("<span style = 'color:{si_palettes$hunter_t[1]};'>USAID'S</span> SHARE OF THE TOTAL PEPFAR BUDGET")) +
      si_style_ygrid() +
      theme(axis.text.y = element_blank(),
            legend.position ="none",
            plot.title = element_markdown()
      )
  }
  

 #test 
  create_budget_trend(df_fsd, "Zimbabwe", T)
  si_preview(scale = .5)
  si

# BUDGET TABLE ------------------------------------------------------------
  
  create_budget_tbl <- function(df, cntry, export = TRUE){
   
    t <- df %>% 
      prep_bdgt_trend(cntry) %>% 
      prep_bdgt_tbl() %>% 
      plot_budget_tbl()
   
   if(export)
     gtsave(t, glue("{cntry}_budget_tbl.png"), path = "Images")
   
   return(t)
   
  }
  
  prep_bdgt_tbl <- function(df){
    
    df_budget_tbl <- df %>% 
      select(-pt_label) %>% 
      pivot_wider(names_from = fundingagency,
                  values_from = cop_budget_total) %>% 
      mutate(`USAID share` = USAID/PEPFAR) %>%
      pivot_longer(-fiscal_year,
                   names_to = "type") %>% 
      group_by(type) %>% 
      mutate(direction = case_when(value > lag(value,  order_by = fiscal_year) ~ "increase",
                                   value < lag(value,  order_by = fiscal_year) ~ "decrease",
                                   TRUE ~ "flat")) %>% 
      ungroup() %>% 
      filter(fiscal_year == max(fiscal_year)) %>% 
      mutate(val_fmt = ifelse(type == "USAID share",
                              percent_format(1)(value),
                              label_currency(1, scale = 1e-6, suffix = "m")(value))
             
      ) %>% 
      select(fiscal_year, type, val_fmt, direction)
    
    return(df_budget_tbl)
  }
  
  #from create_epi_control_summary_table.R
  compress_rows <- function(gt_object, font_size = 12) {
    gt_object %>%
      tab_options(
        data_row.padding = px(1),
        column_labels.padding = px(1),
        summary_row.padding = px(1),
        table.font.size = px(font_size),
        table_body.hlines.width = 0
      )
  }  
  
  plot_budget_tbl <- function(df){
    
    df %>% 
      gt() %>%
      cols_hide(fiscal_year) %>% 
      # Transform the 'direction' column to include icons
      text_transform(
        locations = cells_body(columns = direction),
        fn = function(x) {
          case_when(
            x == "increase" ~ fa("chevron-up", fill = si_palettes$hunter_t[3]) %>% as.character,
            x == "decrease" ~ fa("chevron-down", fill = orchid_bloom) %>% as.character,
            x == "flat"     ~ fa("chevron-right", fill = grey20k) %>% as.character
          )
        }
      ) %>% 
      cols_align(align = "right",
                 columns = "val_fmt") %>% 
      cols_align(align = "center",
                 columns = direction) %>% 
      tab_options(column_labels.hidden = TRUE) %>% 
      # tab_header(title = glue("BUDGET FY{unique(df$fiscal_year) %>% str_sub(-2)}")) %>% 
      opt_align_table_header(align = "left") %>% 
      compress_rows(font_size = 11) 
    
  }
  

  #test 
   create_budget_tbl(df_fsd, "Malawi", FALSE) %>% 
    gtsave("Images/zim_bgt.png")
   
# LOCAL PARTNER SHARE -----------------------------------------------------

  #TODO - should this just be USAID?
  #TODO - should this exclude SCH?
  
  create_lp_share <- function(df, cntry, export = TRUE){
    
    v <- df %>% 
      
      prep_lp_share(cntry) %>% 
      plot_lp_share()
    
    if(export)
      si_save(v, glue("{cntry}_lp_share.png"), path = "Images",
              height = 1)
    
    return(v)
    
  }
  
  prep_lp_share <- function(df, cntry){
    
    df_lp <- df %>% 
      filter(country == cntry)
    
    df_lp <- df_lp %>% 
      filter(fiscal_year == max(fiscal_year)) %>%
      mutate(local_prime_partner = factor(local_prime_partner, 
                                          c("Local", "International", "Unknown")) %>% 
               fct_rev()) %>% 
      group_by(country, fiscal_year, local_prime_partner) %>% 
      summarise(cop_budget_total = sum(cop_budget_total, na.rm = TRUE),
                .groups = "drop") %>% 
      mutate(share = cop_budget_total/ sum(cop_budget_total),
             pt_label = case_when(local_prime_partner == "Local" ~ 
                                    label_percent(1)(share)))
    
    return(df_lp)
  }
 
  plot_lp_share <- function(df){
    
    unkwn_share <- label_percent(1)(df[df$local_prime_partner == "Unknown",]$share)
    
    df %>% 
      ggplot(aes(cop_budget_total, country, fill = local_prime_partner)) +
      geom_col(width = 0.5) +
      scale_fill_manual(values = c("Local" = si_palettes$hunter_t[1], 
                                   "International" = grey20k, 
                                   "Unknown"= grey30k)) +
      geom_text(aes(label = pt_label), na.rm = TRUE, color = si_palettes$hunter_t[1],
                family = "Source Sans Pro", hjust = -.4, size = 11/.pt) +
      labs(x = NULL, y = NULL,
           title = glue("BUDGET SHARE GOING TO <span style = 'color:{si_palettes$hunter_t[1]};'>LOCAL PARTNERS</span>"),
           subtitle = glue("FY{str_sub(df$fiscal_year, -2)} Budget [{unkwn_share} classified as local or international]")) +
           #caption = "Note: Includes SCH") +
      si_style_nolines() +
      theme(axis.text = element_blank(),
            plot.title = element_markdown(),
            legend.position = "none") 
  }
  
  #test 
   create_lp_share(df_fsd, "Malawi", FALSE)
   si_preview(scale = 0.5)
   si_clear_preview()
  
  