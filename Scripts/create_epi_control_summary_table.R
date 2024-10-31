# PROJECT:  C:/Users/tessam/Documents/Github/chatty_whisperer
# PURPOSE:  Create top right hand corner of COP25 1 pager
# AUTHOR:   T. Essam | USAID
# REF ID:   1166f85f
# LICENSE:  MIT
# DATE:     2024-10-30
# UPDATED:

# DEPENDENCIES ------------------------------------------------------------

  # General Libraries
  library(tidyverse)
  library(glue)
  
  # USAID-Specific Libraries
  library(gagglr)
  
  # Visualization Extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(mindthegap)
  library(fontawesome)
  library(gt)

# GLOBAL VARIABLES --------------------------------------------------------

  ref_id <- "1166f85f" # a reference to be places in viz captions
  
  GOAL <- 95
  TARGET_YEAR <- 2023
  ALL_AGE <- "All"
  ALL_SEX <- "All"

# FUNCTIONS FOR TABLE CREATION ---------------------------------------------------------------

  # Compress rows in gt table
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

  # Creates HTML shapes with mapped colors
  make_achv_shape <- function(x, shape = "circle") {
    # Use case_when() to create a vectorized condition
    fills <- dplyr::case_when(
      is.na(x) ~ glitr::grey20k,
      x == TRUE ~ glitr::hw_hunter,
      x == FALSE ~ glitr::hw_orchid_bloom,
      TRUE ~ NA_character_
    )
  
    # Generate Font Awesome circles for each fill color
    result <- purrr::map(fills, function(fill_color) {
      fontawesome::fa(shape, fill = fill_color, height = "1em", prefer_type = "solid", stroke_width = "2px") %>%
        as.character() %>%
        gt::html()
    })
  
    return(result)
  }

  # Subset to countries for 1 table per country
  filter_cntry <- function(df, sel_cntry = "Zambia") {
    df %>%
      filter(country == sel_cntry)
  }

  # Combines the three data frames and creates final epi table
  create_epi_tbl <- function(sel_cntry = "Zambia") {
    cli::cli_alert_info("Creating table for: {sel_cntry}")
  
    bind_rows(
      df_95s %>% filter_cntry(sel_cntry) %>% 
        select(country, indicator, col1 = known_status, col2 = on_art, col3 = vls),
      df_epi_cntrl %>% filter_cntry(sel_cntry) %>% 
        select(country, indicator, col3 = epi_control),
      df_epi_stats %>% filter_cntry(sel_cntry) %>% 
        select(country, indicator = name, col3 = value_formatted),
    ) %>%
      gt() %>%
      text_transform(
        locations = cells_body(c(col3)),
        fn = identity # Use identity as the content is already marked as HTML with `I()`
      ) %>%
      compress_rows(font_size = 11) %>%
      tab_options(column_labels.hidden = TRUE) %>%
      cols_hide(country)
  }

  # Load UNAIDS data
  load_and_filter_data <- function() {
    load_unaids() %>%
      filter(year == TARGET_YEAR, age == ALL_AGE, sex == ALL_SEX)
  }


# FUNCTIONS FOR DATA PREP -------------------------------------------------


  # Data processing functions
  # Process epidemiological statistics
  prepare_epi_stats <- function(data) {
    data %>%
      filter(indicator %in% c("Number Total Deaths to HIV Population", "Number New HIV Infections", "Incidence mortality ratio (IMR)")) %>%
      select(country, iso, indicator, estimate) %>%
      pivot_wider(names_from = "indicator", values_from = estimate) %>%
      rename_with(~ str_replace_all(., c(`.*Deaths.*` = "Deaths", `.*Infections.*` = "New Infections", `.*IMR.*` = "Incidence Mortality Rate"))) %>%
      pivot_longer(cols = where(is.double)) %>%
      mutate(value_formatted = if_else(
        is.numeric(value) & value >= 1000,
        paste0(round(value / 1000), "K"),
        as.character(value)
      )) %>%
      mutate(value_formatted = map(value_formatted, ~ I(glue::glue("<span>{.}</span>"))))
  }


  # Prepare epidemic control data
  prepare_epi_control <- function(data) {
    data %>%
      filter(indicator == "Number AIDS Related Deaths") %>%
      select(country, iso, epi_control = achv_epi_control) %>%
      mutate(epi_control = make_achv_shape(epi_control, shape = "square")) %>%
      mutate(indicator = "Epidemic Control")
  }

  # Prep 95s summary for each country and reshape for appending with other indicators
  prepare_95s_summary <- function(data) {
    data %>%
      filter(indicator %in% c("Percent Known Status of PLHIV", 
                              "Percent on ART with Known Status", 
                              "Percent VLS on ART")) %>%
      select(country, iso, indicator, estimate) %>%
      arrange(country, indicator) %>%
      group_by(country) %>%
      mutate(
        indicator = recode(indicator,
          "Percent Known Status of PLHIV" = "Known Status",
          "Percent on ART with Known Status" = "On ART",
          "Percent VLS on ART" = "VLS"
        ),
        set = recode(indicator, "Known Status" = 1, "On ART" = 2, "VLS" = 3),
        achv = estimate >= GOAL
      ) %>%
      ungroup() %>%
      mutate(goals_achv = sum(achv, na.rm = TRUE)) %>%
      select(country, indicator, achv) %>%
      complete(country, indicator, fill = list(achv = NA)) %>%
      mutate(achv = make_achv_shape(achv)) %>%
      pivot_wider(names_from = indicator, values_from = achv) %>%
      janitor::clean_names() %>%
      mutate(indicator = "Achieved 95s")
  }

# LOAD DATA ---------------------------------------------------------------

  df <- load_and_filter_data()
  df_epi_stats <- prepare_epi_stats(df)
  df_epi_cntrl <- prepare_epi_control(df)
  df_95s <- prepare_95s_summary(df)

# TEST ------------------------------------------------------------------

  # Create's a table for the default country
  create_epi_tbl() %>% 
    gt::gtsave("Images/Zambia.png")
  
  cntry_list <- df %>%
    distinct(country) %>%
    slice(1:10) %>%
    pull()
  
  map(cntry_list, .f = ~ create_epi_tbl(.x))
