# PROJECT: pedal_to_the_metal
# AUTHOR:   B. Betz | USAID
# PURPOSE:  visualize 10-10-10
# REF ID:   030cc546 
# LICENSE:  MIT
# DATE:     2024-11-08
# UPDATED: 


# DEPENDENCIES ------------------------------------------------------------
  
  library(tidyverse)
  library(janitor)
  library(gagglr)
  library(readxl)


  #viz extensions
  library(scales, warn.conflicts = FALSE)
  library(systemfonts)
  library(tidytext)
  library(patchwork)
  library(ggtext)
  library(gt)
  library(fontawesome)


# GLOBAL VARIABLES --------------------------------------------------------
  
ref_id <- "030cc546"

# IMPORT and TRANSFORM ------------------------------------------------------------------
  

  ### Legal and Policy data from Gina, UNAIDS -- ONLY SERVES AS SOURCE HALF OF THE DATA PRESENTED BY Gina
  first90 <- read_xlsx("Data/tententen/punitive_laws.xlsx") %>% 
    pivot_longer(4:13) %>%
    mutate(source = case_when(str_detect(name, "^Source") ~ value ),
           indicator = case_when(str_detect(name, "^Crim") ~ name),
           outcome   = case_when(str_detect(name, "^Crim") ~ value),
           ) %>% 
    select(-name, -value) %>% 
    janitor::clean_names() %>% 
    group_by(country_code, region_unaids, country_name) %>% 
    fill(source, indicator, outcome, .direction = "downup") %>% 
    distinct() %>% ungroup() %>% 
    mutate(year = as.numeric(str_extract(source, "[1][6-9][0-9]{2}|20[0-2][0-9]")),
           outcome_yn = str_extract(outcome, "^[Y,N]"),
                value = case_when(outcome_yn == "Y" ~ 10.001,
                                  outcome_yn == "N" ~ -10),
           indicator_inverted = case_when(
             str_detect(indicator, "HIV") ~ "HIV exposure non-criminalization",
             str_detect(indicator, "sex work") ~ "Sex work non-criminalization",
             str_detect(indicator, "drug") ~ "Drug possession non-criminalization",
             str_detect(indicator, "same-sex") ~ "Same-sex sex non-criminalization",
             str_detect(indicator, "trans") ~ "Transgender non-criminalization",
             ),
           outcome_inverted = case_when(outcome_yn == "Y" ~ "No",
                                        outcome_yn == "N" ~ "Yes"),
           outcome_explanation = str_extract(outcome, "(?<=^Yes[,]?\\s|^No[,]?\\s).+$")
           ) %>% 
    select(-country_code, -region_unaids, -indicator, -outcome, -outcome_yn ) %>% 
    rename(country = country_name) %>% 
    group_by(country, indicator_inverted) %>% 
    mutate(most_recent_year = max(year)) %>% 
    ungroup() %>% 
    filter(year==most_recent_year) %>%
    # duplicates remain. Need to choose the most recent, most truthful, or most detailed. This was my best guess
    select(-most_recent_year) %>% 
    mutate(ten = "first",
           outcome_label = if_else(outcome_inverted=="Yes", "Adopted", "Not Adopted"),
           outcome_label_value = if_else(outcome_inverted=="Yes", -5, 5)) %>% 
    rename(indicator = indicator_inverted,
           outcome = outcome_inverted) %>% 
        print(n=120)
  
  first90 %>% count(indicator)

  ### GAM and BBS data from William via UNAIDS KP_atlas
  second_third90 <- read_xlsx("Data/tententen/Structural barriers by country (1).xlsx", sheet = 1) %>% 
    select(-starts_with("...")) %>% 
    select(1, starts_with("S&D"), starts_with("Violence")) %>% #other data replicating the above can be found but there are no dates which make this more nebulous
    pivot_longer(cols = 2:9, 
                 names_to = c("indicator_sub", "population"), 
                 names_pattern = "(.*) ([A-Z]+) \\(.*\\)",
                 values_to = "value") %>% 
    mutate( ten = case_when(indicator_sub == "S&D" ~ "second",
                            indicator_sub == "Violence" ~ "third"),
            indicator_sub = if_else(indicator_sub == "S&D", "Stigma & discrimination", indicator_sub),
             indicator = str_c(indicator_sub, population, sep = " experienced by "),
          ) %>% 
    rename(country=Country) %>% 
    glimpse()
  

  ### GBV DHS survey and UNAIDS data from Allison Schmalle
  gbv <- read_xlsx("Data/tententen/GBV DHS Data.xlsx", sheet = 2, skip = 3) %>% 
    janitor::clean_names() %>% 
    pivot_wider(names_from = indicator, values_from = value) %>% 
    janitor::clean_names() %>% 
    mutate(year = case_when(!is.na(women_15_49_yrs_who_experienced_physical_or_sexual_violence) ~ year_dhs_unaids)) 
      # count(country, year_dhs) %>% 
      # filter(!is.na(year_dhs)) %>% print(n=50) %>% 
      # group_by(country) %>% summarise(years = n_distinct(year_dhs)) %>%
      # print(n=50)
    ##SOME COUNTRIES HAVE MULTIPLE YEARS DHS, OTHERS ONE OR NONE -> FILL UP
      ##doesn't actually matter because old DHS had no UNAIDS estimates
      
    
    gbv_structured <- gbv %>% filter(!is.na(women_15_49_yrs_who_experienced_physical_or_sexual_violence)) %>% select(1,4,5) %>% 
      group_by(country) %>% 
      mutate(year_recent = max(year)) %>% 
      ungroup %>% 
      mutate(most_recent_year = case_when(year_recent == year ~ TRUE, .default = FALSE),
             most_recent_year_size = if_else(most_recent_year == TRUE, 2, 1),
             women_15_49_yrs_who_experienced_physical_or_sexual_violence) %>% 
      pivot_longer(2, names_to = "indicator", values_to = "value") %>% 
      mutate(values_recent = if_else(most_recent_year==TRUE, value, NA),
             values_least_recent = if_else(most_recent_year==FALSE, value, NA)) %>% 
      print(n=60)
    
    third_90 <- gbv_structured %>% filter(year==year_recent ) %>%
      select(-contains("recent")) %>% 
      mutate(ten = "third" ,
             indicator_sub = "Violence",
             population = "women (15-49)",
             indicator_original = indicator,
             indicator = str_c(indicator_sub, population, sep = " experienced by "),
      ) %>% print()
    
    
    # MUNGE -------------------------------------------------------------------
    order <- rev(c("HIV exposure non-criminalization", "Drug possession non-criminalization", "Sex work non-criminalization", "Same-sex sex non-criminalization", "Transgender non-criminalization", "Stigma & discrimination experienced by PWID" , "Stigma & discrimination experienced by FSW", "Stigma & discrimination experienced by MSM","Stigma & discrimination experienced by TG", "Violence experienced by PWID",  "Violence experienced by FSW",  "Violence experienced by MSM", "Violence experienced by TG",    "Violence experienced by women (15-49)"    )    )
    
  tens <-  bind_rows(first90, second_third90, third_90) %>% 
      mutate(reaction = if_else(value > 10, "highlight", "celebrate")) %>% 
      select(-source) %>% 
      distinct() %>% 
      mutate(indicator = factor(indicator, levels = order))

  tens %>% count(indicator) %>% print(n=790)

    
# VISUALIZE -------------------------------------------------------------------
    tens %>% 
      filter(country=="Malawi") %>% 
      ggplot(aes(y=indicator)) +
      geom_col(aes(x=value, fill = reaction)) +
      geom_text(aes(x=outcome_label_value, label=outcome_label))+
      theme_minimal() +
        theme(axis.title.y = element_blank(),
              panel.grid = element_blank(),
              legend.position = "none",
              plot.title.position = "plot",      # Position the title over the axis
              plot.subtitle.position = "plot"
        ) +
        scale_fill_manual(values = c("highlight" = old_rose, "celebrate" = viking)) +  
     geom_vline(xintercept=10, 
                  color = glitr::grey70k, linewidth = 1, linetype = "dashed"
                ) +   
    labs(x =  "Percent (from 100)",
         # caption = "estimates may not be current",
         title = "10-10-10 indicator goal status and/or values",
         subtitle = "Meeting these goals will require fewer than 10% of each named group experiences stigma & descrimination or physical & sexual violence")


    # gbv_structured %>%
    #   filter(country=="Cameroon") %>% 
    #   ggplot(aes(y=indicator)) + 
    #     geom_col(aes(x=values_recent, fill = most_recent_year)) + 
    #     geom_vline(xintercept=10, 
    #                color = glitr::orchid_bloom,linewidth = 3
    #                 ) +
    #   theme_minimal() +
    #   theme(axis.title = element_blank(), 
    #         panel.grid.major.y = element_blank(),
    #         ) +
    #   scale_fill_manual(values = c("TRUE" = grey70k, "FALSE" = grey20k)) 
    # 
  # EXPORT -------------------------------------------------------------------