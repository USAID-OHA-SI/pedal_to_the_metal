
remotes::install_github("USAID-OHA-SI/mindthegap", ref = "dev_edms_plot")
library(mindthegap)
library(googlesheets4)
library(tidyverse)
library(glue)
library(gagglr)

# Functions and datasets ------------
clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}b"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}m"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}k"),
                   TRUE ~ glue("{x}"))
}


pepfar <- pepfar_country_list
pepfar_short <- pepfar %>%
  select(operatingunit_iso, country, country_iso)

cop_ous <- glamr::pepfar_country_list %>% 
  filter(str_detect(operatingunit, "Region", negate = T)) %>% 
  pull(operatingunit)

cop_ou_iso <- pepfar_short 

options(scipen = 999)


## 1. general financial dataset ----------
financial_filepath <- si_path() %>% 
  return_latest("Financial")
fin_raw <- read_psd(financial_filepath)


## 2. the comprehensive financial dataset -----------

comp_filepath <- si_path() %>% 
  return_latest("Comprehensive")
comp_raw <- read_psd(comp_filepath)

## 3. UNAIDS data -----
unaids_raw <- load_unaids()

## 4. vertical transmission --------
vt_url <- "1U5K9ppZJvF4aWbVvP2cXeTu6N2Ua6SUOiLxqwcB2f4o"
vt_raw <- read_sheet(vt_url)

## 5. MER data --------
all_ou_filepath <- si_path() %>% 
  return_latest("OU_IM_FY22-25")

global <- read_psd(all_ou_filepath)

## 6. age dependency data - originally from here: https://databank.worldbank.org/source/gender-statistics/Series/SP.POP.DPND# ----------
ad_url <- "1UM1f-DepxmATmmMOUXaPLAuICLsueBA71AZaC7CQXfQ"
ad_raw <- read_sheet(ad_url)

## 7. run in nurses/midwives data ---------
nurses_url <- "1t-iwMadiQU1xrMaOxN2ISUvUl8IGOCIgc1d97nh56fE"
nurses_raw <- read_sheet(nurses_url)

## 8. gov health expenditure, originall here: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/domestic-general-government-health-expenditure-(gghe-d)-as-percentage-of-general-government-expenditure-(gge) -----
gov_url <- "1X4iVKzhUiG_q5GYE_lU6fCrak7d94N8q1OGFoH5bqro"
gov_raw <- read_sheet(gov_url)


# Calculate budget table ----------

  # filter data
  fin_FY23 <- fin_raw %>%
    filter(fiscal_year == 2024)
  
  fin_FY23 <- fin_FY23 %>%
    mutate(fundingagency2 = case_when(str_detect(fundingagency, "USAID") ~ "USAID",
                                      TRUE ~ fundingagency))
  
  ## find Above site programming cost ------
  
  fin_FY23_USAID_ASP <- fin_FY23 %>%
    filter(fundingagency2 == "USAID",
           program == "ASP")
  
  fin_FY23_USAID_ASP_2 <- fin_FY23_USAID_ASP %>%
    group_by(country) %>%
    summarise(asp = sum(cop_budget_total))

  fin_FY23_USAID_ASP_2$asp <- clean_number(fin_FY23_USAID_ASP_2$asp)
  
  ## find OVC cost ---------
  
  fin_FY23_USAID_OVC <- fin_FY23 %>%
    filter(fundingagency2 == "USAID",
           targeted_beneficiary == "OVC")
  
  fin_FY23_USAID_OVC_2 <- fin_FY23_USAID_OVC %>%
    group_by(country) %>%
    summarise(ovc = sum(cop_budget_total))
  
  fin_FY23_USAID_OVC_2$ovc <- clean_number(fin_FY23_USAID_OVC_2$ovc)
  
  
  ## find DREAMS data with comprehensive dataset ----------
  
  comp <- comp_raw %>%
    mutate(fundingagency2 = case_when(str_detect(fundingagency, "USAID") ~ "USAID",
                                      TRUE ~ fundingagency))
  
  comp_dreams <- comp %>%
    filter(initiative_name == "DREAMS",
           planning_cycle == "COP23",
           fundingagency2 == "USAID")
  
  comp_dreams2 <- comp_dreams %>%
    group_by(country) %>%
    summarise(dreams = sum(cop_budget_total, na.rm = TRUE))
  
  comp_dreams2$dreams <- clean_number(comp_dreams2$dreams)
  
  

## Total COP23 budget ------------------------
  bud_sum_count_agency <- fin_FY23 %>%
    group_by(country, fundingagency2) %>%
    summarise(agency_budget = sum(cop_budget_total)) %>%
    ungroup()
  
  bud_sum_count_agency_usaid <- bud_sum_count_agency %>%
    filter(fundingagency2 == "USAID") %>%
    rename(budget_usaid = agency_budget)
  
  bud_sum_count <- fin_FY23 %>%
    group_by(country) %>%
    summarise(budget_o = sum(cop_budget_total)) %>%
    ungroup()
  
  percent <- bud_sum_count %>%
    full_join(bud_sum_count_agency_usaid)
  
  # this provides the total budget for the country for COP23, USAID's budget, and the percentage
  percent <- percent %>%
    filter(!is.na(budget_usaid)) %>%
    select(!fundingagency2) %>%
    mutate(perc_usaid = round((budget_usaid / budget_o)*100, 0))
  
  # make a table for each country
  country_budget <- percent %>%
    mutate(budget_o = {clean_number(budget_o)},
           budget_usaid = {clean_number(budget_usaid)})
  
  country_budget$perc_usaid <- ifelse(!is.na(country_budget$perc_usaid), paste0(country_budget$perc_usaid, "%"), NA)
  
# Create budget table ------
  budget_table <- country_budget %>%
    full_join(comp_dreams2) %>%
    full_join(fin_FY23_USAID_OVC_2) %>%
    full_join(fin_FY23_USAID_ASP_2)
  

# Calculate result/share indicators ---------
global_1 <- global %>%  
  filter(indicator %in% c("HTS_TST", "PrEP_NEW", "TX_CURR"),
         fiscal_year == 2024) %>%
    select(operatingunit, country, indicator, funding_agency, standardizeddisaggregate, cumulative)
  
  agency_totals <- global_1 %>%
    group_by(operatingunit, indicator, funding_agency) %>%
    summarise(ind_c = sum(cumulative, na.rm = T)) # confirm this is right with the "Dedup" category - does it subtract?
  
  agency_totals_2 <- agency_totals %>%
    ungroup() %>%
    group_by(operatingunit, indicator) %>%
    mutate(sh = round((ind_c / sum(ind_c, na.rm = TRUE)) * 100, 0))
  
  # filter to USAID and make table
  usaid_totals <- agency_totals_2 %>%
    ungroup() %>%
    filter(funding_agency == "USAID") %>%
    select(!c(funding_agency)) %>%
    rename(res = ind_c) %>%
    mutate(res = {clean_number(res)})
  
  usaid_totals$sh <- ifelse(!is.na(usaid_totals$sh), paste0(usaid_totals$sh, "%"), NA)

  
# Create Result/Share table ---------  
  usaid_totals_2 <- usaid_totals %>%
    pivot_wider(
      names_from = "indicator", 
      values_from = c(res, sh),
      names_sep = "_"
    )

  
# Calculate prevalence/incidence with UNAIDS data ---------
  
  ## New infections ----
  new_youth <- unaids_raw %>%
    filter(year == "2023",
           age == "0-14",
           indicator == "Number New HIV Infections") %>%
    select(iso, estimate) %>%
    rename(NI_peds = estimate)
  new_youth$NI_peds <- clean_number(new_youth$NI_peds)
  
  
  new_adults <- unaids_raw %>%
    filter(year == "2023",
           age == "15+",
           indicator == "Number New HIV Infections") %>%
    select(iso, estimate) %>%
    rename(NI_adu = estimate)
  new_adults$NI_adu <- clean_number(new_adults$NI_adu)
  
  ## Prevalence -----
  prev <- unaids_raw %>%
    filter(year == "2023",
           age == "15-49",
           indicator == "Prevalence") %>%
    select(iso, estimate) %>%
    rename(prev = estimate)
  prev$prev <- ifelse(!is.na(prev$prev), paste0(prev$prev, "%"), NA)
  
  ## New infections share ------
  new_inf <- unaids_raw %>%
    filter(year == "2023",
           indicator == "Number New HIV Infections") %>%
    select(iso, indicator, age, estimate)
  
  new_inf_2 <- new_inf %>%
    pivot_wider(names_from = age, values_from = estimate) %>%
    rename(peds = "0-14", adults = "15+")
  
  new_inf_2 <- new_inf_2 %>%
    mutate(Sh_NI_Ped = peds/All,
           Sh_NI_Adu = adults/All)
  
  new_inf_2$Sh_NI_Ped <- ifelse(!is.na(new_inf_2$Sh_NI_Ped),
                       paste0(round(new_inf_2$Sh_NI_Ped * 100, 0), "%"),
                       NA)
  new_inf_2$Sh_NI_Adu <- ifelse(!is.na(new_inf_2$Sh_NI_Adu),
                         paste0(round(new_inf_2$Sh_NI_Adu * 100, 0), "%"),
                         NA)
  
  new_inf_3 <- new_inf_2 %>%
    select(iso, Sh_NI_Ped, Sh_NI_Adu)

  ## Vertical transmission -----
  vt <- vt_raw %>%
    select(Country, "2023") %>%
    rename(country = Country,
           vt = "2023") %>%
    mutate(country = case_when(country == "DRC" ~ "Democratic Republic of the Congo",
                               TRUE ~ country))
  
  # Add a percent sign to each number in the 'value' column
  vt$vt <- ifelse(!is.na(vt$vt), paste0(vt$vt, "%"), NA)
  
  # figure out ISO codes
  vt2 <- vt %>%
    left_join(pepfar_short, by = join_by(country))
  
  vt2 <- vt2 %>%
    select(country_iso, vt) %>%
    rename(iso= country_iso)
  
# Create UNAIDS table --------  
  unaids_table <- new_youth %>%
    full_join(new_adults, by = join_by(iso)) %>%
    full_join(new_inf_3) %>%
    full_join(prev) %>%
    full_join(vt2)
  
  
# Calculate sustainability metrics -----------     
  ad <- ad_raw %>%
    select("Country Name", "Country Code", "2023 [YR2023]") %>%
    rename(age_dep = "2023 [YR2023]",
           country = "Country Name",
           country_iso = "Country Code") %>%
    mutate(age_dep_d = case_when(age_dep > 58.13 ~ "High Burden",
                                             TRUE ~ "Low Burden"),
           age_dep = round(age_dep, 0)) %>%
    select(!country)
  ad$age_dep <- as.character(ad$age_dep)
  
  
  # nurses/midwives data
  nurses <- nurses_raw %>%
    select("Countries, territories and areas", "Year", "Nursing and midwifery personnel (per 10 000 population)") %>%
    rename(country = "Countries, territories and areas", 
           nurses = "Nursing and midwifery personnel (per 10 000 population)")
  
  nurses_2 <- nurses %>%
    group_by(country) %>%
    filter(Year == max(Year)) %>%
    ungroup() %>%
    mutate(nurses = round(nurses, 0))
  
  nurses_2 <- nurses_2 %>%
    mutate(nurses_d = case_when(nurses <= 40 ~ "Below Adequate",
                                nurses > 40 & nurses <= 100 ~ "Moderate",
                                nurses > 100 ~ "Optimal",
                                     TRUE ~ NA_character_)) %>%
    select(!Year) %>%
    mutate(country = case_when(country == "Myanmar" ~ "Burma",
                               country == "Lao People's Democratic Republic" ~ "Laos",
                               country == "United Republic of Tanzania" ~ "Tanzania",
                               country == "Viet Nam" ~ "Vietnam",
                               TRUE ~ country))
  
  # need to combine on year and check for spelling
  nurses_3 <- pepfar_short %>%
    left_join(nurses_2) %>%
    select(!c(operatingunit_iso, country))
  
  nurses_3$nurses <- as.character(nurses_3$nurses)

    # gov expenditure data
  gov <- gov_raw %>%
    select(SpatialDimValueCode, Location, Period, Value) %>%
    rename(country = Location,
           year = Period,
           h_exp = Value)
  
  gov <- gov %>%  
    group_by(country) %>%
    filter(year == max(year)) %>%
    ungroup() %>%
    mutate(h_exp = round(h_exp, 1))
  
  gov <- gov %>%  
    mutate(h_exp_d = case_when(h_exp <= 10 ~ "Low",
                               h_exp > 10 & h_exp <= 15 ~ "Moderate",
                               h_exp > 15 ~ "Optimal")) %>%
    select(!c(year, country)) %>%
    rename(country_iso = SpatialDimValueCode)
  gov$h_exp <- ifelse(!is.na(gov$h_exp), paste0(gov$h_exp, "%"), NA)
  
  
# Create sustainability table ----------
  sus1 <- pepfar_short %>%
    left_join(gov) %>%
    left_join(ad) %>%
    left_join(nurses_3)

  
# JOIN all into mega table and cut down to COP countries -------

# cut them all to COP ous
  budget_cop <- budget_table %>%
    filter(country %in% cop_ous)
  
  results_cop <- usaid_totals_2 %>%
    filter(operatingunit %in% cop_ous) %>%
    rename(country = operatingunit)
  
  unaids_cop <- unaids_table %>%
    left_join(pepfar_short, by = c("iso" = "country_iso")) %>%
    filter(country %in% cop_ous) %>%
    select(!c(iso, operatingunit_iso))
  
  sus_cop <- sus1 %>%
    filter(country %in% cop_ous) %>%
    select(!c(operatingunit_iso))
  
 # combine 
  df <- budget_cop %>%
    full_join(results_cop) %>%
    full_join(unaids_cop) %>%
    full_join(sus_cop)

  df <- df %>%
    select(country, country_iso, everything())
  

# WRITE CSV with "-" for NAs ----------------------------------------------

  df %>% 
    mutate(age_dep_d = str_remove_all(age_dep_d, " Dependency")) %>% 
    write_csv("Dataout/COP_numerical_summary.csv", na = "-")
  