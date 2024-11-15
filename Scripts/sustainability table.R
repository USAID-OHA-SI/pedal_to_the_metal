
library(googlesheets4)
library(tidyverse)
library(gt)
library(glamr)

pepfar <- pepfar_country_list
pepfar_short <- pepfar %>%
  select(operatingunit_iso, country, country_iso)

# run ingtable# run in age dependency data - originally from here: https://databank.worldbank.org/source/gender-statistics/Series/SP.POP.DPND#
ad_url <- "1UM1f-DepxmATmmMOUXaPLAuICLsueBA71AZaC7CQXfQ"
ad_raw <- read_sheet(ad_url)

# run in nurses/midwives data
nurses_url <- "1t-iwMadiQU1xrMaOxN2ISUvUl8IGOCIgc1d97nh56fE"
nurses_raw <- read_sheet(nurses_url)

# gov health expenditure, originall here: https://www.who.int/data/gho/data/indicators/indicator-details/GHO/domestic-general-government-health-expenditure-(gghe-d)-as-percentage-of-general-government-expenditure-(gge)
gov_url <- "1X4iVKzhUiG_q5GYE_lU6fCrak7d94N8q1OGFoH5bqro"
gov_raw <- read_sheet(gov_url)

# Age dependency
ad <- ad_raw %>%
  select("Country Name", "Country Code", "2023 [YR2023]") %>%
  rename(age_dependency_ratio = "2023 [YR2023]",
         country = "Country Name",
         country_iso = "Country Code") %>%
  mutate(age_dependency_binary = case_when(age_dependency_ratio > 58.13 ~ "High Dependency",
                                           TRUE ~ "Low Dependency"),
         age_dependency_ratio = round(age_dependency_ratio, 1)) %>%
  select(!country)

# nurses/midwives data
nurses <- nurses_raw %>%
  select("Countries, territories and areas", "Year", "Nursing and midwifery personnel (per 10 000 population)") %>%
  rename(country = "Countries, territories and areas", 
         nurses_per_10k = "Nursing and midwifery personnel (per 10 000 population)")

nurses_2 <- nurses %>%
  group_by(country) %>%
  filter(Year == max(Year)) %>%
  ungroup() %>%
  mutate(nurses_per_10k = round(nurses_per_10k, 0))

nurses_2 <- nurses_2 %>%
  mutate(nurses_binned = case_when(nurses_per_10k <= 40 ~ "Low",
                                   nurses_per_10k > 40 & nurses_per_10k <= 100 ~ "Moderate",
                                   nurses_per_10k > 100 ~ "High",
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


# gov expenditure data
gov <- gov_raw %>%
  select(SpatialDimValueCode, Location, Period, Value) %>%
  rename(country = Location,
         year = Period,
         health_exp = Value)

gov <- gov %>%  
  group_by(country) %>%
  filter(year == max(year)) %>%
  ungroup() %>%
  mutate(health_exp = round(health_exp, 1))

gov <- gov %>%  
  mutate(health_exp_binned = case_when(health_exp <= 10 ~ "Low",
                                       health_exp > 10 & health_exp <= 15 ~ "Moderate",
                                       health_exp > 15 ~ "High")) %>%
  select(!c(year, country)) %>%
  rename(country_iso = SpatialDimValueCode)


# combine into one table
sus1 <- pepfar_short %>%
  left_join(gov) %>%
  left_join(ad) %>%
  left_join(nurses_3)
