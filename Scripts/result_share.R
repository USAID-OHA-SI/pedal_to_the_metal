

remotes::install_github("USAID-OHA-SI/mindthegap", ref = "dev_edms_plot")
library(mindthegap)
library(googlesheets4)
library(tidyverse)
library(glue)
library(gagglr)




# load UNAIDS data
unaids_raw <- load_unaids()


# New infections and prevalence ---------------

# Define the function
create_unaids_table <- function(country_name) {
  
  # Filter UNAIDS data for the specified country
  unaids_country <- unaids_raw %>%
    filter(country == country_name)

new_youth <- unaids_country %>%
  filter(year == "2023",
         age == "0-14",
         indicator == "Number New HIV Infections") %>%
  pull(estimate)

new_adults <- unaids_country %>%
  filter(year == "2023",
         age == "15+",
         indicator == "Number New HIV Infections") %>%
  pull(estimate)

prev <- unaids_country %>%
  filter(year == "2023",
         age == "15-49",
         indicator == "Prevalence") %>%
  pull(estimate)

# calculate share
new_inf <- unaids_country %>%
  filter(year == "2023",
         indicator == "Number New HIV Infections") %>%
  select(country, indicator, age, estimate)

new_inf_2 <- new_inf %>%
  pivot_wider(names_from = age, values_from = estimate) %>%
  rename(peds = "0-14", adults = "15+")

new_inf_2 <- new_inf_2 %>%
  mutate(Share_Peds = peds/All,
        Share_Adults = adults/All)

Share_Peds <- ifelse(!is.na(new_inf_2$Share_Peds),
                     paste0(round(new_inf_2$Share_Peds * 100, 1), "%"),
                     NA)
Share_Adults <- ifelse(!is.na(new_inf_2$Share_Adults),
                       paste0(round(new_inf_2$Share_Adults * 100, 1), "%"),
                       NA)


# vertical transmission --------
vt_url <- "1U5K9ppZJvF4aWbVvP2cXeTu6N2Ua6SUOiLxqwcB2f4o"

# Read the data into R
vt_raw <- read_sheet(vt_url)

vt <- vt_raw %>%
  select(Country, "2023") %>%
  rename(vertical_transmission_rate = "2023")


# Add a percent sign to each number in the 'value' column
vt$vertical_transmission_rate <- ifelse(!is.na(vt$vertical_transmission_rate), paste0(vt$vertical_transmission_rate, "%"), NA)

# create number for each country
country_vt <- vt %>%
  filter(Country == country_name) %>%
  pull(vertical_transmission_rate)


# create UNAIDS table ----------------------
# Combine the indicators' names and values into a data frame
unaids_table <- data.frame(
  Indicator = c("New Infections (0-14)", "New Infections (15+)", "Prevalence", "Vertical Transmission Rate"),
  Result = c({clean_number(new_youth)}, {clean_number(new_adults)}, prev, country_vt),
  Share = c(Share_Peds, Share_Adults, NA, NA)
)

return(unaids_table)
}

south_africa_table <- create_unaids_table("South Africa")


# result and share of MER indicators -------------------

options(scipen = 999)

all_ou_filepath <- si_path() %>% 
  return_latest("OU_IM_FY22-25")

global <- read_psd(all_ou_filepath)

# Define the function
create_usaid_MER_totals <- function(country_name) {
  mer_country <- global %>%
    filter(operatingunit == country_name)

mer_country_1 <- mer_country %>%
  filter(indicator %in% c("HTS_TST", "PrEP_NEW", "TX_CURR"),
         fiscal_year == 2024) %>%
  select(operatingunit, country, indicator, funding_agency, standardizeddisaggregate, cumulative)

agency_totals <- mer_country_1 %>%
  group_by(operatingunit, indicator, funding_agency) %>%
  summarise(total_sum = sum(cumulative, na.rm = T)) # confirm this is right with the "Dedup" category - does it subtract?

agency_totals_2 <- agency_totals %>%
  ungroup() %>%
  group_by(operatingunit, indicator) %>%
  mutate(share = round((total_sum / sum(total_sum, na.rm = TRUE)) * 100, 0))

# filter to USAID and make table
usaid_totals <- agency_totals_2 %>%
  ungroup() %>%
  filter(funding_agency == "USAID") %>%
  select(!c(funding_agency, operatingunit)) %>%
  rename(Result = total_sum) %>%
  mutate(Result = {clean_number(Result)})

usaid_totals$share <- ifelse(!is.na(usaid_totals$share), paste0(usaid_totals$share, "%"), NA)


return(usaid_totals)
}

# Example usage
south_africa_MER_totals <- create_usaid_MER_totals("South Africa")

