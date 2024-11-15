

clean_number <- function(x, digits = 0){
  dplyr::case_when(x >= 1e9 ~ glue("{round(x/1e9, digits)}B"),
                   x >= 1e6 ~ glue("{round(x/1e6, digits)}M"),
                   x >= 1e3 ~ glue("{round(x/1e3, digits)}K"),
                   TRUE ~ glue("{x}"))
}


# the general financial dataset ----------
financial_filepath <- si_path() %>% 
  return_latest("Financial")
fin_raw <- read_psd(financial_filepath)

head(fin_raw)
table(fin_raw$targeted_beneficiary)
table(fin_raw$program)

# the comprehensive financial dataset -----------

comp_filepath <- si_path() %>% 
  return_latest("Comprehensive")
comp_raw <- read_psd(comp_filepath)

table(comp_raw$initiative_name)



# using general financial dataset ---------------------

# filter to FY23

create_sub_budget_table <- function(country_name) {

# load datasets ---
  
  # the general financial dataset ----------
  financial_filepath <- si_path() %>% 
    return_latest("Financial")
  fin_raw <- read_psd(financial_filepath)
  
  # the comprehensive one with DREAMS data
  comp_filepath <- si_path() %>% 
    return_latest("Comprehensive")
  comp_raw <- read_psd(comp_filepath)
  

# filter data
fin_FY23 <- fin_raw %>%
  filter(fiscal_year == 2024)

fin_FY23 <- fin_FY23 %>%
  mutate(fundingagency2 = case_when(str_detect(fundingagency, "USAID") ~ "USAID",
                                    TRUE ~ fundingagency))

# find Above site programming cost ------

fin_FY23_USAID_ASP <- fin_FY23 %>%
  filter(fundingagency2 == "USAID",
         program == "ASP")

fin_FY23_USAID_ASP_2 <- fin_FY23_USAID_ASP %>%
  group_by(country) %>%
  summarise(sum_asp = sum(cop_budget_total))

country_asp <- fin_FY23_USAID_ASP_2 %>%
  filter(country == "South Africa") %>%
  pull(sum_asp)


# find OVC cost ---------

fin_FY23_USAID_OVC <- fin_FY23 %>%
  filter(fundingagency2 == "USAID",
         targeted_beneficiary == "OVC")

fin_FY23_USAID_OVC_2 <- fin_FY23_USAID_OVC %>%
  group_by(country) %>%
  summarise(sum_ovc = sum(cop_budget_total))

country_ovc <- fin_FY23_USAID_OVC_2 %>%
  filter(country == "South Africa") %>%
  pull(sum_ovc)


# using comprehensive dataset to get DREAMS data ----------

comp <- comp_raw %>%
  mutate(fundingagency2 = case_when(str_detect(fundingagency, "USAID") ~ "USAID",
                                    TRUE ~ fundingagency))

comp_dreams <- comp %>%
  filter(initiative_name == "DREAMS",
         planning_cycle == "COP23",
         fundingagency2 == "USAID")

comp_dreams2 <- comp_dreams %>%
  group_by(country) %>%
  summarise(sum_dreams = sum(cop_budget_total))

country_dreams <- comp_dreams2 %>%
  filter(country == "South Africa") %>%
  pull(sum_dreams)

# DREAMS/OVC/ABS output --------------------
# Combine all into a data frame
budget_table <- data.frame(
  Indicator = c("Above Site", "DREAMS", "OVC"),
  Result = c({clean_number(country_asp)}, {clean_number(country_dreams)}, {clean_number(country_ovc)})
)

return(budget_table)
}

south_africa_sub_budgets <- create_sub_budget_table("South Africa")


# find total COP23 budget ------------------------
create_topline_budget_table <- function(country_name) {

bud_sum_count_agency <- fin_FY23 %>%
  group_by(country, fundingagency2) %>%
  summarise(agency_budget = sum(cop_budget_total)) %>%
  ungroup()

bud_sum_count_agency_usaid <- bud_sum_count_agency %>%
  filter(fundingagency2 == "USAID")

bud_sum_count <- fin_FY23 %>%
  group_by(country) %>%
  summarise(total_budget = sum(cop_budget_total)) %>%
  ungroup()

percent <- bud_sum_count %>%
  full_join(bud_sum_count_agency_usaid)

# this provides the total budget for the country for COP23, USAID's budget, and the percentage
percent <- percent %>%
  filter(!is.na(agency_budget)) %>%
  select(!fundingagency2) %>%
  mutate(percent_USAID = round((agency_budget / total_budget)*100, 0))

# make a table for each country
country_budget <- percent %>%
  mutate(total_budget = {clean_number(total_budget)},
         agency_budget = {clean_number(agency_budget)})

country_budget$percent_USAID <- ifelse(!is.na(country_budget$percent_USAID), paste0(country_budget$percent_USAID, "%"), NA)

country_budget <- country_budget %>%
  filter(country == country_name)

return(country_budget)
}

south_africa_topline_budgets <- create_topline_budget_table("South Africa")
remove(south_africa_topline_budgets)
