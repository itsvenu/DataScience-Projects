
rm(list = ls())

setwd("~/persProjects/02_bioTech_salaries/scripts")

library(tidyverse)

INPUT <- "../data"
RESULT <- "../results"

dat <- readxl::read_xlsx(paste0(INPUT, "/r_biotech salary and company survey.xlsx"), sheet = 1)

# data cleaning -----------------------------------------------------------

# clean colnames
dat <- dat %>% 
  janitor::clean_names() %>%
  dplyr::rename(work_country = what_country_do_you_work_in,
                closest_bigcity = where_is_the_closest_major_city_or_hub,
                employee_location = where_are_you_located,
                biotech_subindustry = biotech_sub_industry,
                company_name = company_or_institution_name,
                company_type = company_details_public_private_start_up_subsidiary_of,
                company_size = company_detail_approximate_company_size,
                title_current_position = role_title_of_current_position,
                education_level = what_degrees_do_you_have,
                education_other_certifications = list_other_relevant_and_recognized_certifications,
                position_description = optional_briefly_describe_your_position,
                compensation_annual_base = compensation_annual_base_salary_pay,
                compensation_recent_raise_percent = compensation_most_recent_annual_yearly_raise_percent)

## remove unnecessary columns

dat <- dat %>% 
  dplyr::select(-c(timestamp, education_other_certifications,
                   compensation_overtime_pay:optional_sign_on_relocation_assistance_total_value,
                   survey_feedback)) %>% 
  dplyr::rename(work_life_balance = optional_work_life_balance_on_average_how_many_hours_do_you_work_per_week,
                company_review = optional_company_review)

## countries
dat <- dat %>% 
  dplyr::mutate(work_country = toupper(work_country),
                employee_location = toupper(employee_location)) 

## create a unified location/country
dat <- dat %>% 
  tibble::rowid_to_column(var = "id") %>% 
  dplyr::mutate(id = paste0("employee_", id)) 

# dim(dat)
# write.table(dat,
#             file = paste0(RESULT, "/data_cleaned.txt"),
#             sep = "\t", quote = FALSE, row.names = FALSE)

## from the survey Q
## follpowing locations are US
us_locations <- c("PHARMA CENTRAL (NY, NJ, PA)",
                  "NEW ENGLAND (MA, CT, RI, NH, VT, ME)",
                  "DC METRO AREA (DC, VA, MD, DE)",
                  "CAROLINAS & SOUTHEAST (FROM NC TO AR, SOUTH FL AND LA)",
                  "MIDWEST (FROM OH TO KS, NORTH TO ND)",
                  "SOUTH & MOUNTAIN WEST (TX TO AZ, NORTH TO MT)",
                  "WEST COAST (CALIFORNIA & PACIFIC NORTHWEST)",
                  "OTHER US LOCATION (HI, AK, PR, ETC.)",
                  "CO", "SAN DIEGO, CA", "REMOTE - US",
                  "RESEARCH TRIANGLE PARK, NC", "BOSTON", "TENNESSEE")

dat <- dat %>% 
  dplyr::mutate(country = dplyr::case_when(grepl("^US|USA|UNITED STATES|UNITED STATES OF AMERICA", work_country) ~ "USA",
                                           grepl("UK|UNITED KINGDOM", work_country) ~ "UK",
                                           TRUE ~ work_country)) %>% 
  
  dplyr::mutate(country_upd = dplyr::case_when(employee_location %in% us_locations ~ "USA",
                                               TRUE ~ employee_location)) %>% 
  dplyr::mutate(p = paste0(country, ", ", country_upd)) %>% 
  dplyr::mutate(p = sub(", NA", "", p),
                p = sub("NA, ", "", p)) %>% 
  dplyr::select(-c(country, country_upd)) %>% 
  dplyr::rename(country = p) 

dat <- dat %>% 
  dplyr::filter(country != "NA") 

dat <- dat %>% 
  dplyr::mutate(country = sub("UNITED KINGDOM AND IRELAND", "UK", country),
                country = sub("NL", "NETHERLANDS", country))

## fix education level
## PhD
## MD
## PharmD
## JD
## Masters
## Bachelors
## Associate degree
## High school


all_degrees <- dat$education_level %>% unique()

## PhD
all_phds <- all_degrees[grepl("PhD", all_degrees)]
all_degrees <- all_degrees[!grepl("PhD", all_degrees)]

## MD
all_mds <- all_degrees[grepl("MD", all_degrees)]
all_degrees <- all_degrees[!grepl("MD", all_degrees)]

## pharmD
all_pharmDs <- all_degrees[grepl("PharmD", all_degrees)]
all_degrees <- all_degrees[!grepl("PharmD", all_degrees)]

## JD
all_jds <- all_degrees[grepl("JD", all_degrees)]
all_degrees <- all_degrees[!grepl("JD", all_degrees)]

## Masters
all_masters <- all_degrees[grepl("Masters", all_degrees)]
all_degrees <- all_degrees[!grepl("Masters", all_degrees)]

## bacherlos
all_bachelors <- all_degrees[grepl("Bachelors", all_degrees)]
all_degrees <- all_degrees[!grepl("Bachelors", all_degrees)]


dat %>% 
  ggpubr::ggboxplot(x = "education_level",
                    y = "compensation_annual_base")+
  ggpubr::rotate_x_text(angle = 45)



# dat %>%
#   dplyr::select(work_country, employee_location) %>% 
#   dplyr::mutate(cc = dplyr::case_when(employee_location %in% us_locations ~ "USA")) 


# company review - sentiment analysis of positive/negative ----------------





# image -------------------------------------------------------------------

save.image("01_making_money.R.RData")



