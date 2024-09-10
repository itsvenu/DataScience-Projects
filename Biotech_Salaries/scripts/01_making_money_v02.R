
rm(list = ls())

setwd("~/persProjects/02_bioTech_salaries/scripts")

library(tidyverse)

theme_vt <- function(){
  theme_classic(base_size=14) %+replace%
    theme(axis.ticks.length = unit(0.3, "cm"),
          axis.ticks = element_line(color = 'black'),
          axis.text = element_text(color="black"),
          axis.title = element_text(color="black"),
          legend.text = element_text(color="black"),
          legend.title = element_text(color="black"))
}


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

## Associate’s degree
all_associates <- all_degrees[grepl("Associate", all_degrees)]
all_degrees <- all_degrees[!grepl("Associate", all_degrees)]

## high school
all_highschool <- all_degrees[grepl("High School", all_degrees)]
#all_degrees[!grepl("High School", all_degrees)]

# all_phds
# all_mds
# all_pharmDs
# all_jds
# all_masters
# all_bachelors
# all_associates
# all_highschool

dat <- dat %>% 
  dplyr::mutate(highest_degree = dplyr::case_when(education_level %in% all_phds ~ "PhD",
                                                 education_level %in% all_mds ~ "MD",
                                                 education_level %in% all_pharmDs ~ "PharmD",
                                                 education_level %in% all_jds ~ "JD",
                                                 education_level %in% all_masters ~ "Masters",
                                                 education_level %in% all_bachelors ~ "Bachelors",
                                                 education_level %in% all_associates ~ "Associate",
                                                 education_level %in% all_highschool ~ "High-school")) %>% 
  dplyr::select(-c(education_level))


## manually remove outlier cases

dat_upd <- dat %>% 
  dplyr::arrange(-compensation_annual_base) %>% 
  dplyr::select(id, country, highest_degree, biotech_subindustry:company_size,
                years_of_experience:compensation_annual_base,
                dplyr::everything()) %>% 
  dplyr::select(-c(work_country, closest_bigcity, employee_location))

## remove doubt-full data points with crazy salariews

## 950000 -> 95000
## highest salary in the data 950000/almost a million, 
## who is a postdoc with 5 years of experience. THIS MUST BE 95K

## 810000 -> 81000
## who is a scientist with 7 years of experience with bachelor degree
## this could be 81000

## 650000 -> 65000
## who is a Strain Engineer with 1 year experience with Masters degree
## this could be 65k

dat_upd <- dat_upd %>% 
  dplyr::mutate(compensation_annual_base = sub("950000", "95000", compensation_annual_base),
                compensation_annual_base = sub("810000", "81000", compensation_annual_base),
                compensation_annual_base = sub("650000", "65000", compensation_annual_base))

dat_upd <- dat_upd %>% 
  dplyr::mutate(compensation_annual_base = as.numeric(compensation_annual_base)) 

dat_upd <- dat_upd %>% 
  dplyr::arrange(compensation_annual_base) %>% 
  dplyr::filter(compensation_annual_base > 10000)


# analysis starts ---------------------------------------------------------


# salary distribution -----------------------------------------------------


pdf(paste0(RESULT, "/01_salary_distribution.pdf"), height = 5, width = 8, useDingbats = FALSE)

dat_upd %>% 
  ggplot(aes(x = compensation_annual_base))+
  geom_density(fill = "#3cb44b")+
  theme_vt()+
  scale_x_continuous(breaks = c(50000, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = c("50k", "100k", "200k", "300k", "400k", "500k", "600k"),
                     expand = c(0, 0))+
  xlab("base salary/year")+ylab("")+
  scale_y_continuous(expand = c(0, 0))+
  theme(axis.text.y = element_blank(),
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.y = element_line(linetype = 2, color = "gray90"))+
  geom_vline(xintercept = c(50000, 100000, 200000, 300000),
             linetype = 2, color = "black", alpha = 0.4)

dev.off()

dat_upd %>% 
  dplyr::mutate(salary_bucket = dplyr::case_when(compensation_annual_base <= 50000 ~ "<50k",
                                                 compensation_annual_base > 50000 & compensation_annual_base <= 100000 ~ "50k-100k",
                                                 compensation_annual_base > 100000 & compensation_annual_base <= 200000 ~ "100k-200k",
                                                 compensation_annual_base > 200000 & compensation_annual_base <= 300000 ~ "200k-300k",
                                                 compensation_annual_base > 300000 ~ ">300k")) %>% 
  dplyr::count(salary_bucket) %>% 
  dplyr::mutate(percent = n/sum(n) * 100) %>% 
  dplyr::arrange(percent)
  


dat_upd$compensation_annual_base %>% summary()

# Min. 1st Qu.  Median    Mean 3rd Qu.    Max. 
# 11100   89000  121250  130697  160000  575000

# country vs salary -------------------------------------------------------
req_countries <- dat_upd %>% 
  dplyr::count(country) %>% 
  dplyr::filter(n >= 3) %>% 
  dplyr::pull(country)

dat_countries <- dat_upd %>% 
  dplyr::filter(country %in% req_countries) %>% 
  dplyr::group_by(country) %>% 
  dplyr::summarise(m = median(compensation_annual_base)) %>% 
  dplyr::arrange(-m)


pdf(paste0(RESULT, "/02_country_vs_salary.pdf"),
    height = 7, width = 9, useDingbats = FALSE)

dat_upd %>% 
  dplyr::filter(country %in% req_countries) %>% 
  dplyr::mutate(country = factor(country, levels = rev(dat_countries$country))) %>% 
  merge(dat_countries, by = "country") %>% 
  ggplot(aes(x = compensation_annual_base, y = country, fill = m))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(width = .25, size = 0.9, alpha = 0.3)+
  theme_vt()+
  scale_fill_gradient(low = "white", high = "#3cb44b")+
  scale_x_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = c("0", "50k", "100k", "200k", "300k", "400k", "500k", "600k"),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 2, color = "gray90"))+
   geom_rect(
    aes(ymin = xmin, ymax = xmax),
    data = data.frame(xmin = c(0.5, 2.5, 4.5, 6.5, 8.5), 
                      xmax = c(1.5, 3.5, 5.5, 7.5, 9.5)),
    xmax = Inf,
    xmin = -Inf,
    alpha = 0.1,
    inherit.aes = FALSE)+xlab("base salary/year")+
  ylab("")

dev.off()

# education vs salary -----------------------------------------------------
dat_degree_medians <- dat_upd %>% 
  filter(highest_degree != "JD") %>% 
  dplyr::group_by(highest_degree) %>% 
  dplyr::summarise(m = median(compensation_annual_base)) %>% 
  dplyr::arrange(-m)

pdf(paste0(RESULT, "/03_degree_vs_salary.pdf"), height = 5, width = 8, useDingbats = FALSE)

dat_upd %>% 
  dplyr::filter(highest_degree != "JD") %>% 
  merge(dat_degree_medians, by = "highest_degree") %>% 
  dplyr::mutate(highest_degree = factor(highest_degree, levels = rev(dat_degree_medians$highest_degree))) %>% 
  ggplot(aes(x = compensation_annual_base, y = highest_degree, fill = m))+
  geom_boxplot(outlier.colour = NA)+
  geom_jitter(width = .25, size = 0.9, alpha = 0.3)+
  theme_vt()+
  scale_fill_gradient(low = "white", high = "#3cb44b")+
  scale_x_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = c("0", "50k", "100k", "200k", "300k", "400k", "500k", "600k"),
                     expand = c(0, 0))+
  theme(legend.position = "none",
        axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 2, color = "gray90"))+
  geom_rect(
    aes(ymin = xmin, ymax = xmax),
    data = data.frame(xmin = c(0.5, 2.5, 4.5, 6.5), 
                      xmax = c(1.5, 3.5, 5.5, 7.5)),
    xmax = Inf,
    xmin = -Inf,
    alpha = 0.1,
    inherit.aes = FALSE)+xlab("base salary/year")+
  ylab("")

dev.off()

# experience vs salary ----------------------------------------------------

pdf(paste0(RESULT, "/04_experience_vs_salary.pdf"), 
    height = 5, width = 7, useDingbats = FALSE)

dat_upd %>% 
  #dplyr::filter(grepl("Master|PhD", highest_degree)) %>% 
  ggplot(aes(x = years_of_experience, y = compensation_annual_base))+
  geom_point(color = "#3cb44b", alpha = 0.3, size = 3)+
  ggpubr::stat_cor(size = 5)+
  stat_smooth(method = "lm", color = "black")+
  theme_vt()+
  scale_y_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = c("0", "50k", "100k", "200k", "300k", "400k", "500k", "600k"))+
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35))+
  xlab("work experience (years)")+
  ylab("base salary/year")
  
dev.off()

# masters vs phd - salary -------------------------------------------------

pdf(paste0(RESULT, "/04_degree_experience_vs_salary.pdf"),
    height = 5, width = 7, useDingbats = FALSE)

dat_upd %>% 
  dplyr::filter(grepl("Master|PhD", highest_degree)) %>% 
  ggplot(aes(x = years_of_experience, y = compensation_annual_base))+
  geom_point(aes(color = highest_degree), alpha = 0.5, size = 3)+
  ggpubr::stat_cor(size = 5)+
  stat_smooth(method = "lm", color = "black")+
  theme_vt()+
  scale_y_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000, 600000),
                     labels = c("0", "50k", "100k", "200k", "300k", "400k", "500k", "600k"))+
  scale_x_continuous(breaks = c(0, 5, 10, 15, 20, 25, 30, 35))+
  scale_color_manual(values = c("#4363d8", "#f58231"), name = "degree")+
  xlab("work experience (years)")+
  ylab("base salary/year")

dev.off()


## same experience salary difference - upto 20 years of experience

pdf(paste0(RESULT, "/04_sameExperience_degree_vs_salary.pdf"),
    height = 7, width = 6.5, useDingbats = FALSE)

dat_upd %>% 
  dplyr::filter(grepl("Master|PhD", highest_degree),
                years_of_experience <= 20) %>% 
  dplyr::mutate(years_of_experience = as.character(years_of_experience)) %>% 
  dplyr::mutate(years_of_experience = factor(years_of_experience, 
                                             levels = rev(gtools::mixedsort(unique(years_of_experience))))) %>% 
  dplyr::mutate(highest_degree = factor(highest_degree, levels = c("Masters", "PhD"))) %>% 
  ggplot(aes(x = compensation_annual_base, y = years_of_experience, fill = highest_degree))+
  geom_boxplot(outlier.colour = NA)+
  theme_vt()+
  scale_fill_manual(values = c("#4363d8", "#f58231"), name = "degree")+
  scale_x_continuous(breaks = c(50000, 100000, 200000, 300000),
                     labels = c("50k", "100k", "200k", "300k"))+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major.x = element_line(linetype = 2, color = "gray90"))+
  geom_rect(
    aes(ymin = xmin, ymax = xmax),
    data = data.frame(xmin = seq(0.5, 20.5, 2), 
                      xmax = seq(1.5, 21.5, 2)),
    xmax = Inf,
    xmin = -Inf,
    alpha = 0.1,
    inherit.aes = FALSE)+
  xlab("base salary/year")+
  ylab("work experience (years)")
  
dev.off()

# public/private/govt vs salary -------------------------------------------
dat_companyType <- dat_upd %>% 
  dplyr::group_by(company_type) %>% 
  dplyr::summarise(m = median(compensation_annual_base)) %>% 
  dplyr::arrange(-m)


pdf(paste0(RESULT, "/05_academiaIndustry_vs_salary.pdf"), 
    height = 5, width = 7.5, useDingbats = FALSE)

dat_upd %>% 
  merge(dat_companyType, by = "company_type") %>% 
  dplyr::mutate(company_type = factor(company_type, levels = rev(dat_companyType$company_type))) %>% 
  ggplot(aes(x = compensation_annual_base, y = company_type, fill = m))+
  geom_boxplot()+
  theme_vt()+
  scale_fill_gradient(low = "white", high = "#3cb44b")+
  scale_x_continuous(breaks = c(0, 50000, 100000, 200000, 300000, 400000, 500000),
                     labels = c("0", "50k", "100k", "200k", "300k", "400k", "500k"))+
  theme(axis.line.y = element_blank(),
        axis.ticks.y = element_blank(),
        legend.position = "none",
        panel.grid.major.x = element_line(linetype = 2, color = "gray90"))+
  geom_rect(
    aes(ymin = xmin, ymax = xmax),
    data = data.frame(xmin = seq(0.5, 4.5, 2), 
                      xmax = seq(1.5, 5.5, 2)),
    xmax = Inf,
    xmin = -Inf,
    alpha = 0.1,
    inherit.aes = FALSE)+
  xlab("base salary/year")+
  ylab("academia/industry")
  
dev.off()
  

# company review - sentiment analysis of positive/negative ----------------
dat_upd %>% 
  dplyr::select(id, company_review) %>% 
  dplyr::filter(!is.na(company_review)) %>% 
  dplyr::arrange(company_review) %>% 
  dplyr::mutate(company_review = paste0("rating: ", company_review)) %>% 
  write.table(file = "../results/data_cleaned.txt", 
              sep = "\t", quote = FALSE, row.names = FALSE)


## sentiments were analysed with a 
## multi-lngual transformer model - "lxyuan/distilbert-base-multilingual-cased-sentiments-student"

data_sentiments <- data.table::fread(paste0(RESULT, "/06_company_review_sentiments.txt"))
data_sentiments <- merge(dat_upd, data_sentiments, by = "id")

data_sentiments %>% 
  dplyr::arrange(-negative) %>% View()

data_sentiments %>% 
  dplyr::mutate(salary_bracket = dplyr::case_when(compensation_annual_base <= 100000 ~ "<100k",
                                                  compensation_annual_base > 100000 & compensation_annual_base < 200000 ~ "100k-200k",
                                                  compensation_annual_base >=200000 ~ ">200k")) %>% 
  dplyr::group_by(salary_bracket) %>% 
  dplyr::summarise(positive = mean(positive),
                   nutral = mean(nutral),
                   negative = mean(negative)) 

## most negative employees
  ## company-size
  ## degree
  ## years of experiences etc
  ## salary

## same for most poisitive companies
# 279

data_sentiments_extreme <- data_sentiments %>% 
  dplyr::arrange(-positive) %>% head(n=100) %>%
  dplyr::mutate(opinion = "positive") %>% 
  rbind(data_sentiments %>% 
          dplyr::arrange(-negative) %>% head(n=100) %>% 
          dplyr::mutate(opinion = "negative"))

data_sentiments_extreme <- data_sentiments_extreme %>% 
  dplyr::select(-c(positive, nutral, negative))



# effect of degree on opinion ---------------------------------------------

data_sentiments_extreme %>% 
  dplyr::count(opinion, highest_degree) %>% 
  tidyr::pivot_wider(names_from = "opinion", values_from = "n")

data_sentiments_degrees <- c("PhD", "Masters", "Bachelors")

## are there more people with certain degree in positive comments
## compared to negative ones?

data_sentiments_degrees_res <- NULL

for(i in seq_along(data_sentiments_degrees)){
  
  i_deg = data_sentiments_degrees[i]
  # i_deg = "PhD"
  i_dat <- data_sentiments_extreme %>% 
    dplyr::mutate(f = dplyr::case_when(highest_degree == get("i_deg") ~ "yes",
                                       TRUE ~ "no")) %>% 
    dplyr::select(opinion, f) 
  
  i_counts <- i_dat %>% 
    dplyr::count(opinion, f) %>% 
    tidyr::pivot_wider(names_from = "f", values_from = "n") %>% 
    dplyr::select(opinion, yes, no)
  
  i_counts_mat <- i_counts[match(c("positive", "negative"), i_counts$opinion),] %>% 
    tibble::column_to_rownames(var = "opinion") %>% as.matrix()
  
  #i_counts_mat[4]
  
  i_final_res <- fisher.test(i_counts_mat, alternative = "g") %>% broom::tidy() %>% 
    dplyr::mutate(highest_degree = i_deg,
                  positive_with_degree = i_counts_mat[1],
                  negative_with_degree = i_counts_mat[2],
                  positive_wo_degree = i_counts_mat[3],
                  negative_wo_degree = i_counts_mat[4]) %>% 
    dplyr::select(highest_degree, odds = estimate, p.value, positive_with_degree:negative_wo_degree)
  
  data_sentiments_degrees_res <- rbind(data_sentiments_degrees_res, i_final_res)
}

# effect of company size on opinion ---------------------------------------
data_sentiments_extreme %>% 
  dplyr::count(opinion, company_size) %>% 
  tidyr::pivot_wider(names_from = "opinion", values_from = "n")

data_sentiments_companySize <- data_sentiments_extreme$company_size %>% unique()

data_sentiments_companySize_res <- NULL

for(i in seq_along(data_sentiments_companySize)){
  
  i_deg = data_sentiments_companySize[i]
  # i_deg = "PhD"
  i_dat <- data_sentiments_extreme %>% 
    dplyr::mutate(f = dplyr::case_when(company_size == get("i_deg") ~ "yes",
                                       TRUE ~ "no")) %>% 
    dplyr::select(opinion, f) 
  
  i_counts <- i_dat %>% 
    dplyr::count(opinion, f) %>% 
    tidyr::pivot_wider(names_from = "f", values_from = "n") %>% 
    dplyr::select(opinion, yes, no)
  
  i_counts_mat <- i_counts[match(c("positive", "negative"), i_counts$opinion),] %>% 
    tibble::column_to_rownames(var = "opinion") %>% as.matrix()
  
  #i_counts_mat[4]
  
  i_final_res <- fisher.test(i_counts_mat, alternative = "g") %>% broom::tidy() %>% 
    dplyr::mutate(highest_degree = i_deg,
                  positive_with_degree = i_counts_mat[1],
                  negative_with_degree = i_counts_mat[2],
                  positive_wo_degree = i_counts_mat[3],
                  negative_wo_degree = i_counts_mat[4]) %>% 
    dplyr::select(highest_degree, odds = estimate, p.value, positive_with_degree:negative_wo_degree)
  
  data_sentiments_companySize_res <- rbind(data_sentiments_companySize_res, i_final_res)
}



# effect of experience on opinion -----------------------------------------

pdf(paste0(RESULT, "/07_employeeOpinion_vs_workExperience.pdf"),
    height = 4.5, width = 3.5, useDingbats = FALSE)

data_sentiments_extreme %>% 
  #dplyr::filter(compensation_annual_base <= 300000) %>% 
  ggpubr::ggboxplot(x = "opinion", y = "years_of_experience", fill = "opinion")+
  ggpubr::stat_compare_means(method = "t.test", label = "p.format", size = 4.5)+
  theme_minimal(base_size = 14)+
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        panel.grid = element_line(linetype =2, color = "gray95"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values = c("#3cb44b", "#e6194B"))+
  ylab("work experience (years)")+
  xlab("employee opinion\nof the company")

dev.off()


# effect of salary on opinion -----------------------------------------

pdf(paste0(RESULT, "/07_employeeOpinion_vs_salary.pdf"),
    height = 4.5, width = 3.5, useDingbats = FALSE)

data_sentiments_extreme %>% 
  dplyr::filter(compensation_annual_base <= 300000) %>% 
  ggpubr::ggboxplot(x = "opinion", y = "compensation_annual_base", fill = "opinion")+
  ggpubr::stat_compare_means(method = "t.test", label = "p.format", size = 4.5)+
  theme_minimal(base_size = 14)+
  scale_y_continuous(breaks = c(0, 50000, 100000, 150000, 200000, 250000, 300000),
                     labels = c("$0", "$50,000", "$100,000", "$150,000", "$200,000", "$250,000", "$300,000"))+
  theme(axis.text = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        panel.grid = element_line(linetype =2, color = "gray95"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none")+
  scale_fill_manual(values = c("#3cb44b", "#e6194B"))+
  xlab("employee opinion\nof the company")+
  ylab("base salary/year")

dev.off()

# public/private ----------------------------------------------------------
data_sentiments_extreme %>% 
  dplyr::count(opinion, company_type) %>% 
  tidyr::pivot_wider(names_from = "opinion", values_from = "n")

data_sentiments_companyType <- c("Private", "Public", "Start-up")

data_sentiments_companyType_res <- NULL

for(i in seq_along(data_sentiments_companyType)){
  
  i_deg = data_sentiments_companyType[i]
  # i_deg = "PhD"
  i_dat <- data_sentiments_extreme %>% 
    dplyr::mutate(f = dplyr::case_when(company_type == get("i_deg") ~ "yes",
                                       TRUE ~ "no")) %>% 
    dplyr::select(opinion, f) 
  
  i_counts <- i_dat %>% 
    dplyr::count(opinion, f) %>% 
    tidyr::pivot_wider(names_from = "f", values_from = "n") %>% 
    dplyr::select(opinion, yes, no)
  
  i_counts_mat <- i_counts[match(c("positive", "negative"), i_counts$opinion),] %>% 
    tibble::column_to_rownames(var = "opinion") %>% as.matrix()
  
  #i_counts_mat[4]
  
  i_final_res <- fisher.test(i_counts_mat, alternative = "g") %>% broom::tidy() %>% 
    dplyr::mutate(highest_degree = i_deg,
                  positive_with_degree = i_counts_mat[1],
                  negative_with_degree = i_counts_mat[2],
                  positive_wo_degree = i_counts_mat[3],
                  negative_wo_degree = i_counts_mat[4]) %>% 
    dplyr::select(highest_degree, odds = estimate, p.value, positive_with_degree:negative_wo_degree)
  
  data_sentiments_companyType_res <- rbind(data_sentiments_companyType_res, i_final_res)
}


# does computational people make more money? ------------------------------
# dat_upd %>% 
#   dplyr::select(id, compensation_annual_base, position_description) %>% 
#   dplyr::filter(!is.na(position_description)) %>% 
#   dplyr::mutate(position_description = tolower(position_description)) %>% 
#   dplyr::mutate(computational = dplyr::case_when(grepl("bioinformatic|machine|artificial|data science", position_description) ~ "yes",
#                 TRUE ~ "no")) %>% 
#   dplyr::filter(compensation_annual_base < 300000) 

lm_md <- lm(compensation_annual_base ~ years_of_experience, data = dat_upd)
summary(lm_md)$r.squared * 100


# image -------------------------------------------------------------------

save.image("01_making_money.R.RData")



