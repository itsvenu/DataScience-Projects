
## COVID-19 - An exploratory data analysis!

## An explorative analysis on 
## COVID-19 data
## data available for download: https://www.kaggle.com/imdevskp/corona-virus-report

## analysis steps

## 1. fraction of total = affected + death + recoevred
## 2. visualize over countries
## 3. visulize which country has the most cases (3a)
## 3a. affected countries - continous scale color (by numbers)
## 4. increase in cases per-day, in each country
## 5. which weekday most cases were reported on an average for each country
## 6. calculate how dangerous each country: https://www.quora.com/How-dangerous-is-the-coronavirus-6/answer/Richard-Muller-3



setwd("~/Desktop/work/blog/COVID-19")

library(tidyverse)
library(ggforce)
library(ggrepel)

dat <- readr::read_csv("covid_19_clean_complete.csv")

# 1. number of detected cases in each country -----------------------------

num_cases_percountry <- dat %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  dplyr::mutate(Total = Confirmed+Deaths+Recovered) %>% 
  dplyr::group_by(`Country/Region`) %>% 
  dplyr::summarise(SUM = sum(Total)) %>% 
  dplyr::arrange(desc(SUM))


## split into 3 figures

## chaina - 1855018
## all others - 8713-1000
## 1000-100
## < 100 cases

num_cases_percountry %>% 
  dplyr::mutate(y = SUM/sum(SUM))

num_cases_percountry$`Country/Region` <- factor(num_cases_percountry$`Country/Region`, levels = rev(num_cases_percountry$`Country/Region`))

num_cases_percountry <- num_cases_percountry %>% 
  dplyr::mutate(Group = dplyr::case_when(SUM == 1855018 ~ "n=18,55,018",
                                         SUM < 9000 & SUM >= 1000 ~ "n= <9k & > 1k",
                                         SUM < 1000 & SUM >= 100 ~ "n= <1k & > 100",
                                         SUM < 100 ~ "n= < 100"))
  
## arrange for facets
num_cases_percountry$Group <- factor(num_cases_percountry$Group, levels = c("n=18,55,018", "n= <9k & > 1k", "n= <1k & > 100", "n= < 100"))

# num_cases_percountry_plot <- 
num_cases_percountry_plot <- ggplot(num_cases_percountry, aes(x = `Country/Region`, y = SUM, fill = SUM))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_c(direction = -1, name = "number of total cases",
                       limits = c(1, 10000),
                       oob = scales::squish,
                       breaks = c(1000, 2000, 4000, 6000, 8000, 10000),
                       labels = c("<1k", "2k", "4k", "6k", "8k", "10k"),
                       guide = guide_colourbar(barwidth=1.3, barheight = 20,
                                               label.position="right",
                                               title.position = "right",
                                               title.theme = element_text(angle = 90, size = 18, family = "Seravek", hjust = 0.5)))+
  theme_minimal(base_size = 13, base_family = "Seravek")+
  scale_y_continuous(labels = scales::comma)+
  theme(axis.text = element_text(color = "black", family = "Seravek"),
        axis.title = element_text(color = "black", family = "Seravek"),
        legend.title = element_text(color = "black", family = "Seravek"),
        legend.text = element_text(color = "black", family = "Seravek"),
        panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        strip.background = element_rect(fill = "gray", color = NA),
        plot.title = element_text(size = 18))+
  facet_wrap(.~Group, scales = "free")+
  ylab("number of cases")+
  coord_flip()+
  ggtitle("Number of people affected by COVID-19 (22nd Jan, 2020 - 27th Feb, 2020)")
  

ggsave(file = "01_COVID_numberOfCasesPerCountry.pdf", num_cases_percountry_plot,
       height = 8, width = 13, cairo_pdf)

# 2. visualize number of cases on world map ----------------------------------
# here visualize number of cases on world map using lattitue and longitude coordinates

## country names
dat_countries <- dat %>% 
  dplyr::group_by(`Country/Region`) %>% 
  dplyr::group_modify(~head(.x, 1L)) %>% 
  dplyr::select(`Country/Region`, Lat, Long)

## keep only top10 countries with most cases
dat_countries <- num_cases_percountry %>% 
  head(n=10) %>% 
  dplyr::select(`Country/Region`) %>% 
  merge(dat_countries, by = "Country/Region")


map_world <- map_data("world")
top10_countries_plot <- ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = dat, 
             aes(x = Long, y = Lat, color = "red"), size = 0.9)+
  geom_label_repel(data = dat_countries, aes(label = `Country/Region`,
                x = Long, y = Lat), color = "black",
                segment.colour = "darkgray")+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(legend.position = "none",
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.grid.minor = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  ggtitle("Top 10 countries with highest number of affected people")

ggsave(filename = "02_COVID_Top10AffectedCountry.pdf", top10_countries_plot,
       height = 4, width = 7, cairo_pdf)

# 3. number of cases in each country on a world map -----------------------

## country coordinates
## add number of cases in each country
## keep 0 countries as well
## plot on a map

num_cases_percountry2 <- num_cases_percountry

num_cases_percountry2$`Country/Region` <- as.character(num_cases_percountry2$`Country/Region`)
num_cases_percountry2$`Country/Region`[num_cases_percountry2$`Country/Region` == "Mainland China"] <- "China"

world_affected_countries <- merge(num_cases_percountry2, map_world, by.x = "Country/Region", by.y = "region")

ggplot(world_affected_countries, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = SUM ), color = "white")+
  scale_fill_viridis_c(option = "C")


countries_cases_affected <- merge(num_cases_percountry, map_world, by.x = "Country/Region", by.y = "region") %>% 
  dplyr::select(`Country/Region`, SUM, long, lat, group, order) 

all_countries_cases_affected <- setdiff(map_world$region, countries_cases_affected$`Country/Region`) %>% 
  as.data.frame() %>% 
  dplyr::rename(region = ".") %>% 
  merge(map_world, by = "region") %>% 
  dplyr::rename(`Country/Region` = region) %>% 
  dplyr::mutate(SUM = 0) %>% 
  dplyr::select(`Country/Region`, SUM, long, lat, group, order) %>% 
  rbind(countries_cases_affected) 

ggplot(countries_cases_affected, aes(x = long, y = lat, group = group))+
  geom_polygon(aes(fill = SUM ), color = "white")+
  scale_fill_viridis_c(option = "C")

# 4. fraction of diagnosis, deaths, recovered in affected countries -------
# why estimate = deaths/(deaths+recovered)

fatality_rate_countries <- dat %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  dplyr::group_by(`Country/Region`) %>% 
  dplyr::summarise_all(.funs = sum) %>%
  dplyr::mutate(`Fatality rate` = Deaths/(Deaths+Recovered))  
  
fatality_rate_countries$`Fatality rate`[is.na(fatality_rate_countries$`Fatality rate`)] <- 0

fatality_rate_countries <- fatality_rate_countries %>% 
  dplyr::arrange(desc(`Fatality rate`)) %>% 
  head(n=10)

fatality_rate_countries$`Country/Region` <- factor(fatality_rate_countries$`Country/Region`, levels = rev(fatality_rate_countries$`Country/Region`))

fatality_rate_countries_plot <- ggplot(fatality_rate_countries, aes(x = `Country/Region`, y = `Fatality rate`, fill = `Fatality rate`))+
  geom_bar(stat = "identity")+
  scale_fill_viridis_c(option = "C", direction = -1)+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))+
  coord_flip()+
  ggtitle("Fatality rate of affected\ncountries (Deaths/(Deaths+Recovered))")

ggsave(filename = "03_COVID_FatalityRateAffectedCountries.pdf", fatality_rate_countries_plot,
       height = 4, width = 6, cairo_pdf)

## global fatality rate
global_confirmed <- dat %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  dplyr::mutate(Total = Confirmed+Deaths) %>% 
  dplyr::pull(Total) %>% sum

global_deaths <- dat %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  dplyr::pull(Deaths) %>% sum

global_deaths/global_confirmed

# 5. fraction of confirmations, deaths, recovered for each country ------------------------

fraction_conf_death_reco <- dat %>% 
  dplyr::select(`Country/Region`, Confirmed, Deaths, Recovered) %>% 
  dplyr::group_by(`Country/Region`) %>% 
  dplyr::summarise_all(.funs = sum) %>% 
  dplyr::mutate(Total = Confirmed+Deaths+Recovered) %>% 
  dplyr::mutate(Confirmed = Confirmed/Total,
                Deaths = Deaths/Total,
                Recovered = Recovered/Total) %>% 
  dplyr::arrange(desc(Total)) %>%
  dplyr::select(-c(Total))

fraction_conf_death_reco$`Country/Region` <- factor(fraction_conf_death_reco$`Country/Region`, levels = rev(fraction_conf_death_reco$`Country/Region`))
fraction_conf_death_reco_mlt <- fraction_conf_death_reco %>% 
  reshape2::melt() 

fraction_conf_death_reco_mlt$variable <- factor(fraction_conf_death_reco_mlt$variable, levels = rev(c("Confirmed", "Recovered", "Deaths")))

fraction_conf_death_reco_plot <- ggplot(fraction_conf_death_reco_mlt, aes(x = `Country/Region`, y = value, fill = variable))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("Confirmed" = "#911eb4",
                               "Recovered" = "#3cb44b",
                               "Deaths" = "red"))+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(panel.grid = element_blank(),
        legend.title = element_blank(),
        legend.position = "bottom",
        axis.text = element_text(color = "black"),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5))+
  guides(fill=guide_legend(nrow=1,byrow=TRUE))+
  scale_y_continuous(expand = c(0, 0))+
  ylab("")+
  coord_flip()+
  ggtitle("Statistics of total cases")

ggsave(filename = "04_COVID_TotalCasesStats.pdf", fraction_conf_death_reco_plot,
       height = 8, width = 5, cairo_pdf)


# 6. increase in number of cases each day ---------------------------------
# only in china & average of other countries

dat_date_centered <- dat %>% 
  dplyr::select(`Country/Region`,
                Date, Confirmed) %>% 
  dplyr::mutate(Date = lubridate::as_date(lubridate::mdy(Date)))

## per-day cases only in china
dat_date_centered_perday <- dat_date_centered %>% 
  dplyr::filter(`Country/Region` == "Mainland China") %>% 
  dplyr::group_by(Date) %>% 
  dplyr::summarise(`China confirmed` = sum(Confirmed))


china_perday_plot <- ggplot(dat_date_centered_perday, aes(x = Date, y = `China confirmed`, color = `China confirmed`))+
  geom_line(size = 0.9)+
  geom_point(size = 2.5)+
  scale_color_viridis_c(direction = -1, option = "C", name = "Confirmed cases")+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  scale_y_continuous(labels = scales::comma)+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5),
        legend.position = "none")+
  ylab("number of cases")+xlab("date")+
  ggtitle("China: 500 to 78.5k COVID-19 cases in 37 days")
  
ggsave(filename = "05_COVID_ChinaPerDayCases.pdf", china_perday_plot,
       height = 4, width = 6.5, cairo_pdf)


# 7. on which day most cases were diagnosed -------------------------------
dat_date_centered_perday2 <- dat_date_centered_perday
dat_date_centered_perday2$Day <- weekdays(dat_date_centered_perday2$Date)
dat_date_centered_perday2$Day <- factor(dat_date_centered_perday2$Day, levels= c("Sunday", "Monday", 
                                                  "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday"))

dat_date_centered_perday2_plot <- ggplot(dat_date_centered_perday2, aes(x = Day, y = `China confirmed`, fill = Day))+
  geom_boxplot()+
  scale_fill_brewer(palette = "Dark2")+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        axis.text = element_text(color = "black"),
        plot.title = element_text(hjust = 0.5))+
  scale_x_discrete(position = "top")+
  scale_y_continuous(labels = scales::comma)+
  xlab("")+ylab("number of diagnosed cases")+
  ggtitle("Weekdays and COVID-19 diagnosis in China")

ggsave(filename = "06_WeekDayDiagnosisChina.pdf", dat_date_centered_perday2_plot,
       height = 4, width = 7.5, cairo_pdf)

## male & female & age associations
## data is from paper: https://www.thelancet.com/journals/landig/article/PIIS2589-7500(20)30026-1/fulltext

## female, male
## age groups
## worldCloud of symptoms

## sympton_onset -> hospital_visit date distribution
## how many days they took to go to hospital

l_dat <- readxl::read_xlsx("CoV_Lancet_DigitalHealth_Sun_Viboud.xlsx")


# 8. number of male & female cases ----------------------------------------
l_dat_male_female <- l_dat %>% 
  dplyr::count(gender) %>% 
  dplyr::arrange(desc(n))

l_dat_male_female$gender <- factor(l_dat_male_female$gender,
                                   levels = rev(l_dat_male_female$gender))

l_dat_male_female_plot <- ggplot(l_dat_male_female, aes(x = gender, y = n, fill = n))+
  geom_bar(stat = "identity")+
  coord_flip()+
  geom_text(aes(label = n), vjust = 0.5, hjust = -0.1, size = 4)+
  scale_fill_viridis_c(option = "B", direction = -1)+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(panel.grid.minor = element_blank(),
        panel.grid.major.y = element_blank(),
        axis.text = element_text(color = "black"),
        legend.position = "none",
        plot.title = element_text(hjust = 0.5))+
  ylab("number of cases")+xlab("Gender")+
  ggtitle("number of Male & Female cases in China")

ggsave(filename = "07_COVID_numberOfMaleFemaleCases.pdf", l_dat_male_female_plot,
       height = 4, width = 7, cairo_pdf)

# 9. affected age groups --------------------------------------------------
# 1-10; 10-20; 20-30; 30-40; 40-50; 50-60; 70-80; 80-90

l_dat_aged <- l_dat %>% 
  dplyr::select(gender, age) %>% 
  dplyr::filter(age != "NA") %>% 
  dplyr::mutate(age = as.numeric(age)) %>% 
  dplyr::arrange(age)

l_dat_aged_groups <- l_dat_aged %>% 
  dplyr::mutate(age_group = dplyr::case_when(age >= 1 & age <=10 ~ "1-10y",
                                             age > 10 & age <=20 ~ "10-20y",
                                             age > 20 & age <=30 ~ "20-30y",
                                             age > 30 & age <=40 ~ "30-40y",
                                             age > 40 & age <=50 ~ "40-50y",
                                             age > 50 & age <=60 ~ "50-60y",
                                             age > 70 & age <=80 ~ "60-70y",
                                             age > 80 & age <=90 ~ "80-90y"))

l_dat_aged_groups_clean <- l_dat_aged_groups %>% 
  dplyr::mutate(xx = paste0(gender, " ", age_group)) %>% 
  dplyr::count(xx) %>% 
  tidyr::separate(xx, into = c("gender", "age_group"), sep = " ") %>% 
  dplyr::filter(gender != "NA")
  

l_dat_aged_groups_clean_plot <- ggplot(l_dat_aged_groups_clean, aes(x = age_group, y = n, fill = gender))+
  geom_bar(stat = "identity")+
  scale_fill_manual(values = c("violet", "maroon"))+
  facet_grid(gender~., scales = "free")+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(axis.text = element_text(color = "black"),
        panel.grid.minor = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "none",
        strip.background = element_rect(fill = "gray", color = NA))+
  ylab("number of cases")+xlab("age group")
  
ggsave(filename = "08_COVID_MaleFemaleAgeGroupCases.pdf", l_dat_aged_groups_clean_plot,
       height = 4, width = 8, cairo_pdf)

# 10. number of days between symp_onset & host_visit ----------------------

## something wrong with l_dat columns
## directly exported above 2 columns from the xlsx file

## 348 patients data
l_dat_dates <- read.delim("SymptomOnset_HospVisit_dates.csv", sep = ";", header = TRUE)
l_dat_dates <- l_dat_dates %>% 
  dplyr::filter(symptom_onset != "NA" & hosp_visit_date != "NA") 

## format dates
l_dat_dates$symptom_onset <- lubridate::as_date(lubridate::dmy(l_dat_dates$symptom_onset))
l_dat_dates$hosp_visit_date <- lubridate::as_date(lubridate::dmy(l_dat_dates$hosp_visit_date))
l_dat_dates$days_apart <- difftime(l_dat_dates$hosp_visit_date, l_dat_dates$symptom_onset, units = c("days")) %>% 
  as.character() %>% as.numeric()


l_dat_dates_hos_visit_plot <- ggplot(l_dat_dates, aes(x = days_apart))+
  geom_density(fill = "#808000", color = "#808000")+
  theme_minimal(base_size = 14, base_family = "Seravek")+
  theme(panel.grid.minor = element_blank(),
        axis.text = element_text(color = "black"),
        axis.text.y = element_blank(),
        plot.title = element_text(hjust = 0.5))+
  geom_vline(xintercept = c(10, 5), linetype = 2)+
  annotate("text", x = 12, y = 0.21, label = "before 10th Jan, 2020", color = "red", family = "Seravek")+
  annotate("text", x = 7, y = 0.21, label = "before 16th Jan, 2020", color = "red", family = "Seravek")+
  annotate("text", x = 2, y = 0.21, label = "after 16th Jan, 2020", color = "red", family = "Seravek")+
  ylab("")+xlab("number of days apart")+
  ggtitle("number of days between onset of symptoms\nand first hospital visit")

ggsave(filename = "09_COVID_OnsetSymptomsFirstHospitalVisit.pdf", l_dat_dates_hos_visit_plot,
       height = 5, width = 7, cairo_pdf)


# 11. word cloud of symptoms ----------------------------------------------

l_dat_symptoms <- l_dat %>% 
  dplyr::select(symptom) %>% unique() %>% 
  dplyr::filter(symptom != "NA") %>% 
  dplyr::pull(symptom)

## some spelling correactions
l_dat_symptoms[1] <- "fever, cough, difficult in breathing"
l_dat_symptoms[22] <- "fever"

all_symptoms <- c()

for(i in seq_along(l_dat_symptoms)){
  
  i_symp <- l_dat_symptoms[i]
  i_symp_clean <- stringr::str_split(i_symp, pattern = ",")[[1]] %>% 
    stringr::str_squish()
  
  all_symptoms <- c(all_symptoms, i_symp_clean)
}

all_symptoms_df <- all_symptoms %>% 
  as.data.frame() %>% 
  dplyr::rename(symptom = ".") %>% 
  dplyr::count(symptom)

pdf("10_COVID_SymptomsWordCloud.pdf", height = 3, width = 4)

set.seed(1234567)
wordcloud::wordcloud(words = all_symptoms_df$symptom, 
          freq = all_symptoms_df$n, 
          min.freq = 1,
          max.words = 200, 
          random.order = FALSE, 
          rot.per=0.35,
          colors = RColorBrewer::brewer.pal(8, "Dark2"))

dev.off()


# end ---------------------------------------------------------------------

# save.image("COVID-19-EDA.R.RData")
load("COVID-19-EDA.R.RData")

