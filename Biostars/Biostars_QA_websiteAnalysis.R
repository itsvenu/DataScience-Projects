
## data analysis on Biostars questions 

setwd("/Users/venu/Desktop/work/blog/Biostars")

library(rvest)
library(tidyverse)
library(mondate)
library(RColorBrewer)

## get question, add relevant tags

## function to return a dataframe with
## question and tags & question asked time per-each question for each page

return_questions_tags_df <- function(page_num){
  
  ## base url
  url_part_one <- "https://www.biostars.org/?page="
  url_part_two <- "&sort=update&limit=all%20time&q="
  
  total_url <- paste0(url_part_one, page_num, url_part_two)
  
  ## read url
  url_dat <- read_html(total_url)
  
  ## get questions in this page
  question_in_page <- url_dat %>% 
    html_nodes(".post-title") %>% 
    html_text(trim = TRUE) %>% 
    as.data.frame() %>% 
    dplyr::rename(question = ".") 
  
  ## get tags for each question in this page
  
  tags_str <- url_dat %>% 
    html_nodes(".tags") %>% 
    html_text(trim = TRUE) 
  
  tags_df <- stringr::str_replace(gsub("\\s+", ",", str_trim(tags_str)), "B", "b") %>% 
    as.data.frame() %>% 
    dplyr::rename(tags = ".") 
  
  ## get time of each question in this page
  ## if shows minutes, add today's date
  
  question_date <- url_dat %>% 
    html_nodes(".userlink") %>% 
    html_text(trim = TRUE) %>% 
    {gsub("by.*", "", .)} %>% 
    as.data.frame() %>% 
    dplyr::rename(written_date = ".") 
  
  ## prepare df with all 3 data
  my_df <- cbind(question_in_page, tags_df, question_date) 
  
  return(my_df)
  
}

## total pages in biostars site, 2295

biostars_dat <- NULL

## These are the number of pages when you open incognito mode
## this number of different when you open biostars as a user
## I took the number from incognito mode

all_pages <- 1:2295

for(i in 1:length(all_pages)){
  
  my_res <- return_questions_tags_df(page_num = all_pages[i])
  
  biostars_dat <- rbind(biostars_dat, my_res)
  
  message(" ## Page: ", all_pages[i])
  
}

## figure out exact date relative to today's date

######## add dates ######
## convert written_date column into 
## months format
## and subtract months column from todays date -> we'll get date for every post

## when the data is scraped 23.02.2019
today_date <- Sys.Date()

biostars_dat2 <- biostars_dat %>% 
  dplyr::mutate(written_date = gsub("written ", "", written_date)) %>% 
  dplyr::mutate(num = tidyr::extract_numeric(written_date))

## if written column contains
##  minutes/hours -> today's date
##  days -> num/30 (avg number of days)
##  week -> num / 4
##  months -> keep it as months
##  years -> num * 12

## and finally subtract extra column from todays date

### Feature engineering ###

## todays posts
todays_posts <- biostars_dat2 %>% 
  dplyr::filter(grepl("minute|hour", written_date)) %>% 
  dplyr::mutate(written_date_exact = 0) 

## posts with `days ago` converted to months format
daysTOmonths_posts <- biostars_dat2 %>% 
  dplyr::filter(grepl("day", written_date)) %>% 
  dplyr::mutate(written_date_exact = num / 30)

## posts with `weeks` in it converted to months
weeksTOmonths_posts <- biostars_dat2 %>% 
  dplyr::filter(grepl("week", written_date)) %>% 
  dplyr::mutate(written_date_exact = num / 4) 

## posts with `months` in it
months_posts <- biostars_dat2 %>% 
  dplyr::filter(grepl("month", written_date)) %>% 
  dplyr::mutate(written_date_exact = num) 

## posts with `year` in it
years_posts <- biostars_dat2 %>% 
  dplyr::filter(grepl("year", written_date)) %>% 
  dplyr::mutate(written_date_exact = num * 12)

## combine above 4 dfs into one and create date column

biostars_dat3 <- rbind(todays_posts,
                       daysTOmonths_posts,
                       weeksTOmonths_posts,
                       months_posts,
                       years_posts) %>% 
  dplyr::mutate(date_frmt = mondate(today_date) - written_date_exact) 

## add month name and year
biostars_dat4 <- biostars_dat3 %>% 
  dplyr::mutate(date_frmt = lubridate::as_date(date_frmt)) %>% 
  dplyr::mutate(month_name = lubridate::month(date_frmt, label = TRUE),
                year = lubridate::year(date_frmt))

### Visualization ####

## word cloud of all tags from all years
all_tags_freq <- biostars_dat4 %>% 
  dplyr::pull(tags) %>% 
  as.character() %>% 
  strsplit(split = ",") %>% 
  unlist(recursive = FALSE) %>% 
  tolower() %>% 
  as.data.frame() %>% 
  dplyr::rename(tags = ".") %>% 
  dplyr::count(tags) %>% 
  dplyr::arrange(desc(n)) 


pdf("Biostars_allTagsWordcloud.pdf", height = 6, width = 7)


set.seed(1234)
wordcloud::wordcloud(words = all_tags_freq$tags, freq = all_tags_freq$n,
                     max.words=500, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2"))

dev.off()

### programming langauge questions per-year
### R, python, perl

pl_per_year <- biostars_dat4 %>% 
  dplyr::select(tags, year) %>% 
  dplyr::mutate(all_info = paste0(year, "_", tags))

List <- strsplit(pl_per_year$tags %>% as.character(),  split = ",")

pl_per_year_split <- data.frame(all_info = rep(as.character(pl_per_year$all_info), sapply(List, length)), tags = unlist(List))

pl_per_year_split_count <- pl_per_year_split %>% 
  tidyr::separate(all_info, into = c("year", "allTags"), extra = "merge") %>% 
  dplyr::select(year, tags) %>% 
  dplyr::mutate(tags = tolower(tags)) %>% 
  dplyr::filter(tags == "r" | tags == "python" | tags == "perl") %>% 
  dplyr::group_by(year, tags) %>% 
  dplyr::count(tags) %>% 
  dplyr::arrange(desc(year))

pdf("Biostars_perYear_languageQuestions.pdf", height = 7, width = 11)

ggplot(pl_per_year_split_count, aes(year, n, fill = tags)) +
  geom_bar(stat = "identity", position = "dodge")+
  scale_fill_brewer(palette = "Set1")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        legend.position = c(0.2, 0.9),
        plot.title = element_text(size = 20, face = "bold"))+
  xlab("Year")+
  ylab("number of questions with tag")+
  ggtitle("Biostars: programming language questions per year")

dev.off()

## question per-year

pdf("Biostars_perYear_numberOfQuestions.pdf", height = 7, width = 11)

biostars_dat4 %>% 
  dplyr::count(year) %>% 
  dplyr::mutate(year = factor(year)) %>% 
  ggplot(aes(year, n))+
  geom_bar(stat = "identity")+
  theme(axis.text = element_text(size = 18),
        axis.title = element_text(size = 18),
        legend.title = element_blank(),
        legend.text = element_text(size=14),
        plot.title = element_text(size = 20, face = "bold"))+
  scale_y_continuous(labels = scales::comma)+
  xlab("Year")+
  ylab("number of questions")+
  ggtitle("Biostars: total number of question per-year")

dev.off()

save.image("da_Biostars.R.RData")
