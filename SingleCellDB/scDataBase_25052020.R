
## Asking/Creating questions from a spread sheet!

# EDA on single-cell database
# https://docs.google.com/spreadsheets/d/1En7-UV0k0laDiIfjFkdn7dggyR7jIk3WH8QgXaMOZF0/edit#gid=0
# Downloaded on 25-05-2020
# https://twitter.com/vallens/status/1263477436513206273

setwd("~/Desktop/work/blog/DataScience-Projects/SingleCellDatabase")

library(tidyverse)
library(extrafont)
library(ggbump)
library(grid)
library(gridExtra)

## custom theme
theme_vt <- function(){ 
  theme_minimal(base_size=14, base_family="Seravek") %+replace% 
    theme(axis.text = element_text(color = "black"))
}


## Questions
# 0. Year ~ number of published papers
# 1. Journal over-representation?
# 2. Single-cell in title?
# 3. number of cells ~ Journal ?
# 4. Technique ~ according to year?
# 5. Most studied tissue?
# 6. studied tissue with technique?
# 7. number of authors ~ Journal? IF?
# 8. number of authors ~ number of cells?
# 9. number of authors ~ year?
# 10. number of cells ~ Organism?
# 11. 

dat <- readxl::read_xlsx("Single cell studies database.xlsx", sheet = 1)

# Total observations - 1017

# 0. Year ~ number of published papers ------------------------------------
sc_year_studies <- dat %>% 
  dplyr::select(Date) %>% 
  dplyr::mutate(Year = gsub("-.*", "", Date)) %>% 
  dplyr::count(Year) 

sc_year_studies <- sc_year_studies %>% 
  rbind(data.frame(Year = c(2003, 2007), n = c(0, 0))) %>% 
  dplyr::arrange(Year)

plt_sc_year_studies <- ggplot(sc_year_studies, aes(x = Year, y = n))+
  geom_bar(stat = "identity", fill = "black")+
  theme_vt()+
  scale_y_continuous(expand = expand_scale(mult = 0))+
  theme(panel.grid = element_blank(),
        plot.caption = element_text(color = "#EC0108"),
        plot.title = element_text(hjust = 0.5))+
  geom_hline(yintercept = c(0, 100, 200, 300), linetype = 2, color = "gray")+
  ylab("number of published studies")+
  coord_flip()+
  labs(caption = "@itsvenu_")+ggtitle("Yearly published studies")

ggsave(filename = "01_plt_sc_year_studies.pdf", 
       plt_sc_year_studies, height = 4, width = 5.5,
       cairo_pdf)

# 02 Journal over-representation ------------------------------------------
# How many times each journal is represented

dat$Journal %>% unique()
# Proceedings of the National Academy of Sciences - PNAS
# Proc Natl Acad Sci USA - PNAS

# Nucleic Acids Research - NAR

# dat %>% 
#   dplyr::select(Journal) %>% 
#   dplyr::filter(grepl("Proc", Journal))

journal_nums <- dat %>% 
  dplyr::select(Journal)

journal_nums$Journal[journal_nums$Journal == "Proceedings of the National Academy of Sciences"] <- "PNAS"
journal_nums$Journal[journal_nums$Journal == "Proc Natl Acad Sci USA"] <- "PNAS"

journal_nums$Journal[journal_nums$Journal == "Nucleic Acids Research"] <- "NAR"

journal_nums <- journal_nums %>% 
  dplyr::count(Journal) %>% 
  dplyr::arrange(desc(n)) %>% 
  head(n=50) %>% 
  dplyr::arrange(n)

journal_nums$Journal <- factor(journal_nums$Journal, levels = journal_nums$Journal)

plt_journal_nums <- ggplot(journal_nums, aes(x = Journal, y = n))+
  geom_bar(stat = "identity", fill = "black")+
  theme_vt()+
  scale_y_continuous(expand = expand_scale(mult = 0))+
  geom_hline(yintercept = c(0, 50, 100), linetype = 2, color = "gray")+
  theme(panel.grid = element_blank(),
        axis.text.y = element_text(size = 8),
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "#EC0108"))+
  ylab("number of published studies")+
  coord_flip()+
  ggtitle("Top 50 journals")+
  labs(caption = "@itsvenu_")

ggsave(filename = "02_journal_nums.pdf", plt_journal_nums,
       height = 5.5, width = 6.5, cairo_pdf)

# 03 single-cell in title? ------------------------------------------------
# how many papers have single cell in title?
# If yes, what journal is more represented? 

dat$Journal[dat$Journal == "Proceedings of the National Academy of Sciences"] <- "PNAS"
dat$Journal[dat$Journal == "Proc Natl Acad Sci USA"] <- "PNAS"

dat$Journal[dat$Journal == "Nucleic Acids Research"] <- "NAR"


sc_in_title <- dat %>% 
  dplyr::select(Title, Journal) %>% 
  dplyr::mutate(`single-cell in title` = dplyr::case_when(grepl("Single|single", Title) ~ "Yes",
                                                          !grepl("Single|single", Title) ~ "No")) 
sc_in_title <- sc_in_title %>% 
  dplyr::count(`single-cell in title`) %>% 
  dplyr::mutate(TotalSample = sum(n)) %>% 
  dplyr::mutate(Fraction = n/TotalSample)

plt_sc_in_title <- ggplot(sc_in_title, aes(x = `single-cell in title`, y = Fraction))+
  geom_bar(stat = "identity", fill = "black")+
  geom_label(aes(label = n),vjust = 0.5, hjust = 1.5, size = 4, font = "Seravek")+
  theme_vt()+
  theme(plot.title = element_text(hjust = 0.5),
        panel.grid = element_blank())+
  geom_hline(yintercept = c(0, 0.2, 0.4, 0.6), linetype = 2, color = "gray")+
  scale_y_continuous(expand = c(0, 0))+
  coord_flip()+
  ylab("fraction of papers")+xlab("")+
  ggtitle("Does title contain \"single-cell\"?")

ggsave(filename = "03_plt_sc_in_title.pdf", plt_sc_in_title,
       height = 2, width = 4, cairo_pdf)


# 04 if yes, which journals frequetly published these studies? ------------
sc_in_title_journal <- dat %>% 
  dplyr::select(Title, Journal) %>% 
  dplyr::mutate(`single-cell in title` = dplyr::case_when(grepl("Single|single", Title) ~ "Yes",
                                                          !grepl("Single|single", Title) ~ "No")) 


## n - yes; n- no for each journal
sc_in_title_journal_yesno <- sc_in_title_journal %>% 
  dplyr::mutate(tt = paste0(Journal, "_", `single-cell in title`)) %>% 
  dplyr::count(tt) %>% 
  dplyr::arrange(desc(n)) %>% 
  tidyr::separate(col = tt, into = c("Journal", "Single-cell in title"), sep = "_") 

sc_in_title_journal_yesno_fractions <- sc_in_title_journal_yesno %>% 
  reshape2::dcast(Journal~`Single-cell in title`, value.var = "n") %>% 
  merge(journal_nums, by = "Journal") 
  
sc_in_title_journal_yesno_fractions[is.na(sc_in_title_journal_yesno_fractions)] <- 0

sc_in_title_journal_yesno_fractions_mlt <- sc_in_title_journal_yesno_fractions %>% 
  dplyr::mutate(Yes_frac = Yes/n,
                No_frac = No/n) %>% 
  dplyr::arrange(desc(n)) %>% head(n=15) %>% 
  dplyr::arrange(n) %>% 
  dplyr::select(1,5,6) %>%
  reshape2::melt() %>% 
  dplyr::mutate(variable = gsub("_frac", "", variable)) %>% 
  dplyr::rename(`Single-cell in title` = variable) 
  
sc_in_title_journal_yesno_fractions_mlt$Journal <- factor(sc_in_title_journal_yesno_fractions_mlt$Journal, levels = unique(sc_in_title_journal_yesno_fractions_mlt$Journal))

sc_in_title_journal_yesno_fractions_plt <- ggplot(sc_in_title_journal_yesno_fractions_mlt, aes(x = Journal, y = value, fill = `Single-cell in title`))+
  geom_bar(stat = "identity")+
  theme_vt()+
  theme(plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "#EC0108"))+
  scale_fill_manual(values = c("maroon", "blue"))+
  ylab("fraction of papers")+
  coord_flip()+
  labs(caption = "@itsvenu_")+
  ggtitle("`Single-cell` in paper title?")

ggsave(filename = "04_sc_in_title_top15_journals.pdf", sc_in_title_journal_yesno_fractions_plt,
       height = 4, width = 6, cairo_pdf)


# 05 number of cells and Journal? -----------------------------------------
# top 15 journals, boxplot; y-number of cells, x-journal name
journal_numcells <- dat %>% 
  dplyr::select(Journal, `Reported cells total`) %>% 
  merge(sc_in_title_journal_yesno_fractions_mlt %>% 
          dplyr::select(1) %>% unique(), by = "Journal") %>% 
  dplyr::filter(`Reported cells total` != "NA") %>% 
  dplyr::mutate(`Reported cells total` = log2(`Reported cells total` + 1))

## order journals according to median

plt_journal_numcells <- ggplot(journal_numcells, 
       aes(x = reorder(Journal, `Reported cells total`, FUN = median), 
           y = `Reported cells total`, fill = Journal))+
  geom_boxplot(alpha = 0.8)+
  scale_fill_manual(values = unname(pals::alphabet(n = 15)))+
  xlab("Journal")+ylab("Reported cells total (Log2)")+
  coord_flip()+
  theme_vt()+
  theme(legend.position = "none",
        plot.title = element_text(hjust = 0.5),
        plot.caption = element_text(color = "#EC0108"))+
  ggtitle("Journal vs number of cells in a study")+
  labs(caption = "@itsvenu_")

ggsave(filename = "05_journal_numcells.pdf", plt_journal_numcells,
       height = 4.5, width = 5.5, cairo_pdf)


# 06 technique according to year ------------------------------------------
## rankplot according to year
## top 5 techniques in 2019
## plot these guys status in last 10 years

year_tech <- dat %>% 
  dplyr::select(Date, Title, Technique) %>%
  dplyr::mutate(Year = gsub("-.*", "", Date)) %>% 
  dplyr::mutate(YT = paste0(Year, "_", Technique)) %>% 
  dplyr::count(YT) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(YT != "2019_NA") %>% 
  tidyr::separate(YT, into = c("Year", "Technique"), sep = "_") %>% 
  dplyr::mutate(Year = as.numeric(Year)) %>% 
  dplyr::filter(Year >= 2010) 

dat %>% 
  dplyr::select(Date, Title, Technique) %>%
  dplyr::mutate(Year = gsub("-.*", "", Date)) %>% 
  dplyr::mutate(YT = paste0(Year, "_", Technique)) %>% 
  dplyr::count(YT) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(grepl("InDrops", YT))
  #dplyr::filter(YT != "2019_NA") %>% 
  #dplyr::filter(grepl("2019", YT))


# Chromium
# Smart-seq2
# Drop-seq
# SMARTer (C1)
# InDrops 

# rank of each of the above techs in each year
# based on publications
top5_2019_techs <- year_tech %>% 
  dplyr::filter(grepl("^Chromium$|^Smart-seq2$|^Drop-seq$|^SMARTer \\(C1\\)$|^InDrops$", Technique)) %>% 
  dplyr::arrange(desc(Year)) 

top5_2019_tech_ranks <- top5_2019_techs %>% 
  dplyr::group_by(Year) %>% 
  dplyr::mutate(Rank = order(n, decreasing = TRUE)) %>% 
  dplyr::ungroup()

top5_2019_tech_ranks <- top5_2019_tech_ranks %>% 
  dplyr::select(1,2,4) %>% 
  reshape2::dcast(Year~Technique, value.var = "Rank") %>% 
  dplyr::mutate(Year = as.character(Year)) %>% 
  reshape2::melt() %>% 
  dplyr::rename(Technique = variable,
                Rank = value)

#top5_2019_tech_ranks[is.na(top5_2019_tech_ranks)] <- 0

top5_2019_tech_ranks <- top5_2019_tech_ranks %>% 
  dplyr::mutate(Year = as.numeric(Year))


plt <- ggplot(top5_2019_tech_ranks, aes(Year, Rank, color = Technique)) +
  geom_vline(xintercept = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020), linetype = 2, color = "gray")+
  geom_point(size = 7)+
  geom_bump(size = 2, smooth = 8)+
  cowplot::theme_minimal_grid(font_size = 14, line_size = 0)+
  theme(axis.text = element_text(family = "Seravek"),
        axis.title = element_text(family = "Seravek"),
        legend.position = "bottom",
        legend.title = element_blank(),
        plot.caption = element_text(family = "Seravek", color = "#EC0108"))+
  scale_y_reverse()+
  scale_x_continuous(breaks = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020),
                     labels = c(2013, 2014, 2015, 2016, 2017, 2018, 2019, 2020))+
  scale_color_manual(values = unname(pals::alphabet(n=5)))+
  ylab("Rank based on number\nof publications")+xlab("")+
  labs(caption = "@itsvenu_")

ggsave(filename = "06_YearTechRanks.pdf", plt,
       height = 4, width = 7, cairo_pdf)

## 07 which technique was published more in which journal
## above 5 techniques in which journal
## which journals published most papers with above top 5 techniqes

## so for each technique top5 journals with most publication?

#tech_journal_nums <- 
  
tech_journal_nums <- dat %>% 
  dplyr::select(Technique, Journal) %>%
  dplyr::filter(grepl("^Chromium$|^Smart-seq2$|^Drop-seq$|^SMARTer \\(C1\\)$|^InDrops$", Technique)) %>% 
  dplyr::mutate(JT = paste0(Technique, "_", Journal)) %>% 
  dplyr::count(JT) %>% 
  dplyr::arrange(desc(n)) %>% 
  tidyr::separate(JT, into = c("Technique", "Journal"), sep = "_") %>% 
  dplyr::group_by(Technique) %>% 
  dplyr::arrange(desc(n), .by_group = TRUE) %>%
  dplyr::top_n(n=5)

tech_journal_nums$Journal <- factor(tech_journal_nums$Journal, levels = unique(tech_journal_nums$Journal))

tech_journal_nums$Technique %>% unique()

# > unname(pals::alphabet(n=5))
# [1] "#F0A0FF" "#0075DC" "#993F00" "#4C005C" "#191919"

plot_top5_journals <- function(tech_journal_nums = tech_journal_nums, tech, tech_color = "#F0A0FF"){
  
  # tech = "Chromium"
  tj <- tech_journal_nums %>% 
    dplyr::ungroup() %>% 
    dplyr::filter(Technique == get("tech")) %>% 
    dplyr::arrange(desc(n)) %>% head(n=5) %>% 
    dplyr::arrange(n) 
  
  tj$Journal <- as.character(tj$Journal)
  
  tj$Journal <- factor(tj$Journal, levels = tj$Journal)
  
  ggplot(tj, aes(x = Journal, y = n, fill = Technique))+
    geom_bar(stat = "identity", fill = tech_color)+
    facet_wrap(.~Technique, scales = "free")+
    theme_vt()+
    theme(strip.background.x = element_rect(fill = "gray", color = NA))+
    coord_flip()+
    xlab("")+ylab("")
}

# [1] "Chromium"     "Drop-seq"     "InDrops"      "Smart-seq2"  
# [5] "SMARTer (C1)"

plt1 <- plot_top5_journals(tech_journal_nums = tech_journal_nums, tech = "Chromium", tech_color = "#F0A0FF")
plt2 <- plot_top5_journals(tech_journal_nums = tech_journal_nums, tech = "Drop-seq", tech_color = "#0075DC")
plt3 <- plot_top5_journals(tech_journal_nums = tech_journal_nums, tech = "InDrops", tech_color = "#993F00")
plt4 <- plot_top5_journals(tech_journal_nums = tech_journal_nums, tech = "Smart-seq2", tech_color = "#4C005C")
plt5 <- plot_top5_journals(tech_journal_nums = tech_journal_nums, tech = "SMARTer (C1)", tech_color = "#191919")

title1 = grid::textGrob("Top 5 journals with highest number of publicaions", gp=gpar(fontfamily = "Seravek"))
bot_title = grid::textGrob("number of publicaions", gp=gpar(fontfamily = "Seravek"))

plt15_plt <- gridExtra::grid.arrange(plt1, plt2, plt3, plt4, plt5, 
                        top = title1, bottom = bot_title)

ggsave("06_top5techs_top_journals.pdf", plt15_plt,
       height = 6, width = 7.5, cairo_pdf)


# save.image("scDataBase_25052020.R.RData")
load("scDataBase_25052020.R.RData")







