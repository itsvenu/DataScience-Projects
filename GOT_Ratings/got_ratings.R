## IMDB rating of Game Of Thrones

## rating
## length
## budget (https://metro.co.uk/2019/05/21/much-game-thrones-season-cost-make-9622963/)
## Season-1 = $60 million
## Season-2 = $69 million
## Season-3 = $50 million
## Season-4 = -
## Season-5 = - 
## Season-6 = $100 million
## Season-7 = -

setwd("/Users/venu/Desktop/work/blog/GOT_Ratings")

library(tidyverse)
library(extrafont)
library(ggpubr)
library(EnvStats)

font_import()
loadfonts()

## read titles and ratings
titles <- data.table::fread("title.akas.tsv.gz", header = TRUE)
episodes <- data.table::fread("title.episode.tsv.gz", header = TRUE)
ratings <- data.table::fread("title.ratings.tsv.gz", header = TRUE)

## get GOT episodes and numbers
got_id <- titles %>% 
  dplyr::filter(grepl("Game of thrones", ignore.case = TRUE, title)) %>% 
  head(n=1) %>% pull(titleId)

episode_ratings <- episodes %>% 
  dplyr::filter(parentTconst == get("got_id")) %>% 
  dplyr::left_join(ratings) %>% 
  dplyr::mutate(episode = paste0(seasonNumber, "-", episodeNumber),
                episodeNumber = as.numeric(episodeNumber),
                seasonNumber = as.numeric(seasonNumber)) %>% 
  dplyr::arrange(seasonNumber, episodeNumber) %>% 
  dplyr::mutate(Season = paste0("Season-", seasonNumber))

#episode_ratings$episode <- factor(episode_ratings$episode, levels = episode_ratings$episode)

season_shade <- episode_ratings %>% 
  dplyr::select(Season) %>% unique()

##
pdf("GOT_Ratings.pdf", height = 6, width = 10)

ggplot(episode_ratings, aes(x = factor(episodeNumber), y = averageRating, group = 1))+
  geom_line()+
  geom_point(size = 3)+
  theme_minimal(base_size = 18)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black", family = "Seravek"),
        axis.title = element_text(family = "Seravek"),
        strip.text.x = element_text(colour = "red", face = "bold", family = "Seravek"),
        plot.caption = element_text(color = "#EC0108", family = "Seravek"),
        plot.title = element_text(hjust = 0.5, family = "Seravek"))+
  labs(caption = '@nerd_yie / itsvenu.github.io')+
  xlab("")+
  ylab("Average  Rating")+
  facet_grid(. ~ Season, scales="free_x")+
  ggtitle("Game of thrones")

dev.off()

## plt
ggsave("GOT_Ratings.pdf", plt, device = cairo_pdf, height = 5, width = 10.5)


plt_violin <- ggviolin(episode_ratings, x = "Season", y = "averageRating", fill = "gray")+
  geom_boxplot(width=0.15)+
  theme_minimal(base_size = 18)+
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_text(color = "red"),
        axis.title.x = element_blank(),
        axis.text = element_text(color = "black", family = "Seravek"),
        axis.title = element_text(family = "Seravek"),
        strip.text.x = element_text(colour = "red", face = "bold", family = "Seravek"),
        plot.caption = element_text(color = "#EC0108", family = "Seravek"),
        plot.title = element_text(hjust = 0.5, family = "Seravek"))+
  labs(caption = '@nerd_yie / itsvenu.github.io')+
  scale_x_discrete(position = "top")+
  ylab("Average  Rating")+
  ggtitle("Game of thrones")+
  stat_n_text(size = 6, family = "Seravek")
  
ggsave("GOT_Ratings_violin.pdf", plt_violin, device = cairo_pdf, height = 5, width = 10.5)  


save.image("got_ratings.R.RData")










