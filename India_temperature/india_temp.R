
## Visuzlize temperatures of 100 years in India
## data: https://www.kaggle.com/venky73/temperatures-of-india/version/1

setwd("/Users/venu/Desktop/work/blog/DataScience-Projects/India_temperature")

library(tidyverse)
library(extrafont)
loadfonts()

dat <- read.delim("temperatures.csv", header = TRUE, sep = ",")
dat <- dat %>% dplyr::select(YEAR:DEC) 

dat$YEAR <- factor(dat$YEAR, levels = dat$YEAR)

dat <- dat %>% reshape2::melt()

plt <- dat %>% 
  dplyr::group_by(YEAR) %>% 
  dplyr::summarise(AVG = mean(value)) %>% as.data.frame() %>% 
  dplyr::mutate(Y = 1) %>% 
  ggplot(aes(x = YEAR, y = Y, fill = AVG))+
  geom_tile()+
  scale_fill_gradient(low = "blue", high = "red", space = "Lab",
                      guide = "colourbar", name = "temp (°C)")+
  theme_minimal(base_size = 14)+
  scale_x_discrete(breaks = c("1901", "2017"),
                     labels = c("1901", "2017"))+
  theme(axis.text = element_text(color = "black", family = "Seravek"),
        axis.text.y = element_blank(),
        axis.title = element_blank(),
        panel.grid = element_blank(),
        plot.title = element_text(family = "Seravek", hjust = 0.5),
        legend.title = element_text(family = "Seravek", hjust = 0.5),
        plot.caption = element_text(color = "#EC0108", family = "Seravek"))+
  ggtitle("Temperature changes in India 1901-2017")+
  labs(caption = '@itsvenu_ / itsvenu.github.io')

ggsave(filename = "India_temperatarures.pdf", plot = plt, width = 10, 
       height = 2.5, dpi = 600, device = cairo_pdf)
