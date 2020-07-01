
# @itsvenu_
# 30.06.2020

setwd("~/Desktop/work/blog/SparkasseExpenses")

library(tidyverse)
library(extrafont)

dat <- data.table::fread("20200630-1002884557-umsatz.CSV", sep = ";", header = TRUE)
dat <- dat %>% 
  dplyr::select(`Day of entry`, Amount) %>% 
  dplyr::filter(grepl("-", Amount)) %>% 
  dplyr::mutate(Amount = gsub("-", "", Amount),
                Amount = gsub(",", ".", Amount)) %>% 
  dplyr::mutate(Amount = as.numeric(Amount))

## remove 350, 1000?
dat <- dat %>% 
  dplyr::filter(Amount < 200) 

dat2 <- dat2[seq(dim(dat2)[1], 1), ]

###
dat4 <- dat2 %>% 
  dplyr::mutate(month = dplyr::case_when(grepl(".01.", `Day of entry`) ~ "Jan",
                                         grepl(".02.", `Day of entry`) ~ "Feb",
                                         grepl(".03.", `Day of entry`) ~ "March",
                                         grepl(".04.", `Day of entry`) ~ "April",
                                         grepl(".05.", `Day of entry`) ~ "May",
                                         grepl(".06.", `Day of entry`) ~ "June"))
  
all_months <- dat4$month %>% as.character() %>% unique()

all_months_res <- NULL

for(i in seq_along(all_months)){
  
  i_month <- all_months[i]
  i_res <- dat4 %>% 
    dplyr::filter(month == get("i_month")) %>% 
    dplyr::pull(Amount) %>% 
    zoo::rollmean(k=3) %>% 
    as.data.frame() %>% 
    dplyr::rename(avg_exp = ".") %>% 
    dplyr::mutate(month = i_month)
  
  all_months_res <- rbind(all_months_res, i_res)
  
}

all_months_res <- all_months_res %>% 
  tibble::rowid_to_column(var = "day")

all_months_res$month <- factor(all_months_res$month, levels = c("Jan", "Feb", "March", "April", "May", "June"))

plt <- ggplot(all_months_res, aes(x = day, y = avg_exp))+
  geom_hline(yintercept = c(0, 10, 20, 30, 40, 50), linetype = 2, color = "gray")+
  geom_line(size = 0.8)+
  facet_grid(.~month, scales = "free_x")+
  theme_minimal(base_family = "Seravek", base_size = 16)+
  theme(panel.grid = element_blank(),
        axis.text.x = element_blank(),
        axis.text = element_text(color = "black"),
        strip.text.x = element_text(size = 18, color = "#EC0108"),
        plot.caption = element_text(color = "#EC0108", family = "Seravek"),
        plot.title = element_text(hjust = 0.5))+
  scale_y_continuous(breaks = c(0, 10, 20, 30, 40, 50),
                     labels = c(0, 10, 20, 30, 40, 50))+
  xlab("")+ylab("3-day average expenses (€)")+
  labs(caption = '@itsvenu_ / itsvenu.github.io')+
  ggtitle("6 months daily expenses in 2020")
  
ggsave(filename = "expenses_plot.pdf", plt, 
       height = 4, width = 10, cairo_pdf)






