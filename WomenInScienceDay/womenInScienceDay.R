
## #WomenInScience

## get 100k tweets with #WomenInScience
## Which country produced highest number of tweets
## Which tweet has highest favourites

setwd("/Users/venu/Desktop/work/blog/womenInScienceDay")

library(tidyverse)
library(rtweet)
library(ggmap)

## 0. Data collection 

wis <- search_tweets("#WomenInScienceDay", n=100000, include_rts = FALSE, retryonratelimit = TRUE)

wis_rt <- search_tweets("#WomenInScienceDay", n=100000, include_rts = TRUE, retryonratelimit = TRUE)


## 1. Data Cleaning 

# select required columns for analysis
select_colc <- c("created_at", "screen_name", "text",
                 "favorite_count", "retweet_count", "retweet_location",
                 "status_url", "location")

wis_rt_clean <- wis_rt %>% 
  dplyr::select(one_of(select_colc)) %>% 
  dplyr::arrange(desc(favorite_count)) 
  
## 2. Data analysis

## top 5 most liked tweets
wis_rt_clean %>% 
  dplyr::select(status_url) %>% head(n=5)

# status_url                                                   
# <chr>                                                        
# 1 https://twitter.com/ChelseaClinton/status/1094950941005103104
# 2 https://twitter.com/EU_Commission/status/1094853542723096579 
# 3 https://twitter.com/AngelinaGa2019/status/1095118176671670273
# 4 https://twitter.com/MonicaBeletsky/status/1095167901470879744
# 5 https://twitter.com/AstroPeggy/status/1095182309593137152  

## top 5 most retweeted tweets
wis_rt_clean %>% 
  dplyr::arrange(desc(retweet_count)) %>% 
  dplyr::select(status_url) %>% head(n=5)

# status_url                                                    
# <chr>                                                         
# 1 https://twitter.com/EiropasKomisija/status/1094968952311164930
# 2 https://twitter.com/EUHomeAffairs/status/1094968420049793024  
# 3 https://twitter.com/ChelseaClinton/status/1094950941005103104 
# 4 https://twitter.com/gakui486/status/1095673003449503745       
# 5 https://twitter.com/preopticarea/status/1095175712745685000

## number of tweets per location
tweets_per_city <- wis_rt_clean %>% 
  dplyr::select(location) %>% 
  dplyr::count(location) %>% 
  dplyr::arrange(desc(n)) %>% 
  dplyr::filter(n != 5751)

## get lat long of city names

register_google(key = "my_key")

city_lat_lon <- geocode(as.character(tweets_per_city$location))

tweets_per_city_ll <- cbind(tweets_per_city, city_lat_lon)

## world map

map_world <- map_data("world")

pdf("wis_tweetsFromWorld.pdf", height = 7, width = 11)

ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = tweets_per_city_ll, 
             aes(x = lon, y = lat, color = "red"), size = 0.8)+
  theme_bw(base_size = 18)+
  theme(legend.position = "none")

dev.off()

## for cover image
pdf("~/Desktop/wis_tweetsFromWorld.pdf", height = 7, width = 11)

ggplot() +
  geom_polygon(data = map_world, aes(x = long, y = lat, group = group)) +
  geom_point(data = tweets_per_city_ll, 
             aes(x = lon, y = lat, color = "red"), size = 1.2)+
  theme_dark(base_size = 18)+
  theme(legend.position = "none",
        axis.title = element_blank(),
        axis.ticks = element_blank(),
        axis.text = element_blank())

dev.off()

##
## plot most liked/rtweeted tweets

## keep uniq screen_names with favourite_counts
sn_fc <- wis_rt_clean %>% 
  dplyr::select(screen_name, favorite_count) %>% 
  dplyr::mutate(favorite_count = as.numeric(favorite_count)) %>% 
  as.data.frame()

sn_fc_50 <- sn_fc[!duplicated(sn_fc$screen_name), ] %>% 
  dplyr::arrange(desc(favorite_count)) %>% head(n=50) %>% 
  dplyr::arrange(favorite_count)

## fix x-axis order
sn_fc_50$screen_name <- factor(sn_fc_50$screen_name, levels = sn_fc_50$screen_name)

pdf("wis_favouriteCounts.pdf", height = 7.5, width = 6)

sn_fc_50 %>% 
  ggplot(aes(screen_name, favorite_count))+
  geom_bar(stat = "identity")+
  coord_flip()+
  theme_bw()

dev.off()

save.image("wis.R.RData")
load("wis.R.RData")
