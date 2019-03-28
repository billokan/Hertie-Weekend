source("packages.r")
library(anytime)
library(leaflet)
devtools::install_github("hrbrmstr/ipapi")
library(ipapi)
library(maps)
library(RColorBrewer)
library(rworldmap)
library(ggthemes)
library(reshape)
library(plotly)
library(quantmod)



###
df_conn<-read.delim("datafest2018_data_and_documentation/201803232125.data/conn.csv")
df_conn$ts <- anytime(df_conn$ts)

###
any(is.na(df_conn$uid))
any(!complete.cases(df_conn))
dim(df_conn)
glimpse(df_conn)

###
df_conn_SPS_01 <- filter(df_conn, id.resp_h == "192.168.0.12") %>% 
        group_by(id.orig_h) %>%
        mutate(count = n())
#
df_conn_SPS_01 <- head(df_conn_SPS_01,5000)
sites <- sample(df_conn_SPS_01$id.orig_h) 
sites <- gsub("/", "", sites)
#locations <- geolocate(sites)
colnames(locations)[9] <- "id.orig_h"

###
df_conn_SPS_02 <- filter(df_conn, id.resp_h == "192.168.0.13") %>% 
        group_by(id.orig_h) %>%
        mutate(count = n()) %>% 
        arrange(desc(count))
###
df_merged_times <- df_conn %>%
        mutate(date = substr(ts, 1, 10)) %>%
        mutate(year = substr(ts, 1, 4)) %>%
        mutate(month = substr(ts, 6, 7)) %>%
        mutate(day = substr(ts, 9, 10)) %>%
        mutate(time = substr(ts, 12, 19))
###

location <- read.delim(file = "locations.csv", sep = ",")
location2 <- read.delim(file = "locations2.csv", sep = ",")
location$X <- NULL
location2$X <- NULL
colnames(location)[9] <- "id.orig_h"
colnames(location2)[9] <- "id.orig_h"
###
df_1 <-merge(df_conn_SPS_01, location, by="id.orig_h") %>% filter(!is.na(lat))
df_2 <-merge(df_conn_SPS_02, location2, by="id.orig_h") %>% filter(!is.na(lat))

###
n_ip_count <- df_1 %>% group_by(country) %>%
        summarise(ip_countries = n()) %>%
        arrange(desc(ip_countries))

num_ip_count_top <- head(n_ip_count,5)

####
df_1_filt <- df_1 %>% 
        filter(country %in% 
         c("Germany", "China", "United States", "Russia", "France"))

####
n_ip_count2 <- df_2 %>% group_by(country) %>%
        summarise(ip_countries2 = n()) %>%
        arrange(desc(ip_countries2))

num_ip_count_top2 <- head(n_ip_count2,5)

###
df_2_filt <- df_2 %>% 
        filter(country %in% 
                       c("Germany", "China", "United States", "Russia", "Netherlands"))

####

VOX <- data.frame(df_1_filt %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))


countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=df_2_filt,aes(x= lon ,y= lat ,size=count,color=count),alpha=.50) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='IP addresses locations cought by Honey pot',
             subtitle='from April,2013 to Aug., 2013') 
        
###
VOX2 <- data.frame(df_2_filt %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))


countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=df_1_filt,aes(x= lon ,y= lat ,size=count,color=count),alpha=.50) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='IP addresses locations cought by Honey pot',
             subtitle='from April,2013 to Aug., 2013') 

##### Filtering for attacks from same IP to both 
test <- df_1 %>% 
        filter(id.orig_h %in% df_2$id.orig_h)
test2 <- df_1 %>% 
        filter(!(id.orig_h %in% df_2$id.orig_h))  ## only attack sps1 
test3 <- df_2 %>% 
        filter(!(id.orig_h %in% df_1$id.orig_h))
###### graph attacks both SPS

VOX <- data.frame(test %>% 
                           dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                           dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo<-ggplotGrob(
        top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
                geom_bar(stat='identity',  fill = "#287D8EFF") + coord_flip() + theme_fivethirtyeight() + 
                theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses'))

countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=test,aes(x= lon ,y= lat ,size=count,color=count),alpha=.10) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting both SPS01 and SPS02',
             subtitle='from August 14th,2014 to May 5th, 2015') +
        annotation_custom(grob = histo, xmin = 80, xmax = 210, ymin = -100, ymax = -40)

#####
test <- test %>%
        mutate(date = substr(ts, 1, 10)) %>%
        mutate(year = substr(ts, 1, 4)) %>%
        mutate(month = substr(ts, 6, 7)) %>%
        mutate(day = substr(ts, 9, 10)) %>%
        mutate(time = substr(ts, 12, 19))

######  Graph SPS01 with box

VOX2 <- data.frame(test2 %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX2 %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo<-ggplotGrob(
        top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
                geom_bar(stat='identity', fill = "#31288E") + coord_flip() + theme_fivethirtyeight() + 
                theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses'))

countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=test2,aes(x= lon ,y= lat ,size=count,color=count),alpha=.10) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#31288E", high = "#8E7A28") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting SPS01',
             subtitle='from August 14th,2014 to May 5th, 2015') +
        annotation_custom(grob = histo, xmin = 80, xmax = 210, ymin = -100, ymax = -40)


##### Remove box from graph only SPS01

###spso1
VOX <- data.frame(df_1_vox %>% 
                           dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                           dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo<- top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
                geom_bar(stat='identity', fill = "#287D8EFF") + coord_flip() + theme_fivethirtyeight() + 
                theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses')
histo
countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=test2,aes(x= lon ,y= lat ,size=count,color=count),alpha=.10) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting SPS01',
             subtitle='from August 14th,2014 to May 5th, 2015') 

#### remove the box from graph for both SPS

VOX <- data.frame(test %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo1<- top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
                geom_bar(stat='identity',  fill = "#287D8EFF") + coord_flip() + theme_fivethirtyeight() + 
                theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses')
histo1
countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=test,aes(x= lon ,y= lat ,size=count,color=count),alpha=.10) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting both SPS01 and SPS02',
             subtitle='from August 14th,2014 to May 5th, 2015') 

### attack on SPS02

VOX3 <- data.frame(test3 %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX3 %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo1<- top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
        geom_bar(stat='identity',  fill = "#287D8EFF") + coord_flip() + theme_fivethirtyeight() + 
        theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses')
histo1
countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=test,aes(x= lon ,y= lat ,size=count,color=count),alpha=.10) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting both SPS01 and SPS02',
             subtitle='from August 14th,2014 to May 5th, 2015') 


df_conn_SPS_01_country <- merge(df_conn_SPS_01, location, by ="id.orig_h")
df_conn_SPS_02_country <- merge(df_conn_SPS_02, location2, by= "id.orig_h")

df_1_vox <- data.frame(df_conn_SPS_01_country %>% 
                           dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                           dplyr::summarize(count=n()) %>% arrange(-count))

df_2_vox<- data.frame(df_conn_SPS_02_country %>% 
                              dplyr::group_by(country,id.orig_h, lat, lon, country) %>% 
                              dplyr::summarize(count=n()) %>% arrange(-count))

sum(df_2_vox$count)
sum(head(df_2_vox$count), 5)

####
top5<-data.frame(df_2_vox %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo<- top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
        geom_bar(stat='identity', fill = "#31288E") + coord_flip() + theme_fivethirtyeight() + 
        theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses')
histo
countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=df_2_vox,aes(x= lon ,y= lat ,size=count,color=count),alpha=.60) +
        scale_color_gradient2(name='', low = "#B8DE29FF", mid = "#31288E", high = "#8E7A28") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,10)) + 
        labs(title='Top IP addresses targeting SPS02',
             subtitle='from August 14th,2014 to May 5th, 2015') 
       
### attacks on SPS01

VOX <- data.frame(test2 %>% 
                          dplyr::group_by(country,id.orig_h, lat, lon) %>% 
                          dplyr::summarize(count=n()) %>% arrange(-count))

top5<-data.frame(VOX %>% top_n(5))
top5$fullIP<-paste0(top5$id.orig_h,'(',top5$country,')')                  
histo <- top5 %>% ggplot(aes(x=reorder(fullIP,count),y=count)) + 
        geom_bar(stat='identity', fill = "#287D8EFF") + coord_flip() + theme_fivethirtyeight() + 
        theme(axis.text=element_text(size=8)) + labs(subtitle='Top 5 bad IP addresses')
histo
countries_map <-map_data("world")
world_map<-ggplot() + 
        geom_map(data = countries_map, map = countries_map,aes(x = long, y = lat, map_id = region, group = group), fill = "white", color = "black", size = 0.1) + 
        theme_fivethirtyeight() + 
        theme(axis.text=element_blank())
world_map + 
        geom_point(data=VOX,aes(x= lon ,y= lat ,size=count,color=count),alpha=.60) +
        scale_color_gradient2(name='',low = "#B8DE29FF", mid = "#287D8EFF", high = "#440154FF") + 
        guides(color=FALSE,size=F) + 
        scale_radius(range=c(1,8)) + 
        labs(title='Top IP addresses targeting SPS01',
             subtitle='from August 14th,2014 to May 5th, 2015') 




#### Percentage share of Top 5 bad IPs per SPS system
sum(df_1_vox$count)
sum(head(df_1_vox$count), 5)
#Top 5 bad IPs represent 35% of the subset
sum(VOX2$count)
sum(head(VOX2$count), 5)
#Top 5 bad IPs represent 86% of the subset 

## 
glimpse(test)
test$count <- as.character(as.numeric(test$count))


p <- plot_ly(test, x = ~ts) %>%
        add_lines(y = ~id.orig_h, name = "Apple") %>%
        layout(
                title = "Stock Prices",
                xaxis = list(
                        rangeselector = list(
                                buttons = list(
                                        list(
                                                count = 1,
                                                label = "1 mo",
                                                step = "month",
                                                stepmode = "backward"),
                                        list(step = "all"))),
                        
                        rangeslider = list(type = "date")),
                
                yaxis = list(title = "Count"))
chart_link = api_create(p, filename="rangeslider_datafest")
chart_link
Sys.setenv("plotly_username"="lorenzoand")
Sys.setenv("plotly_api_key"="a3HoivjvrC9kFQwsTst5")
