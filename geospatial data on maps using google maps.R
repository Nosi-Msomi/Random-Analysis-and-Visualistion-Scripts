#ggmaps with google maps 
#NB NICD Firewall blocks access to google api, make sure you're running it on your own network or NICD guest wifi 
# this website can be used to determine long and lat of SA cities https://www.latlong.net/country/south-africa-205.html 


#Gauteng lon = 28.112268 , lat = -26.270760
#Joburg lon = 28.034088 , lat = -26.1952460
#this seems to be a good coordinate if you want zoom in on all 3: lon = 28.21413 , lat = -25.99565

library(ggmap)
library(rstudioapi)
library(tidyverse)
library(lubridate)
library(ggrepel)


register_google(key = 'insert-google-key')

token <- "insert-api-token-here"
url <- "inser-url-here"
formData <- list("token"=token,
                 content='record',
                 action='export',
                 format='csv',
                 type='flat',
                 csvDelimiter='',
                 rawOrLabel='raw',
                 rawOrLabelHeaders='raw',
                 exportCheckboxLabel='false',
                 exportSurveyFields='false',
                 exportDataAccessGroups='false',
                 returnFormat='json'
)
response <- httr::POST(url, body = formData, encode = "form")
result <- httr::content(response)
#print(result)

map <- result

map <- map %>%
  select(record_id, sam_site_id, site_latitude, site_longitude, sam_col_date, sam_col_tim)

map$epiweek <- lubridate::epiweek(ymd(map$sam_col_date)) #generate epiweek
map$year <- strftime(map$sam_col_date, "%Y") #Creating year column  
map$month <- strftime(map$sam_col_date, "%m") #Creating month column  
map$week <- "w" #added column with w
my_cols <- c("year", "week", "epiweek") #new data object with 3 columns combined
map$epiweek2 <- do.call(paste, c(map[my_cols],sep ="")) #created new variable using concat columns


# points on map- all time 

png("~/Maps/COE.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coe_sites <- ggmap(get_googlemap(center = c(lon = 28.23570, lat = -25.99873),
                                 zoom =12,
                                 scale = 2,
                                 maptype = "hybrid",
                                 color = "color")) +
  geom_point(data = map, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coe_sites

dev.off()



png("~/Maps/COJ.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coj_sites <- ggmap(get_googlemap(center = c(lon = 27.91593 , lat = -26.31008),
                                 zoom = 12,
                                 maptype = "hybrid",
                                 color = "color")) +
  geom_point(data = map, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coj_sites
dev.off()

png("~/Maps/COT.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

cot_sites <- ggmap(get_googlemap(center = c(lon = 28.12 , lat = -25.76257),
                                 zoom = 12,
                                 maptype = "hybrid",
                                 color = "color")) +
  geom_point(data = map, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

cot_sites

dev.off()

# points on map- this week 

map2 <- map%>% 
  filter(year == 2024)%>% 
  filter(epiweek == 5)

############################################################################

png("~/Maps/COE-current_epiweek.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coe_sites2 <- ggmap(get_googlemap(center = c(lon = 28.23570, lat = -25.99873),
                                  zoom =12,
                                  scale = 2,
                                  maptype = "hybrid",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coe_sites2

dev.off()


##############################################################################

png("~/Maps/COJ-current_epiweek.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coj_sites2 <- ggmap(get_googlemap(center = c(lon = 27.91593 , lat = -26.31008),
                                  zoom = 12,
                                  maptype = "hybrid",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coj_sites2
dev.off()

###############################################################################

png("~/Maps/COT-current_epiweek.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

cot_sites2 <- ggmap(get_googlemap(center = c(lon = 28.12 , lat = -25.76257),
                                  zoom = 12,
                                  maptype = "hybrid",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

cot_sites2

dev.off()


#############################################################################
#How the other views looks 

png("~/Maps/COE-roadmap.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coe_sites2 <- ggmap(get_googlemap(center = c(lon = 28.23570, lat = -25.99873),
                                  zoom =12,
                                  scale = 2,
                                  maptype = "roadmap",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coe_sites2

dev.off()


png("~/Maps/COE-hybrid.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coe_sites2 <- ggmap(get_googlemap(center = c(lon = 28.23570, lat = -25.99873),
                                  zoom =12,
                                  scale = 2,
                                  maptype = "hybrid",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coe_sites2

dev.off()

png("~/Maps/COE-terrain.png", 
    width = 5*900,
    height = 5*500, 
    res = 300,
    pointsize = 8)

coe_sites2 <- ggmap(get_googlemap(center = c(lon = 28.23570, lat = -25.99873),
                                  zoom =12,
                                  scale = 2,
                                  maptype = "terrain",
                                  color = "color")) +
  geom_point(data = map2, aes(x= site_longitude, y=site_latitude),
             color = "red", 
             size= 2) + 
  geom_label_repel(data = map2, 
                   aes(x= site_longitude, y=site_latitude, label = sam_col_tim),
                   size = 6)+
  theme(axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank())

coe_sites2

dev.off()

