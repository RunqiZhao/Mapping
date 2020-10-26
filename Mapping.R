library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)
library(ggplot)
addRepo("geanders")


data("hurr_tracks")

data("rain")

head(hurr_tracks)

head(rain)

# tracking and rainfall but no data information
map_counties(storm = "Floyd-1999", metric = "rainfall") +
  ggtitle("Floyd-1999") +
  theme(plot.title = element_text(hjust = 0.5))

map_rain_exposure(storm ="Allison-2001", 
                  rain_limit = 175, 
                  dist_limit = 500, 
                  days_included =-5:3) +
  ggtitle("Allison-2001") +
  theme(plot.title = element_text(hjust = 0.5))


## colors
## http://sape.inf.usi.ch/quick-reference/ggplot2/colour



library(dplyr)
rain_99 <- filter(rain,storm_id == "Floyd-1999")
rain_99 <- group_by(rain_99, fips)
rain_99 <- summarise(rain_99,sum_rain = sum(precip))
rain_99 <- as.data.frame(rain_99)
rain_99$rainfall <- NA
for (i in 1:dim(dt)[1]){
  rain_99$rainfall[i] <- rain_99$sum_rain[i]%/%25
}
rain_99$rainfall <- ordered(rain_99$rainfall,labels = c("[0.25]","(25,50]","(50,75]", "(75,100)","(100,125]","(125,150]","(150,175]","(175,200)","(200,222]"))



library(usmap)
library(ggplot2)

# plot_usmap("state") +
plot_usmap(data = dt, values = "rainfall", color = "grey", include = dt$fips) + 
   scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)") +
# scale_fill_hue(h = c(0, 360) + 15, c = 20, l = 65, name = "Rainfall(mm)")+
  # scale_fill_continuous(
  #   low = "white", high = "lightblue", name = "Rainfall(mm)", label = scales::comma) + 
    labs(title = "Floyd-1999") +
    theme(plot.title = element_text(hjust = 0.5), size = 2) +
    theme(legend.position = "right") +
    geom_line(data = )


