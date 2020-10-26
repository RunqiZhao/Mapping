library(tidyverse)
library(drat)
library(hurricaneexposuredata)
library(hurricaneexposure)

addRepo("geanders")

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
Floyd99 <- filter(rain,storm_id == "Floyd-1999")
dt_rain <- group_by(Floyd99, fips)
dt <- summarise(dt_rain,sum_rain = sum(precip))
dt <- as.data.frame(dt)
dt$rainfall <- NA
for (i in 1:dim(dt)[1]){
  dt$rainfall[i] <- dt$sum_rain[i]%/%25
}
dt$rainfall <- ordered(dt$rainfall,labels = c("[0.25]","(25,50]","(50,75]", "(75,100)","(100,125]","(125,150]","(150,175]","(175,200)","(200,222]"))



library(usmap)
library(ggplot2)

# plot_usmap("state") +
plot_usmap(data = dt, values = "rainfall", color = "grey", include = dt$fips) + 
   scale_fill_brewer(palette = "Blues", name = "Rainfall(mm)") +
# scale_fill_hue(h = c(0, 360) + 15, c = 20, l = 65, name = "Rainfall(mm)")+
  # scale_fill_continuous(
  #   low = "white", high = "lightblue", name = "Rainfall(mm)", label = scales::comma) + 
    labs(title = "Floyd-1999") +
    theme(plot.title = element_text(hjust = 0.5)) +
    theme(legend.position = "right")

















eq_transformed <- usmap_transform(earthquakes)

plot_usmap("counties") +
  geom_point(data = eq_transformed, aes(x = lon.1, y = lat.1, size = mag),
             color = "red", alpha = 0.25) +
  labs(title = "US Earthquakes",
       subtitle = "Source: USGS, Jan 1 to Jun 30 2019",
       size = "Magnitude") +
  theme(legend.position = "right")