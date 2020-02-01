library(readxl)
library(tidyverse)
library(stringr)
library(dplyr)
library(ggplot2)
library(ggmap)
library(maps)
library(mapdata)
library(scales)
library(tpltheme)

tx_vac <- read_excel("./Data/2018-2019 School Vaccination Coverage Levels - Kindergarten (XLS) .xlsx", skip = 2)
grouped <- tx_vac %>% 
  mutate(avgvac = (`DTP/DTaP/DT/Td`+`Hepatitis A`+`Hepatitis B`+MMR+Polio+Varicella)/6) %>%
  group_by(County) %>%
  summarize(avgvac = mean(avgvac, na.rm = TRUE)) %>%
  mutate(County = tolower(County)) %>%
  rename("subregion" = County) %>%
  filter(subregion != "king")

counties <- map_data("county")

tx_county <- subset(counties, region == "texas")

merged <- left_join(tx_county, grouped, by = "subregion")

tx <- ggplot(data = merged, mapping = aes(x = long, y = lat, group = group, fill = avgvac*100)) +
  coord_fixed(1.3) +
  geom_polygon(color = "black") +
  labs(fill = "Vaccination Rate") +
  #theme_nothing(legend = TRUE) +
  theme(legend.title = element_text(),
        #legend.key.width = unit(.1, "in"),
        panel.border = element_blank(), panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(), axis.line = element_blank(), 
        axis.text.x = element_blank(), axis.text.y = element_blank(),
        axis.ticks = element_blank(),
        panel.background = element_rect(fill = "white"),
        text = element_text(family = "Lato"),
        legend.position = "bottom") +
   labs(x = element_blank(),
        y = element_blank(),
        title = "Texas Vaccination Rate by County",
        subtitle = "Among Kindergartners") + 
  tpltheme::scale_fill_continuous()

#devtools::install_github("tylermorganwall/rayshader")
library(rgl)
options(rgl.useNULL = FALSE)
library(rayshader)

par(mfrow = c(1,1))
rayshader::plot_gg(tx, width = 5, raytrace = TRUE, multicore = TRUE, height = 5, scale = 50)
#render_snapshot(clear = TRUE)
#render_camera(theta = 45, phi = 45, zoom = 1, fov = 0)

phivechalf = 30 + 60 * 1/(1 + exp(seq(-7, 20, length.out = 180)/2))
phivecfull = c(phivechalf, rev(phivechalf))
thetavec = -90 + 60 * sin(seq(0,359,length.out = 360) * pi/180)
zoomvec = 0.45 + 0.2 * 1/(1 + exp(seq(-5, 20, length.out = 180)))
zoomvecfull = c(zoomvec, rev(zoomvec))

render_movie(filename = "./Outputs/tx_vac_vid", type = "custom", 
             frames = 360,  phi = phivecfull, zoom = zoomvecfull, theta = thetavec)