library(tidyverse)
library(ggmap)
library(ggplot2)
library(RColorBrewer)
library(rnaturalearth)
library(rnaturalearthdata)

getData <- function() {
  print("Running function")
  if (file.exists('water.rds')) {
    print("File exists")
    df <- readRDS(file = 'water.rds')
    return (df)
  } else {
    print("File does not exist")
    df <- tidytuesdayR::tt_load('2021-05-04')
    water_df <- df$water
    out <- saveRDS(water_df, file = "water.rds")
    return (water_df)
  }
}

water <- getData()
#water %>% 
#  filter((water_source == "Protected Shallow Well" | water_source == "Protected Spring")) 


map_bounds <- c(left=-18.9274824383, bottom = -36.3991412345, right = 56.8171495674, top = 37.1809147042)
coords.map <- get_stamenmap(map_bounds, zoom = 3, maptype = "toner-lite")
coords.map <- ggmap(coords.map, extent="device", legend = "none") +
  stat_density2d(data = water, aes(x=lon_deg, y=lat_deg, fill=..level.., alpha = ..level..), geom="polygon") +
  scale_fill_gradientn(colours=rev(brewer.pal(7, "Spectral"))) +
  geom_point(data = water, aes(x=lon_deg, y=lat_deg), fill = "red", shape = 23, alpha = 0.8) +
  theme_bw()

ggsave(filename = "coords.png")

ggthemr::ggthemr("earth", type="outer", layout="scientific", spacing=2)


water3 <- water %>% 
  drop_na(water_tech) %>%
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Hand Pump"), "Hand Pump")) %>%
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Mechanized Pump"), "Mechanized Pump")) %>%
  mutate(water_tech = replace(water_tech, str_detect(water_tech, "Functional"), "Functional")) %>%
  mutate(manual = case_when(
    water_tech == "Hand Pump" ~ "Manual",
    water_tech == "Rope and Bucket" ~ "Manual",
    TRUE ~ "Powered")
  )

top_countries %>% 
  left_join(water3) %>% 
  group_by( install_year, manual) %>% 
  filter(install_year >= 1950 & install_year <= 2021) %>%
  summarize(count = n()) %>% 
  ggplot(aes(x = install_year, y = count, fill = manual)) +
  geom_area()
#  scale_y_continuous(trans='log2')
#  facet_wrap(~ country_name)

  
water4 <- water3 %>% 
  filter(status_id == "y" | status_id == "u") %>%
  group_by(country_name, water_source) %>% 
  summarize(count = n()) %>% 
  filter(count == max(count))

water5 <- water3 %>% 
  filter(status_id == "y" | status_id == "u") %>%
  group_by(country_name, water_tech) %>% 
  summarize(count = n()) %>% 
  filter(count == max(count))


africa <- ne_countries(scale = "medium", returnclass = "sf", continent = "Africa")

africa2 <- africa %>% left_join(water4, by=c("name" = "country_name"))
africa3 <- africa2 %>% left_join(water5, by=c("name" = "country_name"))

wsources <- ggplot(africa3)+
  geom_sf(aes(fill = water_source)) +
#  geom_point(data = water4, aes(x = lon_deg, y = lat_deg, color=water_tech),alpha=.5)+
  theme(axis.line = element_blank(),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(face = "italic", size = 6, color = "grey"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank())+
  #scale_x_continuous("", limits = c(-30,50))+
  #scale_y_continuous("", limits = c(-30,50))+
  labs(title = "Water Sources in Africa", 
       subtitle = "Most common water sources by country in Africa", 
       caption = "Data source: Water Point Data Exchange")
  
wtech <- ggplot(africa3)+
  geom_sf(aes(fill = water_tech)) +
  #  geom_point(data = water4, aes(x = lon_deg, y = lat_deg, color=water_tech),alpha=.5)+
  theme(axis.line = element_blank(),
        plot.title = element_text(face="bold"),
        plot.subtitle = element_text(size=12),
        plot.caption = element_text(face = "italic", size = 6, color = "grey"),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        panel.grid = element_blank(),
        legend.title = element_blank())+
  #scale_x_continuous("", limits = c(-30,50))+
  #scale_y_continuous("", limits = c(-30,50))+
  labs(title = "Water Technology in Africa", 
       subtitle = "Most common mechanism for transporting water to point of collection", 
       caption = "Data source: Water Point Data Exchange")

ggpubr::ggarrange(wsources,wtech, nrow=2)

gridExtra::grid.arrange(wsources, wtech, nrow=2)
