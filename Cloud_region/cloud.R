#read the Excel 

library(readxl)

cloud <- read_excel("cloud_region.xlsx")

#inner join to add long and lat
library(tidyverse)
library(tidygeocoder)

cloud_city <- geocode(cloud, city = City, lat = "lat", long = "long")
View(cloud_city)


#plot map
library(tidyverse)
library(sf)
library(mapview)

#set colors for vendor
ccolor <- c("brown 1", "darkviolet", "aquamarine", "yellow")

webshot::install_phantomjs()

mapview(cloud_city, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE,
        zcol = "Vendor")

if (FALSE) {
  library(utils)
  
  m = mapview(cloud_city, xcol = "long", ycol = "lat", crs = 4269, grid = FALSE,
              zcol = "Vendor", alpha = 0.2, col.regions = ccolor )
  html_fl = tempfile(fileext = ".html")
  png_fl = tempfile(fileext = ".png")
  
  ## create standalone .html
  mapshot(m, url = html_fl)
  browseURL(html_fl)
  
  #save html
  mapshot(m, url = "m.html")

  
  ## create standalone .png; temporary .html is removed automatically unless
  ## 'remove_url = FALSE' is specified
  mapshot(m, file = png_fl)
  browseURL(png_fl)
  mapshot(m, file = png_fl,
          remove_controls = c("homeButton", "layersControl"))
  browseURL(png_fl)
  
  ## create .html and .png
  mapshot(m, url = html_fl, file = png_fl)
  browseURL(png_fl)
  browseURL(html_fl)
}

#interactive map
library(gganimate)
library(gifski)

world <- map_data("world") %>% filter(region != "Antarctica") %>% fortify

base_map <- ggplot(data = world, mapping = aes(x = long, y = lat, group = group)) +
  geom_polygon(color = "grey80", fill = "wheat") +
  coord_quickmap() +
  theme(text = element_text(family = "Roboto"),
        plot.title = element_text(hjust = 1),
        plot.title.position = "left",
        plot.subtitle = element_text(hjust = 1))+
  theme_void()+
  labs(
    title = "Cloud Data Centers by Vendor",
    subtitle = "Opening year",
    caption = "Data & Autor: Arantxa Herranz"
  )


base_map


map_with_data <- base_map +
  geom_point(data = cloud_city, aes(x = long, y = lat, group=Vendor, color = Vendor))
map_with_data

map_with_animation <- map_with_data +
  transition_time(Year) +
  ggtitle('Year: {frame_time}',
          subtitle = 'Frame {frame} of {nframes}')
num_years <- max(cloud_city$Year) - min(cloud_city$Year) + 1
animate(map_with_animation, nframes = num_years, fps = 2)
anim_save("cloudmap.gif")


