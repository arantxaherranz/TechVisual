library(ggplot2)
library(readxl)

#reading Excel and create dataframe
iphone <- read_excel("iPhone.xlsx")

head(iphone)
View(iphone)

library(forcats)
library(ggrepel)

set.seed(42)

#plot
fig <- ggplot(iphone, aes(fct_inorder(Screen_size), fct_inorder(Storage_max), 
                   label = Model, color = Price))+
  geom_point(aes(size = fct_inorder(RAM)))+
  scale_color_gradient(low = "purple", high = "orange")+
  geom_label_repel(aes(label = Model,
                       fill = Price), color = "white",
                   size = 2.5, max.overlaps = Inf,
                   show.legend = FALSE)+
  theme(text = element_text(family = "San Francisco"))+
  theme_minimal()+
  labs(title = "Compare iPhones models",
       subtitle = "by screen, storage and price",
       caption = "Source: Own elaboration based on public information | Author: Arantxa Herranz",
       alt = "scatter plot comparing all iPhones models by screen size, storage capcity, RAM and price",
       x= "Screen size",
       y="Storage capacity (max)",
       size = "RAM")

fig


#add image
library(magick) 
library(cowplot)

url <- "https://www.apple.com/newsroom/images/product/iphone/standard/iPhone_X_family_line_up_big.jpg.large_2x.jpg"
img <- magick::image_read(url)

fig +
  annotation_custom(
    grid::rasterGrob(
      image = img,
      x =.7,
      y = .25,
      width = .3
    )
  )+
  coord_cartesian(clip = "off")+
  theme(plot.margin = margin(12,12,130,12))

#other plots
ggplot(iphone, aes(fct_inorder(Screen_size), fct_inorder(Storage_max),
                   size = Price, color = fct_inorder(RAM)))+
  geom_point()+
  facet_wrap(~Year)

#annimate
library(gganimate)

fig +  transition_time(Year)+
  enter_fade()+
  exit_shrink()+
  ease_aes("linear")

anim_save("iphone_chart.gif", animation = last_animation(), path = "iPhone/")

fig +  transition_time(Price)+
  enter_fade()+
  exit_shrink()+
  ease_aes("linear")+
  labs(title = "Comparing iPhone By Price")

anim_save("iphone_chart_price.gif", animation = last_animation(), path = "iPhone/")
