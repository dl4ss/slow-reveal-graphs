#
# FILE:
# anti_lynching_bill_map.R
#
# DESCRIPTION
# Colored Women’s Clubs of Michigan's Red Record of Lynching Map

# https://catalog.archives.gov/id/149268727
#
#
# AUTHORS:
#   Louis Cousino




# setup
library(usmap)
library(maps)
library(mapdata)
library(showtext)
library(tidyverse)


# fonts
font_add_google("Puritan", "Puritan")
showtext_auto()

# reading in text data
text_dat <- readxl::read_xlsx(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "text_position.xlsx"))

# data frame from mapdata library
state <- map_data("state")

#Adding state abbreviations
centroids <- data.frame(region = tolower(state.name), long = state.center$x, lat = state.center$y)
centroids$abb <- state.abb[match(centroids$region, tolower(state.name))]

#creating new abbreviations 
Abb <- c("ALA.","AK","ARIZ.","ARK.","CALIF.","COLO.","CONN.","DEL.","FLA.","GA.","HI",'IDAHO',"ILL.",
         "IND.","IOWA","KANS.","KY.","LA.","MAINE","MD.","MASS.","MICH.","MINN.","MISS.","MO.","MONT.",
         "NEBR.","NEV.","N.H.","N.J.","N.MEX.","N.Y","N.C.","N.DAK.","OHIO","OKLA.","OREG.","PA.",
         "RI.","S.C.","S.DAK.","TENN.","TEXAS","UTAH","VT.","VA.","WASH.","W.VA.","WIS.","WYO.")


#Adding the new abbreviations
centroids[["abb"]] <- Abb
colnames(centroids)[colnames(centroids) == "abb"] <- "Abb"

#Removing Alaska and Hawaii Abbreviations
new_centroids <- centroids[-c(2,11),]

# Importing Images

img_main <- png::readPNG(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "01_full_unmask.png"))
img_main_grob <- grid::rasterGrob(img_main)

img1 <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_1_cut.png"))
img1_grob <- grid::rasterGrob(img1)

img2 <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_2_cut.png"))
img2_grob <- grid::rasterGrob(img2)

img3 <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_3_cut.png"))
img3_grob <- grid::rasterGrob(img3)

img4 <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_4_cut.png"))
img4_grob <- grid::rasterGrob(img4)

img5 <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_5_cut.png"))
img5_grob <- grid::rasterGrob(img5)

border_tx <- state |> 
  filter(long > -106.805485,
         long < -101.026627,
         lat > 31.758682,
         lat < 36.568721,
         region %in% c("texas"))

border_ok <- state |> 
  filter(long > -103.026627,
         long < -94.622286,
         lat > 36.482823,
         lat < 36.568721,
         region %in% c("oklahoma"))

border_ak <- state |> 
  filter(long > -94.622286,
         long < -89.666579,
         lat > 35.951676,
         lat < 36.968721,
         region %in% c("arkansas"))

border_tn <- state |> 
  filter(long > -89.733393,
         long < -89.506837,
         lat > 36.000256,
         lat < 36.506694,
         region %in% c("tennessee"))

border <- border_tx |> 
  bind_rows(border_ok, border_ak, border_tn) |> 
  mutate(order = row_number())

border |> 
  ggplot() +
  geom_path(aes(x=long, y=lat, group = group),
               color = "black",, linewidth = 0.5)

#plot with data from census.gov
plot <- state |> 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group),
               color = "black", fill = "#FCF2DA", linewidth = 0.5) +
  ggtext::geom_richtext(aes(x = x, y = y, label = text),
                        label.padding = grid::unit(rep(0, nrow(text_dat)), "pt"),
                        label.color = NA,
                        fill = NA,
                        size = 18,
                        data=text_dat |>
                          filter(display_step != 0)) + # Set 1 for red text reveal and 0 for all text
  coord_map("azequidistant") + # to make the shape of the map look globular
  labs(title = "THE RED RECORD OF LYNCHING - 1889 TO 1921 - GEOGRAPHICALLY DISTRIBUTED", 
       subtitle = "") +
  theme(axis.title.x = element_blank(),
        axis.text.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.title.y = element_blank(),
        axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        plot.subtitle = ggtext::element_markdown(size = 100),
        panel.background = element_rect(colour = "black", fill =  "#FCF2DA")) +
  theme(legend.position = "bottom",
        legend.text = element_text(family = "Puritan", size = 12))

# ggsave(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "01_full_unmask.png"), 
#        plot = plot, width = 5732, height = 2700, units = "px", dpi = 300)

plot_test <- ggplot() +
  annotation_custom(grob = img_main_grob,
                    xmin = 0,
                    ymin = 0) +
  annotation_custom(grob = img1_grob,
                    xmin = 0.1,
                    xmax = 0.45,
                    ymin = -0.6) +
  annotation_custom(grob = img2_grob,
                    xmin = 0.25,
                    xmax = 0.65,
                    ymin = 0.75) +
  annotation_custom(grob = img3_grob,
                    xmin = 0.62,
                    xmax = 0.72,
                    ymin = -0.82) +
  annotation_custom(grob = img4_grob,
                    xmin = 0.655,
                    xmax = 0.755,
                    ymin = 0.45) +
  annotation_custom(grob = img5_grob,
                    xmin = 0.775,
                    xmax = 0.925,
                    ymin = -0.4)


ggsave(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "02_full_img.png"), 
       plot = plot_test, width = 5732, height = 2700, units = "px", dpi = 300)

#plot with state abbreviations
plot2 <- plot + with(new_centroids, 
                     annotate(geom = "text", x = long, y = lat, label = Abb, family = "Puritan"))
