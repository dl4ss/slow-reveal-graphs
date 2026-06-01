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
library(showtext)
library(tidyverse)


# fonts
font_add_google("Puritan", "Puritan")
showtext_auto()

# reading in text data
text_dat <- readxl::read_xlsx(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "text_position.xlsx"),
                              sheet = "Horizontal Text") |> 
  mutate(x_adj = x + 0)

# data frame from ggplot2 library
state <- map_data("state")

#Adding state abbreviations
centroids <- data.frame(region = str_to_lower(state.name), long = state.center$x, lat = state.center$y) |> 
  mutate(region = str_replace_all(region, " ", "_"))

# Loading additional data on state names

state_text <- readxl::read_xlsx(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "text_position.xlsx"),
                              sheet = "State") |>
  as.data.frame() |>
  mutate(region = str_replace_all(region, "\\s+", "_"))

#Removing Alaska and Hawaii Abbreviations and merging
new_centroids <- centroids[-c(2,11),] |> 
  mutate(region = str_trim(region)) |> 
  left_join(state_text, by = join_by(region == region)) |> 
  mutate(
    lat = case_when(
      Abb == "LA." ~ lat + 0.75,
      Abb == "MD." ~ lat - 0.5,
      Abb == "R.I." ~ lat - 1,
      Abb == "N.J." ~ lat - 0.5,
      TRUE ~ lat
    ),
    long = case_when(
      Abb == "DEL." ~ long + 1,
      Abb == "N.J." ~ long + 1,
      Abb == "MD." ~ long + 0.5,
      TRUE ~ long
    )
  )

# Importing Images

img1_grob <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_1_cut.png")) |> 
  grid::rasterGrob()

img2_grob <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_2_cut.png")) |> 
  grid::rasterGrob()

img3_grob <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_3_cut.png")) |> 
  grid::rasterGrob()

img4_grob <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_4_cut.png")) |> 
  grid::rasterGrob()

img5_grob <- png::readPNG(here::here("Slow reveal Recreations", "anti-lynching-bill-map", "image-elements", "img_5_cut.png")) |> 
  grid::rasterGrob()



# border |> 
#   filter(region == "arkansas") |> 
#   ggplot() +
#   geom_path(aes(x=long, y=lat, group = group),
#                color = "red", linewidth = 2) +
#   geom_polygon(aes(x=long, y=lat, group = group),
#                color = "black", fill = "#FCF2DA", linewidth = 0.5,
#                data = state |> 
#                  filter(region == "arkansas"))

#####
temp_state <- "ohio"

state |>  
  filter(region == temp_state) |> 
  # filter(between(lat, 31.125, 32.5))
  filter(lat == min(lat))
# filter(long == min(long))
# filter(order == min(order))
# filter(between(lat, 36, 36.5))


state |> 
  mutate(
    border_color = case_when(
      # region == "arkansas" & between(order, 392, 442) ~ "red",
      region == "texas" & between(order, 13030, 13056) ~ "red",
      region == "oklahoma" & between(order, 10955, 10962) ~ "red",
      region == "oklahoma" & between(order, 10680, 10708) ~ "red",
      region == "ohio" & between(order, 10524, 10579) ~ "red",
      region == "kentucky" & between(order, 4035, 4550) ~ "red",
      region == "missouri" & between(order, 7950, 8046) ~ "red",
      region == "tennessee" & between(order, 11915, 11950) ~ "red",
      region == "west virginia" & between(order, 15033, 15141) ~ "red",
      region == "pennsylvania" & between(order, 11322, 11341) ~ "red",
      region == "maryland" & between(order, 5443, 5457) ~ "red",
      region == "maryland" & between(order, 5468, 5478) ~ "red",
      region == "delaware" & between(order, 1357, 1438) ~ "red",
      TRUE ~ "black"
    ),
    border_width = case_when(
      border_color == "red" ~ 2,
      TRUE ~ 0.5
    )
  ) |> 
  # filter(region == temp_state) |>
  # filter(region %in% c(temp_state, "kentucky")) |>
  ggplot(aes(x=long, y=lat, group = group, color = border_color, linewidth = border_width)) +
  geom_polygon(fill = "white") +
  geom_path() +
  scale_color_identity() +
  scale_linewidth_identity()

#####

#plot with data from census.gov
plot <- state |> 
  mutate(
    border_color = case_when(
      # region == "arkansas" & between(order, 392, 442) ~ "red",
      region == "texas" & between(order, 13030, 13056) ~ "red",
      region == "oklahoma" & between(order, 10955, 10962) ~ "red",
      region == "oklahoma" & between(order, 10680, 10708) ~ "red",
      region == "ohio" & between(order, 10524, 10579) ~ "red",
      region == "kentucky" & between(order, 4035, 4550) ~ "red",
      region == "missouri" & between(order, 7950, 8046) ~ "red",
      region == "tennessee" & between(order, 11915, 11950) ~ "red",
      region == "west virginia" & between(order, 15033, 15141) ~ "red",
      region == "pennsylvania" & between(order, 11322, 11341) ~ "red",
      region == "maryland" & between(order, 5443, 5457) ~ "red",
      region == "maryland" & between(order, 5468, 5478) ~ "red",
      region == "delaware" & between(order, 1357, 1438) ~ "red",
      TRUE ~ "black"
    ),
    border_width = case_when(
      border_color == "red" ~ 2,
      TRUE ~ 0.5
    )
  ) |> 
  ggplot() +
  geom_polygon(aes(x=long, y=lat, group = group),
               color = "black", fill = "#FCF2DA", linewidth = 0.5) +
  geom_path(aes(x=long, y=lat, group = group, color = border_color, linewidth = border_width)) +  
  ggtext::geom_richtext(aes(x = x_adj, y = y, label = text, size = size, angle = angle),
                        label.padding = grid::unit(rep(0, nrow(text_dat)), "pt"),
                        label.color = NA,
                        fill = NA,
                        data=text_dat |>
                          filter(display_step != 0)) + # Set 1 for red text reveal and 0 for all text
  geom_label(aes(x = long, y = lat, label = Abb, angle = angle),
             family = "Puritan", size = 10,
             label.padding = grid::unit(rep(0.01, nrow(new_centroids)), "pt"),
             label.r = grid::unit(rep(0, nrow(new_centroids)), "pt"),
             fill = "#FCF2DA",
             border.color = NA,
             data = new_centroids |> 
               filter(Abb == "MD.")) +
  geom_text(aes(x = long, y = lat, label = Abb, angle = angle),
             family = "Puritan", size = 16,
             data = new_centroids |> 
               filter(Abb !="MD.")) +
  scale_color_identity() +
  scale_linewidth_identity() +
  scale_size_identity() +
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

ggsave(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "01_full_test.png"),
       plot = plot, width = 5732, height = 2700, units = "px", dpi = 300)

img_main_grob <- png::readPNG(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "01_full_test.png")) |> 
  grid::rasterGrob()

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


ggsave(here::here("Slow Reveal Recreations", "anti-lynching-bill-map", "recreated-imgs", "04_full_img.png"), 
       plot = plot_test, width = 5732, height = 2700, units = "px", dpi = 300)

#plot with state abbreviations
plot2 <- plot + with(new_centroids, 
                     annotate(geom = "text", x = long, y = lat, label = Abb, family = "Puritan"))
