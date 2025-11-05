#
# FILE:
# hull_house_map1.R
#
# DESCRIPTION:
# This code is for the Hull House Nationality Map of a section of Chicago by Florence Kelley.  
# The png shows the final map which could be turned into a slow reveal in the future while the key folder contains color key for the specific colors in the code.  
# Source: https://homicide.northwestern.edu/docs_fk/homicide/HullHouse/NATMAP1.pdf  
#
# SLOW REVEAL ORDER:
#   
# N: RECREATED GRAPHIC
#
# AUTHORS:
#   Robert Bilyk (main code)
# Ian Curtis (2023, code to save plots)


library(tidyverse)
library(showtext)

font_add_google(name = "Space Mono", family = "Text")
font_add_google(name = "Cutive Mono", family = "Main")

showtext_auto()

# Importing data

segments <- readxl::read_xlsx(here::here("Slow Reveal Recreations", "hull-house-nationalities", "segments.xlsx"))

polygons <- readxl::read_xlsx(here::here("Slow Reveal Recreations", "hull-house-nationalities", "polygons.xlsx"))

# Forming polygon data

poly_data1 <- polygons |>
  # rowwise() |>
  mutate(group = 1:n()) |>
  pivot_longer(cols = starts_with("x"), values_to = "x", names_to = NULL) |>
  mutate(id = 1:n()) |>
  select(-starts_with("y"))

poly_data2 <- polygons |>
  # rowwise() |>
  mutate(group = 1:n()) |>
  pivot_longer(cols = starts_with("y"), values_to = "y", names_to = NULL) |>
  mutate(id = 1:n()) |>
  select(id, y)

poly_data <- poly_data1 |>
  left_join(poly_data2, join_by(id))

# Drawing initial maps

# block for previewing
block_filter <- 6.1

segments |> 
  filter(block == block_filter) |>
  ggplot() +
  geom_polygon(aes(x = x, y = y, fill = fill, group = group),
               color = NA,
               linewidth = 0,
               data = poly_data |>
                 filter(block == block_filter)) +
  geom_segment(aes(x = x1, y = y1, xend = x2, yend = y2, linetype = linetype)) +
  scale_linetype_identity() +
  theme_void()





