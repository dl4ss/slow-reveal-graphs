# TITLE: Recreation of Annual numbers of patents issued, logarithmic scale, 1790-1860
# ORIGINAL SOURCE: https://www.nber.org/system/files/chapters/c8014/c8014.pdf
# SOURCE DATA: https://www.uspto.gov/web/offices/ac/ido/oeip/taf/h_counts.htm
# Author: Louis Cousino
# DATE: 02/04/2025


# Loading Packages
library(tidyverse)
library(ggtext)
library(rvest)
library(polite)


# Importing Data

# Asking for permission

session <- bow("https://www.uspto.gov/web/offices/ac/ido/oeip/taf/h_counts.htm")

html_page <- scrape(session)

#Extracting Table

pto_table_list <- html_page |> 
  html_elements("table") |> 
  html_table()

pto_table <- pto_table_list[[3]]

# Filtering Data and Renaming Variables

colnames(pto_table) <- colnames(pto_table) |> 
  str_replace_all(" ", "\\.") |> 
  str_replace_all("\\(|\\)", "") |> 
  str_to_lower()

pto_table_filtered <- pto_table |> 
  select(calendar.year, utilitypatents.einventions) |> 
  mutate(utilitypatents.einventions = str_replace(utilitypatents.einventions, ",",""),
         across(everything(), as.numeric)) |> 
  filter(calendar.year <= 1860)

# Creating Graphic

pto_table_filtered |> 
  ggplot(aes(x = calendar.year, y = utilitypatents.einventions)) +
  geom_line() +
  labs(caption = "<b>Fig. 8.1  Annual numbers of patents issued, logarithmic scale, 1790-1860</b><br><i>Source</i>: Bureau of the Census (1975, ser. W99).") +
  scale_x_continuous(breaks = (c(seq(1790,1860,10))),
                     minor_breaks = c(seq(1790,1860,1)),
                     expand = expansion(mult = c(0,0)),
                     guide = guide_axis(minor.ticks = TRUE)) +
  scale_y_log10(breaks = c(1,10,100,1000,10000),
                minor_breaks = c(seq(2,9,1),seq(20,90,10), seq(200,900,100),seq(2000,9000,1000)),
                expand = expansion(mult = c(0,0)),
                guide = guide_axis(minor.ticks = TRUE),
                limits = c(1,10000)) +
  theme_bw() +
  theme(axis.title = element_blank(),
        axis.text = element_text(face = "bold",
                                 color = "black"),
        panel.grid.major.x = element_blank(),
        panel.grid.minor.x = element_blank(),
        panel.grid.major.y = element_line(linetype = "dotted",
                                          color = "black",
                                          linewidth = 0.25),
        panel.grid.minor.y = element_line(linetype = "dotted",
                                          color = "black",
                                          linewidth = 0.25),
        plot.caption = element_markdown(vjust = 1,
                                        hjust = 0),
        plot.margin = margin(15,15,10,10))

# Saving plot
ggsave(here::here("Slow Reveal Recreations", "patents-growth", "recreated-imgs", "patents_1790_1860.png"), width = 1934/150, height = 1470/150,
       units = "in")
  