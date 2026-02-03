#
# FILE:
#  image_compare.R
#
# DESCRIPTION:
#
# R script comparing our recreations with Friendly's HistData package.
#
# AUTHORS:
#   Louis Cousino (main code)
#

# Loading packages

library(tidyverse)

hist_datasets <- tibble::tribble(
  ~Item,                ~Title,
  "Arbuthnot",          "Arbuthnot’s Data on Male and Female Birth Ratios",
  "Armada",             "La Felicisima Armada",
  "Bowley",             "Bowley’s data on values of British and Irish trade, 1855-1899",
  "Breslau",            "Halley’s Breslau Life Table",
  "Cavendish",          "Cavendish’s Determinations of the Density of the Earth",
  "ChestSizes",         "Chest measurements of Scottish Militiamen",
  "ChestStigler",       "Chest measurements of Scottish Militiamen",
  "Cholera",            "William Farr’s Data on Cholera in London, 1849",
  "CholeraDeaths1849",  "Daily Deaths from Cholera and Diarrhaea in England, 1849",
  "CushnyPeebles",      "Cushny-Peebles Data: Soporific Effects of Scopolamine Derivatives",
  "CushnyPeeblesN",     "Cushny-Peebles Data: Soporific Effects of Scopolamine Derivatives",
  "Dactyl",             "Edgeworth’s counts of dactyls in Virgil’s Aeneid",
  "DrinksWages",        "Elderton and Pearson’s (1910) data on drinking and wages",
  "EdgeworthDeaths",    "Edgeworth’s Data on Death Rates in British Counties",
  "Fingerprints",       "Waite’s data on Patterns in Fingerprints",
  "Galton",             "Galton’s data on the heights of parents and their children",
  "GaltonFamilies",     "Galton’s data on the heights of parents and their children, by child",
  "Guerry",             "Data from A.-M. Guerry, “Essay on the Moral Statistics of France”",
  "HalleyLifeTable",    "Halley’s Life Table",
  "Jevons",             "W. Stanley Jevons’ data on Numerical Discrimination",
  "Langren.all",        "van Langren’s Data on Longitude Distance between Toledo and Rome",
  "Langren1644",        "van Langren’s Data on Longitude Distance between Toledo and Rome",
  "Macdonell",          "Macdonell’s Data on Height and Finger Length of Criminals, used by Gosset (1908)",
  "MacdonellDF",        "Macdonell’s Data on Height and Finger Length of Criminals, used by Gosset (1908)",
  "Mayer",              "Mayer’s Data on the Libration of the Moon.",
  "Michelson",          "Michelson’s Determinations of the Velocity of Light",
  "MichelsonSets",      "Michelson’s Determinations of the Velocity of Light",
  "Minard.cities",      "Data from Minard’s famous graphic map of Napoleon’s march on Moscow",
  "Minard.temp",        "Data from Minard’s famous graphic map of Napoleon’s march on Moscow",
  "Minard.troops",      "Data from Minard’s famous graphic map of Napoleon’s march on Moscow",
  "Nightingale",        "Florence Nightingale’s data on deaths in the Crimean War",
  "OldMaps",            "Latitudes and Longitudes of 39 Points in 11 Old Maps",
  "PearsonLee",         "Pearson and Lee’s data on the Heights of Parents and Children by Gender",
  "Playfair1824",       "Playfair’s Linear Chronology",
  "PolioTrials",        "Polio Field Trials Data",
  "Pollen",             "Pollen Data Challenge",
  "Prostitutes",        "Parent-Duchatelet’s time-series data on the number of prostitutes in Paris",
  "Pyx",                "Trial of the Pyx",
  "Quarrels",           "Statistics of Deadly Quarrels",
  "Saturn",             "Laplace’s Saturn data.",
  "Snow.dates",         "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Snow.deaths",        "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Snow.deaths2",       "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Snow.polygons",      "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Snow.pumps",         "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Snow.streets",       "John Snow’s Map and Data on the 1854 London Cholera Outbreak",
  "Virginis",           "John F. W. Herschel’s Data on the Orbit of the Twin Stars gamma Virginis",
  "Virginis.interp",    "John F. W. Herschel’s Data on the Orbit of the Twin Stars gamma Virginis",
  "Wheat",              "Playfair’s Data on Wages and the Price of Wheat",
  "Wheat.monarchs",     "Playfair’s Data on Wages and the Price of Wheat",
  "Yeast",              "Student’s (1906) Yeast Cell Counts",
  "YeastD.mat",         "Student’s (1906) Yeast Cell Counts",
  "ZeaMays",            "Darwin’s Heights of Cross- and Self-fertilized Zea May Pairs"
)

recreated <- read_delim(here::here("Image Compairson", "Graphics for R Recreation - U.S. History.csv"),
                        delim = ",") |> 
  select("Title", "Creator") |> 
  mutate(combined = str_c(Title, " ", Creator)) |> 
  filter(is.na(combined) == FALSE)


list_to_bind <- list()

for (i in 1:nrow(hist_datasets)) {
  
  list_to_bind[[i]] <- tibble(
    previous_folder = recreated$combined,
    # previous_path = previous$value,
    # previous_number = previous$number,
    # current_path = rep(current$value[i], times = nrow(previous)),
    # current_number = rep(current$number[i], times = nrow(previous)),
    current_folder = rep(hist_datasets$Title[i], times = nrow(recreated)),
    # current_file = rep(current$file[i], times = nrow(previous)),
    match = 0,
    pct_match = 0
  )
  
}

examine <- list_to_bind |> 
  bind_rows()


for (i in 1:nrow(examine)) {
  
  count <- 0 # Setting initial match count to 0
  
  for (j in 1:(str_count(examine$previous_folder[i], boundary("word")))) {
    
    target_word <- str_extract_all(examine$previous_folder[i], boundary("word"))[[1]][j] # This parses the current string into its constituent words and then pulls them one at a time.
    
    if (str_detect(examine$current_folder[i], str_c("(?<!\\w)", target_word, "(?!\\w)")) == TRUE) {
      
      count <- count + 1
      
    }
    
  }
  
  examine$match[i] <- count
  examine$pct_match[i] <- count/(str_count(examine$previous_folder[i], boundary("word")))
  
}

examine_filtered <- examine |> 
  filter(pct_match >= 0.5) |> 
  mutate(folder_match = case_when(pct_match == 1 ~ "Y",
                                 TRUE ~ ""))

exact_match <- examine_filtered |> 
  filter(folder_match == "Y") |> 
  pull(current_folder) |> 
  as_tibble()

# Removing that matching folders from the overall list.

non_match <- examine |> 
  anti_join(exact_match, by = join_by("current_folder" == "value"))

non_match <- non_match |> 
  anti_join(exact_match, by = join_by("previous_folder" == "value"))

# Cleaning non-matching list

non_match <- non_match |> 
  select(ends_with("folder")) |> 
  pivot_longer(cols = everything()) |> 
  distinct(value, .keep_all = TRUE) |> 
  rename("source" = name,
         "folder" = value) |>
  mutate(source = str_replace_all(source, "_folder", "")) |> 
  arrange(source)

