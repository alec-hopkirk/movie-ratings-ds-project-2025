#**************************movies-ds-project***********************************
#author:alec_hopkirk
#date:20251012
#description: main script for processing raw movie ratings and tag data
#
#******************************************************************************

# Initialize env ----------------------------------------------------------

rm(list = ls())
cat("\014")
dev.off()

# Load libraries ----------------------------------------------------------

library(pacman)
libraries <- c("plotly",
               "tidyverse")
p_load(char = libraries)

# Global variables --------------------------------------------------------

projectDir <- getwd()
rawDataFolder <- "data_raw/ml-32m/"
cleanDataFolder <- "data_clean/"

# Import raw data ---------------------------------------------------------

movieDataRaw <- read_csv(paste0(rawDataFolder, "movies.csv"))
ratingsDataRaw <- read_csv(paste0(rawDataFolder, "ratings.csv"))
userMovieTagsRaw <- read_csv(paste0(rawDataFolder, "tags.csv"))

# Clean and Aggregate Data ------------------------------------------------

# Summarize user ratings
ratingsSummaryData <- ratingsDataRaw %>%
  group_by(movieId) %>%
  summarise(., 
            "Average_Rating" = mean(rating),
            "Rating_Count" = n()) %>%
  na.omit()


# Unite and clean ratings and movie classification data

movieGenres <- movieDataRaw$genres %>%
  unlist() %>%
  lapply(., strsplit, split = "\\|") %>%
  unlist() %>%
  unique()

movieDataMain <- movieDataRaw %>%
  na.omit() %>%
  left_join(., ratingsSummaryData, by = join_by(movieId == movieId)) %>%
  mutate(., "genres" = strsplit(genres, split = "\\|")) %>%
  unnest(., genres) %>%
  mutate(., reporter = 1) %>%
  pivot_wider(., names_from = genres, values_from = reporter)

movieDataMain[is.na(movieDataMain)] <- 0

# Export Clean Data -------------------------------------------------------

write_csv(movieDataMain, paste0(cleanDataFolder, "cleanMovieData.csv"))
