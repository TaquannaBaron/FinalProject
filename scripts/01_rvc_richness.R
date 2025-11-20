# MMES 565 - Project 1
# Goal: Use rvc package to get USVI fish data,
#       calculate species richness by site,
#       explore habitat patterns,
#       look at the top 20 species,
#       make simple plots, and export a CSV for ArcGIS.

#Setwd

getwd()

#Load packages

install.packages("devtools")

devtools::install_github("jeremiaheb/rvc", force = TRUE)

library(tidyverse)  
library(janitor)  
library(rvc) 
library (stringr)

#Load data from rvc

USVI <- getRvcData(
  years   = 2017:2023,
  regions = c("STTSTJ", "STX")
)

names(USVI)

sample_raw <- USVI$sample_data

str(sample_raw)
glimpse(sample_raw)



#Clean up names & select columns I need

sample <- sample_raw %>%
  janitor::clean_names() %>%    
  dplyr::select(
    primary_sample_unit,       
    species_cd,                
    num,                    
    year,
    lat_degrees,
    lon_degrees,
    habitat_cd,
    region
  ) %>%
  dplyr::rename(
    site_id = primary_sample_unit,
    species = species_cd,
    count   = num,
    lat     = lat_degrees,
    lon     = lon_degrees,
    habitat = habitat_cd
  )

glimpse(sample)
sapply(sample, class)

sample_present <- sample %>%
  filter(count > 0)

glimpse(sample_present)

#Site-level coordinates

site_coords <- sample %>%
  filter(!is.na(lat), !is.na(lon)) %>%
  group_by(site_id) %>%
  summarise(
    lat = mean(lat, na.rm = TRUE),
    lon = mean(lon, na.rm = TRUE)
  )

head(site_coords)

#Species richness by site

richness_site <- sample %>%
  group_by(site_id) %>%
  summarise(
    species_richness = n_distinct(species)
  ) %>%
  left_join(site_coords, by = "site_id")

head(richness_site)

#Species richness by site and habitat

richness_site_habitat <- sample %>%
  group_by(site_id, habitat) %>%
  summarise(
    species_richness = n_distinct(species)
  )

head(richness_site_habitat)

#Species richness by site and year

richness_site_year <- sample %>%
  distinct(site_id, year, species) %>%
  count(site_id, year, name = "richness")

head(richness_site_year)

#Export CSVs

write_csv(richness_site,
          "outputs/fish_richness_site.csv")

write_csv(richness_site_habitat,
          "outputs/fish_richness_site_habitat.csv")

write_csv(richness_site_year,
          "outputs/fish_richness_site_year.csv")

#Number of unique sites

n_sites <- sample_present %>%
  summarise(n_sites = n_distinct(site_id))

n_sites

#Number of unique species

n_species <- sample_present %>%
  summarise(n_species = n_distinct(species))

n_species

#Year range

years_summary <- sample_present %>%
  summarise(
    min_year = min(year),
    max_year = max(year),
    n_years = n_distinct(year)
  )

years_summary

#Sites by region

sites_by_region <- sample_present %>%
  distinct(site_id, region) %>%
  count(region, name = "n_sites")

sites_by_region

write_csv(sites_by_region, "outputs/sites_by_region.csv")

#Top 20 species

top_species <- sample_present %>%
  count(species, sort = TRUE) %>%
  slice_max(n, n = 20)

top_species

write_csv(top_species, "outputs/top_species.csv")



