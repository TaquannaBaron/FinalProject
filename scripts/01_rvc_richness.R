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

devtools::install_github("jeremiaheb/rvc")

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

#Species richness by site, year, and habitat (for habitat plots)

site_year_richness_habitat <- sample_present %>%
  group_by(site_id, year, habitat) %>%
  summarise(
    species_richness = n_distinct(species),
    .groups = "drop"
  )

summary(site_year_richness_habitat$species_richness)
head(site_year_richness_habitat)


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

#Mean species richness by habitat

habitat_names <- c(
  "AGRF" = "Aggregate Reef",
  "BDRK" = "Bedrock",
  "HARD" = "Hardbottom",
  "PTRF" = "Patch Reef",
  "PVMT" = "Pavement",
  "SCR"  = "Scattered Coral / Rubble"
)

richness_habitat_stats <- site_year_richness_habitat %>%
  group_by(habitat) %>%
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness   = sd(species_richness, na.rm = TRUE),
    n_samples     = n(),
    se_richness   = sd_richness / sqrt(n_samples)
  )

richness_habitat_stats

#Mean species richness

p_rich_habitat <- richness_habitat_stats %>%
  ggplot(aes(x = habitat, y = mean_richness, fill = habitat)) +
  geom_col() +
  geom_errorbar(aes(
    ymin = mean_richness - se_richness,
    ymax = mean_richness + se_richness
  ), width = 0.2) +
  scale_fill_brewer(palette = "Set2", labels = habitat_names) +
  theme_minimal() +
  labs(
    title = "Mean Fish Species Richness by Habitat (USVI, 2017–2023, ± SE)",
    x = "Habitat Code",
    y = "Mean Species Richness per Site-year",
    fill = "Habitat Type"
  )

p_rich_habitat

ggsave("figs/fig_richness_by_habitat.png",
       p_rich_habitat, width = 7, height = 5, dpi = 300)


#Distribution of site-level richness by habitat 

p_box_habitat <- site_year_richness_habitat %>%
  ggplot(aes(x = habitat, y = species_richness, fill = habitat)) +
  geom_boxplot() +
  scale_fill_brewer(palette = "Set2", labels = habitat_names) +
  theme_minimal() +
  labs(
    title = "Distribution of Site-year Fish Species Richness Across Habitats",
    x = "Habitat Code",
    y = "Species Richness per Site-year",
    fill = "Habitat Type"
  )

p_box_habitat

ggsave("figs/fig_boxplot_richness_habitat.png",
       p_box_habitat, width = 7, height = 5, dpi = 300)

#Mean species in each region by year

richness_region_year <- sample_present %>%
  group_by(site_id, year, region) %>%
  summarise(
    species_richness = n_distinct(species),
    .groups = "drop"
  )

region_year_stats <- richness_region_year %>%
  group_by(region, year) %>%
  summarise(
    mean_richness = mean(species_richness, na.rm = TRUE),
    sd_richness   = sd(species_richness, na.rm = TRUE),
    n_samples     = n(),
    se_richness   = sd_richness / sqrt(n_samples),
    .groups = "drop"
  )

p_region_line <- region_year_stats %>%
  ggplot(aes(x = year, y = mean_richness, color = region, group = region)) +
  geom_line(size = 1.2) +
  geom_point(size = 2) +
  geom_ribbon(aes(
    ymin = mean_richness - se_richness,
    ymax = mean_richness + se_richness,
    fill = region
  ), alpha = 0.15, color = NA) +
  scale_color_brewer(palette = "Set2") +
  scale_fill_brewer(palette = "Set2") +
  theme_minimal() +
  labs(
    title = "Mean Fish Species Richness by Region Over Time (USVI)",
    x = "Year",
    y = "Mean Species Richness per Site-Year",
    color = "Region",
    fill = "Region"
  )
p_region_line

ggsave("figs/fig_region_richness_line.png",
       p_region_line, width = 7, height = 5, dpi = 300)
