## Description: Map of Global Biodiversity Information Facility pyrosome data faceted by year
## Date: Oct 7, 2023
## Author: Catherine Kim

library(rgbif)
library(ggplot2)
library(dplyr)
library(purrr)
library(sf)
library(rnaturalearth)

#### Get pyrosome Global Biodiversity Information Facility (GBIF) download ####
# GBIF download request from search using the "Pyrosoma atlanticum" taxon key
# doi:  https://doi.org/10.15468/dl.vv3adq

d <- occ_download_get('0005799-231002084531237') %>%
  occ_download_import() |>
  # Rename 4 to 4wd, f to Front, r to Rear
  mutate(basisOfRecord = recode(basisOfRecord,
                                "HUMAN_OBSERVATION" = "Human Observation",
                                "OCCURRENCE" = "Occurence",
                                "PRESERVED_SPECIMEN" = "Preserved Specimen"))
names(d)

#### World map of occurrences ####
worldmap <- ne_countries(scale = 'medium', type = 'map_units',returnclass = 'sf')

d_sf <- sf::st_as_sf(d, coords = c("decimalLongitude", "decimalLatitude"),
                     crs = "EPSG:4326")


# count number per basisOfRecord
# in figure 2 caption, map of GBIF pyrosome data
d |>
  group_by(basisOfRecord) |>
  count()

d |>
  group_split(basisOfRecord) |>
  map(group_by,institutionCode) |>
  map(count)

# filter latitude between -23.5 and 23.5
trop <- d |>
  filter(decimalLatitude >= -23.4362 & decimalLatitude <= 23.4362) |>
  st_as_sf(coords = c("decimalLongitude", "decimalLatitude"),
           crs = "EPSG:4326")

# count tropical observations by country
trop |>
  group_by(countryCode) |>
  count()
1 - (189/3604) # % in subtropics/temperate

# Timor-Leste points of pyrosome sightings
behau <- st_point(c(124.8, -8.5))
beloi <- st_point(c(124.6, -8.2))

tl_pts <- st_sfc(behau, beloi, crs = "WGS84")

# plot pyrosome data in tropics
ggplot() +
  geom_sf(data = worldmap, fill = "gray85",  color = NA) +
  geom_sf(data = trop, aes(color = countryCode), size = 1.5, alpha = 0.5)
# NA country looks to be off the coast of Africa

# break into time bins present after 2010, 1990-2010, pre 1990
dt <- d_sf |>
  mutate(time = if_else(year >= 2010, "2010-2023",
                        ifelse(year >= 1990 & year < 2010, "1990-2009",
                               ifelse(year < 1990, "1880-1990", "No Date")
                        )
  )
  )

# facet by basis of record
ggplot() +
  geom_sf(data = worldmap, fill = "gray85",  color = NA) +
  geom_sf(data = dt, aes(color=time), alpha = 0.3, size = 0.8) +
  scale_color_discrete(type = RColorBrewer::brewer.pal(4, "Dark2"), # dark pal for visiiblity
                       name = "",
                       breaks = c("1880-1990", "1990-2009", "2010-2023", NA),
                       labels = c("1880-1990", "1990-2009", "2010-2023", "No Date")
  ) +
  facet_grid(rows = vars(basisOfRecord), switch = "y") +
  # add observataion locations in Timor-Leste
  geom_sf(data = tl_pts, color = "red", size = 1.9, pch = 18) +
  # add tropics lines of latitude
  geom_hline(yintercept = 23.5, linetype = "dashed", color = "gray20") +
  geom_hline(yintercept = -23.5, linetype = "dashed", color = "gray20") +
  annotate("text", x = -180, y = 30, label = "23.5", size = 2) +
  annotate("text", x = -180, y = -30, label = "-23.5", size = 2) +
  labs(y = "", x = "", title = "Figure 2") +
  theme(text = element_text(size = 14),
        panel.background = element_blank(),
        panel.border = element_rect(fill = NA),
        plot.margin = unit(c(0, 0, 0, -1), "lines"),
        panel.ontop = FALSE,
        legend.position = c(.14, .42),
        legend.background = element_blank(),
        legend.key = element_blank(),  # removes gray box behind point in legend
        legend.key.size = unit(2,"point")) +
  guides(color = guide_legend(override.aes = list(size = 2,
                                                  alpha = 1) ) )

# change file extension for different files e.g., pdf, png
ggsave("figures/gbif-yearcolor-facetrecord.pdf",
       height = 22, width =18, units = "cm", dpi = 600)
