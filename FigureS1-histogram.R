## Description: Plot histogram of Global Biodiversity Information Facility pyrosome data by year
## Date: Oct 7, 2023
## Author: Catherine Kim

library(rgbif)
library(ggplot2)
library(dplyr)
library(sf)

#### Get pyrosome Global Biodiversity Information Facility (GBIF) download ####
# GBIF download request from search using the "Pyrosoma atlanticum" taxon key
# doi:  https://doi.org/10.15468/dl.vv3adq

d <- occ_download_get('0005799-231002084531237') %>%
  occ_download_import() |>
  # Rename recode basis of record factors
  mutate(basisOfRecord = recode(basisOfRecord,
                                "HUMAN_OBSERVATION" = "Human Observation",
                                "OCCURRENCE" = "Occurence",
                                "PRESERVED_SPECIMEN" = "Preserved Specimen"))
names(d)  # check names

# Basis of Record info
# https://docs.gbif.org/course-data-use/en/basis-of-record.html
# human observation = an output of human observation process eg. evidence of an occurrence taken from field notes or literature or a records of an occurrence without physical evidence nor evidence captured with a machine.
# occurrence = ??
# preserved specimen = a speciment that has been preserved, for example, a plant on an herbarium shett or a cataloged lot of fish in a jar.

#### Histogram of observations by year ####
ggplot(d, aes(year, fill = basisOfRecord)) +
  geom_histogram(binwidth = 1) +
  theme_bw() +
  labs(x = "Year", y = "") +
  scale_x_continuous(breaks = c(seq(1880, 2025, by=10))) +
  scale_fill_discrete(name = "Basis of Record") +
  #                    breaks = c("HUMAN_OBSERVATION", "OCCURRENCE", "PRESERVED_SPECIMEN"),
  #                    labels = c("Human Observation", "Occurence", "Preserved Specimen")) +
  theme(text = element_text(size=10),
        legend.title = element_text(size = 10),
        axis.text.x = element_text(size = 10),
        panel.grid = element_blank(),
        legend.position = c(0.14, 0.87),
        plot.margin = unit(c(0, 0, 0, 0), "lines"))

# save histogram in plots folder
# change file extension for different files e.g., pdf, png
ggsave("figures/gbif-histogram.jpg",
       width = 18, height = 12, units = "cm", dpi = 600)

# range of event date
str(d)
range(d$eventDate, na.rm = TRUE)
range(d$decimalLatitude, na.rm = TRUE)
range(d$decimalLongitude, na.rm = TRUE)

no_time <- d[is.na(d$eventDate), ]
no_time

#### Histogram of observations by year without iNaturalist ####
#d_noNat <-



d |>
  group_split(basisOfRecord) |>
  map(group_by,institutionCode, year) |>
  map(count) -> basis

basis[[1]] |> View()
basis[[3]]

basis|> map(~ ggplot(.x, aes(x = year)) +
        geom_bar(aes(fill = institutionCode)), stat = "identity")

# fix IEO year


d |>
  filter(institutionCode != "iNaturalist") |>
  group_by(basisOfRecord, year) |>
  count() |>
  ggplot(aes(x = year, y = n)) +
  geom_bar(aes(fill = basisOfRecord), stat = "identity") +
  scale_x_continuous(breaks = c(seq(1880, 2025, by=10)))
