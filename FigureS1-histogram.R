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

d <- occ_download_get('0005799-231002084531237',
                      path = "data/") |> # save to data directory
  occ_download_import() |>
  # Rename recode basis of record factors
  mutate(basisOfRecord = recode(basisOfRecord,
                                "HUMAN_OBSERVATION" = "Human Observation",
                                "OCCURRENCE" = "Occurence",
                                "PRESERVED_SPECIMEN" = "Preserved Specimen"))
names(d)  # check names

# clean time data
d |>
  # fill missing day/month/year
  # first from eventDate, second from dateIdentified - same for IEO institution
  mutate(year = case_when(is.na(year) ~ year(eventDate),
                          .default = year),
         month = case_when(is.na(month) ~ month(eventDate),
                           .default = month),
         day = case_when(is.na(day) ~ day(eventDate),
                         .default = day),
         year = case_when(is.na(year) ~ year(dateIdentified),
                          .default = year),
         month = case_when(is.na(month) ~ month(dateIdentified),
                           .default = month),
         day = case_when(is.na(day) ~ day(dateIdentified),
                         .default = day)
  )



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
d |>
  filter(institutionCode != "iNaturalist") |>
  group_by(basisOfRecord, year) |>
  count() |>
  ggplot(aes(x = year, y = n)) +
  geom_bar(aes(fill = basisOfRecord), stat = "identity") +
  scale_x_continuous(breaks = c(seq(1880, 2025, by=10)))


d |>
  group_split(basisOfRecord) |>
  map(group_by,institutionCode, year) |>
  map(count) -> basis

basis[[1]] |> View()
basis[[2]] |> View()
basis[[3]] |> View()

basis|> map(~ ggplot(.x, aes(x = year)) +
        geom_bar(aes(fill = institutionCode)), stat = "identity")


# Find missing institution codes dataset key
d |>
  filter(institutionCode == "") |>
  distinct(datasetKey)

# look up organisation via the datasetKey: # https://www.gbif.org/dataset/<datasetKey>
# datasetKey - assigned InsitutionCode - description
# 8e9974a4-12e2-475f-9aec-5abdf24a1f50 - OFB-CRNS-MNHN Paris - French program CROMIS
# 25e3e8f1-86ee-440c-adfc-8e63759a6505 - OFB-CRNS-MNHN Paris - BioObs
# 79e21918-887b-4b40-9305-abc5f57494dd - CMS-UAlg - Luix Saldanha Marine Park
# 7ebef267-9d72-4c21-a276-cc84281a8590 - NatureMapr - Australia
# a0a4d131-f53f-43b2-a1ba-254473b8a006 - USGS - FL biodiversity Collection, FL Fish Wildlife commission
# 27c84cf2-c04f-444a-9884-9d499533c4ba - Flanders Marine Institute (FMI) - Programa Poseidon
# 9a025855-803d-4fa7-8417-ac7142544553 - Not found
# 266628c1-56a0-46cb-b136-3b77dbc32268 - Flanders Marine Institute - BIOFUN trans-Mediterranean deep-sea cruise
# 7c0cd863-8b81-4937-84f9-2f596fd3fa79 - Pacific Community (SPC) - SPC NECTALIS
# 350f00a7-db1f-4133-bc07-71de716339da - USGS - Rockfish Recruitment and Ecosystem Assessment Survey

# assign each code with project code

View()
  mutate(year = case_when())
  filter(institutionCode == "IEO")


