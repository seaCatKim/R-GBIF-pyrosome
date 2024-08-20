## Copernicus Chlorophyll a data
## Credits: EU Copernicus Marine Service information - https://marine.copernicus.eu
## Product: OCEANCOLOUR_GLO_BGC_L3_MY_009_103
## Dataset: cmems_obs-oc_glo_bgc-plankton_my_l3-multi-4km_P1D
## Variable: Mass concentration of chlorophyll a in sea water (CHL) milligram m-3
## Beh'au Geometry: POLYGON((125.85619160588331 -8.441231132291861
## 125.85481831486769 -8.47693669869811
## 125.9262294476802 -8.47968328072936
## 125.93172261174269 -8.43848455026061
## 125.93172261174269 -8.440544486784047
## 125.85619160588331 -8.441231132291861))
## Beloi Geometry: POINT(125.63097187932082 -8.231117606901234)
## Values from graph: v(t): value vs. time

library(tidyverse)

# read in Behau and Beloi Chl a data
beh <- read.csv("data/Behau-4closestpixels.csv") |>
  mutate(time = as.Date(time),
         year = year(time),
         month = month(time),
         day = day(time),
         Site = "Be'hau") |> # add site name
  drop_na()
# |>
#   filter(year == 2019) # only look at relevant year
plot(beh$time, beh$CHL)

bel <- read.csv("data/Beloi.csv") |>
  mutate(time = as.Date(time),
         year = year(time),
         month = month(time),
         day = day(time),
         Site = "Beloi") |> # add site name
  drop_na()
# |>
#   filter(year == 2019)
plot(bel$time, bel$CHL)

# combine into one dataframe for plot
chl <- rbind(beh, bel)

ggplot(chl, aes(x = time, y = CHL)) +
  geom_point(color = "palegreen3") +
  #geom_smooth(lty = "dashed", color = "gray50", alpha = 0.8) +
  facet_wrap(vars(Site), nrow = 2, strip.position = "top",
             scales = "free_y") +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0)) +
  labs(x = "", y = "Chlorophyll a [mg m-3]") +
  scale_x_date(limits = as.Date(c("2019-01-01", "2019-12-14")),
               date_breaks = "1 month",
               date_labels = "%b") +
  # lines on dates where pyrosomes were observed
  # Beloi
  geom_vline(xintercept = as.Date("2019-09-24"), lty = 2, color = "gray40") +
  # Be'hau
  geom_vline(xintercept = as.Date("2019-10-8"), lty = 2, color = "gray40")

ggsave("plots/chl-a.jpg",
       width = 12, height = 8, units = "cm", dpi = 600)

# Summary stats
# annual site average
chl |>
  group_by(Site) |>
  summarize(ave = mean(CHL), SD = sd(CHL))

# monthly average
chl_ave <- chl |>
  group_by(month, as.factor(Site))|>
  summarize(ave = mean(CHL), SD = sd(CHL), SE = SD/sqrt(n()))
chl_ave


## Southern Oscillation Index data
## downloaded from https://www.cpc.ncep.noaa.gov/data/indices/soi on 20 August 2024
soi <- read.csv("data/SOI-sealevelpress-standardized.csv",
                na.strings = "-999.9")
anomaly <- read.csv("data/SOI-sealevelpress-anomaly.csv",
                    na.strings = "-999.9")

l_soi <- list(soi = soi, anomaly = anomaly) # store in a list to map functions

# reformat data

(soi_long <- l_soi %>%
    map(pivot_longer, JAN:DEC, names_to = "MONTH", values_to = "SOI") %>%
    map(mutate, DATE = paste(YEAR, MONTH, sep = "-"),
               DATE = parse_date_time(DATE, "ym"))
  )

# plot all data
soi_long %>%
  map(~ggplot(.x, aes(x = DATE, y = SOI)) +
        geom_point() +
        theme_bw())

# subset matching data with CHL period starting Sept 1997
soi_long %>%
  map(filter, DATE >= "1997-09-01") %>%
  map(~ggplot(.x, aes(x = DATE, y = SOI)) +
        geom_point() +
        geom_line() +
        theme_bw())

