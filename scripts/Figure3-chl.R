## Description: Copernicus Chlorophyll a data
## Date: March 9, 2025
## Author: Catherine Kim
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

# install.packages("tidyverse")
library(tidyverse)
# install.packages("patchwork")
library(patchwork)
# install.packages("zoo")
library(zoo)
# install.packages("ggtext")
library(ggtext) # ggplot superscript in label

# read in Behau and Beloi Chl a data
beh <- read.csv("data/Behau-4closestpixels.csv") |>
  mutate(time = as.Date(time),
         year = year(time),
         month = month(time),
         day = day(time),
         Site = "Be'hau") |> # add site name
  drop_na()
plot(beh$time, beh$CHL)

ggplot(beh, aes(x = time, y = CHL)) + geom_line() + scale_y_log10()

bel <- read.csv("data/Beloi.csv") |>
  mutate(time = as.Date(time),
         year = year(time),
         month = month(time),
         day = day(time),
         Site = "Beloi") |> # add site name
  drop_na()

plot(bel$time, bel$CHL)

ggplot(bel, aes(x = time, y = CHL)) + geom_line() + scale_y_log10()

# filter 2019 CHL data and combine into one dataframe for plot
chl <- rbind(beh |> filter(year == 2019),
             bel |> filter(year == 2019))
range(chl$CHL)
summary(chl)

site_labels <- c( `Be'hau` = "(a)",
                            `Beloi` = "(b)")

# plot chl-a abundance for two sites over 2019
ggplot(chl, aes(x = time, y = CHL)) +
  geom_point(color = "palegreen3", size = 0.8) +
  #geom_smooth(lty = "dashed", color = "gray50", alpha = 0.8) +
  facet_wrap(vars(Site), nrow = 2, strip.position = "top",
             scales = "free_y",
             labeller = as_labeller(site_labels)) +
  theme_bw() +
  theme(panel.grid = element_blank(),
        axis.text.x = element_text(angle = 45, hjust = 1, vjust = 1),
        strip.background = element_blank(),
        strip.text = element_text(hjust = 0),
        text = element_text(size = 8),
        axis.title = element_markdown()) +
  labs(x = "", y = "Chlorophyll-a [mg m<sup>-3</sup>]") +
  scale_x_date(limits = as.Date(c("2019-01-01", "2019-12-14")),
               date_breaks = "1 month",
               date_labels = "%b") +
  # lines on dates where pyrosomes were observed
  # Beloi
  geom_vline(xintercept = as.Date("2019-09-24"), lty = 2, color = "gray40") +
  # Be'hau
  geom_vline(xintercept = as.Date("2019-10-8"), lty = 2, color = "gray40") +
  ggtitle("Figure 3")

# save to figures directory
ggsave("figures/Figure3.pdf", width = 8, height = 6, units = "cm", dpi = 600)

