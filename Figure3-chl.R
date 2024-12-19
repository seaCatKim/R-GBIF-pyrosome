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
library(patchwork)
library(zoo)

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

bel$t_int <- as.numeric(bel$time) - as.numeric(bel$time[1])

ggplot(bel, aes(x = time, y = CHL)) + geom_line() + scale_y_log10()

# filter 2019 CHL data and combine into one dataframe for plot
chl <- rbind(beh |> filter(year == 2019),
             bel |> filter(year == 2019))
range(chl$CHL)
summary(chl)

site_labels <- c(`Be'hau` = "(a)",
                 `Beloi` = "(b)")

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
        text = element_text(size = 8)) +
  labs(x = "", y = "Chlorophyll a [mg m-3]") +
  scale_x_date(limits = as.Date(c("2019-01-01", "2019-12-14")),
               date_breaks = "1 month",
               date_labels = "%b") +
  # lines on dates where pyrosomes were observed
  # Beloi
  geom_vline(xintercept = as.Date("2019-09-24"), lty = 2, color = "gray40") +
  # Be'hau
  geom_vline(xintercept = as.Date("2019-10-8"), lty = 2, color = "gray40") +
  ggtitle("Figure 3")

ggsave("plots/chl-a.jpg", width = 8, height = 6, units = "cm", dpi = 600)



map(1:length(plot_roll_chl), function(x, idx) {
  a <- names(plot_roll_chl[x])
  # assign a value to a name in an environment
  assign(paste("plot_meanroll", a, sep = "_"), plot_roll_chl[x], envir = globalenv())
}
)

# plot by faceting sites
bind_clim_mean_roll <- clim_mean_roll |> bind_rows()

ggplot(bind_clim_mean_roll, aes(time, roll_anomaly)) +
  geom_col() +
  facet_wrap(vars(Site))

# try 75% or 95% percentile of monthly data to average
# take raw time series, 95% for jan the first year

#### CHL data from Timor Strait ####
ts <- read_csv("data/chl-timorstrait/cmems_obs-oc_glo_bgc-plankton_nrt_l3-multi-4km_P1D_1725414362075.cs")


#### Southern Oscillation Index data ####
## downloaded from https://www.cpc.ncep.noaa.gov/data/indices/soi on 20 August 2024
soi <- read.csv("data/SOI-sealevelpress-standardized.csv",
                na.strings = "-999.9")
anomaly <- read.csv("data/SOI-sealevelpress-anomaly.csv",
                    na.strings = "-999.9")

l_soi <- list(soi = soi, anomaly = anomaly) # store in a list to map functions

# reformat data
(soi_long <- l_soi |>
    map(pivot_longer, JAN:DEC, names_to = "MONTH", values_to = "SOI") |>
    map(mutate, DATE = paste(YEAR, MONTH, sep = "-"),
               DATE = parse_date_time(DATE, "ym"))
  )

# plot all data
soi_long |>
  map(~ggplot(.x, aes(x = DATE, y = SOI)) +
        geom_point() +
        theme_bw())

# save standardized as csv
#write.csv(soi_long[["soi"]], "data/soi-std.csv")

# subset matching data with CHL period starting Sept 1997
(plot_soi <- soi_long |>
  map(filter, DATE >= "1997-09-01") |>
  map(~ggplot(.x, aes(x = DATE, y = SOI)) +
      #  geom_point() +
        geom_line() +
        theme_bw()
      # +
      #   labs(title = deparse(substitute(.))))
      )
)
plot_soi$anomaly

# calculate 3 month rolling mean
soi_roll <- soi_long |>
  map(mutate, rolling_SOI = rollmean(SOI, 3, fill = NA))

(plot_soi_roll <- soi_roll |>
    map(filter, DATE >= "1997-09-01") |>
    map(~ggplot(.x, aes(x = DATE, y = rolling_SOI)) +
          #  geom_point() +
          geom_line() +
          theme_bw() +
          theme(text = element_text(size = 14)) +
          labs(x = "")
        # +
        #   labs(title = deparse(substitute(.))))
    )
)

(plot_soiroll_col <- soi_roll$soi |>
    mutate(DATE = as.Date(DATE)) |>
    filter(DATE > as.Date("1997-09-01") & DATE < as.Date("2024-06-01")) |>
    ggplot(aes(as.Date(DATE), rolling_SOI)) +
    # geom_col gives uneven column widths, something about pixels
    geom_segment(data = soi_long$soi |> filter(SOI <= 0),
                 aes(xend = as.Date(DATE), y = 0, yend = SOI), color = "red") +
    geom_segment(data = soi_long$soi |> filter(SOI >= 0),
                 aes(xend = as.Date(DATE), y = 0, yend = SOI), color = "blue") +
    theme_bw() +
  theme(text = element_text(size = 8),
        axis.text.x = element_text(angle = 90, vjust = 0.5),
        panel.grid = element_blank()) +
  labs(x = "", y = "SOI") +
  scale_x_date(name = "",
               date_breaks = "1 year",
               date_labels = "%Y",
               limits = (c(as.Date("1997-09-01"), as.Date("2024-06-01"))))
               )

plot_soiroll_col / plot_meanroll_behau / plot_meanroll_beloi +
  plot_layout(axes = "collect") +
  plot_annotation(tag_levels = "A")
ggsave(filename = "plots/soi-behau-beloi-rollingmean.png",
       width = 8.5, heigh = 10.5, units = "cm")


plot_soi_roll$anomaly

plot_soi$anomaly / plot_soi_roll$anomaly


plot_chl / plot_soi$anomaly

plot_chl_yr / plot_soi$anomaly

plot_chl_roll / plot_soi$anomaly
plot_chl_roll / plot_soi_roll$anomaly
plot(soi_roll$rolling_SOI, ch)

plot_soi_roll$anomaly / plot_chl_detrend

# 3 month rolling soi and chl climatological anomaly seasonally detrended
plot_soi_roll$soi / plot_roll_chl$beloi / plot_roll_chl$behau

#### Correlations ####
# join soi rolling average and CHL anomaly - mean without seasonal signal
chl_soi <- left_join(chl_detrend_roll, soi_roll$anomaly, by = join_by(time == DATE))

ggplot(chl_soi, aes(roll_anomaly, rolling_SOI)) +
  geom_point() +
  facet_grid(vars(Site)) +
  geom_smooth(method = "lm") +
  theme_bw()

ggplot(chl_soi |> filter(roll_anomaly < 0.25), # filter outliers
       aes(roll_anomaly, rolling_SOI)) +
  geom_point() +
  facet_grid(vars(Site)) +
  geom_smooth(method = "lm") +
  theme_bw()

l_chl_soi <- chl_soi |>
  group_by(Site) |>
  group_split(Site)
# tried filtering for rolling chl anomaly < 0.25 and did not make a difference

# behau
shapiro.test(l_chl_soi[[1]]$roll_anomaly)
shapiro.test(l_chl_soi[[1]]$rolling_SOI)

qqnorm(l_chl_soi[[1]]$roll_anomaly)
qqnorm(l_chl_soi[[1]]$rolling_SOI)

cor.test(l_chl_soi[[1]]$roll_anomaly, l_chl_soi[[1]]$rolling_SOI, method = "kendall")
cor.test(l_chl_soi[[1]]$roll_anomaly, l_chl_soi[[1]]$rolling_SOI, method = "spearman")

# beloi
shapiro.test(l_chl_soi[[2]]$roll_anomaly)
shapiro.test(l_chl_soi[[2]]$rolling_SOI)

qqnorm(l_chl_soi[[2]]$roll_anomaly)
qqnorm(l_chl_soi[[2]]$rolling_SOI)

cor.test(l_chl_soi[[2]]$roll_anomaly, l_chl_soi[[2]]$rolling_SOI, method = "kendall")
cor.test(l_chl_soi[[2]]$roll_anomaly, l_chl_soi[[2]]$rolling_SOI, method = "spearman")

##### rolling soi and rolling chl anomaly, monthly mean, seasonally detrended, climatological mean
(climchl_soi <- clim_mean_roll |> map(left_join, soi_roll$anomaly, by = c("time" = "DATE")))

plot(climchl_soi$behau$roll_anomaly, climchl_soi$behau$rolling_SOI)

## plot rolling soi vs chl anomaly
climchl_soi |>
  map(~  ggplot(data = .x, aes(rolling_SOI, roll_anomaly)) +
        geom_point() +
        geom_smooth(method = "lm") +
        theme_bw() +
        ggtitle(paste(.x$Site[1], "Correlation" )))

## correlation test
## beloi
shapiro.test(climchl_soi[[1]]$roll_anomaly)  # not normal
shapiro.test(climchl_soi[[1]]$rolling_SOI)

qqnorm(climchl_soi[[1]]$roll_anomaly)  # not normal
qqnorm(climchl_soi[[1]]$rolling_SOI)

cor.test(climchl_soi[[1]]$roll_anomaly, climchl_soi[[1]]$rolling_SOI, method = "kendall")
cor.test(climchl_soi[[1]]$roll_anomaly, climchl_soi[[1]]$rolling_SOI, method = "spearman")

## behau
shapiro.test(climchl_soi[[2]]$roll_anomaly)  # not normal
shapiro.test(climchl_soi[[2]]$rolling_SOI)

qqnorm(climchl_soi[[2]]$roll_anomaly)  # not normal
qqnorm(climchl_soi[[2]]$rolling_SOI)

cor.test(climchl_soi[[2]]$roll_anomaly, climchl_soi[[2]]$rolling_SOI, method = "kendall")
cor.test(climchl_soi[[2]]$roll_anomaly, climchl_soi[[2]]$rolling_SOI, method = "spearman")

## beloi
shapiro.test(climchl_soi[[2]]$roll_anomaly)  # not normal
shapiro.test(climchl_soi[[2]]$rolling_SOI)

qqnorm(climchl_soi[[2]]$roll_anomaly)  # not normal
qqnorm(climchl_soi[[2]]$rolling_SOI)

cor.test(climchl_soi[[2]]$roll_anomaly, climchl_soi[[2]]$rolling_SOI, method = "kendall")
cor.test(climchl_soi[[2]]$roll_anomaly, climchl_soi[[2]]$rolling_SOI, method = "spearman")
