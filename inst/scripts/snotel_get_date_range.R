
# snow1 <- download_snotel(
#   freq = "daily",
#   destpath = paste0(getwd(), "/data-raw/snotel"),
#   sites = cal_nev_sites$id,
#   begin_date = "1900-01-01",
#   end_date = Sys.Date()
# )
#
# save(snow1, file = paste0(getwd(), "/data-raw/snotel/snow1.rda"))


load(file = paste0(getwd(), "/data-raw/snotel/snow1.rda"))

snow1$date <- as.Date(snow1$date)

snow2 <- snow1 %>%
  dplyr::filter(!is.na(snow_water_equivalent_mm_start_of_day_values)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    mean_snow = mean(snow_water_equivalent_mm_start_of_day_values),
    min_date = min(date), max_date = max(date), total = n(),
    years = round((as.numeric(max(date)) - as.numeric(min(date))) / 365)
  ) %>%
  dplyr::select(
    id, mean_snow,
    min_date, max_date, total, years
  ) %>%
  dplyr::distinct()

regmatches(snow1$date, regexpr("-04-01", snow1$date))
regexpr("-04-01", snow1$date)
library(tidyverse)

snow2 <- snow1 %>%
  dplyr::filter(!is.na(snow_water_equivalent_mm_start_of_day_values)) %>%
  dplyr::group_by(id) %>%
  dplyr::mutate(
    min_date = min(date), max_date = max(date), total = n(),
    years = round((as.numeric(max(date)) - as.numeric(min(date))) / 365)
  ) %>%
  dplyr::filter(regexpr("-04-01", date)!=-1) %>%
  dplyr::mutate(mean_snow = mean(snow_water_equivalent_mm_start_of_day_values)) %>%
  dplyr::select(
    id, mean_snow,
    min_date, max_date, total, years
  ) %>%
  dplyr::distinct()


# length(unique(snow2$id))
#
# table(snow2$max_date)
# table(snow2$min_date)
# length(unique(nev_sites$id))

nev <- sf::st_as_sf(maps::map("state",
  region = "nevada",
  plot = F, fill = TRUE
))

# meta_sn <- read.csv(paste0(
#   "C:/Users/kelvy/OneDrive/",
#   "KelvynsFiles/Doctorate/",
#   "SnowProject/meta_snotel_all.csv"
# ), header = T)

load("C:/Users/kelvy/github/packages/rsnodas/Data/meta_snotel.rda")

meta_sn <- meta_snotel

nev_sites <- meta_sn %>% filter(state == "NV")

cn <- read.csv("C:/Users/kelvy/github/packages/rsnodas/data-raw/CA_NV_SNOTEL.csv",
               header = T)
cn$Network <- "SNTL"
cn$State <- ifelse(cn$State == "Nevada", "NV",
                   ifelse(cn$State == "California", "CA", "")
)

cn$url_id <- paste(cn$ID, cn$State,
                   cn$Network,
                   sep = ":"
)

cal_nev_sites <- meta_sn %>% dplyr::filter(site_num %in% cn$ID)

nev_sites2 <- nev_sites %>%
  filter(id %in% snow2$id) %>%
  select("id", "longitude", "latitude")

cal_nev_sites2 <- cal_nev_sites %>%
  filter(id %in% snow2$id) %>%
  select("id", "longitude", "latitude")

df_snow <- dplyr::left_join(snow2, cal_nev_sites2, by = c("id" = "id")) %>%
  filter(!is.na(longitude))


df_snow1 <- sf::st_as_sf(df_snow,
  coords = c("longitude", "latitude"),
  crs = sf::st_crs("OGC:CRS84")
)


h <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow1, aes(size = total, col = total),
    alpha = 0.4
  ) +
  theme_bw()

min(df_snow1$min_date)


## In a report, overleaf, how many have at least 10 years of record?
# 75% of the days in 10 years?


unique(df_snow1$max_date)
table(df_snow1$min_date)

df_snow_ch <- df_snow1 %>% filter(min_date < "2012-07-30")
df_snow_ch1 <- df_snow1 %>% filter(years > 9) %>%
  filter(total > .75 * (as.numeric(Sys.Date()) - as.numeric(min_date)))

# years be size of point, col = by overall mean

g <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow_ch1, aes(
      size = years,
      col = sqrt(mean_snow)
    ),
    alpha = 0.4
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-120, -114, by = 2)) +
  scale_color_continuous(name = "Sqrt Mean of \n Apr. 1st SWE") +
  scale_size_continuous(name = "Years") +
  ggtitle("Nevada Snotel Sites") +
  theme(plot.title = element_text(hjust = 0.5))
g


h1 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow1, aes(col = years),
    alpha = 0.4
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-120, -114, by = 2)) +
  scale_color_continuous(name = "Years") +
  ggtitle("Nevada Snotel Sites") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(
  "C:/Users/kelvy/OneDrive/",
  "KelvynsFiles/Doctorate/",
  "SnowProject/data_snotel/",
  "test1.jpg"
), g, width = 4, height = 5)

# ggsave() or pdf() or jpg() # export plot
# figure 1 shows what
# talk about website, format daily or 15 min, m^3 / s, info about a
# whole bunch of other stuff


# 5 plots every quarter century
# how many sites on 1900, 1925, 1950, 1975, 2000, 2022
# plot sites, no size or color

df_snow_filt <- df_snow_ch1 %>% filter(min_date < "2010-01-01")

g4 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("Snotel 2010") +
  theme(plot.title = element_text(hjust = 0.5))

df_snow_filt <- df_snow_ch1 %>% filter(min_date < "2000-01-01")

g3 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("Snotel 2000") +
  theme(plot.title = element_text(hjust = 0.5))

df_snow_filt <- df_snow_ch1 %>% filter(min_date < "1990-01-01")

g2 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("Snotel 1990") +
  theme(plot.title = element_text(hjust = 0.5))


df_snow_filt <- df_snow_ch1 %>% filter(min_date < "1980-01-01")

g1 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_snow_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("Snotel 1980") +
  theme(plot.title = element_text(hjust = 0.5))

f <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

ggsave(paste0(
  "C:/Users/kelvy/OneDrive/",
  "KelvynsFiles/Doctorate/",
  "SnowProject/data_snotel/",
  "test2.jpg"
), f, width = 4, height = 5)
