
# deep <- download_usgs(
#   freq = "dv",
#   destpath = paste0(getwd(), "/data-raw/usgs"),
#   sites = nev_meta$SITE_NO, begin_date = "1850-01-01",
#   end_date = Sys.Date()
# )

# deep1 <- deep[, 1:5]
# deep1 <- deep1 %>% filter(!is.na(v00060))
#
# save(deep1, file = paste0(getwd(), "/data-raw/usgs/deep1.rda"))

load(file = paste0(getwd(), "/data-raw/usgs/deep1.rda"))

deep2 <- deep1 %>%
  dplyr::filter(!is.na(v00060)) %>%
  dplyr::group_by(site_no) %>%
  dplyr::mutate(
    mean_flow = round(mean(v00060), 4),
    min_date = min(datetime), max_date = max(datetime),
    total = n()
  ) %>%
  dplyr::select(site_no, mean_flow, min_date, max_date, total) %>%
  dplyr::distinct() %>%
  dplyr::mutate(years = round((as.numeric(as.Date(max_date)) -
    as.numeric(as.Date(min_date))) / 365))
#
#
# table(deep2$max_date)
# table(deep2$min_date)
# table(deep2$max_date == Sys.Date() - 1)
# length(unique(nev_meta$SITE_NO))


nev <- sf::st_as_sf(maps::map("state",
  region = "nevada",
  plot = F, fill = TRUE
))

meta <- read.csv(paste0(
  "C:/Users/kelvy/OneDrive/KelvynsFiles/Doctorate/",
  "SnowProject/meta_gages.csv"
), header = T)

nev_meta <- meta %>% filter(STATE == "NV")

nev_meta2 <- nev_meta %>%
  filter(SITE_NO %in% as.numeric(deep2$site_no)) %>%
  select("SITE_NO", "LON_SITE", "LAT_SITE")

deep2$site_no <- as.numeric(deep2$site_no)

df_deep <- dplyr::left_join(deep2, nev_meta2, by = c("site_no" = "SITE_NO"))


df_deep1 <- sf::st_as_sf(df_deep,
  coords = c("LON_SITE", "LAT_SITE"),
  crs = sf::st_crs("OGC:CRS84")
)


h <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep1, aes(size = total, col = total),
    alpha = 0.5
  ) +
  theme_bw()

## In a report, overleaf, how many have at least 10 years of record?
# 75% of the days in 10 years?

df_deep_ch <- df_deep1 %>% filter(years > 9)
df_deep_ch1 <- df_deep_ch %>%
  mutate(data_flag = .75 * (as.numeric(as.Date(max_date)) -
    as.numeric(as.Date(min_date)))) %>%
  filter(total > data_flag)

df_deep_ch2 <- df_deep_ch %>%
  mutate(data_flag = .75 * (as.numeric(as.Date(max_date)) -
    as.numeric(as.Date(min_date)))) %>%
  filter(total < data_flag)

# years be size of point, col = by overall mean

ug <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep_ch1, aes(
      size = years,
      col = log(mean_flow + 1)
    ),
    alpha = 0.5
  ) +
  theme_bw() +
  scale_x_continuous(breaks = seq(-120, -114, by = 2)) +
  scale_color_continuous(name = "Log Mean Flow") +
  scale_size_continuous(name = "Years") +
  ggtitle("Nevada USGS Sites") +
  theme(plot.title = element_text(hjust = 0.5))

ggsave(paste0(
  "C:/Users/kelvy/OneDrive/",
  "KelvynsFiles/Doctorate/",
  "SnowProject/data_snotel/",
  "test3.jpg"
), ug, width = 4, height = 5)



# figure 1 shows what
# talk about website, format daily or 15 min, m^3 / s, info about a
# whole bunch of other stuff


# 5 plots every quarter century
# how many sites on 1900, 1925, 1950, 1975, 2000, 2022
# plot sites, no size or color

summary(as.Date(df_deep1$min_date))

df_deep_filt <- df_deep_ch1 %>% filter(as.Date(min_date) < "2000-01-01")

g4 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("USGS 2000") +
  theme(plot.title = element_text(hjust = 0.5))

df_deep_filt <- df_deep_ch1 %>% filter(min_date < "1975-01-01")

g3 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("USGS 1975") +
  theme(plot.title = element_text(hjust = 0.5))

df_deep_filt <- df_deep_ch1 %>% filter(min_date < "1950-01-01")

g2 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("USGS 1950") +
  theme(plot.title = element_text(hjust = 0.5))


df_deep_filt <- df_deep_ch1 %>% filter(min_date < "1925-01-01")

g1 <- ggplot() +
  geom_sf(data = nev) +
  geom_sf(
    data = df_deep_filt,
    alpha = 0.4
  ) +
  scale_x_continuous(breaks = seq(-120, -114, by = 3)) +
  theme_bw() +
  ggtitle("USGS 1925") +
  theme(plot.title = element_text(hjust = 0.5))

f <- gridExtra::grid.arrange(g1, g2, g3, g4, nrow = 2)

ggsave(paste0(
  "C:/Users/kelvy/OneDrive/",
  "KelvynsFiles/Doctorate/",
  "SnowProject/data_snotel/",
  "test4.jpg"
), f, width = 4, height = 5)
