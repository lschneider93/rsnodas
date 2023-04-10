#
library(daymetr)
head(april_1_snotel_data)

# Website: https://www.statology.org/select-first-row-in-group-dplyr/#:~:text=Often%20you%20may%20want%20to%20select%20the%20first,df%20%25%3E%25%20group_by%28group_var%29%20%25%3E%25%20arrange%28values_var%29%20%25%3E%25%20filter%28row_number%20%28%29%3D%3D1%29
new_df <- april_1_snotel_2003 %>%
  group_by(ID) %>%
  arrange(LATITUDE, LONGITUDE) %>%
  filter(row_number ()==1)

dim(new_df)
length(unique(april_1_snotel_2003$ID))


# Not needed --------------------------------------------------------------
unique(april_1_snotel_2003$LATITUDE)
unique(april_1_snotel_2003$LONGITUDE)

ids <- unique(april_1_snotel_2003$ID)

df <- april_1_snotel_2003 %>%
  filter(ID == ids[1]) %>%
  select(ID, LATITUDE, LONGITUDE)

for (i in 2:length(ids)) {
  row <- april_1_snotel_data %>%
    filter(ID == ids[i]) %>%
    select(ID, LATITUDE, LONGITUDE)

  df <- row
  class(row)

}


# Downlaoding daymet data -------------------------------------------------

# daymet_data <- download_daymet_batch(file_location = )

# OR

daymet_list <- vector("list", length(new_df$ID))

for (i in 1:length(daymet_list)) {
  daymet_data <- download_daymet(site = new_df$ID[i],
                                 lat = new_df$LATITUDE[i],
                                 lon = new_df$LONGITUDE[i],
                                 start = 2004,
                                 end = 2021,
                                 internal = TRUE,
                                 simplify = TRUE)

  # Just get the SWE values
  daymet_list[[i]] <- daymet_data %>%
    filter(measurement == "swe..kg.m.2.")

}

april_1_snotel

# adding daymet estimates to april_1_snotel_2003 ---------------------------------------------------------
# initialize a vector for daymet
april_1_snotel_2003$daymet_value <- 999999

table(april_1_snotel_2003$daymet_value == 999999)
april_1_snotel_2003[april_1_snotel_2003$daymet_value == 999999, ]

for (i in 1:length(april_1_snotel_2003$DATE)) {
  year <- lubridate::year(april_1_snotel_2003$DATE[i])
  num_of_list <- which(april_1_snotel_2003$ID[i] == new_df$ID)

  a1_day <- ifelse(year %in% seq(2004, 2022, 4), 92, 91)
  april_1_snotel_2003$daymet_value[i] <- daymet_list[[num_of_list]][daymet_list[[num_of_list]]$year == year &
                                                                    daymet_list[[num_of_list]]$yday == a1_day, ]$value
}

# double check and make sure only 2022 is blank
table(april_1_snotel_2003$daymet_value == 999999)
april_1_snotel_2003[april_1_snotel_2003$daymet_value == 999999, ]

brennan <- april_1_snotel_2003[april_1_snotel_2003$DATE == "2020-04-01", c("VALUE", "daymet_value", "SNODAS_VALUE")]

brennan$SNODAS_RESIDUALS <- brennan$VALUE - brennan$SNODAS_VALUE
brennan$daymet_residuals <- brennan$VALUE - brennan$daymet_value

hist(brennan$VALUE - brennan$SNODAS_VALUE,
     main = "2021 SNODAS residuals",
     xlab = "Error Value (VALUE - SNODAS prediction)",
     ylim = c(0, 60),
     # breaks = 10)
     # breaks = c(-400, -350, -300, -250, -200, -150, -100, -50, 0,
                # 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)) # 2021
     breaks = c(-400, -350, -300, -250, -200, -150, -100, -50, 0,
                50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)) # 202
axis(1, at = seq(-300, 500, by = 100))

hist(brennan$VALUE - brennan$daymet_value,
     main = "2021 Daymet residuals",
     xlab = "Error Value (VALUE - Daymet prediction)",
     xlim = c(-400, 600),
     ylim = c(0, 50),
     # breaks = 10)
     # breaks = c(-400, -350, -300, -250, -200, -150, -100, -50, 0,
     # 50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600)) # 2021
     breaks = c(-400, -350, -300, -250, -200, -150, -100, -50, 0,
                50, 100, 150, 200, 250, 300, 350, 400, 450, 500, 550, 600, 650, 700, 750, 800)) #2020
axis(1, at = seq(-300, 600, by = 100))



sqrt(mean((brennan$VALUE - brennan$SNODAS_VALUE)^2))
sqrt(mean((brennan$VALUE - brennan$daymet_value)^2))


plot(brennan$VALUE - brennan$SNODAS_VALUE,
     main = "2021 SNODAS residuals",
     xlab = "Error Value (VALUE - SNODAS prediction)")
plot(brennan$VALUE - brennan$daymet_value,
     main = "2021 Daymet residuals",
     xlab = "Error Value (VALUE - SNODAS prediction)")

sqrt(mean((brennan$VALUE - brennan$SNODAS_VALUE)^2))
sqrt(mean((brennan$VALUE - brennan$daymet_value)^2))

brennan %>%
  pivot_longer(cols = c("SNODAS_RESIDUALS", "daymet_residuals"),
               names_to = "model")
ggplot()

# stars::st_extract(april_1_snotel_2003[april_1_snotel_2003$DATE == "2015-04-01", ])
# april_1_snotel_2003[april_1_snotel_2003$DATE == "2015-04-01", ]
