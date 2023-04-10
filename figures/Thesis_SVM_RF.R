y15
y15_2 <- april_1_snotel_data[april_1_snotel_data$DATE > "2015-04-01", ]
y15_3 <- april_1_snotel_data[april_1_snotel_data$DATE == "2015-04-01", ]

table(y15_2$DATE)
# Variable importance plot
rf1 <- randomForest::randomForest(VALUE ~ ELEVATION + LATITUDE + LONGITUDE +
                                     tmax_normal_annual + ppt_normal_annual + vpdmin_normal_annual +
                                     tmin_normal_annual + tmean_normal_annual + vpdmax_normal_annual +
                                     slope, data = y15_2, importance = TRUE)

rf1

randomForest::varImpPlot(rf1)


#
set.seed(1234)
i = 1
ye <- april_1_snotel_data[april_1_snotel_data$DATE == paste0(years[i], "-04-01"), ]
svm <- try(e1071::svm(VALUE ~ elevation + LATITUDE + LONGITUDE +
                         tmax_normal_annual + ppt_normal_annual + vpdmin_normal_annual +
                         tmin_normal_annual + tmean_normal_annual + vpdmax_normal_annual +
                         slope, data = ye, importance = TRUE), silent = TRUE)

pdp_elevations <- pdp::partial(svm, pred.var = c("elevation"), chull = TRUE)
pdp_tmaxs <- pdp::partial(svm, pred.var = c("tmax_normal_annual"), chull = TRUE)
pdp_ppts <- pdp::partial(svm, pred.var = c("ppt_normal_annual"), chull = TRUE)
pdp_vpdmaxs <- pdp::partial(svm, pred.var = c("vpdmax_normal_annual"), chull = TRUE)

for (i in 2:length(years)) {
  ye <- april_1_snotel_data[april_1_snotel_data$DATE == paste0(years[i], "-04-01"), ]
  svm <- try(e1071::svm(VALUE ~ elevation + LATITUDE + LONGITUDE +
                          tmax_normal_annual + ppt_normal_annual + vpdmin_normal_annual +
                          tmin_normal_annual + tmean_normal_annual + vpdmax_normal_annual +
                          slope, data = ye, importance = TRUE), silent = TRUE)
  pdp_elevation <- pdp::partial(svm, pred.var = c("elevation"), chull = TRUE)
  pdp_tmax <- pdp::partial(svm, pred.var = c("tmax_normal_annual"), chull = TRUE)
  pdp_ppt <- pdp::partial(svm, pred.var = c("ppt_normal_annual"), chull = TRUE)
  pdp_vpdmax <- pdp::partial(svm, pred.var = c("vpdmax_normal_annual"), chull = TRUE)

  pdp_elevations <- rbind(pdp_elevations, pdp_elevation)
  pdp_tmaxs <- rbind(pdp_tmaxs, pdp_tmax)
  pdp_ppts <- rbind(pdp_ppts, pdp_ppt)
  pdp_vpdmaxs <- rbind(pdp_vpdmaxs, pdp_vpdmax)

}




group <- rep(years, each = 51)

e_df <- data.frame(cbind(pdp_elevations, group))
t_df <- data.frame(cbind(pdp_tmaxs, group))
p_df <- data.frame(cbind(pdp_ppts, group))
vp_df <- data.frame(cbind(pdp_vpdmaxs, group))

ggplot(e_df, aes(x = elevation, y = yhat, group = group)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +#, legend.position = "bottom") +
  ylab(paste0("Value")) + xlab("") +
  # scale_y_continuous(limits = c(-500, 500)) +
  scale_y_continuous(breaks = c(200, 300, 400, 500, 600),
                     limits = c(-200, 600))

ggplot(t_df, aes(x = tmax_normal_annual, y = yhat, group = group)) +
  geom_line()

# annual precipitation partial dependence plot
g <- ggplot(p_df, aes(x = ppt_normal_annual, y = yhat, group = group)) +
  geom_line() +
  theme(plot.title = element_text(hjust = 0.5, size = 30),
        text = element_text(size = 28),
        legend.title = element_text(size = 32)) +#, legend.position = "bottom") +
  ylab(paste0("Value")) + xlab("") +
  # scale_y_continuous(limits = c(-500, 500)) +
  scale_y_continuous(breaks = c(0, 100, 200, 300, 400, 500, 600, 700, 800),
                     limits = c(0, 800)) +
  scale_x_continuous("Annual Precipitation",
                     breaks = c(200, 400, 600, 800, 1000, 1200, 1400, 1600),
                     limits = c(200, 1500))

ggsave(filename = paste0("Partial Dependence Plot", ".png"),
       plot = g,
       width = 10.46,
       height = 8.42,
       path = paste0(getwd())
)

ggplot(vp_df, aes(x = vpdmax_normal_annual, y = yhat, group = group)) +
  geom_line()


