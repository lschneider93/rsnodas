c <- short_df[short_df$DIFFERENCE > 6000, ]
unique(c$ID)


# sp::coordinates(long_df1) <- c("LONGITUDE", "LATITUDE")
sp::coordinates(c) <- c("LONGITUDE", "LATITUDE")

raster::plot(c)
