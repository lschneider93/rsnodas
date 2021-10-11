april_gchnd_df <- read.csv("april_data_gchnd.csv")

ut_stat_info <- read.csv("GHCND_stations_for_25.csv")


nrcs_25 <- read.csv("Stations with 25 observation.csv")

nrcs_25
plot(nrcs_25$WESD4[1:25], main = paste(ut_stat_info$NAME[1]))
plot(nrcs_25$WESD4[26:50], main = paste(ut_stat_info$NAME[2]))
plot(nrcs_25$WESD4[51:75], main = paste(ut_stat_info$NAME[3]))
plot(nrcs_25$WESD4[76:100], main = paste(ut_stat_info$NAME[4]))
plot(nrcs_25$WESD4[101:125], main = paste(ut_stat_info$NAME[5]))
plot(nrcs_25$WESD4[126:150], main = paste(ut_stat_info$NAME[6]))


station5

i = 1
for (i in 1:length(ut_stat_info)){
  plot(nrcs_25$WESD4[(25*i-24):25* i], main = paste(ut_stat_info$NAME[i]),
       xlim = c(0, 25), ylim = c(0, 100)
       )
  lines(nrcs_25$WESD4[(25*i-24):(25* i)])
  lines(nrcs_25$WESD5[(25*i-24):(25* i)])
  lines(nrcs_25$WESD6[(25*i-24):(25* i)])
  lines(nrcs_25$WESD7[(25*i-24):(25* i)])
  lines(nrcs_25$WESD8[(25*i-24):(25* i)])
  lines(nrcs_25$WESD9[(25*i-24):(25* i)])
  Sys.sleep(1)
}


d <- signif(ut_stat_info$LATITUDE, 6)
n <- signif(ut_stat_info$LONGITUDE, 7)

