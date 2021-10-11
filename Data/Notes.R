################################################################################
###### Creating April Raster and Downloading     #########
################################################################################

# format_snodas_daily # Function is located in this.  This creates and stores the 

april_swe # brick, map (all the rasters of full US)

map # Raster of each years swe

utah_april_swe_brick # map (all the rasters of UT)

# getting the shape of utah
boundary
new_boundary

utah_april_swe_rasters # list of rasters of utah's swe
utah_april_swe_brick # rasterbrick of the utah's swe.

################################################################################
###### Creating DF of WESD in Lists CC  #########
################################################################################
april_df_ghcnd # List for each year, with info: LONG, LAT, ELE, DIFF, VALUE, DATE

# this is all of the stations in a data frame with their ID, Name, LONG, LAT
ut_stat_info # total of 906 stations
station_info

################################################################################
###### Calculating Differences from Model   #########
################################################################################
april_df_ghcnd_spatial

df_of_all_years # all observations in a data frame
df_of_all_years_copy #spatial

test_values # list with all of the extracted values at the stations LONG/LAT
bilinear_values # list with all of the extracted values at the stations LONG/LAT

# also the bubble of each year
for(i in years){
  #print(paste0(i, '-04-01'))
  print(sp::bubble(df_of_all_years_spatial[df_of_all_years_spatial$DATE
                                           == paste0(i, '-04-01'), ], 
                   'DIFFERENCE', main = paste0(i)))
  Sys.sleep(1) 
}

################################################################################
###################       Evaluating the Errors         ######################## 
################################################################################

#Look at the range of LAT/LONG and Elevation we are dealing with.
individ_station # list where each element is a station and the LONG, LAT, MODEL, WESD, DIFF, and Date
individ_station5# list where each element is a station with at least 5 observations

# this is broken down to those that had positive/negative errors.
positive_stations
negative_stations
zero_stations

# This gives the the station ID, sum of the errors, total error, and MSE, with LAT/LONG, ELEV
nonzero_stations #<- error_stations[error_stations$total_error != 0, ] and ordered highest to lowest

################################################################################
###### Get Raster from Prism and IWD    #########
################################################################################

# list of rasters that used Inverse Distance Weighting to make predictions about the 
utah_april_error_rasters 

################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################


################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################


################################################################################
###### Making a Long Data Frame of the VAriables to see all of them    #########
################################################################################


