#' Download the Stations with ActionID
#'
#' @param dates date with YYYY-MM-dd written as a six digit character string
#'
#' @return a dataframe with station names with action Ids
#'
#' @details This function helps with getting stations that have an Action Id.
#'   The stations with an Action Id are in both the NRCS and GHCND.
#'
#' @export

download_actionid_stations <- function(){
  URL = "https://wcc.sc.egov.usda.gov/reportGenerator/view_csv/customMultiTimeSeriesGroupByStationReport/daily/start_of_period/332:UT:SNTL%7C455:UT:SNTL%7Cstate=%22UT%22%7Cname/0,0/stationId,network.code,network.name,dco.code,actonId,shefId?fitToScreen=false"

  # Use the RCurl package to download to avoid timeouts typical of the base R
  # option.
  snowURL <- RCurl::getURL(URL)
  text_station_info <- unlist(strsplit(snowURL, '\n'))
  first_line <- min(grep('^[[:alpha:]]', text_station_info))
  snowCSV <- read.csv(textConnection(snowURL), skip = first_line - 1)

  return(snowCSV)
  # dim(snowCSV)
  # snowCSV$Station.Id
  # nrcs_csv$Station.Id
  # rm(snowURL, text_station_info, first_line)

  # this is to check how many are in the NRCS
  # for(i in 1:length(snowCSV$Station.Id)){
  #   print(c(i, snowCSV$Station.Id[[i]] == nrcs_csv$Station.Id[[i]]))
  # }

}
