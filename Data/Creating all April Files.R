# Download all the years you want

years <- 2004:2021
for (i in 1:length(years)){
  source_url <- paste0("ftp://ftp.ncdc.noaa.gov/pub/data/ghcn/daily/by_year/", years[i], ".csv.gz")
  compressed_file <- paste0(years[i], ".csv.gz")
  download.file(source_url, compressed_file)
  R.utils::gunzip(compressed_file)
  #print(compressed_file)
}


h2004 <- utils::read.csv("2004.csv")
h2005 <- utils::read.csv("2005.csv")
h2006 <- utils::read.csv("2006.csv")
h2007 <- utils::read.csv("2007.csv")
h2008 <- utils::read.csv("2008.csv")
h2009 <- utils::read.csv("2009.csv")
h2010 <- utils::read.csv("2010.csv")

h2011 <- utils::read.csv("2011.csv")
h2012 <- utils::read.csv("2012.csv")
h2013 <- utils::read.csv("2013.csv")
h2014 <- utils::read.csv("2014.csv")
h2015 <- utils::read.csv("2015.csv")
h2016 <- utils::read.csv("2016.csv")

h2017 <- utils::read.csv("2017.csv")
h2018 <- utils::read.csv("2018.csv")
h2019 <- utils::read.csv("2019.csv")
h2020 <- utils::read.csv("2020.csv")
h2021 <- utils::read.csv("2021.csv")

#Subset and just get UTAH
utah_2004 <- h2004[grep("^USC0042", h2004$AE000041196), ]
utah_2005 <- h2005[grep("^USC0042", h2005$AE000041196), ]
utah_2006 <- h2006[grep("^USC0042", h2006$AE000041196), ]
utah_2007 <- h2007[grep("^USC0042", h2007$AE000041196), ]
utah_2008 <- h2008[grep("^USC0042", h2008$AE000041196), ]
utah_2009 <- h2009[grep("^USC0042", h2009$AE000041196), ]
utah_2010 <- h2010[grep("^USC0042", h2010$AE000041196), ]

utah_2011 <- h2011[grep("^USC0042", h2011$AE000041196), ]
utah_2012 <- h2012[grep("^USC0042", h2012$AE000041196), ]
utah_2013 <- h2013[grep("^USC0042", h2013$AE000041196), ]
utah_2014 <- h2014[grep("^USC0042", h2014$AE000041196), ]
utah_2015 <- h2015[grep("^USC0042", h2015$AE000041196), ]
utah_2016 <- h2016[grep("^USC0042", h2016$AE000041196), ]
 
utah_2017 <- h2017[grep("^USC0042", h2017$AE000041196), ]
utah_2018 <- h2018[grep("^USC0042", h2018$AE000041196), ]
utah_2019 <- h2019[grep("^USC0042", h2019$AE000041196), ]
utah_2020 <- h2020[grep("^USC0042", h2020$AE000041196), ]
utah_2021 <- h2021[grep("^USC0042", h2021$AE000041196), ]

colnames(utah_2004) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2005) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2006) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2007) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2008) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2009) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2010) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2011) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2012) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2013) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2014) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2015) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2016) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2017) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2018) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2019) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2020) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")
colnames(utah_2021) <- c("Station_ID", "Date", "Code", "Value",
                         "IDK", "IDK2", "IDK3", "IDK4")




utah_try <- utah_2004[1:20,]

#Write these back into a CSV for each year.
class(utah_2004)

df <- data.frame(name = c('jon', 'spencer', "logan"), 
           age = c(20, 22, 27))
df
getwd()
utils::write.csv(utah_try, "C:/Users/Logan/Desktop/try.csv", row.names = FALSE)

utils::write.csv(utah_2004, "C:/Users/Logan/Desktop/utah_2004.csv", row.names = FALSE)
utils::write.csv(utah_2005, "C:/Users/Logan/Desktop/utah_2005.csv", row.names = FALSE)
utils::write.csv(utah_2006, "C:/Users/Logan/Desktop/utah_2006.csv", row.names = FALSE)
utils::write.csv(utah_2007, "C:/Users/Logan/Desktop/utah_2007.csv", row.names = FALSE)
utils::write.csv(utah_2008, "C:/Users/Logan/Desktop/utah_2008.csv", row.names = FALSE)
utils::write.csv(utah_2009, "C:/Users/Logan/Desktop/utah_2009.csv", row.names = FALSE)
utils::write.csv(utah_2010, "C:/Users/Logan/Desktop/utah_2010.csv", row.names = FALSE)

utils::write.csv(utah_2011, "C:/Users/Logan/Desktop/utah_2011.csv", row.names = FALSE)
utils::write.csv(utah_2012, "C:/Users/Logan/Desktop/utah_2012.csv", row.names = FALSE)
utils::write.csv(utah_2013, "C:/Users/Logan/Desktop/utah_2013.csv", row.names = FALSE)
utils::write.csv(utah_2014, "C:/Users/Logan/Desktop/utah_2014.csv", row.names = FALSE)
utils::write.csv(utah_2015, "C:/Users/Logan/Desktop/utah_2015.csv", row.names = FALSE)
utils::write.csv(utah_2016, "C:/Users/Logan/Desktop/utah_2016.csv", row.names = FALSE)

utils::write.csv(utah_2017, "C:/Users/Logan/Desktop/utah_2017.csv", row.names = FALSE)
utils::write.csv(utah_2018, "C:/Users/Logan/Desktop/utah_2018.csv", row.names = FALSE)
utils::write.csv(utah_2019, "C:/Users/Logan/Desktop/utah_2019.csv", row.names = FALSE)
utils::write.csv(utah_2020, "C:/Users/Logan/Desktop/utah_2020.csv", row.names = FALSE)
utils::write.csv(utah_2021, "C:/Users/Logan/Desktop/utah_2021.csv", row.names = FALSE)



april_2004 <- utah_2004[utah_2004$Date == "20040401", ]
april_2005 <- utah_2005[utah_2005$Date == "20050401", ]
april_2006 <- utah_2006[utah_2006$Date == "20060401", ]
april_2007 <- utah_2007[utah_2007$Date == "20070401", ]
april_2008 <- utah_2008[utah_2008$Date == "20080401", ]
april_2009 <- utah_2009[utah_2009$Date == "20090401", ]
april_2010 <- utah_2010[utah_2010$Date == "20100401", ]

april_2011 <- utah_2011[utah_2011$Date == "20110401", ]
april_2012 <- utah_2012[utah_2012$Date == "20120401", ]
april_2013 <- utah_2013[utah_2013$Date == "20130401", ]
april_2014 <- utah_2014[utah_2014$Date == "20140401", ]
april_2015 <- utah_2015[utah_2015$Date == "20150401", ]
april_2016 <- utah_2016[utah_2016$Date == "20160401", ]

april_2017 <- utah_2017[utah_2017$Date == "20170401", ]
april_2018 <- utah_2018[utah_2018$Date == "20180401", ]
april_2019 <- utah_2019[utah_2019$Date == "20190401", ]
april_2020 <- utah_2020[utah_2020$Date == "20200401", ]
april_2021 <- utah_2021[utah_2021$Date == "20210401", ]


dim(april_2004)
dim(april_2005)
dim(april_2006)
dim(april_2007)
dim(april_2008)
dim(april_2009)
dim(april_2010)


utils::write.csv(april_2004, "C:/Users/Logan/Desktop/april_2004.csv", row.names = FALSE)
utils::write.csv(april_2005, "C:/Users/Logan/Desktop/april_2005.csv", row.names = FALSE)
utils::write.csv(april_2006, "C:/Users/Logan/Desktop/april_2006.csv", row.names = FALSE)
utils::write.csv(april_2007, "C:/Users/Logan/Desktop/april_2007.csv", row.names = FALSE)
utils::write.csv(april_2008, "C:/Users/Logan/Desktop/april_2008.csv", row.names = FALSE)
utils::write.csv(april_2009, "C:/Users/Logan/Desktop/april_2009.csv", row.names = FALSE)
utils::write.csv(april_2010, "C:/Users/Logan/Desktop/april_2010.csv", row.names = FALSE)

utils::write.csv(april_2011, "C:/Users/Logan/Desktop/april_2011.csv", row.names = FALSE)
utils::write.csv(april_2012, "C:/Users/Logan/Desktop/april_2012.csv", row.names = FALSE)
utils::write.csv(april_2013, "C:/Users/Logan/Desktop/april_2013.csv", row.names = FALSE)
utils::write.csv(april_2014, "C:/Users/Logan/Desktop/april_2014.csv", row.names = FALSE)
utils::write.csv(april_2015, "C:/Users/Logan/Desktop/april_2015.csv", row.names = FALSE)
utils::write.csv(april_2016, "C:/Users/Logan/Desktop/april_2016.csv", row.names = FALSE)

utils::write.csv(april_2017, "C:/Users/Logan/Desktop/april_2017.csv", row.names = FALSE)
utils::write.csv(april_2018, "C:/Users/Logan/Desktop/april_2018.csv", row.names = FALSE)
utils::write.csv(april_2019, "C:/Users/Logan/Desktop/april_2019.csv", row.names = FALSE)
utils::write.csv(april_2020, "C:/Users/Logan/Desktop/april_2020.csv", row.names = FALSE)
utils::write.csv(april_2021, "C:/Users/Logan/Desktop/april_2021.csv", row.names = FALSE)






utah_2004 
utah_2005 
utah_2006 
utah_2007 
utah_2008 
utah_2009 
utah_2010 
utah_2011 
utah_2012 
utah_2013 
utah_2014
utah_2015 
utah_2016 
utah_2017 
utah_2018 
utah_2019 
utah_2020 
utah_2021

