setwd("~/Desktop/Graduate classes 2021 Spring/Stat Viz 2/Final Project")

library(maps)
library(maptools)
library(RColorBrewer)
library(classInt)
library(dplyr)
library(mapdata)
library(readxl)
library(sp)
library(micromap)
library(stringr)
library(tidyverse)

# Import the data and look at the first six rows
data <- read.csv(file = 'Health_Insurance_Coverage_-_States.csv')             

# Changes the column names to be ...
colnames(data) <- c("Object ID", 
                    "Geo_Id", 
                    "Geo_Name", 
                    "FIPS_Code", 
                    "Employer Insurance only - Pop < 19",
                    "Employer Insurance only - Pop < 19 Margin of Error", 
                    "Employer Insurance only - Pop 19-34",
                    "Employer Insurance only - Pop 19-34 MOE", 
                    "Employer Insurance only - Pop 35-64", 
                    "Employer Insurance only - Pop 35-64 MOE",
                    "Employer Insurance only - Pop 65+", 
                    "Employer Insurance only - Pop 65+ MOE",
                    "Direct Purchase only - Pop < 19",
                    "Direct Purchase only - Pop < 19 Margin of Error", 
                    "Direct Purchase only - Pop 19-34",
                    "Direct Purchase only - Pop 19-34 MOE", 
                    "Direct Purchase only - Pop 35-64", 
                    "Direct Purchase only - Pop 35-64 MOE",
                    "Direct Purchase only - Pop 65+", 
                    "Direct Purchase only - Pop 65+ MOE",
                    "Medicare only - Pop < 19",
                    "Medicare only - Pop < 19 Margin of Error",
                    "Medicare only - Pop 19-34",
                    "Medicare only - Pop 19-34 MOE", 
                    "Medicare only - Pop 35-64", 
                    "Medicare only - Pop 35-64 MOE",
                    "Medicare only - Pop 65+", 
                    "Medicare only - Pop 65+ MOE",
                    "Medicaid only - Pop < 19",
                    "Medicaid only - Pop < 19 Margin of Error", 
                    "Medicaid only - Pop 19-34",
                    "Medicaid only - Pop 19-34 MOE", 
                    "Medicaid only - Pop 35-64", 
                    "Medicaid only - Pop 35-64 MOE",
                    "Medicaid only - Pop 65+", 
                    "Tricare only - Pop < 19",
                    "Tricare only - Pop < 19 Margin of Error", "Tricare only - Pop 19-34",
                    "Tricare only - Pop 19-34 MOE", "Tricare only - Pop 35-64", "Tricare only - Pop 35-64 MOE",
                    "Tricare only - Pop 65+", "Tricare only - Pop 65+ MOE",
                    "VA Healthcare only - Pop < 19",
                    "VA Healthcare only - Pop < 19 Margin of Error", "VA Healthcare only - Pop 19-34",
                    "VA Healthcare only - Pop 19-34 MOE", "VA Healthcare only - Pop 35-64", "VA Healthcare only - Pop 35-64 MOE",
                    "VA Healthcare only - Pop 65+", "VA Healthcare only - Pop 65+ MOE",
                    "2+ only - Pop < 19",
                    "2+ only - Pop < 19 Margin of Error", "2+ only - Pop 19-34",
                    "2+ only - Pop 19-34 MOE", "2+ only - Pop 35-64", "2+ only - Pop 35-64 MOE",
                    "2+ only - Pop 65+", "2+ only - Pop 65+ MOE",
                    "No Insurance only - Pop < 19",
                    "No Insurance only - Pop < 19 Margin of Error", "No Insurance only - Pop 19-34",
                    "No Insurance only - Pop 19-34 MOE", "No Insurance only - Pop 35-64", "No Insurance only - Pop 35-64 MOE",
                    "No Insurance only - Pop 65+", "2+ only - Pop 65+ MOE",
                    "Civilian noninstitutionalized population with Health Coverage",
                    "Civilian noninstitutionalized population with Health Coverage MOE",
                    "Total Civilian Noninstitutionalized Population - With health insurance coverage",
                    "Total Civilian Noninstitutionalized Population - With health insurance coverage MOE",
                    "Total Civilian Noninstitutionalized Population - With private health insurance",
                    "Total Civilian Noninstitutionalized Population - With private health insurance MOE",
                    "Total Civilian Noninstitutionalized Population - With public coverage",
                    "Total Civilian Noninstitutionalized Population - With public coverage MOE",
                    "Total Civilian Noninstitutionalized Population - No health insurance coverage",
                    "Total Civilian Noninstitutionalized Population - No health insurance coverage MOE",
                    "Civilian noninstitutionalized population under 19 years",
                    "Civilian noninstitutionalized population under 19 years MOE",
                    "Total Civilian Noninstitutionalized Population under 19 yrs - No health insurance coverage",
                    "Total Civilian Noninstitutionalized Population under 19 yrs - No health insurance coverage MOE",
                    "Percent of Population with No Health Insurance Coverage",
                    "Percent of Population with No Health Insurance Coverage MOE"
)

######### Creating new columns calculating the percent uninsured for each age group
######### Those that are under 19
data$Percent_Uninsured_L19 <- data$`No Insurance only - Pop < 19`/ (data$`Employer Insurance only - Pop < 19` + data$`Direct Purchase only - Pop < 19` + 
                                                                      data$`Medicare only - Pop < 19` + data$`Medicaid only - Pop < 19` +
                                                                      data$`Tricare only - Pop < 19` + data$`VA Healthcare only - Pop < 19` + 
                                                                      data$`2+ only - Pop < 19` + data$`No Insurance only - Pop < 19`) * 100

##################################
#### Uninsured < 19 years old ####
##################################

breaks <- c(1, 3.5, 6, 8.5, 11)
m.class <- cut(data$Percent_Uninsured_L19, breaks)

m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Equal sized bins of Uninsured Percentages for Ages under 19", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)
dev.off()

##################################
#### Uninsured < 19 years old ####
##################################
# Quintiles
breaks <- c(1, 3.1, 3.9, 4.98, 6.77, 10.6)
m.class <- cut(data$Percent_Uninsured_L19, breaks)

m.col <- brewer.pal(5, "Blues")[m.class]

pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Quintiles of Uninsured Percentages for Ages under 19 ", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

##################################
### Uninsured 19-34 years old ####
##################################
data$Percent_Uninsured_19_34 <- data$`No Insurance only - Pop 19-34`/ (data$`Employer Insurance only - Pop 19-34` + data$`Direct Purchase only - Pop 19-34` + 
                                                                         data$`Medicare only - Pop 19-34` + data$`Medicaid only - Pop 19-34` +
                                                                         data$`Tricare only - Pop 19-34` + data$`VA Healthcare only - Pop 19-34` + 
                                                                         data$`2+ only - Pop 19-34` + data$`No Insurance only - Pop 19-34`) * 100

#### 19-34 Equal Bin Sizes
breaks <- c(5, 10, 15, 20, 25, 30)
m.class <- cut(data$Percent_Uninsured_19_34, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Equal sized bins of Uninsured Percentages for Ages 19-34", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#### 19-34 and over Quintiles
breaks <- c(5, 11.2, 12.94, 16.28, 19.83, 30)
m.class <- cut(data$Percent_Uninsured_19_34, breaks)

m.col <- brewer.pal(5, "Blues")[m.class]

pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Quintiles of Uninsured Percentages for Ages 19-34 ", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

##################################
### Uninsured 35-64 years old ####
##################################
data$Percent_Uninsured_35_64 <- data$`No Insurance only - Pop 35-64`/ (data$`Employer Insurance only - Pop 35-64` + data$`Direct Purchase only - Pop 35-64` + 
                                                                         data$`Medicare only - Pop 35-64` + data$`Medicaid only - Pop 35-64` +
                                                                         data$`Tricare only - Pop 35-64` + data$`VA Healthcare only - Pop 35-64` + 
                                                                         data$`2+ only - Pop 35-64` + data$`No Insurance only - Pop 35-64`) * 100
#### 35-64 and over Equal Bin Sizes
breaks <- c(3, 7, 11, 15, 19, 23)
m.class <- cut(data$Percent_Uninsured_35_64, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Equal sized bins of Uninsured Percentages for Ages 35-64", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#### 35-64  and over Quintiles
breaks <- c(3, 6.97, 8.5, 10.9, 13.66, 20.3)
m.class <- cut(data$Percent_Uninsured_35_64, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Quintiles of Uninsured Percentages for Ages 35-64 ", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

##################################
#### Uninsured 65 + years old ####
##################################
data$Percent_Uninsured_65 <- data$`No Insurance only - Pop 65+`/ (data$`Employer Insurance only - Pop 65+` + data$`Direct Purchase only - Pop 65+` + 
                                                                    data$`Medicare only - Pop 65+` + #data$`Medicaid only - Pop 65+` +
                                                                    data$`Tricare only - Pop 65+` + data$`VA Healthcare only - Pop 65+` + 
                                                                    data$`2+ only - Pop 65+` + data$`No Insurance only - Pop 65+`) * 100

#### 65 and over Equal Bin Sizes
breaks <- c(.10, .50, .90, 1.30, 1.70, 2.0)
m.class <- cut(data$Percent_Uninsured_65, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Equal bin size of Uninsured Percentages for Ages over 65 ", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#### 65 and over Quintiles
classIntervals(data$Percent_Uninsured_65, 5)
breaks <- c(.15, .35, .45, .575, .93, 1.9)
m.class <- cut(data$Percent_Uninsured_65, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Quintiles of Uninsured Percentages for Ages over 65 ", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

##################################
####### Overall Uninsured  #######
##################################

### Equal Bin sizes
breaks <- c(2.5, 6, 9.5, 13, 16.5, 20)
m.class <- cut(data$`Percent of Population with No Health Insurance Coverage`, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$Geo_Name == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$Geo_Name == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Equal bin size of Overall Uninsured Percentages", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#### Overall Quintiles
classIntervals(data$`Percent of Population with No Health Insurance Coverage`, 5)
breaks <- c(2.75, 6.1, 7.3, 9.16, 11, 17.5)
m.class <- cut(data$`Percent of Population with No Health Insurance Coverage`, breaks)

m.col <- brewer.pal(5, "Blues")[m.class]

pct.m.col <- m.col[match.map("state", data$Geo_Name)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$Geo_Name == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[data$Geo_Name == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Quintiles of Overall Uninsured Percentages", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

dev.off()




################################################################################## 
# Political Trifecta data came from this website: https://ballotpedia.org/State_government_trifectas
# Unemployment data came from this website: https://www.bls.gov/charts/state-employment-and-unemployment/state-unemployment-rates-animated.htm

setwd("~/Desktop/Graduate classes 2021 Spring/Stat Viz 2/Final Project")
#senators <- read_excel("Senators.xlsx")
pdata <- read.csv("Political Trifecta.csv")
pdata

# Change the local gov. from Republican to -1
# Change the local gov. from Divided to 0
# Change the local gov. from Democratic to -1
pdata$ngovernment <- ifelse(pdata$Local.Government == "Republican trifecta", -1, pdata$Local.Government)
pdata$ngovernment <- ifelse(pdata$Local.Government == "Divided government", 0, pdata$ngovernment)
pdata$ngovernment <- ifelse(pdata$Local.Government == "Democratic trifecta", 1, pdata$ngovernment)
pdata$ngovernment <- as.numeric(pdata$ngovernment)

# See which states are a divided government
pdata[pdata$Local.Government == "Divided government", ]
### States with a divided Government
# Alaska, Kansas, Kentucky, Louisiana, Maryland, Massachusetts, 
# Michigan, Minnesota, N. Carolina, Pennsylvania, Vermont, Wisconsin


pdf("Trifectas.pdf")
breaks <- seq(-1, 1.1, by = .7)
m.class <- cut(pdata$ngovernment, breaks, include.lowest = TRUE, right = FALSE)

m.col <- brewer.pal(3, "RdBu")[m.class]
levels(m.class)[1] <- "Republican"
levels(m.class)[2] <- "Divided"
levels(m.class)[3] <- "Democratic"

pct.m.col <- m.col[match.map("state", pdata$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3, 
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(3, "RdBu")[m.class[pdata$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(3, "RdBu")[m.class[pdata$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("2020 State political Trifectas", cex = 1)

legend("bottomright", legend = levels(m.class), title = "Trifectas",
       fill = brewer.pal(3, "RdBu"), cex = 1.5)

# Adding columns into the pdata
pdata$Percent_Uninsured_L19  <- data$Percent_Uninsured_L19 
pdata$Percent_Uninsured_19_34  <- data$Percent_Uninsured_19_34 
pdata$Percent_Uninsured_35_64  <- data$Percent_Uninsured_35_64 
pdata$Percent_Uninsured_65  <- data$Percent_Uninsured_65 
pdata$`Percent of Population with No Health Insurance Coverage`  <- data$`Percent of Population with No Health Insurance Coverage` 

# comparing Republic vs Democratic vs Divided
red <- pdata[pdata$ngovernment == -1, ]
blue <- pdata[pdata$ngovernment == 1, ]
divided <- pdata[pdata$ngovernment == 0, ]
summary(red$`Percent of Population with No Health Insurance Coverage`)
summary(blue$`Percent of Population with No Health Insurance Coverage`)
summary(divided$`Percent of Population with No Health Insurance Coverage`)

RefinedPlot2 <- mmplot(stat.data = pdata,
                       map.data = statePolys,
                       panel.types = c("map", "labels", "dot", "dot"),
                       panel.data = list(NA, "State", "Percent of Population with No Health Insurance Coverage", "ngovernment"),
                       ord.by = "ngovernment",
                       rev.ord = FALSE,
                       grouping = 5,
                       map.link = c("State", "ID"),
                       
                       plot.height = 9,
                       plot.width = 5,
                       colors = c("red", "blue", "green", "purple", "orange"),
                       two.ended.maps = TRUE,
                       map.all = TRUE,
                       map.color2 = "lightgray",
                       
                       plot.panel.spacing = 0,
                       panel.att = list(
                         list(1, header = "Two-ended\nCumulative Maps",
                              inactive.border.color = gray(0.7), 
                              inactive.border.size = 1,
                              panel.width = 1.2),
                         list(2, header = "States", 
                              panel.width = 0.9,
                              align = "left", 
                              text.size = 0.7),
                         list(3, header = "All Ages Percent Uninsured",
                              header.color = "black",
                              #graph.bgcolor = "lightgray", 
                              point.size = 1,
                              #xaxis.ticks = seq(5, 30, by = 5), 
                              #xaxis.labels = seq(5, 30, by = 5),
                              xaxis.ticks = seq(2, 18, by = 3), 
                              xaxis.labels = seq(2, 18, by = 3),
                              xaxis.title = "Uninsured 2018"),
                         list(4, header = "Type of Government",
                              header.color = "black",
                              graph.bgcolor = "lightgray", 
                              point.size = 1,
                              xaxis.ticks = seq(-1, 1, by = .5), 
                              xaxis.labels = seq(-1, 1, by = .5),
                              xaxis.title = "-1 = Republican"
                         ) 
                       )
                       
)
printLMPlot(RefinedPlot2, name = "Refined_All_Ages.pdf", res = 300)

####################################
####### Micro maps and data  #######
####################################

#Stat Viz II Project Data Cleaning
cdc_dat <- read.csv('/Users/braydenross/Desktop/Spring Semester 2021/Stat Viz 2/Provisional_COVID-19_Death_Counts_by_Sex__Age__and_State.csv')
cdc_dat <- cdc_dat[cdc_dat$State != 'United States',]
cdc_dat <- cdc_dat[cdc_dat$Start.Date != '01/01/2020',]
cdc_dat <- cdc_dat[,-c(1:4, 11:16)]
cdc_dat[is.na(cdc_dat)] <- 0
#write.csv(cdc_dat, 'Cleaned CDC Data for Visualization.csv', row.names = FALSE)

#Hospitalizations data cleaning
hosp <- read.csv('/Users/braydenross/Desktop/Spring Semester 2021/Stat Viz 2/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv')

hosp_sub <- hosp[,c(2, 3, 10, 13, 17, 19)]
new_df <- as.data.frame(str_split_fixed(hosp_sub$collection_week, "/", 3))
hosp_sub <- cbind(hosp_sub, new_df)
colnames(hosp_sub) <- c('Collection Date', 'state', 'fips','adult_beds', 'all_adult', 'all_pediatric','Year', 'Month', 'Day')
hosp_sub[hosp_sub == -999999.0] <- 0

hosp_sub <- hosp_sub[hosp_sub$Month %in% c('01','08'),]
grouped_hosp <- hosp_sub %>%
  group_by(state, Month) %>%
  summarise(total_hosps = sum(all_adult) + sum(all_pediatric))
grouped_hosp[is.na(grouped_hosp)] <- 0
aug_hosp <- grouped_hosp[grouped_hosp$Month == '08',]
jan_hosp <- grouped_hosp[grouped_hosp$Month == '01',]
aug_hosp <- aug_hosp[,-2]
jan_hosp <- jan_hosp[,-2]

deaths <- read.csv("/Users/braydenross/Desktop/Spring Semester 2021/Stat Viz 2/Cleaned CDC Data for Visualization.csv")
hosp <- read.csv('/Users/braydenross/Desktop/Spring Semester 2021/Stat Viz 2/COVID-19_Reported_Patient_Impact_and_Hospital_Capacity_by_Facility.csv')

deaths <- deaths[!deaths$State %in% c('New York City', 'Puerto Rico'),]

deaths_aug <- deaths[deaths$Month == 8,]
deaths_jan <- deaths[deaths$Month == 1,]

deaths_map <- merge(deaths_map_aug, deaths_map_jan, by = 'State')
data('USstates')
statePolys <- create_map_table(USstates, 'ST_NAME')
lmplot(stat.data = deaths_map,
       map.data = statePolys,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('State','aug_deaths','jan_deaths', NA),
       ord.by = 'aug_deaths',   
       grouping = 5,
       colors = brewer.pal(5, "Spectral"),
       # how to merge the two data frames:
       map.link = c('State', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       # how to save the result:
       print.file = "Deaths_map.png",
       print.res = 600)
lmplot(stat.data = deaths_map,
       map.data = statePolys,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('State','aug_deaths','jan_deaths', NA),
       ord.by = 'aug_deaths',   
       grouping = 5,
       colors = brewer.pal(5, "Spectral"),
       # how to merge the two data frames:
       map.link = c('State', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       # how to save the result:
       print.file = "Deaths_map.png",
       print.res = 600,
       # attributes of the panels:
       panel.att = list(
         list(1, header = "States", 
              panel.width = 0.8, 
              text.size = 0.8, 
              align = 'right',
              panel.header.face = "bold",
              left.margin = 0,
              right.margin = 0.8),
         list(2, header = "August 2020 COVID Death\nCounts (In Hundreds)", 
              xaxis.title = 'Count',
              xaxis.ticks = list(0, 5000, 10000,15000,  20000,25000),
              xaxis.labels = list(0, 50, 100, 150, 200, 250),
              graph.bgcolor = 'white',
              graph.grid.color = "black",
              graph.border.color = "black",
              panel.header.face = "bold"),
         list(3, header = "January 2021 COVID Death\nCounts (In Hundreds)", 
              xaxis.title = 'Count',
              xaxis.ticks = list(0, 5000, 10000,15000,  20000,25000),
              xaxis.labels = list(0, 50, 100, 150, 200, 250),
              graph.bgcolor = 'white',
              graph.grid.color = "black",
              graph.border.color = "black",
              panel.header.face = "bold"),
         list(4, header = 'Light gray means\nhighlighted above', 
              panel.width = 0.8,
              panel.header.face = "italic")
       ))
####.####

colnames(jan_hosp) <- c('State','jan_hosps')
colnames(aug_hosp) <- c('State','aug_hosps')
hosp_map <- merge(jan_hosp, aug_hosp, by = 'State')
hosp_map <- hosp_map[!hosp_map$State %in% c('GU', 'PR', 'MP'),]
statePolys <- create_map_table(USstates, 'ST')
lmplot(stat.data = hosp_map,
       map.data = statePolys,
       panel.types = c('labels', 'dot', 'dot','map'),
       panel.data = list('State','aug_hosps','jan_hosps', NA),
       ord.by = 'aug_hosps',   
       grouping = 5,
       colors = brewer.pal(5, "Spectral"),
       # how to merge the two data frames:
       map.link = c('State', 'ID'),
       # stroke colour of the borders of previously-drawn areas:
       # how to save the result:
       print.file = "Hosps_map.png",
       print.res = 600,
       # attributes of the panels:
       panel.att = list(
         list(1, header = "States", 
              panel.width = 0.8, 
              text.size = 0.8, 
              align = 'right',
              panel.header.face = "bold",
              left.margin = 1,
              right.margin = 0.8),
         list(2, header = "August 2020 COVID \n Hospitalizations (100s)", 
              xaxis.title = 'Count',
              xaxis.ticks = list(0, 5000, 10000,15000,  20000,25000),
              xaxis.labels = list(0, 50, 100, 150, 200, 250),
              graph.bgcolor = 'white',
              graph.grid.color = "black",
              graph.border.color = "black",
              panel.header.face = "bold"),
         list(3, header = "January 2021 COVID \n Hospitalizations (100s)", 
              xaxis.title = 'Count',
              xaxis.ticks = list(0, 20000, 40000,60000,80000,100000),
              xaxis.labels = list(0, 200, 400, 600, 800, 1000),
              graph.bgcolor = 'white',
              graph.grid.color = "black",
              graph.border.color = "black",
              panel.header.face = "bold"),
         list(4, header = 'Light gray means\nhighlighted above', 
              panel.width = 0.8,
              panel.header.face = "italic")
       ))

#########################################################
####### Maps of Covid Hospitalizations and Deaths #######
#########################################################

total_2020 <- read.csv("Cleaned CDC Data.csv", header = TRUE) %>%
  filter(Year == 2020) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

total_2021 <- read.csv("Cleaned CDC Data.csv", header = TRUE) %>%
  filter(Year == 2021) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

PopData <- read.csv("StatePopulationData2019.csv", header = FALSE) 
PopData <- PopData[1:2, ]
PopData <- as.data.frame(t(PopData))
PopData <- PopData[2:53, ] %>%
  rename(State = 1, Population = 2)
PopData$State <- gsub(".Pop", "", PopData$State)
PopData$State <- gsub("Tennessee!!Total", "Tennessee", PopData$State)
PopData$Population <- as.numeric(gsub(",", "", PopData$Population))

# Counts per capita
PopData20 <- merge(total_2020, PopData, by = "State")
PopData20$PerCapita <- PopData20$COVID.19.Deaths/PopData20$Population * 1000

PopData21 <- merge(total_2021, PopData, by = "State")
PopData21$PerCapita <- PopData21$COVID.19.Deaths/PopData21$Population * 1000

# Covid-19 Deaths for August 2020
breaks <- c(0, 0.150, 0.224, 0.306, 0.419, 0.669)
m.class <- cut(PopData20$PerCapita, breaks)

m.col <- brewer.pal(5, "Blues")[m.class]

pct.m.col <- m.col[match.map("state", PopData20$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska", 
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths for August 2020 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

# Covid-19 Deaths for January 2021
m.class <- cut(PopData21$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData21$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths for January 2021 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

# Difference Maps
total_count <- merge(PopData20, PopData21, by = "State") %>%
  select(-c("Population.x", "Population.y")) %>%
  rename(Covid.19.Deaths_2020 = COVID.19.Deaths.x, PerCapita_2020 = PerCapita.x, 
         Covid.19.Deaths_2021 = COVID.19.Deaths.y, PerCapita_2021 = PerCapita.y)
total_count$Differences <- total_count$PerCapita_2020 - total_count$PerCapita_2021 #If positive, higher numbers in 2020

Aug_Jan <- subset(total_count, total_count$Differences > 0) # August 2020 is greather than January 2021
Jan_Aug <- subset(total_count, total_count$Differences < 0) # January 2021 is greather than August 2020
Jan_Aug$Differences <- abs(Jan_Aug$Differences)

##################################
#### Uninsured < 19 years old ####
##################################

total_2020_L19 <- read.csv("Cleaned CDC Data.csv", header = TRUE) %>%
  filter(Year == 2020) %>%
  group_by(State, Age.Group) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

total_2021_L19 <- read.csv("Cleaned CDC Data.csv", header = TRUE) %>%
  filter(Year == 2021) %>%
  group_by(State, Age.Group) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

Uninsured_L19_2020 <- subset(total_2020_L19, total_2020_L19$Age.Group == c("0-17 years", "Under 1 year")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

Uninsured_L19_2021 <- subset(total_2021_L19, total_2021_L19$Age.Group == c("0-17 years", "Under 1 year")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

# Counts per capita
PopData20_L19 <- merge(Uninsured_L19_2020, PopData, by = "State")
PopData20_L19$PerCapita <- PopData20_L19$COVID.19.Deaths/PopData20_L19$Population * 1000

PopData21_L19 <- merge(Uninsured_L19_2021, PopData, by = "State")
PopData21_L19$PerCapita <- PopData21_L19$COVID.19.Deaths/PopData21_L19$Population * 1000

# Difference Maps
total_count_L19 <- merge(PopData20_L19, PopData21_L19, by = "State") %>%
  select(-c("Population.x", "Population.y")) %>%
  rename(Covid.19.Deaths_2020 = COVID.19.Deaths.x, PerCapita_2020 = PerCapita.x, 
         Covid.19.Deaths_2021 = COVID.19.Deaths.y, PerCapita_2021 = PerCapita.y)
total_count_L19$Differences <- total_count_L19$ PerCapita_2020 - total_count_L19$PerCapita_2021 #If positive, higher numbers in 2020

Aug_Jan <- subset(total_count_L19, total_count_L19$Differences > 0) # August 2020 is greather than January 2021
Jan_Aug <- subset(total_count_L19, total_count_L19$Differences < 0) # January 2021 is greather than August 2020
Jan_Aug$Differences <- abs(Jan_Aug$Differences)

# No Map's as there are no counts for <19!!!!!

#######################################
##### Uninsured 19 - 34 years old #####
#######################################

Uninsured_19_34_2020 <- subset(total_2020_L19, total_2020_L19$Age.Group == c("25-34 years", "18-29 years")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

Uninsured_19_34_2021 <- subset(total_2021_L19, total_2021_L19$Age.Group == c("25-34 years", "18-29 years")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

PopData1 <- read.csv("StatePopulationData2019.csv", header = FALSE) 
PopData1 <- PopData1[c(1, 8:10), ]
PopData1 <- as.data.frame(t(PopData1))
PopData1 <- PopData1[2:52, ] %>%
  rename(State = 1, Twenty_TwentyFour = "8", TwentyFive_TwentyNine = "9", Thirty_ThirtyFour = "10")
PopData1$State <- gsub(".Pop", "", PopData1$State)
PopData1$State <- gsub("Tennessee!!Total", "Tennessee", PopData1$State)
PopData1$Twenty_TwentyFour <- as.numeric(gsub(",", "", PopData1$Twenty_TwentyFour))
PopData1$TwentyFive_TwentyNine <- as.numeric(gsub(",", "", PopData1$TwentyFive_TwentyNine))
PopData1$Thirty_ThirtyFour <- as.numeric(gsub(",", "", PopData1$Thirty_ThirtyFour))
PopData1$Population = rowSums(PopData1[,c(-1)])

# Counts per capita
PopData20_19_34 <- merge(Uninsured_19_34_2020, PopData1, by = "State")
PopData20_19_34$PerCapita <- PopData20_19_34$COVID.19.Deaths/PopData20_19_34$Population * 1000

PopData21_19_34 <- merge(Uninsured_19_34_2021, PopData1, by = "State")
PopData21_19_34$PerCapita <- PopData21_19_34$COVID.19.Deaths/PopData21_19_34$Population * 1000

# Difference Maps
total_count_19_34 <- merge(PopData20_19_34, PopData21_19_34, by = "State") %>%
  select(-c("Population.x", "Population.y")) %>%
  rename(Covid.19.Deaths_2020 = COVID.19.Deaths.x, PerCapita_2020 = PerCapita.x, 
         Covid.19.Deaths_2021 = COVID.19.Deaths.y, PerCapita_2021 = PerCapita.y)
total_count_19_34$Differences <- total_count_19_34$PerCapita_2020 - total_count_19_34$PerCapita_2021 #If positive, higher numbers in 2020

Aug_Jan <- subset(total_count_19_34, total_count_19_34$Differences > 0) # August 2020 is greather than January 2021
Jan_Aug <- subset(total_count_19_34, total_count_19_34$Differences < 0) # January 2021 is greather than August 2020
Jan_Aug$Differences <- abs(Jan_Aug$Differences)

# Covid-19 Deaths for August 2020
#png("Total COVID-19 deaths between 19-34 for August 2020 Per Capita.png")
breaks <- classIntervals(PopData21_19_34$PerCapita, 5)
breaks <- c(0, 0.00335, 0.00704, 0.0079, 0.0088, 0.0355)
m.class <- cut(PopData20_19_34$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData20_19_34$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_19_34$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_19_34$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths between 19-34 for August 2020 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

# Covid-19 Deaths for January 2021
#png("Total COVID-19 deaths between 19-34 for January 2021 Per Capita.png")
m.class <- cut(PopData21_19_34$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData21_19_34$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_19_34$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_19_34$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths between 19-34 for January 2021 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#####################################
#### Uninsured 35 - 64 years old ####
#####################################

Uninsured_35_64_2020 <- subset(total_2020_L19, total_2020_L19$Age.Group == c("35-44 years", "45-54 years", "55-64 years")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

Uninsured_35_64_2021 <- subset(total_2021_L19, total_2021_L19$Age.Group == c("35-44 years", "45-54 years", "55-64 years")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

PopData2 <- read.csv("StatePopulationData2019.csv", header = FALSE) 
PopData2 <- PopData2[c(1, 11:16), ]
PopData2 <- as.data.frame(t(PopData2))
PopData2 <- PopData2[2:52, ] %>%
  rename(State = 1, ThirtyFive_ThirtyNine = "11", Forty_FortyFour = "12", FortyFive_FortyNine = "13", 
         Fifty_FiftyFour = "14", FiftyFive_FiftyNine = "15", Sixty_SixtyFour = "16")
PopData2$State <- gsub(".Pop", "", PopData2$State)
PopData2$State <- gsub("Tennessee!!Total", "Tennessee", PopData2$State)
PopData2$ThirtyFive_ThirtyNine <- as.numeric(gsub(",", "", PopData2$ThirtyFive_ThirtyNine))
PopData2$Forty_FortyFour <- as.numeric(gsub(",", "", PopData2$Forty_FortyFour))
PopData2$FortyFive_FortyNine <- as.numeric(gsub(",", "", PopData2$FortyFive_FortyNine))
PopData2$Fifty_FiftyFour <- as.numeric(gsub(",", "", PopData2$Fifty_FiftyFour))
PopData2$FiftyFive_FiftyNine <- as.numeric(gsub(",", "", PopData2$FiftyFive_FiftyNine))
PopData2$Sixty_SixtyFour <- as.numeric(gsub(",", "", PopData2$Sixty_SixtyFour))
PopData2$Population = rowSums(PopData2[,c(-1)])

# Counts per capita
PopData20_35_64 <- merge(Uninsured_35_64_2020, PopData2, by = "State")
PopData20_35_64$PerCapita <- PopData20_35_64$COVID.19.Deaths/PopData20_35_64$Population * 1000

PopData21_35_64 <- merge(Uninsured_35_64_2021, PopData2, by = "State")
PopData21_35_64$PerCapita <- PopData21_35_64$COVID.19.Deaths/PopData21_35_64$Population * 1000

# Difference Maps
total_count_35_64 <- merge(PopData20_35_64, PopData21_35_64, by = "State") %>%
  select(-c("Population.x", "Population.y")) %>%
  rename(Covid.19.Deaths_2020 = COVID.19.Deaths.x, PerCapita_2020 = PerCapita.x, 
         Covid.19.Deaths_2021 = COVID.19.Deaths.y, PerCapita_2021 = PerCapita.y)
total_count_35_64$Differences <- total_count_35_64$PerCapita_2020 - total_count_35_64$PerCapita_2021 #If positive, higher numbers in 2020

Aug_Jan <- subset(total_count_35_64, total_count_35_64$Differences > 0) # August 2020 is greather than January 2021
Jan_Aug <- subset(total_count_35_64, total_count_35_64$Differences < 0) # January 2021 is greather than August 2020
Jan_Aug$Differences <- abs(Jan_Aug$Differences)

# Covid-19 Deaths for August 2020
classIntervals(PopData21_35_64$PerCapita, 5)
breaks <- c(0, 0.0031, 0.025, 0.056, 0.089, 0.200)
m.class <- cut(PopData20_35_64$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData20_35_64$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_35_64$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_35_64$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths between 35-64 for August 2020 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

# Covid-19 Deaths for January 2021
m.class <- cut(PopData21_35_64$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData21_35_64$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_35_64$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_35_64$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths between 35-64 for January 2021 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

#################################
#### Uninsured 65+ years old ####
#################################

Uninsured_65_2020 <- subset(total_2020_L19, total_2020_L19$Age.Group == c("65-74 years", "75-84 years", "85 years and older")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

Uninsured_65_2021 <- subset(total_2021_L19, total_2021_L19$Age.Group == c("65-74 years", "75-84 years", "85 years and older")) %>%
  group_by(State) %>%
  summarize(COVID.19.Deaths = sum(COVID.19.Deaths))

PopData3 <- read.csv("StatePopulationData2019.csv", header = FALSE) 
PopData3 <- PopData3[c(1, 17:21), ]
PopData3 <- as.data.frame(t(PopData3))
PopData3 <- PopData3[2:52, ] %>%
  rename(State = 1, SixtyFive_SixtyNine = "17", Seventy_SeventyFour = "18", SeventyFive_SeventyNine = "19", 
         Eighty_EightyFour = "20", EightyFive_Older = "21")
PopData3$State <- gsub(".Pop", "", PopData3$State)
PopData3$State <- gsub("Tennessee!!Total", "Tennessee", PopData3$State)
PopData3$SixtyFive_SixtyNine <- as.numeric(gsub(",", "", PopData3$SixtyFive_SixtyNine))
PopData3$Seventy_SeventyFour <- as.numeric(gsub(",", "", PopData3$Seventy_SeventyFour))
PopData3$SeventyFive_SeventyNine <- as.numeric(gsub(",", "", PopData3$SeventyFive_SeventyNine))
PopData3$Eighty_EightyFour <- as.numeric(gsub(",", "", PopData3$Eighty_EightyFour))
PopData3$EightyFive_Older <- as.numeric(gsub(",", "", PopData3$EightyFive_Older))
PopData3$Population = rowSums(PopData3[,c(-1)])

# Counts per capita
PopData20_65 <- merge(Uninsured_65_2020, PopData3, by = "State")
PopData20_65$PerCapita <- PopData20_65$COVID.19.Deaths/PopData20_65$Population * 1000

PopData21_65 <- merge(Uninsured_65_2021, PopData3, by = "State")
PopData21_65$PerCapita <- PopData21_65$COVID.19.Deaths/PopData21_65$Population * 1000

# Difference Maps
total_count_65 <- merge(PopData20_65, PopData21_65, by = "State") %>%
  select(-c("Population.x", "Population.y")) %>%
  rename(Covid.19.Deaths_2020 = COVID.19.Deaths.x, PerCapita_2020 = PerCapita.x, 
         Covid.19.Deaths_2021 = COVID.19.Deaths.y, PerCapita_2021 = PerCapita.y)
total_count_65$Differences <- total_count_65$PerCapita_2020 - total_count_65$PerCapita_2021 #If positive, higher numbers in 2020

Aug_Jan <- subset(total_count_65, total_count_65$Differences > 0) # August 2020 is greather than January 2021
Jan_Aug <- subset(total_count_65, total_count_65$Differences < 0) # January 2021 is greather than August 2020
Jan_Aug$Differences <- abs(Jan_Aug$Differences)

# Covid-19 Deaths for August 2020
classIntervals(PopData21_65$PerCapita, 5)
breaks <- c(0, 0.576, 0.743, 0.926, 1.191, 1.547)
m.class <- cut(PopData20_65$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData20_65$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_65$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData20_65$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths 65+ for August 2020 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

# Covid-19 Deaths for January 2021
m.class <- cut(PopData21_65$PerCapita, breaks)
m.col <- brewer.pal(5, "Blues")[m.class]
pct.m.col <- m.col[match.map("state", PopData21_65$State)]

par(mar = c(0, 0, 0, 0))
layout(matrix(c(1, 1, 0, 0, 0,
                1, 1, 0, 3, 3,
                1, 1, 0, 3, 3,
                3, 3, 3, 3, 3,
                3, 3, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 3, 3, 3,
                2, 2, 0, 3, 3), nrow = 8, byrow = TRUE))

par(mar = c(0, 0, 0, 0))
map("world2Hires", "USA:Alaska",
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_65$State == "Alaska"]])
mtext("Alaska")

par(mar = c(0, 0, 0, 0))
map("world2Hires", "Hawaii", xlim = c(200, 205), ylim = c(18, 24),
    fill = TRUE, col = brewer.pal(5, "Blues")[m.class[PopData21_65$State == "Hawaii"]])
mtext("Hawaii", side = 1)

par(mar = c(0, 0, 0, 0))
map("state", fill = TRUE, col = pct.m.col)
mtext("Total COVID-19 deaths 65+ for January 2021 Per Capita", cex = 1)
legend("bottomright", legend = levels(m.class), fill = brewer.pal(5, "Blues"), cex = 1.5)

load.image("Total_Uninsured.PNG")

