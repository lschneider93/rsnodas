
# Read in
library(dplyr)
crash <- read.csv("KSI_CLEAN.csv", header = T)

# Omit Columns
crash <- subset(crash, INJURY != " ") 
crash <- subset(crash, INVTYPE != "Other") 


# get unique crash with most sever crash
unique(crash$INJURY)
crash$Y <- ifelse(crash$INJURY == "Fatal", 5, crash$INJURY)
crash$Y <- ifelse(crash$INJURY == "Major", 4, crash$Y)
crash$Y <- ifelse(crash$INJURY == "Minor", 3, crash$Y)
crash$Y <- ifelse(crash$INJURY == "Minimal", 2, crash$Y)
crash$Y <- ifelse(crash$INJURY == "None", 1, crash$Y)

colnames(crash)
crash1 <- crash[-c(10:18,21,29:42,56)]


crash1$HOUR <- crash1$HOUR + round(as.numeric(crash1$MINUTES)*1/60, 2)

crash1$MONTH <- crash1$MONTH + round(((crash1$DAY -1)/31), 2)

crash1 <- crash1[-c(4,6)]

table(crash1$Y)

write.csv(crash1, "KSI_1.csv", row.names = F)

# treat NA's and missing value
unique(crash$INVTYPE)

table(crash$INVTYPE)

crashDf <- subset(crash, !(INVTYPE %in% c("Vehicle Owner","Other Property Owner",
                                          "Trailer Owner", "Witness", "", 
                                          "Runaway - No Driver", "Unknown - FTR",
                                          "Pedestrian - Not Hit","In-Line Skater")))


################################################################################
k <- read.csv("KSI_1.csv", header = T)

summary(k)

k$LOCCOORD <- ifelse(k$LOCCOORD == " ", "Unknown", k$LOCCOORD)
k$TRAFFCTL <- ifelse(k$TRAFFCTL == " ", "No Control", k$TRAFFCTL)
k$VISIBILITY <- ifelse(k$VISIBILITY == " ", "Other", k$VISIBILITY)
k$LIGHT <- ifelse(k$LIGHT == " ", "Other", k$LIGHT)
k$RDSFCOND <- ifelse(k$RDSFCOND == " ", "Other", k$RDSFCOND)


k1 <- subset(k, !(INVTYPE %in% c("Vehicle Owner","Other Property Owner",
                                "Trailer Owner", "Witness", "", 
                                "Runaway - No Driver", "Unknown - FTR",
                                "Pedestrian - Not Hit")))

k1$INVTYPE <- ifelse(k1$INVTYPE %in% c("In-Line Skater", "Wheelchair"), 
                     "Pedestrian", k1$INVTYPE)

k1$INVTYPE <- ifelse(k1$INVTYPE == "Driver - Not Hit", "Driver", k1$INVTYPE)

k1$WEEKDAY <- sapply(k1$WEEKDAY + 1, switch, "Monday", "Tuesday", "Wednesday", 
                     "Thursday", "Friday", "Saturday", "Sunday")


k1$LOCCOORD <- ifelse(k1$LOCCOORD %in% c("Entrance Ramp Westbound",
                                       "Park, Private Property, Public Lane"), 
                     "Unknown", k1$LOCCOORD)

k1$RDSFCOND <- ifelse(k1$RDSFCOND == "Spilled liquid", "Other", k1$RDSFCOND)

# remove acclass, accnumber
k2 <- k1[-c(1,14)]


write.csv(k2, "KSI_2.csv", row.names = F)

################################################################################
k2 <- read.csv("KSI_2.csv", header = T)
k2$Y2 <- ifelse(k2$Y > 3, 1, 0)
k2$YEAR <- as.factor(k2$YEAR)
lapply(k2, class)

kmix <- k2[sample(nrow(k2)),]

ktrain <- kmix[1:8000,]
ktest <- kmix[8001:10772,]
 
library(randomForest)
library(caret)
colnames(k2)

rfr <- randomForest(Y~ ., importance = T, data = ktrain[-29])
varImpPlot(rfr, scale=FALSE)
rfr$rsq

pr <- predict(rfr, ktrain[-c(28,29)])
confusionMatrix(as.factor(round(pr)), as.factor(ktrain$Y))

prt <- predict(rfr, ktest[-c(28,29)])
confusionMatrix(as.factor(round(prt)), as.factor(ktest$Y))

rfc <- randomForest(as.factor(Y)~ ., importance = T, data = ktrain[-29])
varImpPlot(rfc, scale=FALSE)

prc <- predict(rfc, ktrain[-c(28,29)])
confusionMatrix(as.factor(prc), as.factor(ktrain$Y))

prct <- predict(rfc, ktest[-c(28,29)])
confusionMatrix(as.factor(prct), as.factor(ktest$Y))


rf2 <- randomForest(as.factor(Y2)~ ., importance = T, data = ktrain[-28])
varImpPlot(rf, scale=FALSE)

p2 <- predict(rf2, ktrain[-c(28,29)])
confusionMatrix(as.factor(p2), as.factor(ktrain$Y2))

p2t <- predict(rf2, ktest[-c(28,29)])
confusionMatrix(as.factor(p2t), as.factor(ktest$Y2))



fitControl = trainControl(method = "cv", number = 10)
rfGrid = expand.grid(mtry = seq(2, 11, 1))
rfFit = train(as.factor(Y2) ~ ., method = "rf", tuneGrid = rfGrid,
               trControl = fitControl, data = ktrain[-28])
rfFit$bestTune

rf2t <- randomForest(as.factor(Y2) ~ . , importance = TRUE, 
                    mtry =34, data = ktrain[-28])

pt2 <- predict(rf2t, ktrain[-c(28,29)])
confusionMatrix(as.factor(pt2), as.factor(ktrain$Y2))

pt2t <- predict(rf2, ktest[-c(28,29)])
confusionMatrix(as.factor(pt2t), as.factor(ktest$Y2))


## Neural Network
library(tensorflow)

library(nnet)
fitControl1 = trainControl(method = "cv", number = 10 )
nnGrid = expand.grid(size = seq(1, 10, 1), decay = seq(.02, .20, .02))
nnFit = train(Y ~ . , method = "nnet",
              tuneGrid = nnGrid, trControl = fitControl1, data = ktrain[-29])
nnFit$bestTune

nnet1 <- nnet(Y ~ ., data = ktrain[-29], linout = F, skip = F, 
              size = 7, decay = 0.18, maxit = 1000, trace = F)

np <- predict(nnet1, ktrain[-29])
confusionMatrix(as.factor(round(np)), ktrain$Y)

npt <- predict(nnet1, ktest[-29])
confusionMatrix(as.factor(round(npt)), ktest$Y)
