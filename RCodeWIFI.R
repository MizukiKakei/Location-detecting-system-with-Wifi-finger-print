######################################################################################
# Location Detecting System                                                          #
#                                                                                    #
# This project is about creating location detecting system with Wifi finger print.   #
# The location of users are predicted based on the signals between phones and        #
# several Wifi spot.                                                                 #
#                                                                                    #
# Version 1.0                                                                        #
#                                                                                    #            
# Date 18.07.2019                                                                    #
#                                                                                    #
# Mizuki Kakei                                                                       #
#                                                                                    #
#                                                                                    #     
######################################################################################

# calling Packages ####
library(readr)                                                                       # Reading data set
library(caret)                                                                       # Prediction
library(dplyr)                                                                       # Data manipulation
library(ggplot2)                                                                     # Plotting
library(plotly)                                                                      # Plotting


## Train data ####
# Calling Train data ####
rawdata <- read.csv("trainingData.csv", TRUE, sep =",")
WifiTrain <- rawdata

## Preprocessing the data set ####
# Creating dataset only with WAPs in Training data 
WifiTrainOnlyWAPs <- as.data.frame(WifiTrain)
WifiTrainOnlyWAPs$LATITUDE <- NULL
WifiTrainOnlyWAPs$FLOOR <- NULL
WifiTrainOnlyWAPs$LONGITUDE <- NULL
WifiTrainOnlyWAPs$RELATIVEPOSITION <- NULL
WifiTrainOnlyWAPs$SPACEID <- NULL
WifiTrainOnlyWAPs$USERID <- NULL
WifiTrainOnlyWAPs$BUILDINGID <- NULL
WifiTrainOnlyWAPs$PHONEID <- NULL
WifiTrainOnlyWAPs$TIMESTAMP <- NULL

# Replacing Non detected data (100) in Training datawith NA 
WifiTrainOnlyWAPs[WifiTrainOnlyWAPs == 100] <- NA

# Excluding 0 variance columns
WifiTrainNoNAWap <- WifiTrainOnlyWAPs[ , colSums(is.na(WifiTrainOnlyWAPs)) <
                                         nrow(WifiTrainOnlyWAPs)]

# Replacing NA values with -105
WifiTrainNoNAWap[is.na(WifiTrainNoNAWap)] <- -105

# Creating data set in Training data with Location, 
# User and time information Without 0 variance columns  
WifiTrainNoNA529col <- as.data.frame(WifiTrainNoNAWap)
WifiTrainNoNA529col$LATITUDE <- WifiTrain$LATITUDE
WifiTrainNoNA529col$TIMESTAMP <- WifiTrain$TIMESTAMP
WifiTrainNoNA529col$LONGITUDE <- WifiTrain$LONGITUDE
WifiTrainNoNA529col$SPACEID <- as.character(WifiTrain$SPACEID)
WifiTrainNoNA529col$USERID <- as.character(WifiTrain$USERID)
WifiTrainNoNA529col$PHONEID <- as.character(WifiTrain$PHONEID)
WifiTrainNoNA529col$BUILDINGID <- as.character(WifiTrain$BUILDINGID)
WifiTrainNoNA529col$RELATIVEPOSITION <- as.character(WifiTrain$RELATIVEPOSITION)
WifiTrainNoNA529col$FLOOR <- as.character(WifiTrain$FLOOR)

# Preparing Matrix of Training data for PCAs
WifiTrainForPCA <- as.matrix(WifiTrainNoNAWap)

# Test(Validation) data ####
# Reading Data
rawdataTest <- read.csv("validationData.csv", TRUE, sep = ",")
WifiTest <- rawdataTest

# Preprocessing Validation dataset ####
# Replacing non - detected data (100) with -105
WifiTestNoNA529col <- WifiTest
WifiTestNoNA529col[WifiTestNoNA529col == 100] <- -105

# Preparing data set with Validation data for PCA ####
WifiTestNoNA529colForPCA <- as.data.frame(WifiTestNoNA529col)
WifiTestNoNA529colForPCA$LATITUDE <- NULL
WifiTestNoNA529colForPCA$FLOOR <- NULL
WifiTestNoNA529colForPCA$LONGITUDE <- NULL
WifiTestNoNA529colForPCA$RELATIVEPOSITION <- NULL
WifiTestNoNA529colForPCA$SPACEID <- NULL
WifiTestNoNA529colForPCA$USERID <- NULL
WifiTestNoNA529colForPCA$BUILDINGID <- NULL
WifiTestNoNA529colForPCA$PHONEID <- NULL
WifiTestNoNA529colForPCA$TIMESTAMP <- NULL

# Converting data set into matrix for PCAs
WifiTestForPCA <- as.matrix(WifiTestNoNA529colForPCA)

# Matching the columns of Training and Valicdation Data for No PCAs
WifiTestNoNA529colMatch <- WifiTestNoNA529col[,match(colnames(WifiTrainNoNA529col), 
                                                     colnames(WifiTestNoNA529col))]

# Data set in validation with Location, User and time indormation for No PCAs
WifiTestNoNA529colMatch <- as.data.frame(WifiTestNoNA529colMatch)
WifiTestNoNA529colMatch$LATITUDE <-  WifiTestNoNA529col$LATITUDE
WifiTestNoNA529colMatch$TIMESTAMP <-  WifiTestNoNA529col$TIMESTAMP
WifiTestNoNA529colMatch$LONGITUDE <- WifiTestNoNA529col$LONGITUDE
WifiTestNoNA529colMatch$SPACEID <-  as.character(WifiTestNoNA529col$SPACEID)
WifiTestNoNA529colMatch$USERID <-  as.character(WifiTestNoNA529col$USERID)
WifiTestNoNA529colMatch$PHONEID <-  as.character(WifiTestNoNA529col$PHONEID)
WifiTestNoNA529colMatch$BUILDINGID <- as.character(WifiTestNoNA529col$BUILDINGID)
WifiTestNoNA529colMatch$RELATIVEPOSITION <-as.character(WifiTestNoNA529col$RELATIVEPOSITION)
WifiTestNoNA529colMatch$FLOOR <- as.character(WifiTestNoNA529col$FLOOR)


## PCAs ####
# Naming the rows of Training data 
rownames(WifiTrainForPCA) <- paste0("Observation", 1:nrow(WifiTrainForPCA))

## Computing PCA in Train Data
PCATrain <- prcomp(WifiTrainForPCA, scale = TRUE) 

# Computing Variance and Standard deviation of PCs
PCATrainVar <- PCATrain$sdev^2

# Showing 90 % of variances and corresponding columns 
sum(PCATrainVar)*0.9
sum(PCATrainVar[1:206])

# Extracting df with 206 Principal Components for the Training set
Train_PCs <- as.data.frame(PCATrain$x)
Train_PCs_206 <- Train_PCs %>% select(1:206)
                 
# Extracting df with 206 Principal Components for the Validation set
TEST_PCs <- as.data.frame(predict(PCATrain, newdata = WifiTestNoNA529colForPCA))
TEST_PCs_206 <- TEST_PCs %>% select(1:206) 


## Modeling ####
# Modeling for Longitude : For Longitude, PCA was applied ####
# Setting the data
Train_PCs_206$LONGITUDE <- WifiTrain$LONGITUDE
TEST_PCs_206$LONGITUDE <- WifiTestNoNA529col$LONGITUDE

# Splitting the data into training and testing set
set.seed(123)
Train_PCs_206_Sample <- Train_PCs_206[sample(1:nrow(Train_PCs_206), 50, replace = FALSE),]
inTraining <- createDataPartition(Train_PCs_206_Sample$LONGITUDE, p = .75, list = FALSE)
training <- Train_PCs_206_Sample[inTraining,]
testing <- Train_PCs_206_Sample[-inTraining,]

# Cross validation within training set
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

# Modeling for LONGITUDE with random forest
rfGrid <- expand.grid(mtry = c(63))
rfmodelLONGI <- train(LONGITUDE ~ ., 
                      data = training,
                      method = "rf", 
                      tuneGrid = rfGrid,
                      trControl = fitControl)
rfmodelLONGI

# Prediction for Longitude ####
# Pprediction for longitude with testing data
rfPredictionModelLONGI <- predict(rfmodelLONGI, testing)

# Checking the accuracy of the model with testing set
postResample(rfPredictionModelLONGI, testing$LONGITUDE)

# Prediction for longitude with Valifation data
rfPredictionLONGI <- predict(rfmodelLONGI, TEST_PCs_206)

# Checking the accuracy of the model with validation set
postResample(rfPredictionLONGI, WifiTestNoNA529col$LONGITUDE)


# Modeling for Floor : For Floor, Building and Latitude predition, PCA was not applied ####
# Setting Data
WifiTrainNoNAModeling <- WifiTrainNoNAWap
WifiTrainNoNAModeling$FLOOR <- as.factor(WifiTrain$FLOOR)
WifiTestNoNA529colMatch$FLOOR <- as.factor(WifiTestNoNA529col$FLOOR)

# Splitting the data into Training and Testing
set.seed(123)
WifiTrainNoNAModeling_Sample <- WifiTrainNoNAModeling[sample(1:nrow(WifiTrainNoNAModeling),
                                                             50, replace = FALSE),]
inTraining <- createDataPartition(WifiTrainNoNAModeling_Sample$FLOOR, p = .75, list = FALSE)
training <- WifiTrainNoNAModeling_Sample[inTraining,]
testing <- WifiTrainNoNAModeling_Sample[-inTraining,]

# Cross validation within training set
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

# Modeling for floor with RandomForest
rfGrid <- expand.grid(mtry = c(63))
rfFitModelFLOOR <- train(FLOOR~ .,
                         data = training,
                         method = "rf",
                         trControl = fitControl,
                         tuneGrid = rfGrid
)

# Prediction for Floor ####
# Predicting for floor with test set
rfPredictionmodelFLOOR <- predict(rfFitModelFLOOR, testing)

# Converting data type into factor for confuion matrix   
rfPredictionmodelFLOOR <- as.factor(rfPredictionmodelFLOOR)
testing$FLOOR <- as.factor(testing$FLOOR)

# Checking confusion matrix 
postResample(rfPredictionmodelFLOOR, testing$FLOOR)

# Prediction for floor with validation
rfPredictionFLOOR <- predict(rfFitModelFLOOR, WifiTestNoNA529col)

# Converting data type into factor for confusion matrix
rfPredictionFLOOR <- as.factor(rfPredictionFLOOR)
WifiTestNoNA529col$FLOOR <- as.factor(WifiTestNoNA529col$FLOOR)
postResample(rfPredictionFLOOR, WifiTestNoNA529col$FLOOR) # checking the accuracy of the prediction

confusionMatrix(rfPredictionFLOOR, WifiTestNoNA529col$FLOOR)


# Modeling for Building ####
WifiTrainNoNAModeling$FLOOR <- NULL
WifiTestNoNA529colMatch$FLOOR <- NULL
WifiTrainNoNAModeling$BUILDINGID <- as.factor(WifiTrain$BUILDINGID)
WifiTestNoNA529colMatch$BUILDINGID <- as.factor(WifiTestNoNA529col$BUILDINGID)

# Splitting the data into train and test set
set.seed(123)
WifiTrainNoNAModeling_Sample <- WifiTrainNoNAModeling[sample(1:nrow(WifiTrainNoNAModeling), 50, replace = FALSE),]
inTraining <- createDataPartition(WifiTrainNoNAModeling_Sample$BUILDINGID, p = .75, list = FALSE)
training <- WifiTrainNoNAModeling_Sample[inTraining,]
testing <- WifiTrainNoNAModeling_Sample[-inTraining,]

# Cross validation within training set
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

# RandomForest for BUILDING
rfGrid <- expand.grid(mtry=c(63))
rfFitModelBUILD <- train(BUILDINGID~ ., data = training, method = "rf", trControl = fitControl, tuneGrid = rfGrid)

# Prediction for Building ####
# Prediction for Building with test set
rfPredictionmodelBUILD <- predict(rfFitModelBUILD, testing)

# Converting data type into factor for confusion matrix
rfPredictionmodelBUILD <- as.factor(rfPredictionmodelBUILD)
testing$BUILDINGID <- as.factor(testing$BUILDINGID)

# checking the accuracy of the model
postResample(pred = rfPredictionmodelBUILD, obs = testing$BUILDINGID)

# Prediction for building with validation set
rfPredictionBUILD <- predict(rfFitModelBUILD, WifiTestNoNA529col)

# Converting data type for confusion matrix 
rfPredictionBUILD <- as.factor(rfPredictionBUILD)
WifiTestNoNA529col$BUILDINGID <- as.factor(WifiTestNoNA529col$BUILDINGID)

# Confusion matrix
postResample(rfPredictionBUILD, WifiTestNoNA529col$BUILDINGID)
confusionMatrix(rfPredictionBUILD, WifiTestNoNA529col$BUILDINGID)


# Modeling for Latitude ####
# Setting the data
WifiTrainNoNAModeling$BUILDINGID <- NULL
WifiTestNoNA529colMatch$BUILDINGID <- NULL 
WifiTestNoNA529colMatch$LATITUDE <-  WifiTestNoNA529col$LATITUDE
WifiTrainNoNAModeling$LATITUDE <-  WifiTrain$LATITUDE


# Splitting the data into training and testing
set.seed(123)
WifiTrainNoNAModeling_Sample <- WifiTrainNoNAModeling[sample(1:nrow(WifiTrainNoNAModeling),
                                                             50, replace = FALSE),]
inTraining <- createDataPartition(WifiTrainNoNAModeling_Sample$LATITUDE, p = .75, list = FALSE)
training <- WifiTrainNoNAModeling_Sample[inTraining,]
testing <- WifiTrainNoNAModeling_Sample[-inTraining,]

# Cross validation within training set
fitControl <- trainControl(method = "repeatedcv", number = 5, repeats = 1)

# Modeling for LATITUDE with random forest
rfGrid <- expand.grid(mtry=c(63))
rfmodelLat <- train(LATITUDE~ .,
                    data = training,
                    method = "rf",
                    tuneGrid = rfGrid,
                    trControl = fitControl)

# Prediction for Latitude ####
# Prediction for Latitude with testing set
rfPredictionModelLat <- predict(rfmodelLat, testing)

# Checking the accuracy with testing set
postResample(rfPredictionModelLat, testing$LATITUDE)

# Prediction for Latitude with validation set
rfPredictionLat <- predict(rfmodelLat, WifiTestNoNA529col)

# Checking the accuracy with validation set
postResample(rfPredictionLat, WifiTestNoNA529col$LATITUDE)

# Creating data frame with prediction
PredictionFBLL <- data.frame(rfPredictionFLOOR, rfPredictionBUILD, rfPredictionLONGI, rfPredictionLat) 
colnames(PredictionFBLL) <- c("FLOOR", "BUIDINGID", "LONGITUDE", "LATITUDE")

# Exporting the prediction
write.csv(PredictionFBLL, file = "PredictionFBLL.csv", row.names = TRUE)


# Visualidation ####
# Setting the data set
TRAINANDTEST <- rbind(WifiTrainNoNA529col,WifiTestNoNA529colMatch)

# 3D Map of the campus
plot_ly(x = TRAINANDTEST$LONGITUDE, y=TRAINANDTEST$LATITUDE, z=TRAINANDTEST$FLOOR,
        type="scatter3d", mode="markers", color = TRAINANDTEST$FLOOR) %>% 
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'Longitude'),
                            yaxis = list(title = 'Latitude'),
                            zaxis = list(title = 'Floor')))


# Comparison between validation and prediction ####
# Setting the data set
rawdataValidationDataFBLL <- read.csv("ValidationFBLL.csv", TRUE, sep =",")
ValidationDataFBLL <- rawdataValidationDataFBLL
rawdataPredictionFBLL <- read.csv("PredictionFBLL.csv", TRUE, sep =",")
PredictionDataFBLL <- rawdataPredictionFBLL
ValiAndPrediction <- rbind(ValidationDataFBLL, PredictionDataFBLL)
ValiAndPrediction$DATASET <- as.factor(ValiAndPrediction$DATASET)

# 3D Map of validation set and prediction
plot_ly(x = ValiAndPrediction$LONGITUDE, y=ValiAndPrediction$LATITUDE, z=ValiAndPrediction$FLOOR,
        type="scatter3d", mode="markers", color = ValiAndPrediction$DATASET) %>% 
        add_markers() %>%
        layout(scene = list(xaxis = list(title = 'Longitude'),
                            yaxis = list(title = 'Latitude'),
                            zaxis = list(title = 'Floor')))

# Mean Error Dinstance ####
errorDistance <- sqrt((PredictionDataFBLL$LATITUDE - ValidationDataFBLL$LATITUDE)^2 +
                      (PredictionDataFBLL$LONGITUDE - ValidationDataFBLL$LONGITUDE)^2)
mean(errorDistance)

library(learningr)
Hypotenuse <- hypotenuse(PredictionDataFBLL$LATITUDE - ValidationDataFBLL$LATITUDE,PredictionDataFBLL$LONGITUDE - ValidationDataFBLL$LONGITUDE)
mean(Hypotenuse)

# confusion Matrix

