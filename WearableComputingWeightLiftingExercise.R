# Module 8

# Machine learning
# Step 1: Train
# Step 2: Predict
# Step 3: ConfusionMatrix

# Linear Regression
# Step 1: run lm 
# Step 2: simplified variable
# Step 3: pick up which model fit better using anova
# Step 4: use lm for "prediction" or "condifident"


# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv
# https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv

downloadData <- function() {
    # if training file is not exist, then download from web
    if (!file.exists("pml-training.csv")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv"
        download.file(fileURL, destfile=("pml-training.csv"))
        list.files(".")
    }
    
    # if training file is not exist, then download from web
    if (!file.exists("pml-testing.csv")) {
        fileURL <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv"
        download.file(fileURL, destfile=("pml-testing.csv"))
        list.files(".")
    }    
}

# Step 1: Download and Loading Data
downloadData()
training_Raw <- read.csv("pml-training.csv")
testing_Raw <- read.csv("pml-testing.csv")

# Step 2: Basic sumarry of Raw Data
dim(training_Raw)
str(training_Raw)
summary(training_Raw)
table(training_Raw$classe, training_Raw$classe)

# Step 3: Cleaning data
# In this step, we will clean the data and get rid of observations with missing values 
# as well as some meaningless variables.
sum(complete.cases(training_Raw))
sum(complete.cases(testing_Raw))

# Step 4: Feature extraction of data
# In this step, we will extract column that has value for our feature selection

training_FeatureSelected <- training_Raw[, colSums(is.na(training_Raw)) == 0]
# training_FeatureRemoved <- training_Raw[, colSums(is.na(training_Raw)) > 0]

testing_FeatureSelected <- testing_Raw[, colSums(is.na(testing_Raw)) == 0]
# testing_FeatureRemoved <- testing_Raw[, colSums(is.na(testing_Raw)) > 0]

# remove X|timestamp|window non relevant features
features <- names(testing_FeatureSelected[,colSums(is.na(testing_FeatureSelected))==0])[8:59]
training_FeatureSelected <- training_Raw[, c(features, "classe")]
testing_FeatureSelected <- testing_Raw[,c(features, "problem_id")]

# free up more memory
rm(training_Raw)
rm(testing_Raw)
rm(features)
rm(downloadData)

# Step 5: Basic sumarry of Clean up Data 
dim(training_FeatureSelected)
str(training_FeatureSelected)
names(training_FeatureSelected) # feature extracted
summary(training_FeatureSelected)
table(training_FeatureSelected$classe, training_FeatureSelected$classe)

# Step 6: Preparing data for training, testing and cross validation
# We can split the cleaned training set into a pure training data set (70%) and 
# a validation data set (30%). We will use the validation data set to 
# conduct cross validation in future steps.
library(caret)
inTrain <- createDataPartition(y=training_FeatureSelected$classe, p=0.70, list=FALSE)
training <- training_FeatureSelected[inTrain,]
validation <- training_FeatureSelected[-inTrain,]
dim(training)

# free up more memory
rm(training_FeatureSelected)

# Step 7a: Try out linear regression
# Step 7a: Using step to Choose a model by AIC in a Stepwise Algorithm
# Step 7b: Using anova
# but to run lm is too slow, and it does not make sense.
# skip LM and use machine learning
start.time <- Sys.time()
modelfit_LM <- lm(classe ~ ., data = training)
summary(modelfit_LM)
end.time <- Sys.time()
time.taken <- end.time - start.time
message("LM time taken: ", time.taken)

start.time <- Sys.time()
modelfit_LM <- step(modelfit_LM, direction = "both")
summary(modelfit_LM)
end.time <- Sys.time()
time.taken <- end.time - start.time
message("LM AIC time taken: ", time.taken)


# Step 8: Try out different machine learning model
## Random Forest Model
## GBM Model
## lda Model

## set ntree = 30 due to memory usage and compute time
start.time <- Sys.time()
trainCntr <- trainControl("oob")
#modelFit_RF <- train(classe ~ ., data = training, method = "rf", ntree=200, importance=T, trControl=trainCntr)
#modelFit_RF <- train(classe ~ ., data = training, method = "rf", ntree=50, importance=T, trControl=trainCntr)
modelFit_RF <- train(classe ~ ., data = training, method = "rf", ntree=30, importance=T, trControl=trainCntr)
end.time <- Sys.time()
time.taken <- end.time - start.time
message("Random Forest Model time taken: ", time.taken)


start.time <- Sys.time()
modelFit_GBM <- train(classe ~., data = training, method = "gbm")    
end.time <- Sys.time()
time.taken <- end.time - start.time
message("GBM Model time taken: ", time.taken)


start.time <- Sys.time()
modelFit_LDA <- train(classe ~., data = training, method = "lda")  
end.time <- Sys.time()
time.taken <- end.time - start.time
message("lda Model time taken: ", time.taken)

# free up more memory
rm(training)

prediction_RF <- predict(modelFit_RF, validation)
prediction_GBM <- predict(modelFit_GBM, validation)
prediction_LDA <- predict(modelFit_LDA, validation)

confusionMatrix(prediction_RF, validation$classe)
confusionMatrix(prediction_GBM, validation$classe)
confusionMatrix(prediction_LDA, validation$classe)

prediction_RF <- predict(modelFit_RF, testing_FeatureSelected)

message("result for assignment: ", prediction_RF)

# Fine Tuning
# Assess Number of relevant variables
varImpObj <- varImp(modelFit_RF)
# Top 40 plot
plot(varImpObj, main = "Importance of Top 40 Variables", top = 40)


