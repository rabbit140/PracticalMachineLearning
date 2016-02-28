#Load the data
training <- read.csv("pml-training.csv")
testing <- read.csv("pml-testing.csv")

#Select complete columns
na.prop <- function(x){sum(is.na(x))/length(x)*100}
cols_prop <- sapply(training, na.prop)
col_names <- names(cols_prop[(cols_prop < 10)])

#Check number of "NA" rows
sum(is.na(training[,col_names]))

training <- training[,col_names]

#Select columns with no empty values (not the same as NA!)
prop.empty <- function(x){sum(x == "")/length(x)*100}
cols_prop <- sapply(training, prop.empty)
col_names <- names(cols_prop[cols_prop < 10])

#Check numer of empty rows
sum((sapply(training[,col_names], prop.empty) > 0))
training <- training[,col_names]

#SAME FOR "TESTING" DATA:

#Select complete columns
na.prop <- function(x){sum(is.na(x))/length(x)*100}
cols_prop <- sapply(testing, na.prop)
col_names <- names(cols_prop[(cols_prop < 10)])

#Check number of "NA" rows
sum(is.na(testing[,col_names]))

testing <- testing[,col_names]

#Select columns with no empty values (not the same as NA!)
prop.empty <- function(x){sum(x == "")/length(x)*100}
cols_prop <- sapply(testing, prop.empty)
col_names <- names(cols_prop[cols_prop < 10])

#Check numer of empty rows
sum((sapply(testing[,col_names], prop.empty) > 0))
testing <- testing[,col_names]

#Remove "X" col
training <- training[,-c(1:5)]
testing <- testing[,-c(1:5)]

#Split data into train and test sets
set.seed(1)
train_rows <- sample(1:nrow(training), size = 2/3*nrow(training))
train_data <- training[train_rows,]
test_data <- training[which(1:nrow(training) %in% train_rows == FALSE),]

#Train the model and predict
library(caret)
qda.fit <- train(classe~., method = "qda", data = train_data)
confusionMatrix(test_data$classe, predict(qda.fit, test_data))