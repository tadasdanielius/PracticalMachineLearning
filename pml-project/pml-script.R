library(caret)
library(corrplot)
set.seed(21312)

url_training <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-training.csv";
url_testing <- "https://d396qusza40orc.cloudfront.net/predmachlearn/pml-testing.csv";

# Downloading data

downloadFile <- function(url,filename) {
    if (!file.exists(filename)) {
        download.file(url,filename) 
    }
    downloaded_data <- pml_train <- read.csv(file = filename, na.strings = c('NA','#DIV/0!',''))
    return(downloaded_data)
}

raw.training <- downloadFile(url_training, "pml-training.csv");
raw.testing <- downloadFile(url_testing, "pml-testing.csv");

# Cleaning Data

columns_to_remove <- grep("(X|min|max|amplitude|avg|var|stddev|total|window|user|raw|timestamp|kurtosis|skewness|problem)",names(raw.training));
training <- subset(raw.training,select=-columns_to_remove);
columns_to_remove <- grep("(X|min|max|amplitude|avg|var|stddev|total|window|user|raw|timestamp|kurtosis|skewness|problem)",names(raw.testing));
testing <- subset(raw.testing,select=-columns_to_remove);

# Preparing samples

inTrain <- createDataPartition(training$classe, p=0.70, list=FALSE)
samples.training <- training[inTrain,]
samples.testing <- training[-inTrain,]

# Analysing variables - correlation

classe_column_num <- dim(training)[[2]];
corMatrix <- cor(training[,-classe_column_num])
diag(corMatrix) <- 0
corrplot(corMatrix,tl.cex=0.3)

# Training

#rfFit <- train(classe~., training, method = "rf", preProcess=c("pca"), trControl = trainControl(method = "cv"))

# Testing predictions

testing.predictions <- predict(rfFit,samples.testing);
cM <- confusionMatrix(samples.testing$classe, testing.predictions);

# Final predictions

answers <- predict(rfFit,testing);

pml_write_files = function(x){
    n = length(x)
    for(i in 1:n){
        filename = paste0("problem_id_",i,".txt")
        write.table(x[i],file=filename,quote=FALSE,row.names=FALSE,col.names=FALSE)
    }
}

pml_write_files(answers);


