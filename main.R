library(rsample)      # data splitting 
library(randomForest) # basic implementation
library(caret)
folds = 5

data_set = AmesHousing::make_ames()
interval = nrow(data_set)/folds

diff_pred = as.null()


for (i in 0:(folds-1)) {
  beginIndex = i * interval
  endIndex = (i+1) * interval - 1
  validation = data_set[beginIndex:endIndex,]
  left = data_set[0:beginIndex,]
  right = data_set[(endIndex+1):(folds*interval),]
  train_set = rbind(left,right)
  model <- randomForest(
    formula = Sale_Price ~ .,
    ntree = 300,
    data    = train_set
  )
  pred_randomForest <- predict(model, validation)
  if (is.null(diff_pred)) {
    diff_pred = (validation['Sale_Price'] - pred_randomForest)^2
  } else {
    diff_pred = rbind(diff_pred,(validation['Sale_Price'] - pred_randomForest)^2)
  }
}
summary(diff_pred)

