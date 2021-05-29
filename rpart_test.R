library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(caret)
source("init.R")

apply_custom_method = FALSE
meth = as.null()

if (apply_custom_method == TRUE) {
  meth = list(eval = etemp, split = stemp, init = itemp)
} else {
  meth = "anova"
}


folds = 5
ntrees = 300
single_tree_data_factor = 0.66

data_set = AmesHousing::make_ames()
interval = nrow(data_set)/folds
single_tree_data_size = as.integer(single_tree_data_factor * nrow(data_set))
diff_pred = as.null()

for (i in 0:(folds-1)) {
  forest_pred = as.null()
  beginIndex = i * interval
  endIndex = (i+1) * interval - 1
  validation = data_set[beginIndex:endIndex,]
  left = data_set[0:beginIndex,]
  right = data_set[(endIndex+1):(folds*interval),]
  train_set = rbind(left,right)
  
  for (j in 0:(ntrees-1)) {
    single_tree_train = train_set[sample(nrow(train_set),single_tree_data_size),]
    model <- rpart(
      formula = Sale_Price ~ .,
      data    = single_tree_train,
      method  = meth
    )
    tree_pred = predict(model, validation)
    if (is.null(forest_pred)) {
      forest_pred = tree_pred
    } else {
      forest_pred = forest_pred + tree_pred
    }
  }
  forest_pred = forest_pred/ntrees;
  
  if (is.null(diff_pred)) {
    diff_pred = (validation['Sale_Price'] - forest_pred)^2
  } else {
    diff_pred = rbind(diff_pred,(validation['Sale_Price'] - forest_pred)^2)
  }
  print(j)
}
summary(diff_pred)


