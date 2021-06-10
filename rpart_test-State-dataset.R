library(rpart)       # performing regression trees
library(rpart.plot)  # plotting regression trees
library(caret)
source("init-random-division.R") #=============== random division method vs. classic division method

apply_custom_method = TRUE
meth = as.null()

if (apply_custom_method == TRUE) {
  meth = list(eval = etemp, split = stemp, init = itemp)
} else {
  meth = "anova"
}


folds = 5
ntrees = 300
single_tree_data_factor = 0.66

#data_set = AmesHousing::make_ames()
data_set <- data.frame(state.x77, region=factor(state.region))

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
      formula = Population ~ .,  #================= dataset parameter change here
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
    diff_pred = (validation['Population'] - forest_pred)^2#================= dataset parameter change here 
  } else {
    diff_pred = rbind(diff_pred,(validation['Population'] - forest_pred)^2)#================= dataset parameter change here 
  }
  print(j)
}
summary(diff_pred)


