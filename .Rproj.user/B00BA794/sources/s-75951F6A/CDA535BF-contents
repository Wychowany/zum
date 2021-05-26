library(rsample)      # data splitting 
library(randomForest) # basic implementation

set.seed(123)
ames_split <- initial_split(AmesHousing::make_ames(), prop = .7)
ames_train <- training(ames_split)
ames_test  <- testing(ames_split)

m1 <- randomForest(
  formula = Sale_Price ~ .,
  data    = ames_train
)

pred_randomForest <- predict(m1, ames_test)

error = sqrt(sum((ames_test['Sale_Price'] - pred_randomForest)^2))
