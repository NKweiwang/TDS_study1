# this is for classification methods for the data
# logistic regression
Logistic_wrapper = function(Data, train_index, test_index){
  train_data = Data[train_index,]
  test_data = Data[test_index,]
  Y = as.matrix(train_data)[,1]
  X = as.matrix(train_data)[,-1]
  new_X = as.matrix(test_data)[,-1]
  new_Y = as.matrix(test_data)[,1]
  cvfit = glmnet::cv.glmnet(X, Y, family = "binomial", type.measure = "class")
  pred = predict(cvfit, newx = new_X, s = "lambda.min", type = "response")
  Y_hat = predict(cvfit, newx = new_X, s = "lambda.min", type = "class")
  return(list(Yhat = Y_hat, f = pred))
}

RF_wrapper = function(Data, train_index, test_index){
  train_data = Data[train_index,]
  test_data = Data[test_index,]
  Y = as.matrix(train_data)[,1]
  Y = as.character(Y)
  X = as.matrix(train_data)[,-1]
  new_train = data.frame(response = Y, X = X)
  new_X = as.matrix(test_data)[,-1]
  colnames(new_X) = colnames(new_train)[-1]
  rm = randomForest::randomForest(response~.,new_train,importance=TRUE,proximity=TRUE)
  pred = predict(rm, new_X,type = "prob")
  pred = pred[,2]
  Y_hat = predict(rm, new_X,type = "class")
  Y_hat = as.character(Y_hat)
  return(list(Yhat = Y_hat, f = pred))
}

SVM_wrapper = function(Data, train_index, test_index){
  train_data = Data[train_index,]
  test_data = Data[test_index,]
  Y = as.matrix(train_data)[,1]
  Y = as.character(Y)
  X = as.matrix(train_data)[,-1]
  new_train = data.frame(response = Y, X = X)
  new_X = as.matrix(test_data)[,-1]
  colnames(new_X) = colnames(new_train)[-1]
  out = e1071::svm(response~.,new_train, probability = TRUE)
  pred = predict(out, new_X,  probability = TRUE)
  Y_hat = as.character(pred)
  p = attr(pred,"probabilities")[,2]
  return(list(Yhat = Y_hat, f = p))
}
