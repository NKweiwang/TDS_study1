# this file is for the feature learning (dimension reduction) method
PCA_wrapper = function(X){
  SVD_X = svd(X)
  V = SVD_X$v[,1:20]
  U = X %*% V
  return(list(X = U))
}

PMA_wrapper = function(X){
  library(PMA)
  out = PMA::PMD(X,K = 20)
  V = out$v[,1:10]
  U = X %*% V
  return(list(X = U))
}

TopicM_wrapper = function(X){
  library(maptpx)
  out = topics(X, K = 20)
  V = out$theta
  U = X %*% V
  return(list(X = U))
}
