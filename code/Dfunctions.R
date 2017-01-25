# this is for the training and test
fold_compare = function(Data, train_index,
                        test_index,
                        method = c("L","R","S")){
  Y_test = as.matrix(Data)[test_index,1]
  # ordinary classifications
  if(method == "L"){
    result = Logistic_wrapper(Data, train_index, test_index)
    scores = rep(0,3)
    scores[1] = hinge_score(Y_test,result$f)
    scores[2] = entropy_score(Y_test,result$f)
    scores[3] = square_score(Y_test,result$f)
    names(scores) = c("hinge_loss","entropy_loss","square_loss")
    return(scores)
  }else if(method == "R"){
    result = RF_wrapper(Data, train_index, test_index)
    scores = rep(0,3)
    scores[1] = hinge_score(Y_test,result$f)
    scores[2] = entropy_score(Y_test,result$f)
    scores[3] = square_score(Y_test,result$f)
    names(scores) = c("hinge_loss","entropy_loss","square_loss")
    return(scores)
  }else if(method == "S"){
    result = SVM_wrapper(Data, train_index, test_index)
    scores = rep(0,3)
    scores[1] = hinge_score(Y_test,result$f)
    scores[2] = entropy_score(Y_test,result$f)
    scores[3] = square_score(Y_test,result$f)
    names(scores) = c("hinge_loss","entropy_loss","square_loss")
    return(scores)
  }

}

CV_compare = function(Data, K = 5,method_1 = c("L","R","S")){
  N = dim(Data)[1]
  P = dim(Data)[2]
  scores = rep(0,3)
  index = sample(N,N)
  K_fold = K
  for(k in 1:K){
    test_index = index[((k-1)*(N/K) + 1) : (k * (N/K))]
    train_index = index[!(index %in% test_index)]
    output = fold_compare(Data, train_index,test_index,method = method_1)
    print(k)
    print(output)
    scores = scores + output
    print(scores)
  }
  scores = scores/K
  names(scores) = c("hinge_loss","entropy_loss","square_loss")
  return(scores)
}


