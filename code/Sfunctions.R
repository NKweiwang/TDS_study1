# this is for the score function
hinge_score = function(Y,f){
  f = 2 * (1*(f > 1/2)) -1
  S = (2*Y - 1) * f
  S = S * (1 * (S > 0))
  s = mean(S)
  return(s)
}

entropy_score = function(Y,f){
  S = Y * log(f) + (1-Y)* log(1-f)
  S = -S
  s = mean(S)
  return(s)
}

square_score = function(Y,f){
  s = mean( (Y - f)^2)
  return(s)
}
