update_u = function(X, v) {

  Xv = X %*% v
  norm = sqrt(sum(Xv ^ 2))

  u = Xv / norm

  return(u)

}
