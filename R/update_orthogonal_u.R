update_orthogonal_u = function(X, v, U_prev) {

  Xv = X %*% v

  u_star = Xv - tcrossprod(U_prev) %*% Xv

  u = u_star / sqrt(sum(u_star ^ 2))

  return(u)

}
