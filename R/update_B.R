update_B = function(X, A, alpha, lambda_g) {

  B = A

  for (i in 1:ncol(B)) {

    B[, i] = coef(glmnet::glmnet(x = X, y = X %*% A[, i],
                                 alpha = alpha[i], lambda = lambda_g[i],
                                 intercept = FALSE, standardize = FALSE))[-1]

  }

  return(B)

}
