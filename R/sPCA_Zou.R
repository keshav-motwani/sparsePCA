#' @export
sPCA_Zou = function(X, lambda, lambda_1, gram, tolerance = 0.001, max_iter = 200) {

  r = length(lambda_1)

  alpha = lambda_1 / (lambda_1 + 2 * lambda)
  lambda_g = (lambda_1 + 2 * lambda)/(2 * nrow(X))

  A_old = svd(X)$v[, 1:r, drop = FALSE]
  B_old = update_B(X, A_old, alpha, lambda_g)

  XtX = crossprod(X)

  iter = 0
  diff = tolerance * 10

  while((iter < max_iter) & (diff > tolerance)) {

    A_new = update_A(XtX, B_old)
    B_new = update_B(X, A_new, alpha, lambda_g)

    diff = check_convergence(B_new, B_old)
    A_old = A_new
    B_old = B_new
    iter = iter + 1

  }

  return(normalize_B(B_new))

}
