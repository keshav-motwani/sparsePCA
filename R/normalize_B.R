normalize_B = function(B) {

  norm = sqrt(crossprod(rep(1, nrow(B)), B^2))

  B %*% diag(c(1/ifelse(norm == 0, 1, norm)))

}
