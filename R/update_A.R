update_A = function(XtX, B) {

  svd_A = svd(XtX %*% B)
  A = tcrossprod(svd_A$u, svd_A$v)

  return(A)

}
