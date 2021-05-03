#' @export
sPCA_Witten = function(X, r, c, orth, tolerance = 1e-7, max_iter = 20) {

  V_init = svd(X)$v[, 1:r, drop = FALSE]

  U_result = matrix(nrow = nrow(X), ncol = r)
  V_result = matrix(nrow = ncol(X), ncol = r)
  d_result = numeric(r)

  for (k in 1:r) {

    if (!orth & k > 1) {
      X = X - result$u %*% result$d %*% t(result$v)
    }

    result = compute_single_loading(X, c, k, V_init, U_result, orth, tolerance, max_iter)

    U_result[, k] = result$u
    V_result[, k] = result$v
    d_result[k] = result$d

  }

  return(V_result)

}
