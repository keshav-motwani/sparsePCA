compute_single_loading = function(X, c, k, V_init, U_result, orth, tolerance, max_iter) {

  v_old = V_init[, k, drop = TRUE]

  diff = tolerance * 10
  iter = 1

  while((iter < max_iter) & (diff > tolerance)) {

    if (orth & (k > 1)) {
      u_new = update_orthogonal_u(X, v_old, U_result[, 1:(k - 1), drop = FALSE])
    } else {
      u_new = update_u(X, v_old)
    }
    v_new = update_v(X, u_new, c)
    d = crossprod(u_new, X) %*% v_new

    diff = max(abs(v_new - v_old))
    v_old = v_new
    iter = iter + 1

  }

  return(list(u = u_new, d = d, v = v_new))

}
