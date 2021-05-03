update_v = function(X, u, c) {

  Xtu = crossprod(X, u)

  delta = binary_search(Xtu, c)

  v = soft_threshold(Xtu, delta)

  v = v / sqrt(sum(v ^ 2))

  return(v)

}
