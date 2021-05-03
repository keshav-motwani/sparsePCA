binary_search = function(a, c) {

  max_iter = 100
  tolerance = 1e-6

  left = 0
  right = abs(max(a))

  if (compute_l1_norm(a, 0) < c) {

    delta = 0

  } else {

    iter = 1

    while((iter < max_iter) & (right - left) > tolerance) {

      mid = (left + right) / 2
      l1_norm = compute_l1_norm(a, mid)

      if (l1_norm > c) {
        left = mid
      } else {
        right = mid
      }

    }

    delta = (left + right) / 2

  }

  return(delta)

}
