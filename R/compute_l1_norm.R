compute_l1_norm = function(a, delta) {

  thresholded = soft_threshold(a, delta)

  normed = thresholded / sqrt(sum(thresholded ^ 2))

  return(sum(abs(normed)))

}