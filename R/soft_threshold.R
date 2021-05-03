soft_threshold = function(a, delta) {

  pmax(0, abs(a) - delta) * sign(a)

}