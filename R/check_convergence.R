check_convergence = function(B_1, B_2) {

  max(abs(normalize_B(B_1) - normalize_B(B_2)))

}