#' Chi-squared goodness-of-fit test for uniform distribution (0, 1)
#'
#' @importFrom stats qchisq
#'
#' @param x numerical vector: the data
#' @param k positive integer: partition number, recommend n / k > 5, where n is the length of x
#'
#' @return  test statistic
#'
#' @keywords internal
#'
#' @examples
#' # x = unif_gen(10000)
#' # chi_squared_test_unif_statistic(x)

chi_squared_test_unif_statistic = function(x, k = 100){
  # the number of each interval
  U = cut(x, breaks = seq(0, 1, by = 1 / k), include.lowest = TRUE)
  f = table(U)

  # the test quantity
  expect = length(x) / k
  test_statistic = sum((f - expect)^2) / expect

  return(test_statistic)
}

