#' Chi-squared goodness-of-fit test for uniform distribution (0, 1)
#'
#' Let \eqn{n} be the size of data \eqn{x},
#' the chi-squared goodness-of-fit test will partition \eqn{[0, 1]} interval into k equal length subinterval,
#' and calculate the number of points in the \eqn{j}-th interval \eqn{f_j}.
#' Then the test statistic is
#' \deqn{\chi^2 = \frac{k}{n} \sum_{j = 1}^{k} \left( f_j - \frac{n}{k} \right)^2}
#' We will reject the hypothesis (\eqn{x \sim_{\text{i.i.d.}} \text{Unif} (0, 1)}) at level \eqn{\alpha} if
#' \deqn{\chi^2 > \chi_{k - 1}^2 (1 - \alpha)}
#' where \eqn{\chi_{k - 1}^2 (1 - \alpha)} is the \eqn{1 - \alpha} quantile of chi-squared distribution with \eqn{k - 1} degree of freedom
#'
#' @param x numerical vector: the data
#' @param k positive integer: partition number, recommend n / k > 5, where n is the length of x
#' @param alpha numerical value in (0, 1): hypothesis level
#'
#' @return a list, level is the hypothesis level, test_statistic is the Chi-Squared test statistic, chi_squared is the test quantity, accept and reject is the result of the test.
#'
#' @export
#'
#' @examples
#' x = unif_gen(4000)
#' chi_squared_test_unif(x)

chi_squared_test_unif = function(x, k = 100, alpha = 0.95){
  # the test statistic
  test_statistic = chi_squared_test_unif_statistic(x, k)
  chi_squared = qchisq(alpha, df = k - 1)

  accept = test_statistic < chi_squared
  reject = !accept

  result = list(level = alpha, test_statistic = test_statistic, chi_squared = chi_squared,
                accept = accept, reject = reject)

  return(result)
}
