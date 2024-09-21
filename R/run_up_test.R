#' The runs up test of testing independent
#'
#' Runs up test is using in testing independent
#'
#' First calculate the runs up
#' \deqn{
#'  r_i
#'  = \begin{cases}
#'    \text{number of runs up of length } i & i = 1, \cdots 5 \\
#'    \text{number of runs up of length } \geq 6 & i = 6
#'  \end{cases}
#' }
#' Then use the following test statistic
#' \deqn{R = \frac{1}{n} \sum_{i = 1}^{6} \sum_{j = 1}^{6} a_{ij} (r_i - n b_i) (r_j - n b_j)}
#' where \eqn{n} is the length of x,
#' \deqn{
#'  a =
#'  \begin{pmatrix}
#'   4529.4 & 9044.9 & 13568 & 18091 &  22615 &  27892  \\
#'   9044.9 & 18097 &  27139 & 36187 &  45234 &  55789  \\
#'   13568 &  27139 &  40721 & 54281 &  67852 &  83685  \\
#'   18091 &  36187 &  54281 & 72414 &  90470 &  111580 \\
#'   22615 &  45234 &  67852 & 90470 &  113262 & 139476 \\
#'   27892 &  55789 &  83685 & 111580 & 139476 & 172860
#'  \end{pmatrix}
#' }
#' and
#' \deqn{
#'  b =
#'  \begin{pmatrix}
#'    \frac{1}{6} & \frac{5}{24} & \frac{11}{120} & \frac{19}{720} & \frac{29}{5040} & \frac{1}{840}
#'  \end{pmatrix}
#' }
#' We will reject the hypothesis of independentce at level alpha (\eqn{\alpha}) if
#' \deqn{
#'  R > \chi_6^2 (1 - \alpha)
#' }
#' where \eqn{\chi_{6}^2 (1 - \alpha)} is the \eqn{1 - \alpha} quantile of chi-squared distribution with 6 degree of freedom
#'
#' See Page 65-68 of "The Art of Computer Programming" Addison-Wesly. by D.E. Kunth, 1981.
#'
#' @param x numerical vector
#' @param alpha numerical value in (0, 1), the hypothesis level
#'
#' @return a list, level is the hypothesis level, test_statistic is the test statistic, chi_squared is the test quantity, accept and reject is the result of the test.
#'
#' @export
#'
#' @examples
#' x = unif_gen(4000)
#' run_up_test(x)

run_up_test = function(x, alpha = 0.95){
  # the runs up number
  runs = run_up(x)
  r = sapply(1:6, function(i){
    sum(runs[which(names(runs) == i)])
  })
  r[6] = sum(runs[which(names(runs) >= 6)])

  # some statistic
  n = length(x)
  a = matrix(data = c(4529.4, 9044.9, 13568, 18091,  22615,  27892,
                      9044.9, 18097,  27139, 36187,  45234,  55789,
                      13568,  27139,  40721, 54281,  67852,  83685,
                      18091,  36187,  54281, 72414,  90470,  111580,
                      22615,  45234,  67852, 90470,  113262, 139476,
                      27892,  55789,  83685, 111580, 139476, 172860),
             nrow = 6)
  b = c(1/6, 5/24, 11/120, 19/720, 29/5040, 1/840)

  # test statistic
  R = sapply(1:6, function(i){
    sapply(1:6, function(j){
      a[i, j] * (r[i] - n * b[i]) * (r[j] - n * b[j])
    })
  })
  R = sum(R) / n
  chi_squared = qchisq(alpha, df = 6)

  # accept or reject
  accept = R < chi_squared
  reject = !accept

  # result
  result = list(level = alpha, runs = runs, test_test = R, chi_squared = chi_squared,
                accept = accept, reject = reject)

  return(result)
}
