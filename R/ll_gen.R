#' Learmonth-Lewis Generator
#'
#' Can be used to generate uniform distribution. Learmonth-Lewis generator is a special case of linear congruential generator, that
#' \deqn{a = 7^5 = 16807}
#' \deqn{c = 0}
#' \deqn{m = 2^{31} - 1 = 2147483647}
#'
#' @param n positive integer: the sample size
#' @param seed positive integer: the generation seed
#'
#' @return a size n vector
#'
#' @keywords internal
#'
#' @examples
#' # ll_gen(10)


ll_gen = function(n, seed = NULL){
  if(is.null(seed)) seed = get_time()

  z = rep(0, n + 1)
  z[1] = seed

  for(i in 1:n){
    z[i + 1] = (16807 * z[i]) %% 2147483647
  }

  u = z[1:n + 1] / 2147483647

  return(u)
}
