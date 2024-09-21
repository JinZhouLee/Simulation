#' Linear Congruential Generator
#'
#' Can be used to generate a uniform distribution, using the recursive method
#' \deqn{Z_t = (a Z_{t-1} + c) \ (\text{mod } m)}
#' and return
#' \deqn{U_t = Z_i / m \sim \text{Unif} (0, 1)}
#'
#' @param n positive integer: sample size
#' @param seed positive integer: generation seed
#' @param a positive integer: multiplier
#' @param c positive integer: increment
#' @param m positive integer: modulus
#'
#' @return a size n vector
#'
#' @keywords internal
#'
#' @examples
#' # the generation parameters in operator system "ZX81"
#' # lc_gen(10, a = 75, c = 74, m = 2^16 + 1)


lc_gen = function(n, seed = NULL, a, c, m){
  # n: size
  # seed: seed number
  # a: multiplier
  # c: increment
  # m: modulus

  if(is.null(seed)) seed = get_time()

  z = rep(0, n + 1)
  z[1] = seed

  for(i in 1:n){
    z[i + 1] = (a * z[i] + c) %% m
  }

  u = z[1:n + 1] / m

  return(u)
}
