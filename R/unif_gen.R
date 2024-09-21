#' Uniform distribution generation
#'
#' Can be used to generate a uniform distribution, using the recursive method
#' \deqn{Z_t = (a Z_{t-1} + c) \ (\text{mod } m)}
#' and return
#' \deqn{U_t = Z_i / m \sim \text{Unif} (0, 1)}
#'
#' Linear congruential generator ("LC" method) must determined a, c, m;
#'
#' Learmonth-Lewis Generator ("LL" method) is a special case of linear congruential generator, that
#' \deqn{a = 7^5 = 16807}
#' \deqn{c = 0}
#' \deqn{m = 2^{31} - 1 = 2147483647}
#'
#' @param n positive integer: sample size
#' @param seed positive integer: the generation seed
#' @param min finite value: the lower limit of uniform distribution
#' @param max finite value: the upper limit of uniform distribution
#' @param method generation method: can be "LL" Learmonth-Lewis Generator (default) or "LC" Linear Congruential Generator
#' @param para parameter of LC method
#'
#' @return a size n vector
#'
#' @export
#'
#' @examples
#' unif_gen(10)


unif_gen = function(n, min = 0, max = 1, seed = NULL,
                    method = c("LL", "LC"),
                    para = list(a = 7^5, c = 0, m = 2^31 - 1)){

  # select method
  gen_method = method[1]

  if(gen_method == "LC"){
    u = lc_gen(n, seed, a = para[["a"]], c = para[["c"]], m = para[["m"]])
  }
  else if(gen_method == "LL"){
    u = ll_gen(n, seed)
  }else{
    print(paste("No method name:", gen_method))
    print("Try LL (Learmonth-Lewis Generator) or LC (Linear Congruential Generator)")
    return()
  }

  u = u * (max - min) + min

  return(u)
}
