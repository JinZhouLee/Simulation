#' Get the current time (in seconds)
#'
#' @keywords internal
#'
#' @return current time (in seconds)

get_time = function(){
  as.numeric(Sys.time())
}
