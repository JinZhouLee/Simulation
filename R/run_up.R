#' Calculate the number of maximal length of increasing run up
#'
#' @param x numerical vector: the data
#'
#' @return a table of each increasing length
#'
#' @keywords internal

run_up = function(x){
  # first difference, check the sequence is increasing or not
  tend = (diff(x) >= 0)

  # second difference, check the segment is change or not;
  # add a F in the front to keep the length consistent
  segment_changes = diff(c(FALSE, tend))

  # find the start and end points of increasing paragraphs
  # starting point is 1, end point is -1
  starts = which(segment_changes == 1)
  ends = which(segment_changes == -1)

  # process the last sequence if it is increasing and ending
  if (length(starts) > length(ends)) {
    ends = c(ends, length(tend) + 1)
  }

  # calculate the length of each incremental paragraph
  runs_lengths = ends - starts + 1

  # put back the runs up length is 1
  runs = table(runs_lengths)
  runs["1"] = length(x) - sum(runs_lengths)
  runs = runs[order(names(runs))]

  return(runs)
}
