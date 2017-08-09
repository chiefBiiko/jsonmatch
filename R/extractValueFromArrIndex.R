#' Extracts the item at given index of an JSON array
#'
#' @param arr JSON array.
#' @param index Array index/key for which to retrieve value, zero-indexed.
#' @return Character vector.
#'
#' @keywords internal
extractValueFromArrIndex <- function(arr, index) {  # zero-indexed !!!
  stopifnot(isTruthyChrVec(arr), is.numeric(index), index %% 1L == 0L)
  # split arr contents on comma not enclosed in [], {} or ""
  cospl <- splitOnUnclosedChar(stripArray(arr), ',')
  # get atoms out of array
  arr.sub <- cospl[index + 1L]
  # error out
  if (anyNA(arr.sub)) stop('index out of bounds')
  # collapse
  rtn <- paste0(arr.sub, collapse=',')
  # serve
  return(rtn)
}
