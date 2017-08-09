#' Strips an array's outer brackets
#'
#' @param json JSON array.
#' @return Stripped JSON array.
#'
#' @keywords internal
stripArray <- function(json) {
  stopifnot(isTruthyChrVec(json))
  if (isArray(json)) {
    return(gsub('^\\[|\\]$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}

#' Strips an object's outer brackets
#'
#' @param json JSON object.
#' @return Stripped JSON object.
#'
#' @keywords internal
stripObject <- function(json) {
  stopifnot(isTruthyChrVec(json))
  if (isObject(json)) {
    return(gsub('^\\{|\\}$', '', json, perl=TRUE))
  } else {
    return(json)
  }
}
