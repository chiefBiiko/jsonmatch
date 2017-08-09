#' Fixes a trailing colon in array selectors
#'
#' @param json JSON string.
#' @param selector Object selector/path containing a trailing colon.
#' @return Chr vector.
#'
#' @keywords internal
handleTrailingColon <- function(json, selector) {
  stopifnot(isTruthyChrVec(json), isTruthyChrVec(selector))
  # regex 2 match the trailing colon part of the selector
  rex.colon.key <- '\\[\\d+\\:\\]'
  # extract the trailing colon part of the selector
  colon.key <- regmatches(selector, regexpr(rex.colon.key, selector, perl=TRUE))
  # get prefix of colon indexer
  pre <- if ((pos <- regexpr(colon.key, selector, fixed=TRUE)[1L]) > 1L) {
    substr(selector, 1L, pos - 1L)
  } else {
    ''
  }
  # array atoms to get length from
  arr.atoms <- stripArray(if (pre != '') jsonmatch(json, pre) else json)
  # get the array length - with base zero
  arr.len <- length(splitOnUnclosedChar(arr.atoms, ',')) - 1L
  # construct a complete array indexer
  xolon.key <- paste0(substr(colon.key, 1L, nchar(colon.key) - 1L),
                      as.character(arr.len),
                      ']')
  # swap original key 4 xolon.key
  glued <- sub(colon.key, xolon.key, selector, fixed=TRUE)
  # check 4 remainders
  clued <- sapply(glued, function(gk) {
    if (grepl('\\[\\d+\\:\\]', gk, perl=TRUE)) {
      handleTrailingColon(json, gk)
    } else {
      gk
    }
  }, USE.NAMES=FALSE)
  # serve
  clued
}
