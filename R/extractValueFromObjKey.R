#' Extracts the item at given key of an JSON object
#'
#' @param obj JSON object.
#' @param key Key for which to retrieve value.
#' @return Character vector.
#'
#' @keywords internal
extractValueFromObjKey <- function(obj, key) {
  stopifnot(isTruthyChrVec(obj), isTruthyChrVec(key),
            grepl(paste0('"', key,'"\\:'), obj, perl=TRUE))
  chars <- strsplit(obj, '', fixed=TRUE)[[1L]]
  beg <- pos <- regexpr(paste0('"', key,'":'), obj)[1L] + nchar(key) + 3L
  opbr <- 0L
  repeat {
    if (chars[pos] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    if (opbr == 0L) {
      return(gsub('^"|"$', '', substr(obj, beg, pos), perl=TRUE))
    }
    pos <- pos + 1L
  }
  stop('invalid JSON')
}
