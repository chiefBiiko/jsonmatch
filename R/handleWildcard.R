#' Returns a chr vector of wildcard matched object keys
#'
#' @param json JSON string.
#' @param selector Object selector/path containing the wildcard character.
#' @return Chr vector.
#'
#' @keywords internal
handleWildcard <- function(json, selector) {
  stopifnot(isTruthyChrVec(json), isTruthyChrVec(selector))
  # regex 2 match the wildcard part of the selector
  rex.wdcd.key <- paste0('\\.[[:alnum:]]+\\*[[:alnum:]]+|',  # <- this case 1st
                         '\\.[[:alnum:]]+\\*|',
                         '(?:\\.\\*[[:alnum:]]+\\*?)+|',
                         '^\\.\\*$')
  # extract the wildcard part of the selector
  wdcd.key <- regmatches(selector, regexpr(rex.wdcd.key, selector, perl=TRUE))
  # get prefix of wdcd.key
  pre <- if ((pos <- regexpr(wdcd.key, selector, fixed=TRUE)[1]) > 1L) {
    substr(selector, 1L, pos - 1L)
  } else {
    ''
  }
  # object context where to look for wildcard matches
  obj.ctx <- gsub('(?:^\\{|^\\[)|(?:\\}$|\\]$)', '',
                  if (!pre %in% c('', '.')) jsonmatch(json, pre) else json,
                  perl=TRUE)
  # make regex from wdcd.key expression
  rex.mtch.keys <- paste0('(?:"', sub('\\*', '[[:alnum:]]+',
                                      sub('^\\.', '', wdcd.key, perl=TRUE),
                                      perl=TRUE),
                          '"\\:)(?![^\\{]*\\})(?![^\\[]*\\])')  # keys on top level only
  # get all matching keys
  mtch.keys <- paste0('.',
                      gsub('["\\:]', '',
                           regmatches(obj.ctx,
                                      gregexpr(rex.mtch.keys,
                                               obj.ctx,
                                               perl=TRUE))[[1L]],
                           perl=TRUE))
  # glue things back together
  glued <- sapply(mtch.keys, function(mk) sub(wdcd.key, mk, selector, fixed=TRUE),
                  USE.NAMES=FALSE)
  # check if any wildcards remained - conditionally repeat
  clued <- sapply(glued, function(gk) {
    if (grepl('\\*', gk, perl=TRUE)) {
      handleWildcard(json, gk)
    } else {
      gk
    }
  }, USE.NAMES=FALSE)
  # serve
  return(clued)
}
