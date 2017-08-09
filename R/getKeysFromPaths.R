#' Takes a split vector of dirty object keys and array indices and returns
#'  a 2d list providing references as required by \code{extractValueFrom*}.
#'
#' @param paths Character vector of jsonmatch-type key references.
#' @return 2d list providing symbols as required by \code{extractValueFrom*}.
#'
#' @keywords internal
getKeysFromPaths <- function(paths) {
  stopifnot(is.character(paths))
  # split paths to path components
  comps <- strsplit(paths,
                    paste0('(?<=\\])(?=\\[)|(?<=[[:print:]])(?=\\[)|',
                           '(?<=\\])(?=\\.)|(?<=[[:print:]])(?=\\.)'),
                    perl=TRUE)
  # strip dots and \\s
  nodots <- lapply(comps, function(comp) {
    comp <- gsub('.', '', comp, fixed=TRUE)
    comp[comp != '']
  })
  # make a 2D list of chr obj keys and numeric indices
  subseq <- lapply(nodots, function(nd) {
    lapply(nd, function(d) {
      if (grepl('^\\[\\d+(?:,\\d+)*(?:\\:(?!\\:)(?:\\d+)?)*\\]$',
                d,
                perl=TRUE)) {
        # parse in integers
        eval(parse(text=sub('^\\[(\\d+(?:,\\d+)*(?:\\:(?!\\:)(?:\\d+)?)*)\\]$',
                            'c(\\1)',
                            d,
                            perl=TRUE)))
      } else {
        d
      }
    })
  })
  # serve 4 extractValueFrom*
  return(subseq)
}
