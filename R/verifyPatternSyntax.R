#' Performs a syntax check on pattern
#'
#' @param json JSON string.
#' @param pattern Chr vector of length 1 specifying the subset pattern.
#' @return Logical indicating whether the syntax is correct.
#'
#' @keywords internal
verifyPatternSyntax <- function(json, pattern) {
  stopifnot(isTruthyChrVec(json), isTruthyChrVec(pattern))
  # root structure
  struct <- if (isArray(json)) {  # case arr
    identical(strsplit(pattern, '', fixed=TRUE)[[1L]][1L], '[')
  } else if (isObject(json)) {     # case obj
    identical(strsplit(pattern, '', fixed=TRUE)[[1L]][1L], '.')
  }
  # check 4 invalid characters ...
  valid <- !grepl('[^[[:print:]]]*', pattern, perl=TRUE)  # chr class locked
  # ... when using the magic wildcard
  wild <- !(grepl('\\*', pattern, perl=TRUE) &&
            any(grepl('[^[:alnum:]]',                     # chr class locked
                      gsub('^"|"\\:$', '',
                           regmatches(json,
                                      gregexpr('"[^"]*"\\:',
                                               json,
                                               perl=TRUE))[[1]],
                           perl=TRUE),
                      perl=TRUE)))
  # early exit
  if (!struct) {
    return(structure(FALSE, msg='Incorrect entry reference in pattern'))
  }
  if (!valid) {
    return(structure(FALSE,
                     msg='input JSON contains non-printable characters'))
  }
  if (!wild) {
    return(structure(FALSE,
                     msg=paste('wildcards can only be used if all object',
                               'keys in json contain alphanumeric',
                               'characters [a-zA-Z0-9] only')))
  }
  # setup syntax check                                    # master regex
  rex <- paste0('^(?:\\.\\*?[[:print:]]*\\*?[[:print:]]*)+|',
                '^(?:\\[\\d+(?:,\\d+)*(?:\\:(?!\\d*\\:)(?:\\d+)?)*\\])')
  comps <- strsplit(pattern, ',', fixed=TRUE)[[1]]        # pattern components
  for (comp in comps) {                                   # do em all
    red <- comp                                           # reduction base
    while (nchar(red) > 0L) {                           # do predicate reduction
      if (!grepl(rex, red, perl=TRUE)) {                # check head
        return(structure(FALSE, msg='syntax error'))    # spotted an error
      }
      red <- sub(rex, '', red, perl=TRUE)               # cut head
    }
  }
  # exit
  return(TRUE)
}
