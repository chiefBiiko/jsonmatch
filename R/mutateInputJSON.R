#' Mutates input JSON for safe processing
#'
#' @param json Input JSON.
#' @return JSON string.
#'
#' @keywords internal
mutateInputJSON <- function(json) {
  stopifnot(isTruthyChrVec(json))
  # allow file references
  if (file.exists(json)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '',
                 paste0(readLines(json, warn=FALSE), collapse=''),
                 perl=TRUE)
  } else if (grepl('\\s(?=(?:(?:[^"]*"){2})*[^"]*$)', json, perl=TRUE)) {
    json <- gsub('\\s+(?=(?:(?:[^"]*"){2})*[^"]*$)', '', json, perl=TRUE)
  }
  # boxing
  if (boxjson::hasUnboxedAtom(json)) json <- boxjson::boxAtoms(json)
  return(json)  # serve
}
