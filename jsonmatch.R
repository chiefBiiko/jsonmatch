# jsonmatch

#' Simple matching on JSON
#' 
#' @details Parameter \code{pattern} allows matching properties of a JSON 
#' object \code{$prop}, and items of a JSON array. Wildcard matching is 
#' not yet supported.
#'  
#' \code{c(i, i, i) and c(i:i)}.
#' 
#' @export
jsonmatch <- function(json, pattern) {
  stopifnot(isTruthyChar(json), isTruthyChar(pattern))
  y <- jsonlite::fromJSON(json)
  spl <- Filter(function(p) p != '',
                strsplit(pattern, '$', fixed=TRUE)[[1]])
  # get subset indicators
  pn <- lapply(spl, function(p) {
    if (grepl('^c\\(.+\\)$', p, perl=TRUE)) {
      eval(parse(text=p))
    } else {
      p
    }
  })
  # setup return           
  rtn <- lapply(y, function(yy) NULL)
  # push values
  for (i in 1L:length(pn)) {
    if (is.character(pn[[i]])) {
      rtn[[pn[[i]]]] <- y[[pn[[i]]]] 
    } else if (is.numeric(pn[[i]])) {
      rtn[pn[[i]]] <- y[pn[[i]]]
    }
  }
  out <- Filter(Negate(function(r) is.null(unlist(r))), rtn)
  cout <- jsonlite::toJSON(if (is.list(y)) out else unlist(out))
  return(cout)
}