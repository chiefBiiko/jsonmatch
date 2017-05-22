# jsonmatch

#' Simple matching on JSON
#' 
#' @details Parameter \code{pattern} allows matching keys of a JSON 
#' object \code{.key}, and keys of a JSON array \code{[0,3] or [0:5]}.
#' 
#' 
#' @export
jsonmatch <- function(json, pattern) {
  stopifnot(isTruthyChar(json), isTruthyChar(pattern))
  spl <- Filter(function(p) p != '',
                strsplit(pattern, ',', fixed=TRUE)[[1]])
  # 4 case one key in pattern
  key <- gsub('[^[:alpha:]]', '', spl, perl=TRUE)
  
  
  
  
  print(spl)
  print(key)
  
  return()
}

#' Extracts the item at given index of an JSON array
#'
#' @internal
extractValueFromIndex <- function(json, index) {  # not zero-indexed !!!
  stopifnot(isTruthyChar(json), is.numeric(index), index %% 1L == 0L)
  chars <- strsplit(json, '')[[1]]
  qts <- beg <- end <- 0L
  pos <- 2L
  #beg <- pos <- regexpr(paste0('"', key,'":'), json)[1] + nchar(key) + 3L
  #opbr <- 0L
  repeat {
    if (chars[pos] %in% c('"', '[', '{')) {
      qts <- qts + 1L
      if (qts %% 2L == 1L && (qts - 1L) %/% 2L == index) {
        bac <- substr(json, pos, nchar(json))
        return(sub('^([^"]}]*)["]}]*.*$', '\\1', bac))
      }
    }
    
    #if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    pos <- pos + 1L
    #if (opbr == 0L) return(substr(json, beg, pos - 1L))
  }
}

#' @internal
extractValueFromKey <- function(json, key) {
  stopifnot(isTruthyChar(json), isTruthyChar(key),
            grepl(paste0('"', key,'":'), json, perl=TRUE))
  chars <- strsplit(json, '')[[1]]
  beg <- pos <- regexpr(paste0('"', key,'":'), json)[1] + nchar(key) + 3L
  opbr <- 0L
  repeat {
    if (chars[pos] %in% c('[', '{')) opbr <- opbr + 1L
    if (chars[pos] %in% c(']', '}')) opbr <- opbr - 1L
    pos <- pos + 1L
    if (opbr == 0L) return(substr(json, beg, pos - 1L))
  }
  stop('error')
}