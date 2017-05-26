# jsonmatch utils

#' @internal
isTruthyChr <- function(char) {
  if (is.character(char) && nchar(char) > 0L) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

#' Performs a syntax check on pattern
#' 
#' @param json JSON string.
#' @param pattern Chr vector of length 1 specifying the subset pattern.
#' @return Logical indicating whether the syntax is correct.
#' 
#' @internal
verifyPatternSyntax <- function(json, pattern) {
  stopifnot(isTruthyChr(json), isTruthyChr(pattern))
  # root structure
  struct <- if (grepl('^\\[.*\\]$', json, perl=TRUE)) {  # case arr
    strsplit(pattern, '', fixed=TRUE)[[1]][1] == '['
  } else if (grepl('^\\{.*\\}$', json, perl=TRUE)) {     # case obj
    strsplit(pattern, '', fixed=TRUE)[[1]][1] == '.'
  }
  # check 4 invalid characters
  valid <- !grepl('[^\\*\\.,\\:\\[\\]\\d[A-Za-z]]*', pattern, perl=TRUE)
  # early exit
  if (!struct || !valid) return(FALSE)
  # setup syntax check                                       # master regex
  rex <- paste0('^(?:\\.[[:alnum:]]+\\*|', 
                '\\.\\*[[:alnum:]]+|', 
                '\\.[[:alnum:]]+\\*[[:alnum:]]+|', 
                '\\.[[:alnum:]]+)|', 
                '^(?:\\[\\d+(?:,\\d+)*(?:\\:\\d+)*\\])')
  comps <- strsplit(pattern, ',', fixed=TRUE)[[1]]           # pattern components
  for (comp in comps) {                                      # do em all
    red <- comp                                              # reduction base
    repeat {                                                 # do predicate reduction
      if (!grepl(rex, red, perl=TRUE)) return(FALSE)    # check head
      red <- sub(rex, '', red, perl=TRUE)               # cut head
      if (nchar(red) == 0L) break                       # trapdoor
    }  
  }
  # exit
  return(TRUE)
}

#' Splits pattern to paths while handling the wildcard
#' 
#' @param json JSON string.
#' @param pattern Subset pattern.
#' @return Chr vector.
#' 
#' @internal
getPathsFromPattern <- function(json, pattern) {
  # selectors split
  selectors <- Filter(function(p) p != '', 
                      strsplit(pattern, ',', fixed=TRUE)[[1]])
  # make a flat vector
  rtn <- unlist(lapply(selectors, function(s) {
    if (grepl('\\*', s, perl=TRUE)) {  # GET ALL *matches in a vector
      handleWildCard(json, s)
    } else {
      s
    }
  }))
  # serve
  return(rtn)
}

#' Returns a chr vector of wildcard matched object keys
#'
#' @param json JSON string.
#' @param selector Object selector/path containing the wildcard character.
#' @return Chr vector.
#' 
#' @details You can use multiple wildcards in one pattern and even within one 
#' path but you can not use multiple wildcards within a single key value, f.x.
#' ALLOWED: '.a*,.b*' or '.*z.d*,.b*' 
#' NOT ALLOWED: '.a*c*' or '.*b*'
#'
handleWildCard <- function(json, selector) {
  # regex 2 match the wildcard part of the selector
  rex.wdcd.key <- paste0('\\.[[:alnum:]]+\\*|\\.\\*[[:alnum:]]+|', 
                         '\\.[[:alnum:]]+\\*[[:alnum:]]+')
  # extract the wildcard part of the selector
  wdcd.key <- regmatches(selector, regexpr(rex.wdcd.key, selector, perl=TRUE))
  # get prefix of wdcd.key
  pre <- sub(paste0(rex.wdcd.key, '(?<!^)'), '', selector, perl=TRUE)
  # object context where to look for wildcard matches
  obj.ctx <- gsub('^\\{|\\}$', '', 
                  if (pre != '') jsonmatch(json, pre) else json, 
                  perl=TRUE)
  # make regex from wdcd.key expression
  rex.mtch.keys <- paste0('(?:"', sub('\\*', '[[:alnum:]]+', 
                                      sub('^\\.', '', wdcd.key, perl=TRUE), 
                                      perl=TRUE), 
                          '"\\:)(?![^\\{]*\\})')  # keys on top level only
  # get all matching keys
  mtch.keys <- paste0('.', 
                      gsub('["\\:]', '', 
                           regmatches(obj.ctx, 
                                      gregexpr(rex.mtch.keys, 
                                               obj.ctx, 
                                               perl=TRUE))[[1]],
                           perl=TRUE))
  # glue things back together
  glued <- sapply(mtch.keys, function(mk) sub(wdcd.key, mk, selector, fixed=TRUE),
                  USE.NAMES=FALSE)
  # check if any wildcards remained - conditionally repeat
  clued <- sapply(glued, function(gk) {
    if (grepl('\\*', gk, perl=TRUE)) {
      handleWildCard(json, gk)
    } else {
      gk
    }
  }, USE.NAMES=FALSE)
  # serve
  return(clued)
}

#' Takes a split vector of dirty object keys and array indices and returns
#'  a 2d list providing references as required by \code{extractValueFrom*}.
#' 
#' @param paths Character vector of jsonmatch-type key references.
#' @return 2d list providing symbols as required by \code{extractValueFrom*}.
#' 
#' @internal
getKeysFromPaths <- function(paths) {
  stopifnot(is.character(paths))
  # split paths to path components
  comps <- strsplit(paths, 
                    paste0('(?<=\\])(?=\\[)|(?<=[[:alnum:]])(?=\\[)|', 
                           '(?<=\\])(?=\\.)|(?<=[[:alnum:]])(?=\\.)'), 
                    perl=TRUE)
  # strip dots and \\s
  nodots <- lapply(comps, function(comp) {
    comp <- gsub('.', '', comp, fixed=TRUE)
    comp[comp != '']
  })
  # make a 2D list of chr obj keys and numeric indices
  subseq <- lapply(nodots, function(nd) {
    lapply(nd, function(d) {
      if (grepl('\\[[[:digit:],:]+\\]', d, perl=TRUE)) {
        eval(parse(text=sub('\\[([[:digit:],:]*)\\]', 'c(\\1)', d)))
      } else {
        d
      }
    })
  })
  # serve 4 extractValueFrom*
  return(subseq)
}

#' Extracts the item at given index of an JSON array
#'
#' @param arr JSON array.
#' @param index Array index/key for which to retrieve value, zero-indexed.
#' @return Character vector.
#'
#' @internal
extractValueFromArrIndex <- function(arr, index) {  # zero-indexed !!!
  stopifnot(isTruthyChr(arr), is.numeric(index), index %% 1L == 0L)
  # split arr contents on comma not enclosed in [] or {}
  # \\d and [:alpha:] on hold: '(?![^\\d]*+\\d)(?![^[A-Za-z]]*+[A-Za-z])'
  cospl <- strsplit(gsub('^\\[|\\]$', '', arr, perl=TRUE), 
                    ',(?![^\\[\\]]*+\\])(?![^\\{\\}]*+\\})',
                    perl=TRUE)[[1]]
  # strip qoutes at head and tail
  dqt <- gsub('^"|"$', '', cospl[index + 1L], perl=TRUE)
  # serve
  if (is.na(dqt)) stop('index out of bounds') else return(dqt)
}

#' Extracts the item at given key of an JSON object
#'
#' @param obj JSON object.
#' @param key Key for which to retrieve value.
#' @return Character vector.
#' 
#' @internal
extractValueFromObjKey <- function(obj, key) {
  stopifnot(isTruthyChr(obj), isTruthyChr(key),
            grepl(paste0('"', key,'":'), obj, perl=TRUE))
  chars <- strsplit(obj, '')[[1]]
  beg <- pos <- regexpr(paste0('"', key,'":'), obj)[1] + nchar(key) + 3L
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

#' Packs atomic data in arrays, optionally adding keys
#'
#' @param accumulator Character vector of target values (JSON extracts).
#' @param keys Character vector of keys to prefix target values with,
#' if supplied must have same length as \code{accumulator}.
#' @return Character vector.
#'
#' @internal
packAtoms <- function(accumulator, keys) {
  stopifnot(is.character(accumulator),
            missing(keys) || is.character(keys))
  return(if (missing(keys)) {  # case no keys passed
    sapply(accumulator, function(a) {
      if (!grepl('(?:^\\[|^\\{).*(?:\\]$|\\}$)', a, perl=TRUE)) {
        if (grepl('^[[:alpha:]]', a, perl=TRUE) &&
            !grepl('^null$|^false$', a, perl=TRUE)) {  # pack strings
          paste0('["', a, '"]') 
        } else {                                       # pack anything else
          paste0('[', a, ']')
        }
      } else {                                         # already packed
        a
      }
    }, USE.NAMES=FALSE) 
  } else {                     # case keys passed
    i <- 0L
    sapply(accumulator, function(a) {
      i <<- i + 1L
      if (!grepl('(?:^\\[|^\\{).*(?:\\]$|\\}$)', a, perl=TRUE)) {
        if (grepl('^[[:alpha:]]', a, perl=TRUE) &&
            !grepl('^null$|^false$', a, perl=TRUE)) {  # pack strings
          paste0(paste0('"', keys[i], '"', ':'), '["', a, '"]')
        } else {                                       # pack anything else
          paste0(paste0('"', keys[i], '"', ':'), '[', a, ']')
        }
      } else {                                         # already packed
        paste0(paste0('"', keys[i], '"', ':'), a) 
      }
    }, USE.NAMES=FALSE)
  })
}