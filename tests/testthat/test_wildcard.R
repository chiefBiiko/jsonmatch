# test jsonmatch

testthat::context('wildcard sugar')

testthat::test_that('return matches wildcard pattern', {
  
  # setup
  maka <- jsonlite::toJSON(list(lo=list(ac=4L, ab=1L, gh=0L), yo=36L, ak=9L))
  daka <- jsonlite::toJSON(list(lo=list(ac=4L, ab=1L, lo=list(ac=2L, ab=3L))))
  laka <- jsonlite::toJSON(list(lo=list(ac=1L, ab=2L), lu=list(ac=3L, ab=4L)))
  kaka <- jsonlite::toJSON(list(abc=1L, dbe=2L))
  
  # wildcard matching
  testthat::expect_identical(jsonmatch(maka, '.lo.a*,.ak'),
                             structure('{"lo.ac":[4],"lo.ab":[1],"ak":[9]}', 
                                       class='json'))
  
  # repetitive names
  testthat::expect_identical(jsonmatch(daka, '.lo.a*,.lo.lo.a*'),
                             structure(paste0('{"lo.ac":[4],"lo.ab":[1],', 
                                              '"lo.lo.ac":[2],"lo.lo.ab":[3]}'), class='json'))
  
  # multiple wildcards in a path
  testthat::expect_identical(jsonmatch(laka, '.l*.a*'),
                             structure(paste0('{"lo.ac":[1],"lo.ab":[2],', 
                                              '"lu.ac":[3],"lu.ab":[4]}'), class='json'))
  
  # wildcard in the middle
  testthat::expect_identical(jsonmatch(kaka, '.d*e'),
                             structure('[2]', class='json'))
  
  # multiple wildcards in one obj key
  testthat::expect_identical(jsonmatch(kaka, '.*b*'),
                             structure('{"abc":[1],"dbe":[2]}', class='json'))
  
})