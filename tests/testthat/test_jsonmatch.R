# test jsonmatch

testthat::context('match consistency')

testthat::test_that('return matches pattern', {
  
  # setup
  waka <- jsonlite::toJSON(list(a=c(1,2),b=list(c(77,44), 'doo')))
  saka <- jsonlite::toJSON(list(a=FALSE, 
                                b=c('zu', 'lu'), 
                                c=list(x=4L, y=1L, z=9L)))
  kafa <- jsonlite::toJSON(list(list(list(36)), list(44)))
  maka <- jsonlite::toJSON(list(lo=list(ac=4L, ab=1L, gh=0L), yo=36L, ak=9L))
  daka <- jsonlite::toJSON(list(lo=list(ac=4L, ab=1L, lo=list(ac=2L, ab=3L))))
  laka <- jsonlite::toJSON(list(lo=list(ac=1L, ab=2L), lu=list(ac=3L, ab=4L)))
  
  # single item
  testthat::expect_identical(jsonmatch(saka, '.b[0]'), 
                             structure('["zu"]', class='json'))
  
  # multiple items pt1
  testthat::expect_identical(jsonmatch(saka, '.b[0:1]'), 
                             structure('["zu","lu"]', class='json'))
  
  # multiple items pt2
  testthat::expect_identical(jsonmatch(saka, '.b[0],.c'),
                             structure(
                               '{"b[0]":["zu"],"c":{"x":[4],"y":[1],"z":[9]}}',
                               class='json'
                             ))
  
  # JSON constant
  testthat::expect_identical(jsonmatch(saka, '.a'),
                             structure('[false]', class='json'))
  
  # 2D array
  testthat::expect_identical(jsonmatch(waka, '.b[0]'),
                             structure('[77,44]', class='json'))
  
  # 3D array
  testthat::expect_identical(jsonmatch(kafa, '[0][0][0]'),
                             structure('[36]', class='json'))
  
  # throws on incorrect horizontal array indexing
  testthat::expect_error(jsonmatch(kafa, '[0][0][1]'))
  testthat::expect_error(jsonmatch(kafa, '[0][1][0]'))
  
  # gracefully handles incorrect vertical array indexing at a path's base
  testthat::expect_identical(jsonmatch(kafa, '[1][0][0]'),  # correct: '[1][0]'
                             structure('[44]', class='json'))
  
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
  
})