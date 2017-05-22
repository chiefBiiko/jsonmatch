# test jsonmatch

testthat::context('match consistency')

testthat::test_that('return matches pattern', {
  
  # setup
  saka <- jsonlite::toJSON(list(a=FALSE, 
                                b=c('zu', 'lu'), 
                                c=list(x=4L, y=1L, z=9L)))
  juju <- jsonlite::toJSON(c(4L:0L, 36L:44L, 1L:9L))
  
  # single item
  testthat::expect_identical(jsonmatch(saka, '.b[0]'), 
                             structure('["zu"]', class='json'))
  
  # multiple items pt1
  testthat::expect_identical(jsonmatch(saka, '.b[0:1]'), 
                             structure('["zu","lu"]', class='json'))
  
  # multiple items pt2
  testthat::expect_identical(jsonmatch(saka, '.b[0],c'),
                             structure(
                               '{"b[0]":[zu],"c":{"x":[4],"y":[1],"z":[9]}}',
                               class='json'
                             ))
  
  # js constants
  testthat::expect_identical(jsonmatch(saka, '.a'),
                             structure('[false]', class='json'))
  
})