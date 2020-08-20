## [[file:~/org/research/disambr/disambr/disambr.src.org::*testthat][testthat:1]]
test_that("stop.unless", {
      expect_warning(stop.unless(FALSE, "Lala", FALSE))
      expect_error(stop.unless(FALSE))
      expect_true(stop.unless(TRUE))
      expect_false(stop.unless("sdfasdf", stop.if.false = FALSE))
      expect_true(stop.unless("sdfasdf", stop.if.false = FALSE, return.if.true = FALSE))
  })
## testthat:1 ends here
