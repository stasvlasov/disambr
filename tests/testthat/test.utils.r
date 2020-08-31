## [[id:org:ptydaz31gti0][stop_unless:2]]
test_that("stop.unless", {
      expect_warning(stop.unless(FALSE, "Lala", FALSE))
      expect_error(stop.unless(FALSE))
      expect_true(stop.unless(TRUE))
      expect_warning(stop.unless("sdfasdf", stop.if.false = FALSE))
      expect_warning(stop.unless("sdfasdf", stop.if.false = FALSE, return.if.true = FALSE))
  })
## stop_unless:2 ends here

## [[id:org:kb3eaz31gti0][parse_files_path:2]]
test_that("parse.files.path", {
      expect_error(parse.files.path(3423))
      expect_warning(parse.files.path(c(".", "gibirish file")))
      expect_is(parse.files.path("."), "character")
      ## empty dirs
      tmp.dir <- "test_dir_for_parse.files.path"
      dir.create(tmp.dir, showWarnings = FALSE)
      expect_equal(parse.files.path(tmp.dir), character(0))
      file.remove(tmp.dir)
  })
## parse_files_path:2 ends here
