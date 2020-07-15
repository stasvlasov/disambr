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
