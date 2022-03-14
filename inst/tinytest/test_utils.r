## -------->>  [[file:../../disambr.src.org::*stop_unless][stop_unless:2]]
expect_warning(stop_unless(FALSE, "Lala", FALSE))
expect_error(stop_unless(FALSE))
expect_true(stop_unless(TRUE))
expect_warning(stop_unless("sdfasdf", stop_if_false = FALSE))
expect_warning(stop_unless("sdfasdf", stop_if_false = FALSE, return_if_true = FALSE))
## --------<<  stop_unless:2 ends here



## -------->>  [[file:../../disambr.src.org::*parse_files_path][parse_files_path:2]]
expect_error(parse_files_path(3423))
expect_warning(parse_files_path(c(".", "gibirish file")))
expect_inherits(parse_files_path("."), "character")

## empty dirs
tmp.dir <- "test_dir_for_parse_files_path"
dir.create(tmp.dir, showWarnings = FALSE)
expect_equal(parse_files_path(tmp.dir), character(0))
file.remove(tmp.dir)
## --------<<  parse_files_path:2 ends here


