## -------->>  [[file:../../disambr.src.org::*disambr_read][disambr_read:2]]
my.file <- system.file("testdata", "wos-tsv-test-recent.txt", package = "disambr")

if(file.exists(my.file)) {
    expect_equal(
        length(disambr_read(my.file)), 4
    )
}
## --------<<  disambr_read:2 ends here


