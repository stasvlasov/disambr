## [[id:org:c7wgnb60jti0][disambr_read:2]]
test_that("disambr_read", {
    my.file <- system.file("testdata", "wos-tsv-test-recent.txt", package = "disambr")
    skip_if_not(file.exists(my.file))
    expect_length(disambr_read(my.file), 4)
})
## disambr_read:2 ends here
