## [[id:org:t4zk2360oti0][match_fuzzy:2]]
test_that("match_fuzzy", {
expect_length(match_fuzzy(c("sdfsdf", "sfawefwsd", "sdfwefad", ";sldwaf", "asdfwaf")
          , method = "lv"
          , max_dist = 3
          , id_name = "id"), 2)

expect_equal(nrow(match_fuzzy(c("sdfsdf", "sfawefwsd", "sdfwefad", ";sldwaf", "asdfwaf")
          , method = "lv"
          , max_dist = 39
          , id_name = "id")), 9)
})
## match_fuzzy:2 ends here
