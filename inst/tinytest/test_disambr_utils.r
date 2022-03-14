## -------->>  [[file:../../disambr.src.org::*match_fuzzy][match_fuzzy:2]]
expect_equal(
    match_fuzzy(c("sdfsdf", "sfawefwsd", "sdfwefad", ";sldwaf", "asdfwaf")
              , method = "lv"
              , max_dist = 3
              , id_name = "id")
, structure(list(id_1 = c("sdfsdf", "sdfsdf", "sfawefwsd", "sdfwefad", 
";sldwaf", ";sldwaf", "asdfwaf", "asdfwaf", "asdfwaf"), id_2 = c("sdfsdf", 
"asdfwaf", "sfawefwsd", "sdfwefad", ";sldwaf", "asdfwaf", "sdfsdf", 
";sldwaf", "asdfwaf")), row.names = c(NA, -9L), class = c("data.table", 
"data.frame")))

expect_equal(nrow(match_fuzzy(c("sdfsdf", "sfawefwsd", "sdfwefad", ";sldwaf", "asdfwaf")
          , method = "lv"
          , max_dist = 39
          , id_name = "id")), 25)
## --------<<  match_fuzzy:2 ends here


