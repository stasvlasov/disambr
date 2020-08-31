## [[id:org:84rht1n0mti0][disambr_in_sets:2]]
test_that("disambr_in_sets", {

a <- list(disambr_set_attr(c(1,2,3), a = 1)
        , disambr_set_attr(c(1,2,3), b = 2, a = 1)
        , disambr_set_attr(c(1,2,3), c = 3, a = 2)
        , disambr_set_attr(c(1,2,3), d = 4, a = 212))

expect_true(all(disambr_in_sets(a)))
expect_false(any(disambr_in_sets(a, b= 2, a = 2)))
expect_true(any(disambr_in_sets(a, b= 2, a = 1)))
expect_true(any(disambr_in_sets(a, a = 1)))
expect_true(any(disambr_in_sets(a, disambr_set_a = 1)))
expect_equal(sum(disambr_in_sets(a, a = 1, match_attr_value_parcially = TRUE)), 3)
})
## disambr_in_sets:2 ends here

## [[id:org:13h9svy0mti0][disambr_get_first_data_set:2]]
test_that("disambr_get_first_data_set", {
  a <- list(disambr_set_attr(c(1,2,3), recipe = "my_function_123")
            , disambr_set_attr(c(1,2,3,4), recipe = "my_function_23")
            , disambr_set_attr(c(1,2,3,4,5), recipe = "my_function_3a")
            , disambr_set_attr(c(1,2,3,4,5,6), recipe = "my_function_1"))

  expect_length(disambr_get_first_data_set(a, "3"), 3)
  expect_length(disambr_get_first_data_set(a, "3a"), 5)
  expect_null(disambr_get_first_data_set(a, "aaaaa"))
})
## disambr_get_first_data_set:2 ends here

## [[id:org:3ra9svy0mti0][disambr_get_last_set:2]]
test_that("disambr_get_last_set", {
a <- list(disambr_set_attr(c(1,2,3), strength = 0.1)
        , disambr_set_attr(c(1,2,3,4), strength = 0.6)
        , disambr_set_attr(c(1,2,3,4,5), strength = 0.5)
        , disambr_set_attr(c(1,2,3,4,5,6), strength = 1))

expect_length(disambr_get_last_set(a), 5)

a <- list(disambr_set_attr(c(1,2,3,4), strength = 0.6)
        , disambr_set_attr(c(1,2), strength = 1))

expect_length(disambr_get_last_set(a), 2)

})
## disambr_get_last_set:2 ends here

## [[id:org:tb303360oti0][disambr_get_last_weak_set:2]]
test_that("disambr_get_last_weak_set", {
a <- list(disambr_set_attr(c(1,2,3), strength = 0.1)
        , disambr_set_attr(c(1,2,3,4), strength = 0.6)
        , disambr_set_attr(c(1,2,3,4,5), strength = 0.5)
        , disambr_set_attr(c(1,2,3,4,5,6), strength = 1))

expect_length(disambr_get_last_weak_set(a), 5)

a <- list(disambr_set_attr(c(1,2,3,4), strength = 0.6)
        , disambr_set_attr(c(1,2,3,4,5,6), strength = 1))

expect_null(disambr_get_last_weak_set(a))

})
## disambr_get_last_weak_set:2 ends here

## [[id:org:xz69svy0mti0][disambr_get_strong_set:2]]
test_that("disambr_get_strong_set", {
    a <- list(disambr_set_attr(list(1,2,3)
                             , type = "similar"
                             , strength = 1)
            , disambr_set_attr(list(1,2,3,4)
                             , type = "similar"
                             , strength = 0.6)
            , disambr_set_attr(list(1,2,3,4,5)
                             , type = "similar"
                             , strength = 0.4)
            , disambr_set_attr(list(1,2,3,4,5,6)
                             , type = "similar"
                             , strength = 1))

    expect_length(disambr_get_strong_set(a), 9)

    a <- list(disambr_set_attr(data.table::data.table(c(1,2,3))
                             , type = "similar"
                             , strength = 1)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4))
                             , type = "similar"
                             , strength = 0.6)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4,5))
                             , type = "similar"
                             , strength = 0.4)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4,5,6))
                             , type = "similar"
                             , strength = 1))

    expect_is(disambr_get_strong_set(a), "data.table")

    a <- list(disambr_set_attr(data.table::data.table(c(1,2,3))
                             , type = "similar"
                             , strength = 0.1)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4))
                             , type = "similar"
                             , strength = 0.6)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4,5))
                             , type = "similar"
                             , strength = 0.4)
            , disambr_set_attr(data.table::data.table(c(1,2,3,4,5,6))
                             , type = "similar"
                             , strength = 0.1))

    expect_null(disambr_get_strong_set(a))

})
## disambr_get_strong_set:2 ends here

## [[id:org:4v89svy0mti0][disambr_get_last_unstrong_set:2]]
test_that("disambr_get_last_unstrong_set", {
a <- list(disambr_set_attr(data.table::data.table(c(1,2,3), c(1,2,8)), strength = 1)
        , disambr_set_attr(data.table::data.table(c(1,2,3,4), c(1,2,3,4)), strength = 0.6)
        , disambr_set_attr(data.table::data.table(c(1,2,3,4,5,6,7,8), c(1,2,3,4,5,6,7,8)), strength = 0.4)
        , disambr_set_attr(data.table::data.table(c(1,2,7,5), c(1,2,7,5)), strength = 1))

expect_equal(nrow(disambr_get_last_unstrong_set(a)), 4)
})
## disambr_get_last_unstrong_set:2 ends here

## [[id:org:kvv4zio0mti0][disambr_set_attr:2]]
test_that("disambr_set_attr", {
    expect_named(
        attributes(
            disambr_set_attr(c(1,2,3)
                           , lalala = "la"
                           , disambr_set_important_attr = "Hi there")))
    expect_match(names(attributes(
        disambr_set_attr(c(1,2,3)
                       , lalala = "la"
                       , disambr_set_important_attr = "Hi there")))[1]
      , "^disambr_set_")
    expect_length(
        attributes(
            disambr_set_attr(c(1,2,3)
                           , lalala = "la"
                           , disambr_set_important_attr = "Hi there")), 2)
})
## disambr_set_attr:2 ends here

## [[id:org:2z9bmie0nti0][disambr_add_set_attr:2]]
test_that("disambr_add_set_attr", {

a <- data.table(a = c(1,2,3,4)
               ,b = c(11,22,33,44))

b <- data.table(a = c(6,7,8)
               ,b = c(66,77,88))

disambr_set_attr(a
               , name = "a"
               , strength = 0.5
               , ts = Sys.time()
               , file = "lalala.rds"
               , recipe = list("second_procedure"
                             , "first_procedure"))

foo <- function(b, a = NULL, ...) {
    disambr_mess_start()
    disambr_add_set_attr(b, a, ...)
    return(attributes(b))
}

expect_length(foo(b, a)$disambr_set_recipe, 3)

expect_equal(foo(b,a, name = "new.name")$disambr_set_name, "new.name")
expect_equal(foo(b,a, strength = 1)$disambr_set_strength, 1)
expect_equal(foo(b,a)$disambr_set_name, "foo")

expect_equal(foo(a)$disambr_set_name, "foo")
expect_null(foo(NULL, a))

expect_length(foo(b, NULL)$disambr_set_recipe, 1)

})
## disambr_add_set_attr:2 ends here

## [[id:org:7m03hcq0hti0][disambr_save_set:2]]
test_that("disambr_save_set", {
    ## check files manually
    expect_null(disambr_save_set(disambr_set_attr(c(1,2,3)
                                                , name = "bar")
                               , save_set_as = NULL
                               , save_set_dir = "../disambr-sets-rds"))
    ## readRDS("../disambr-sets-rds/disambr-set.bar.2020-08-30T10-25.rds")
    ## with making files
    expect_match(disambr_save_set(disambr_set_attr(c(1,2,3)
                                                 , name = "bar")
                                , save_set_as = TRUE
                                , save_set_dir = "disambr-save-set-test")
               , "disambr-save-set-test/disambr-set\\.bar\\.")
    unlink("disambr-save-set-test", recursive = TRUE)
})
## disambr_save_set:2 ends here

## [[id:org:mxuc18v0nti0][disambr_get_output_set:2]]
test_that("disambr_get_output_set", {
a <- list(disambr_set_attr(c(1), name = "foo1")
        , disambr_set_attr(c(1,2), name = "foo", a = 1)
        , disambr_set_attr(c(1,2,3), name = "foo0", a = 2)
        , disambr_set_attr(c(1,2,3,4), name = "bar", a = 212))


foo <- function(sets, ...) {
    return(disambr_get_output_set(sets, ...))
}

expect_length(foo(a, get_output_set = TRUE), 2)
expect_null(foo(a))
})
## disambr_get_output_set:2 ends here

## [[id:org:rb0d18v0nti0][disambr_read_output_set:2]]
test_that("disambr_read_output_set", {
a <- list(disambr_set_attr(c(1), name = "foo1")
        , disambr_set_attr(c(1,2), name = "foo", a = 1)
        , disambr_set_attr(c(1,2,3), name = "foo0", a = 2)
        , disambr_set_attr(c(1,2,3,4), name = "bar", a = 212))


foo <- function(sets) {
    disambr_save_set(sets[[2]]
                   , save_set_as = TRUE
                   , save_set_dir = "disambr_read_output_set_test")
    disambr_read_output_set(read_output_set = TRUE
                          , save_set_dir = "disambr_read_output_set_test")
}

expect_length(foo(a), 2)

unlink("disambr_read_output_set_test", recursive = TRUE)

foo <- function(sets) {
    disambr_save_set(sets[[2]]
                   , save_set_as = "tra-la-la.rds"
                   , save_set_dir = "disambr_read_output_set_test")
    disambr_read_output_set(read_output_set = TRUE
                          , save_set_dir = "disambr_read_output_set_test")
}

expect_null(foo(a))

unlink("disambr_read_output_set_test", recursive = TRUE)

})
## disambr_read_output_set:2 ends here
