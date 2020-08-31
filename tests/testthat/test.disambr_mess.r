## [[id:org:nqfdt1n0mti0][disambr_mess:2]]
test_that("disambr_mess", {
    expect_message(disambr_mess("Hello world!"))
    expect_null(disambr_mess("Hello world!", verbose = FALSE))
})
## disambr_mess:2 ends here

## [[id:org:9rmdt1n0mti0][disambr_mess_start:2]]
test_that("disambr_mess_start", {
    foo <- function() {
        disambr_mess_start()
        return(disambr_start_time)
    }
    expect_is(foo(), c("POSIXt", "POSIXct"))
    expect_message(foo(), )
})
## disambr_mess_start:2 ends here

## [[id:org:m6vdt1n0mti0][disambr_mess_finish:2]]
test_that("disambr_mess_finish", {
    foo <- function() {
        disambr_start_time <- Sys.time() - 1000
        disambr_mess_finish()
    }
    expect_message(foo(), "foo.*in")
})
## disambr_mess_finish:2 ends here

## [[id:org:0xagt1n0mti0][disambr_warn:2]]
test_that("disambr_warn", {
    expect_warning(disambr_warn("Ahtung!"))
})
## disambr_warn:2 ends here

## [[id:org:qlegt1n0mti0][disambr_stop:2]]
test_that("disambr_stop", {
    expect_error(disambr_stop())
})
## disambr_stop:2 ends here
