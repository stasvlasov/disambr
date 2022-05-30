## -------->>  [[file:../../disambr.src.org::*disambr_message_finish][disambr_message_finish:2]]
foo <- function() {
    disambr_start_time <- Sys.time() - 1000
    disambr_message_finish()
}

expect_message(foo(), "foo.*in")
## --------<<  disambr_message_finish:2 ends here


