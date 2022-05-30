## -------->>  [[file:../../disambr.src.org::*disambr_message_start][disambr_message_start:2]]
foo <- function() {
    disambr_message_start()
    return(disambr_start_time)
}
expect_inherits(foo(), c("POSIXt", "POSIXct"))
expect_message(foo())
## --------<<  disambr_message_start:2 ends here


