## -------->>  [[file:../../disambr.src.org::*disambr_mess][disambr_mess:2]]
expect_message(disambr_mess("Hello world!"))
expect_null(disambr_mess("Hello world!", verbose = FALSE))
## --------<<  disambr_mess:2 ends here



## -------->>  [[file:../../disambr.src.org::*disambr_mess_start][disambr_mess_start:2]]
foo <- function() {
    disambr_mess_start()
    return(disambr_start_time)
}
expect_inherits(foo(), c("POSIXt", "POSIXct"))
expect_message(foo())
## --------<<  disambr_mess_start:2 ends here



## -------->>  [[file:../../disambr.src.org::*disambr_mess_finish][disambr_mess_finish:2]]
foo <- function() {
    disambr_start_time <- Sys.time() - 1000
    disambr_mess_finish()
}

expect_message(foo(), "foo.*in")
## --------<<  disambr_mess_finish:2 ends here



## -------->>  [[file:../../disambr.src.org::*disambr_warn][disambr_warn:2]]
expect_warning(disambr_warn("Ahtung!"))
## --------<<  disambr_warn:2 ends here



## -------->>  [[file:../../disambr.src.org::*disambr_stop][disambr_stop:2]]
expect_error(disambr_stop())
## --------<<  disambr_stop:2 ends here


