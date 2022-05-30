## -------->>  [[file:../disambr.src.org::*create_message][create_message:1]]
##' Creates message string for reporting during procedures
    ##' @param mess Message to report. If prefixed by h `h_marks` it will be ouline of level `h`
    ##' @param h Forse specific ouline level of message
    ##' @param indent Forse indentation
    ##' @param prefix Add overal prefix
    ##' @param h_marks Marks that sets outline. Default is "-". Can be many characters, e.g. "-*#".
    ##' @param h_prefix Character vector of prefixes for each outline level
    ##' @param h_prefix_sep Separator between `h_prefix` and `mess`
    ##' @param pretty Whether to use "crayon" package for pretty printing
    ##' @param mess_color Color of message
    ##' @param h_prefix_color Color of ouline prefix
    ##' @param ... Here we can pass `verbose` argument from upper functions. Default is TRUE
    ##' @return Message string
    ##' 
    ##' @export 
    create_message <- function(mess
                          , h = integer(0)
                          , indent = integer(0)
                          , prefix = ""
                          , h_marks = "-"
                          , h_prefix = character()
                          , h_prefix_sep = " "
                          , pretty = getOption("disambr_mess_pretty")
                          , mess_color = "green"
                          , h_prefix_color = "blue"
                          , ...) {
        ## set outline
        if(isTRUE(length(h) != 1)) {
            mess.regex <- paste0("^([", h_marks, "]*)\\s*(.*)")
            mess.parsed <-
                stringi::stri_match_first_regex(mess, mess.regex)
            mess <- mess.parsed[[3]]
            h <- nchar(mess.parsed[[2]]) + 1
        }
        ## set h_prefix
        if(length(h_prefix) < h) {
            h_prefix_l <- length(h_prefix)
            ## if h_prefix is NULL
            if(h_prefix_l == 0) {
                h_prefix <- ""
                h_prefix_l <- 1
            }
            h_prefix <- c(h_prefix, rep(h_prefix[h_prefix_l], h - h_prefix_l))
        }
        ## set indentation
        if(isTRUE(length(indent) != 1)) {
            indent <-
                nchar(paste(c("", h_prefix)[1:h], collapse = "")) +
                (h-1)*nchar(h_prefix_sep)
            indent <- strrep(" ",  indent)
        } else{
            indent <-
                switch(class(indent)
                     , numeric = if(indent == 0) ""
                                 else strrep(" ",  indent)
                     , character = indent)
        }
        ## create message
        h_prefix <- h_prefix[h]
        mess.plain <-
            paste0(prefix
                 , indent
                 , h_prefix
                 , h_prefix_sep
                 , mess)
        if(isTRUE(pretty) &&
           ## in case I want to move crayon to Sugests:
           requireNamespace("crayon", quietly = TRUE)) {
            h_prefix.style <-
                crayon::make_style(h_prefix_color)
            mess.style <-
                crayon::make_style(mess_color)
            mess.style <-
                crayon::combine_styles(crayon::bold, mess.style)
            mess <-
                Reduce(crayon::`%+%`
                     , list(prefix
                          , indent
                          , h_prefix.style(h_prefix)
                          , h_prefix_sep
                          , mess.style(mess)))
        } else {
            mess <- mess.plain
        }
        return(mess)
    }
## --------<<  create_message:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_message][disambr_message:1]]
##' Report a message with message()
##' @param mess Message to report. If prefixed by h `h_marks` it will be ouline of level `h`
##' @param h_prefix Character vector of prefixes for each outline level
##' @inheritDotParams create_message
##' @return Same as `message` returns
##' 
##' @md 
##' @export
disambr_message <- function(mess
                       , h_prefix = c("disambr:", "-")
                       , ...) {
    ## skip is not verbose (verbose by default)
    if(isFALSE(list(...)$verbose)) return()
    mess <- create_message(mess, h_prefix = h_prefix, ...)
    ## post message
    message(mess)
}
## --------<<  disambr_message:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_message_start][disambr_message_start:1]]
##' Post a starting message for disambr procedure. Records time started in `disambr_start_time` variable in its `parent.frame()`
##' @param start_mess_prefix Prefix for staring message
##' @inheritDotParams disambr_message
##' @return time started
##' 
##' @export 
disambr_message_start <- function(start_mess_prefix = "Making set -"
                              , ...) {
    ## get name of running procedure
    running_procedure_name <- deparse(sys.calls()[[sys.nframe() - 1]])
    ## clean the call string
    running_procedure_name <-
        stringi::stri_replace_first_regex(running_procedure_name
                                        , c("^disambr_set_([^()]+).*")
                                        , "$1")
    mess <- paste(start_mess_prefix, running_procedure_name)
    disambr_message(mess, ...)
    ## record the time started
    assign("disambr_start_time", Sys.time(), pos = parent.frame())
}
## --------<<  disambr_message_start:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_message_finish][disambr_message_finish:1]]
##' Post a starting message for disambr procedure. Records time started in `disambr_start_time` variable in its `parent.frame()`
##' @param mess Prefix for staring message
##' @param append_running_procedure_name Whether to append running procedure name
##' @inheritDotParams disambr_message
##' @return time started
##' 
##' @export 
disambr_message_finish <- function(mess = "Finished -"
                              , append_running_procedure_name = TRUE
                              , ...) {
    if(isTRUE(append_running_procedure_name)) {
        ## get name of running procedure
        running_procedure_name <- deparse(sys.calls()[[sys.nframe() - 1]])
        ## clean the call string
        running_procedure_name <-
            stringi::stri_replace_first_regex(running_procedure_name
                                            , c("^disambr_set_([^()]+).*")
                                            , "$1")
        mess <- paste(mess, running_procedure_name)    
    } 
    ## assess procedure duration
    if(exists("disambr_start_time", where = parent.frame())) {
        disambr_duration <-
            dhms(Sys.time() - get("disambr_start_time", pos = parent.frame()))
        mess <- paste(mess, "in", disambr_duration)
    }   
    disambr_message(mess, ...)
}
## --------<<  disambr_message_finish:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_warning][disambr_warning:1]]
##' Report a message with warning()
##' @param mess Message to report. If prefixed by h `h_marks` it will be ouline of level `h`
##' @param h_prefix Character vector of prefixes for each outline level
##' @param call. See `warning`
##' @param immediate. See `warning`
##' @inheritDotParams create_message
##' @return Same as `warning` returns
##' 
##' @md 
##' @export
disambr_warning <- function(mess
                       , h_prefix = c("disambr:", "-")
                       , call. = FALSE
                       , immediate. = TRUE
                       , ...) {
    mess <- create_message(mess, h_prefix = h_prefix, ...)
    ## post message
    warning(mess, call. = call. , immediate. = immediate.)
}
## --------<<  disambr_warning:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_stop][disambr_stop:1]]
##' Report a message with stop()
##' @param mess Message to report. If prefixed by h `h_marks` it will be ouline of level `h`
##' @param call. See `stop`
##' @inheritDotParams create_message
##' @return Same as `stop` returns
##' 
##' @md 
##' @export
disambr_stop <- function(mess
                       , call. = FALSE
                       , ...) {
    parent.call <- deparse(sys.calls()[[sys.nframe() - 1]])
    parent.call <- as.character(parent.call)
    mess <-
        create_message(mess
                  , h_prefix = paste0(parent.call, ":")
                  , ...)
    ## post message
    stop(mess, call. = call.)
}
## --------<<  disambr_stop:1 ends here


