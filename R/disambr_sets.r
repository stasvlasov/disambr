## [[id:org:vd6faz31gti0][disambr_subsets:1]]
##' Filters list of sets
##' @param sets_list list of sets
##' @param attribute_value_list list of attribute values where list elements name correspond attribute names used for filtering sets
##' @param which_to_return whether to return "all", "first" or "last" set from filtered sets
##' @param negate_subsets whether to return sets that was not matched insted
##' @return list of sets or set if `which_to_return` is ethier "first" or "last"
##' 
##' @export 
disambr_subsets <- function(sets_list, attribute_value_list
                          , which_to_return = c("all", "first", "last")
                          , negate_subsets = FALSE) {
    if (!is.list(sets_list))
        stop("disambr: 'sets_list' should be a list!")
    if (!is.list(attribute_value_list))
        stop("disambr: 'attribute_value_list' should be a list!")
    filter_sets <- function(attr_name, attr_value) {
        sapply(lapply(sets_list, attr, attr_name), `[`, 1) %in% attr_value
    }
    sets_list_filters <- mapply(filter_sets
                              , names(attribute_value_list)
                              , attribute_value_list
                              , SIMPLIFY = FALSE
                              , USE.NAMES = TRUE)
    subsets_list <-
        if (isTRUE(negate_subsets)) {
            sets_list[!Reduce(`&`, sets_list_filters)]
        } else {
            sets_list[Reduce(`&`, sets_list_filters)]
        }
    return(switch(which_to_return[1]
                , all = subsets_list
                , first = subsets_list[[1]]
                , last = subsets_list[[length(subsets_list)]]))
}



## a <- c(1,2,3,4)
## b <- c("a","b","c")
## c <- NULL
## attributes(a)$name <- "aaa"
## attributes(b)$name <- "bbb"
## attributes(c)$name <- c("ccc", 3)
## attributes(a)$kind <- "good"
## attributes(b)$kind <- "good"
## attributes(c)$kind <- "bad"

## disambr_subsets(list(a,b,c), list(kind = "good"))

## disambr_subsets(list(a,b,c), list(kind = "good"), which_to_return = "last")

## disambr_subsets(list(a,b,c), list(name = "ccc"
##                                 , kind = "good"))

## disambr_subsets(list(a,b,c), list(name = "ccc"
##                                 , kind = "bad"))
## disambr_subsets:1 ends here

## [[id:org:oak78r30hti0][disambr_set_attributes:1]]
## disambr_entity
## disambr_set_type
## disambr_set_coefficient
## disambr_set_name
## disambr_set_collection
## disambr_entity_id_reference
## disambr_entity_id_reference_md5_sum
## disambr_recipe
disambr_set_attributes <- function(focal_set, ...) {
    attr_value_list <- list(...)
    for (i in 1:length(attr_value_list)) {
        attributes(focal_set)[[names(attr_value_list)[i]]] <-
            attr_value_list[[i]]
    }
    return(focal_set)
}
## disambr_set_attributes:1 ends here

## [[id:org:7m03hcq0hti0][disambr_save_set:1]]
disambr_save_set <- function(set_to_save
                           , save_set_as = TRUE
                           , use_time_stamp = TRUE) {
    if(length(save_set_as) != 0) {
        if(isTRUE(save_set_as)) {
            save_set_as <- attr(set_to_save, "disambr_set_name")
            if(isTRUE(use_time_stamp)) {
                save_set_as <-
                    paste0(save_set_as, "."
                         , format(Sys.time(), "%Y-%m-%dT%H-%M"))

            }
            save_set_as <-
                paste0("disambr_set."
                     , save_set_as
                     , ".rds")
        }
        if(is.character(save_set_as)) {
            saveRDS(set_to_save, file = save_set_as)
            message(
                "- saved set as '", save_set_as, "'")
        } else {
            message(
                "disambr: do not know how to save 'set_to_save' as '", save_set_as, "'")
        }
    }
     return()
 }
## disambr_save_set:1 ends here
