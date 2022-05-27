## -------->>  [[file:../disambr.src.org::*TEMPLATE][TEMPLATE:1]]

## --------<<  TEMPLATE:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_in_sets][disambr_in_sets:1]]
##' Checks if sets with certain attribures are present
##' @param sets Sets to filter on sets attributes
##' @param match_attr_value_parcially whether attribute values can be matched partially
##' @param check_attr_names_prefix Whether to check for short names of attributes. See `attr_names_prefix`.
##' @param attr_names_prefix If name does not start with this prefix (default is 'disambr_set_'), it will add this prefix before attribute name.
##' @param ... Named sets attributes to filter `sets` on
##' @return logical vector of length `length(sets)`
##' 
##' @export 
disambr_in_sets <- function(sets
                          , ...
                          , match_attr_value_parcially = FALSE
                          , check_attr_names_prefix = TRUE
                          , attr_names_prefix = "disambr_set_") {
    ## check if sets is list
    if(!is.list(sets)) disambr_stop("'sets' should be a list of sets!")
    attrs_values <- list(...)
    ## check if ... is provided and if not return all
    attrs_values_length <- length(attrs_values)
    if(attrs_values_length == 0) return(rep(TRUE, length(sets)))
    ## check if all named
    attrs_values_names <- names(attrs_values)
    if(length(attrs_values_names) != attrs_values_length)
        disambr_stop("'...' arguments should be all named!")
    if(isTRUE(check_attr_names_prefix)) {
        ## add "disambr_set_" if attr names are short
        attrs_values_names_short <-
            !stringi::stri_detect_regex(attrs_values_names
                                      , paste0("^", attr_names_prefix))
        if(any(attrs_values_names_short)) {
            attrs_values_names[attrs_values_names_short] <-
                paste0(attr_names_prefix
                     , attrs_values_names[attrs_values_names_short])
        }
    }
    if(isTRUE(match_attr_value_parcially)) {
        filter_sets <- function(attr_name, attr_value) {
            vals <- lapply(sets, attr, attr_name, exact = TRUE)
            vals <- lapply(vals, unlist)
            vals <- lapply(vals, `[`, 1)
            vals <- unlist(lapply(vals, function(a) if(is.null(a)) NA else a))
            sapply(stringi::stri_detect_fixed(vals, attr_value), isTRUE)
        }
    } else {
        filter_sets <- function(attr_name, attr_value) {
            vals <- lapply(sets, attr, attr_name, exact = TRUE)
            vals <- lapply(vals, unlist)
            vals <- lapply(vals, `[`, 1)
            vals <- unlist(lapply(vals, function(a) if(is.null(a)) NA else a))
            vals %in% attr_value
        }
    }
    sets_filters <- mapply(filter_sets
                         , attrs_values_names
                         , attrs_values
                         , SIMPLIFY = FALSE)
    ## return overlap of sets_filters
    return(Reduce(`&`, sets_filters))
}
## --------<<  disambr_in_sets:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_first_data_set][disambr_get_first_data_set:1]]
##' Get first data set in list sets
##' @param sets list of sets
##' @param recipe function that produced the data set (parcial match allowed)
##' @param ... other attributes
##' @param match_parcially whether to match recipe partially
##' @inheritDotParams disambr_in_sets
##' @return 
##' 
##' @export 
disambr_get_first_data_set <- function(sets, recipe, ...
                                     , match_parcially = TRUE) {
    set_num <-
        disambr_in_sets(sets, recipe = recipe, ...
                      , match_attr_value_parcially = match_parcially)
    ## get first
    set_num <- which(set_num)[1]
    if(length(set_num) != 0) {
        return(sets[[set_num]])
    } else {
        disambr_stop(paste("Data set should be available in sets:", recipe))
    }
}
## --------<<  disambr_get_first_data_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_last_set][disambr_get_last_set:1]]
##' Gets last set in sets which strength less or equal than 0.5
##' @param sets sets
##' @param ... other attributes
##' @inheritDotParams disambr_in_sets
##' @return set or NULL if not found
##' 
##' @export 
disambr_get_last_set <- function(sets, ...) {
    set_index <- disambr_in_sets(sets, ...)
    set_index <- which(set_index)
    set_index <- set_index[length(set_index)]
    if(length(set_index) == 1) {
        return(sets[[set_index]])
    } else {
        return()
    }
}
## --------<<  disambr_get_last_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_last_weak_set][disambr_get_last_weak_set:1]]
##' Gets last set in sets which strength less or equal than 0.5
##' @param sets sets
##' @param ... other attributes
##' @inheritDotParams disambr_in_sets
##' @return set or NULL if not found
##' 
##' @export 
disambr_get_last_weak_set <- function(sets, ...) {
    set_index <- disambr_in_sets(sets, ...
                               , strength = seq(from = 0.1, to = 0.5, by = 0.01))
    set_index <- which(set_index)
    set_index <- set_index[length(set_index)]
    if(length(set_index) == 1) {
        return(sets[[set_index]])    
    } else {
        disambr_stop("- can not find last weak set in sets!")
        return()
    }
}
## --------<<  disambr_get_last_weak_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_strong_set][disambr_get_strong_set:1]]
##' Get sets with strength parameter of 1 and rbind them into single set
##' @param sets sets
##' @param ... other attributes
##' @inheritDotParams disambr_in_sets
##' @return set or NULL if none found
##' 
##' @export 
disambr_get_strong_set <- function(sets, ...) {
    sets_index <- disambr_in_sets(sets, ...
                                , type = "similar"
                                , strength = 1)
    ## if not sets return NULL
    if(!any(sets_index)) return()
    sets <- sets[sets_index]
    ## bind sets depending on class
    sets_class <- sapply(lapply(sets, class), `[`, 1)
    if(all(sets_class %in% "data.table")) {
        sets <- data.table::rbindlist(sets)
    } else if(all(sets_class %in% "list")) {
        sets <- do.call(c, sets)
    }
    return(sets)
}
## --------<<  disambr_get_strong_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_truth_set][disambr_get_truth_set:1]]
##' Get sets with strength parameter of 10 (ground thruth)
##' @param sets sets
##' @param ... other attributes
##' @inheritDotParams disambr_in_sets
##' @return set or NULL if none found
##' 
##' @export 
disambr_get_truth_set <- function(sets, ...) {
    sets_index <- disambr_in_sets(sets, ...
                                , type = "similar"
                                , strength = 10)
    ## if not sets return NULL
    if(!any(sets_index)) disambr_stop("- can not find truth set!")
    if(sum(sets_index) != 1) disambr_stop("- more than one truth set found!")
    return(sets[[which(sets_index)]])
}
## --------<<  disambr_get_truth_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_last_unstrong_set][disambr_get_last_unstrong_set:1]]
##' Gets last set from sets with strength <= 0.5 and excludes from this set all sets with strength of 1
##' @param sets sets
##' @return set or NULL
##' @export 
disambr_get_last_unstrong_set <- function(sets) {
    weak_set <- disambr_get_last_weak_set(sets)
    strong_set <- disambr_get_strong_set(sets)
    if(is.null(strong_set) || is.null(weak_set)) {
        return(weak_set)
    } else if("data.table" %in% class(weak_set) &&
              "data.table" %in% class(strong_set)) {
        return(data.table::fsetdiff(weak_set, strong_set))
        ## comb_set <- rbind(weak_set, strong_set)
        ## comb_set <- 
        ##     comb_set[!duplicated(comb_set, fromLast = FALSE) &
        ##              !duplicated(comb_set, fromLast = TRUE)]
        ## return(comb_set) #
        ## return(weak_set[strong_set[[1]] !=  weak_set[[1]] ||
                        ## strong_set[[2]] !=  weak_set[[2]]])
    } else {
        disambr_stop("Weak and strong sets should be data.tables!")
    }
}
## --------<<  disambr_get_last_unstrong_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_set_attr][disambr_set_attr:1]]
## disambr_entity
## disambr_set_type
## disambr_set_coefficient
## disambr_set_name
## disambr_set_collection
## disambr_entity_id_reference
## disambr_entity_id_reference_md5_sum
## disambr_recipe

##' Adds attribures to the set with data.table::setattr
##' @param focal_set Set to add attribute to
##' @param check_attr_names_prefix Whether to check for short names of attributes. See `attr_names_prefix`.
##' @param attr_names_prefix If name does not start with this prefix (default is 'disambr_set_'), it will add this prefix before attribute name.
##' @param ... Named attributes
##' @return `focal_set`
##' 
##' @export 
disambr_set_attr <- function(focal_set
                           , ...
                           , check_attr_names_prefix = TRUE
                           , attr_names_prefix = "disambr_set_") {
    attrs_values <- list(...)
    ## check if ... is provided and if not do nothing
    attrs_values_length <- length(attrs_values)
    if(attrs_values_length == 0) return()
    ## check if all attributes in ... are named
    attrs_values_names <- names(attrs_values)
    if(length(attrs_values_names) != attrs_values_length)
        disambr_stop("'...' arguments should be all named!")
    if(isTRUE(check_attr_names_prefix)) {
    ## add "disambr_set_" if attr names are short
    attrs_values_names_short <-
        !stringi::stri_detect_regex(attrs_values_names
                                  , paste0("^", attr_names_prefix))
    if(any(attrs_values_names_short)) {
        attrs_values_names[attrs_values_names_short] <-
            paste0(attr_names_prefix
                 , attrs_values_names[attrs_values_names_short])
    }
    }
    ## set attributes
    for (i in 1:length(attrs_values)) {
        ## also works for other that data.table objects
        data.table::setattr(focal_set, attrs_values_names[i], attrs_values[[i]])
    }
    return(focal_set)
}
## --------<<  disambr_set_attr:1 ends here


## -------->>  [[file:../disambr.src.org::*disambr_add_set_attr][disambr_add_set_attr:1]]
##' Add disambr attribures to focal set from template set and update some of them
##'
##' It adds attributes by reference (with setattr {data.table}), i.e. without making a copy
##'
##' It updates:
##' - time stamp (disambr_set_st) to current
##' - disambr_set_file to NULL
##' - adds to disambr_set_recipe the calling procedure
##' - adds disambr_set_duration 
##' @param focal_set Set
##' @param template_set Set to inhirit attributes from
##' @param ... other attributes
##' @param attr_names_prefix "disambr_set_" by default. Only prefixed by it will be copied from `template_set` 
##' @inheritDotParams disambr_set_attr
##' @return 
##' 
##' @md 
##' @export 
disambr_add_set_attr <- function(focal_set
                               , template_set = NULL
                               , ...
                               , attr_names_prefix = "disambr_set_") {
    if(is.null(focal_set)) return()
    ## copy only disambr attr from template_set
    template_attr <- attributes(template_set)
    template_attr_disambr <- 
        stringi::stri_detect_regex(names(template_attr)
                                 , paste0("^", attr_names_prefix))
    template_attr <- template_attr[template_attr_disambr]
    mapply(function(a, name) {
        data.table::setattr(focal_set, name, a)
    }
  , template_attr
  , names(template_attr))
    ## remove file attributes
    disambr_set_attr(focal_set, file = NULL)
    ## add time stamp
    disambr_set_attr(focal_set, ts = Sys.time())
    ## add duration
    if(exists("disambr_start_time", where = parent.frame())) {
        disambr_start_time <- 
            get("disambr_start_time", pos = parent.frame())
        disambr_set_attr(focal_set
                       , duration = Sys.time() - disambr_start_time)
    }
    ## add recipe (procedure call) and name
    recipe <- attributes(template_set)$disambr_set_recipe
    procedure_call <- deparse(sys.calls()[[sys.nframe() - 1]])[[1]]
    procedure_name <-
        stringi::stri_extract_first_regex(procedure_call
                                        , c("^[^()]+"))
    procedure_short_name <-
        stringi::stri_replace_first_regex(procedure_name
                                        , paste0("^", attr_names_prefix), "")
    disambr_set_attr(focal_set
                   , name = procedure_short_name
                   , recipe = c(list(list(func = procedure_name
                                        , call = procedure_call))
                              , recipe))
    ## set attributes from ...
    disambr_set_attr(focal_set, ...)
    return(focal_set)
}
## --------<<  disambr_add_set_attr:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_save_set][disambr_save_set:1]]
##' Save set and adds file attribute
##' @param set_to_save set
##' @param save_set_as if TRUE the the file name is made from disambr_set_name attribute
##' @param save_set_prefix file prefix, default is "disambr-set."
##' @param save_set_dir file dir, default is "disambr-sets-rds" in current directory
##' @param use_time_stamp add time stamps at the end of file name, adds by default
##' @return file namej
##' 
##' @export 
disambr_save_set <- function(set_to_save
                           , save_set_as = getOption("disambr_save_as")
                           , save_set_prefix = getOption("disambr_save_set_prefix")
                           , save_set_dir = getOption("disambr_save_set_dir")
                           , use_time_stamp = getOption("disambr_save_set_time_stamp")) {
    ## do not save by default
    if(length(save_set_as) != 0) {
        ## make name if it is just TRUE
        if(isTRUE(save_set_as)) {
            save_set_as <- attr(set_to_save, "disambr_set_name")[[1]]
            ## use timestamps by default
            if(isTRUE(use_time_stamp) ||
               length(use_time_stamp) == 0) {
                save_set_as <-
                    paste0(save_set_as, "."
                         , format(Sys.time(), "%Y-%m-%dT%H-%M"))
            }
            save_set_as <- paste0(save_set_as, ".rds")
            if(length(save_set_prefix) == 0) {
                save_set_prefix <- "disambr-set."
            }
        }
        ## if "save as" provided use it, add prefix if it is provided as well
        if(is.character(save_set_as)) {
            if(length(save_set_prefix) != 0) {
                save_set_as <- paste0(save_set_prefix, save_set_as)
            }
            ## add directory or default
            if(length(save_set_dir) == 0) {
                save_set_dir <- "disambr-sets-rds"
            }
            dir.create(save_set_dir, showWarnings = FALSE, recursive = TRUE)
            save_set_as <- paste0(save_set_dir,"/",save_set_as)
            ## add file attribute
            disambr_set_attr(set_to_save, file = save_set_as)
            ## save
            saveRDS(set_to_save, file = save_set_as[[1]], compress = FALSE)
            disambr_message(paste0(
                "- set saved as '", save_set_as, "'"))
            return(save_set_as)
        } else {
            disambr_message(
                paste0("- do not know how to save 'set_to_save' as '"
                     , save_set_as, "'"))
            return()
        }
    } else {
        return()
    }
}
## --------<<  disambr_save_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_get_output_set][disambr_get_output_set:1]]
##' Gets output set from sets (in case we already made it)
##' @param sets sets
##' @param get_output_set Whether to search for output set. Default is not.
##' @param attr_names_prefix prefix for attributes
##' @return NULL or output set
##' 
##' @export 
disambr_get_output_set <- function(sets
                                 , get_output_set = getOption("disambr_get_output_set")
                                 , attr_names_prefix = "disambr_set_") {
    if(isTRUE(get_output_set)) {
        procedure_call <- deparse(sys.calls()[[sys.nframe() - 1]])
        procedure_name <-
            stringi::stri_extract_first_regex(
                         procedure_call, c("^[^()]+"))
        procedure_short_name <-
            stringi::stri_replace_first_regex(
                         procedure_name
                       , paste0("^", attr_names_prefix), "")
        output_set_index <-
            which(disambr_in_sets(sets, name = procedure_short_name))
        if(length(output_set_index) == 0) {
            return()
        } else if(length(output_set_index) == 1) {
            disambr_message(paste("- reusing output set:", procedure_short_name))
            return(sets[[output_set_index]])
        } else {
            disambr_message(paste("- reusing last output set:", procedure_short_name))
            output_set_index <- output_set_index[length(output_set_index)]
            return(sets[[output_set_index]])
        }
    } else {
        return()
    }
 }
## --------<<  disambr_get_output_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_read_output_set][disambr_read_output_set:1]]
##' Reads last output set saved on disk
##' @param read_output_set toggle. default is no
##' @param save_set_prefix file prefix
##' @param save_set_dir file dir
##' @param attr_names_prefix arrt prefix
##' @return 
##' 
##' @export 
disambr_read_output_set <- function(read_output_set = getOption("disambr_read_output_set")
                                  , save_set_prefix = getOption("disambr_save_set_prefix")
                                  , save_set_dir = getOption("disambr_save_set_dir")
                                  , attr_names_prefix = "disambr_set_") {
    if(isTRUE(read_output_set)){
        ## make defaults if not provided
        if(length(save_set_prefix) == 0) {
            save_set_prefix <- "disambr-set." 
        }
        if(length(save_set_dir) == 0) {
            save_set_dir <- "disambr-sets-rds"
        }
        ## output set name pattern
        procedure_call <- deparse(sys.calls()[[sys.nframe() - 1]])
        procedure_name <-
            stringi::stri_extract_first_regex(
                         procedure_call, c("^[^()]+"))
        procedure_short_name <-
            stringi::stri_replace_first_regex(
                         procedure_name
                       , paste0("^", attr_names_prefix), "")
        output_set_name_pattern <-
            paste0(save_set_prefix, procedure_short_name, ".*", "\\.rds")
        ## match last file
        output_set_file <- 
            list.files(save_set_dir, pattern = output_set_name_pattern)
        if(length(output_set_file) != 0) {
            ## take the last file (as they are sorted alphabetically)
            output_set_file <- output_set_file[length(output_set_file)]
            output_set_file <- file.path(save_set_dir, output_set_file)
            disambr_message(paste("- reusing saved set:", output_set_file))
            return(readRDS(file = output_set_file))
        } else {
            return()
        }
    } else {
        return()
    }
}
## --------<<  disambr_read_output_set:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_subsets][disambr_subsets:1]]
##' Filters list of sets
##' @param sets_list list of sets
##' @param attribute_value_list list of attribute values where list elements name correspond attribute names used for filtering sets
##' @param which_to_return whether to return "all", "first" or "last" set from filtered sets
##' @param negate_subsets whether to return sets that was not matched insted
##' @return list of sets or set if `which_to_return` is either "first" or "last"
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
## --------<<  disambr_subsets:1 ends here



## -------->>  [[file:../disambr.src.org::*disambr_setattr][disambr_setattr:1]]
## disambr_entity
## disambr_set_type
## disambr_set_coefficient
## disambr_set_name
## disambr_set_collection
## disambr_entity_id_reference
## disambr_entity_id_reference_md5_sum
## disambr_recipe
disambr_setattr <- function(focal_set, ...) {
    attr_value_list <- list(...)
    for (i in 1:length(attr_value_list)) {
        setattr(focal_set, names(attr_value_list)[i], attr_value_list[[i]])
    }
    return(focal_set)
}
## --------<<  disambr_setattr:1 ends here


