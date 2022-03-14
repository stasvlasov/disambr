## -------->>  [[file:../disambr.src.org::*analysis][analysis:1]]
##' Make a statistics for a collection of sets (specific algorithm) and writes to the file in dir
##' @param sets collection of sets
##' @param sets_dir dir with collection of sets if first arg is not provided, writes results here
##' @param name name
##' @param remove_data whether to remove first 4 sets (assumed to be a data) when sets arg is set
##' @param save_rds save_rds 
##' @return 
##' @export 
disambr_stats <- function(sets = NULL
                        , sets_dir = getOption("disambr_save_set_dir")
                        , name = sets_dir
                        , remove_data = TRUE
                        , save_rds = TRUE) {
    if(is.null(sets)){
    ## reads sets from directory
    sets <- 
        lapply(dir(sets_dir
                 , full.names = TRUE
                 , pattern = "disambr-set\\..*\\.rds$")
             , readRDS)
    } else if(remove_data){
        ## remove data from the sets
        sets <- sets[-c(1:4)]
    }

    sets_attr <- 
        lapply(sets, attributes)

    sets_attr_duration <- 
        lapply(sets_attr, `[[`, "disambr_set_duration")

    sets_attr_duration <-
        unlist(sapply(sets_attr_duration, as.numeric, units = "mins"))

    dur_mins <- sum(sets_attr_duration)

    strong_set <- 
        disambr_get_strong_set(sets)

    truth_set <- 
        disambr_get_truth_set(sets)

    ## mirror pairs for comparison
    strong <-
        unique(rbind(strong_set, strong_set[, .(author_id2
                                              , author_id1)]))

    truth <-
        unique(rbind(truth_set
                   , truth_set[, .(second
                                 , first)]))
    truth <- truth[, .(author_id1 = first
                     , author_id2 = second)]

    a <- nrow(fintersect(truth, strong))
    b <- nrow(fsetdiff(strong, truth))
    c <- nrow(fsetdiff(truth, strong))
    d <- nrow(sets[[2]]) * (nrow(sets[[2]]) - 1) - nrow(funion(truth, strong))

    pw_presision <- a / (a + b)
    pw_recall <- a / (a + c)
    pw_f1 <- (2 * pw_presision * pw_recall) / (pw_presision + pw_recall)
    pw_accuracy <-  (a + d) / (a + b + c + d)

    stats <- 
        list(name = name
           , true_positives = a
            , false_positives = b
           , false_negatives = c
           , true_negatives = d
          , pw_presision = pw_presision
           , pw_recall = pw_recall
          , pw_f1 = pw_f1
            , pw_accuracy = pw_accuracy
           , dur_mins = dur_mins
           , dur_sets = list(sets_attr_duration))

    ## save on disk
    if(save_rds){
        saveRDS(stats, paste0(sets_dir,"/", "disambr_stats.rds"))
    }
    return(stats)
}

##' Makes comparative table with statistics
##' @param sets_dir dir 
##' @return 
##' 
##' @md 
##' @export 
disambr_stats_table <- function(sets_dir){
files <- dir(sets_dir
           , recursive = TRUE
           , full.names = TRUE
           , pattern = "^disambr_stats\\.rds$")
stats_list <- lapply(files, readRDS)
data.table::rbindlist(stats_list)
}
## --------<<  analysis:1 ends here


