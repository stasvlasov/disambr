## [[file:~/org/research/disambr/disambr/disambr.src.org][]]
##' Returns sets of people ids that are defenetely different based on co-authorship
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.different.authors <- function(sets, procedures = NULL) {
    message("Starting disambr.get.different.authors...")
    if(!is.list(sets)) stop("sets should be list!")
    focal.set <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        ## TODO: implement extraction from different data type
        extract2(1)
    new.set <- focal.set %>%
        {split(1:nrow(.), .$paper_id)}
    ## set set's attributes
    attributes(new.set)$disambr.set.unit <- "person.distinct"
    attributes(new.set)$disambr.set.unit.ref.md5 <- digest(focal.set, "md5")
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>% disambr.get.different.authors
## dt[[4]]



##' Returns set of people with similar initials
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.similar.initials <- function(sets, procedures = NULL) {
    message("Starting disambr.get.similar.initials...")
    ## sapply(sets,attr, "disambr.set.unit")
    set.different.authors <-
        sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person.distinct") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    ## procedurs
    subset.similar.initials <- function(comb) {
        a <- set.different.authors[[comb[1]]]
        b <- set.different.authors[[comb[2]]]
        expand.grid(a, b)
    }
    new.set <- combn(1:length(set.different.authors), 2
                   , simplify = FALSE
                     ## , FUN = subset.similar.initials
                     )
    message("...combn produced ", length(new.set), " pairs of pubs")
    new.set <- pblapply(new.set, subset.similar.initials)
    message("...subset.similar.initials is done")
    new.set <- data.table::rbindlist(new.set)
    message("...rbindlist produced ", nrow(new.set), " pairs")
    new.set <- dplyr::mutate(new.set
                           , initials.dist =
                                 stringdist(set.data$author_initials[Var1]
                                          , set.data$author_initials[Var2]
                                          , method = "lv"))
    new.set <- dplyr::filter(new.set, initials.dist < 2)
    attributes(new.set)$disambr.set.unit <- "similar.initials"
    return(c(sets, list(new.set)))
}


## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials
## dt[[5]]



##' Returns set of people with similar last names
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.similar.last.names <- function(sets, procedures = NULL) {
    message("Starting disambr.get.similar.last.names...")
    ## TODO: extract teh set that we need here (person, dyads)
    set.similar.initials <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.initials") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        dplyr::mutate(
                   set.similar.initials
                 , last.name.dist =
                       stringdist(set.data$author_last_name[Var1]
                                , set.data$author_last_name[Var2]
                                , method = "dl"))
    new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "similar.last.names"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials %>%
    ## disambr.get.similar.last.names
## dt[[6]]


##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.same.email <- function(sets, procedures = NULL) {
    message("Starting disambr.get.same.email...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.email = mapply(function(var1, var2)
                          set.data$author_email[var1] == set.data$author_email[var2]
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.email"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials %>%
    ## disambr.get.similar.last.names %>% 
    ## disambr.get.same.email




##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.same.coauthor <- function(sets, procedures = NULL) {
    message("Starting disambr.get.same.email...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    fun <- function(var1, var2) {
        any(set.data[paper_id %in% paper_id[var1] &
                     !(author_name %in% author_name[var1])]$author_name %in% 
            set.data[paper_id %in% paper_id[var2] &
                     !(author_name %in% author_name[var2])]$author_name)
    }
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(
                   same.co.author = mapply(fun, Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.email"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials %>%
    ## disambr.get.similar.last.names %>% 
    ## disambr.get.same.coauthor

## dt[[7]]

##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.same.affiliation <- function(sets, procedures = NULL) {
    message("Starting disambr.get.same.affiliation...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.affiliations =
                          mapply(function(var1, var2)
                              any(set.data$author_affiliations[var1] %in% 
                                  set.data$author_affiliations[var2])
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.affiliation"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials %>%
    ## disambr.get.similar.last.names %>% 
    ## disambr.get.same.affiliation
## dt[[7]]$same.affiliations


##' Returns set of people with save email addresses
##' @param sets 
##' @param procedures 
##' @inheritDotParams 
##' @return 
##' 
##' @md 
##' @importFrom magrittr %>%
##' @import magrittr data.table dplyr stringr
##' @export 
disambr.get.3.refs.common <- function(sets, procedures = NULL) {
    message("Starting disambr.get.3.refs.common...")
    ## TODO: extract teh set that we need here (person, dyads)
    similar.last.names <- sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "similar.last.names") %>%
        extract2(1)
    set.data <-sets %>%
        extract(sapply(.,attr, "disambr.set.unit") == "person") %>%
        extract2(1)
    new.set <-
        similar.last.names %>% 
        dplyr::mutate(same.affiliations =
                          mapply(function(var1, var2)
                              any(set.data$author_affiliations[var1] %in% 
                                  set.data$author_affiliations[var2])
                          , Var1, Var2))
    ## new.set <- dplyr::filter(new.set, last.name.dist < 2)
    attributes(new.set)$disambr.set.unit <- "same.affiliation"
    return(c(sets, list(new.set)))
}

## my.file2 <- "../data/new_export/savedrecs-ms-recent.txt"
## dt <- disambr.read(my.file2) %>%
    ## disambr.get.different.authors %>%
    ## disambr.get.similar.initials %>%
    ## disambr.get.similar.last.names %>% 
    ## disambr.get.same.affiliation
## dt[[7]]$same.affiliations
 ## ends here
