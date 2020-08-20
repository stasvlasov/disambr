## [[file:~/org/research/disambr/disambr/disambr.src.org::*get.file.extension][get.file.extension:1]]
## my own file.extention extractor
get.file.extension <- function(f) {
    if(length(f) == 1) {
        if(is.character(f)) {
            f %>% basename %>% 
                stri_split_fixed(".") %>% 
                extract2(1) %>%
                extract(ifelse(length(.) == 1, NA, length(.))) %>%
                ifelse(is.na(.), "", .)
        } else if(is.na(f)) {
            NA
        }
    } else {
        NULL
    }
}

get.file.extension(my.file)
get.file.extension(my.file1)
get.file.extension("sdfsdf....")
get.file.extension("sdf")
get.file.extension("")
get.file.extension(NULL)
get.file.extension(NA)
get.file.extension("...sdf...sdf.df...sd.")
get.file.extension(".")
get.file.extension(".....")

## build in
file_ext(my.file)
file_ext(my.file1)
file_ext("sdfsdf....")
file_ext("sdf")
file_ext("")
file_ext(NULL)
file_ext(NA)
file_ext("...sdf...sdf.df...sd.")
file_ext(".")
file_ext(".....")
## get.file.extension:1 ends here
