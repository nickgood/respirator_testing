#___________________________________________________________
# libraries
library(readr)
library(dplyr)
library(lubridate)
library(tidyr)
library(kableExtra)
library(knitr)
library(purrr)
library(zoo)
library(ggplot2)
library(tinytex)
library(stringr)
library(rmarkdown)
#___________________________________________________________

#___________________________________________________________
read_metadata <- function(fldr = params$fldr){
  files <- list.files(fldr,
                      pattern = "^.*_metadata.*.csv$",
                      ignore.case = TRUE,
                      full.names=TRUE)
  
  purrr::map(files,
             read_csv,
             col_types = cols(.default = "c")) %>%
    bind_rows() %>%
    mutate_all(tolower)
}
#___________________________________________________________

#___________________________________________________________
read_data <- function(fldr = params$fldr, id = params$id, tst = params$tst){
  f <- str_subset(tst, regex(paste0("^.*_m", id, ".*.csv$"), ignore_case = TRUE))
  purrr::map(f, read_niosh) %>%
  bind_rows() %>%
  rename(penetration_rt = "penetration")
}
#___________________________________________________________

#___________________________________________________________
# read file
read_niosh <- function(file){
  read_csv(file, col_types = cols()) %>%
  mutate(rep = as.numeric(sub(".*_r(.*)_.*", "\\1", basename(file))))
}
#___________________________________________________________


#___________________________________________________________
clean_names <- function(x){
    x <- tolower(gsub("mg-m3", "conc", x))
    x <- tolower(sub("drx", "ref", x))
    x <- tolower(sub("dusttrak2", "sample", x))
    x
}
#___________________________________________________________
    
#___________________________________________________________
clean_data <- function(x){
  x %>% 
  rename_all(clean_names) %>%
  mutate(datetime = as.POSIXct(time,
                              origin = "1970-01-01",
                              tz = "US/Mountain"),
         dp_mmh2o  = 25.4 * dp_inh20,
         ref_volt = ifelse(ref_volt <= 5,
                           ref_volt, 5),
         sample_volt = ifelse(sample_volt <= 5,
                                 sample_volt, 5)) %>%
    select(-time)
}
#___________________________________________________________





