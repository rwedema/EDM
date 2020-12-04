library(dplyr)
library(readr)
library(tidyr)
library(stringr) 

data_dir <- '../data/'


data_list <- list()
my_files <- c(
  'Chemotaxis Dicty RasC.txt',
  'Chemotaxis Dicty Ric8.txt',
  'Chemotaxis Dicty Roco4_BACKGROUND.txt',
  'Chemotaxis Dicty Galpha8_GppNHp.txt',
  'Chemotaxis Dicty Gbeta1.txt',
  'Chemotaxis Dicty Galpha8_GDP.txt',
  'Chemotaxis Dicty RapA.txt',
  'Chemotaxis Dicty Galpha2_starv.txt',
  'Chemotaxis Dicty Gbeta2.txt',
  'Chemotaxis Dicty Galpha4.txt',
  'Chemotaxis Dicty Galpha8.txt',
  'Chemotaxis Dicty RasB.txt',
  'Chemotaxis Dicty Rac1.txt',
  'Chemotaxis Dicty RasG1.txt',
  'Chemotaxis Dicty Galpha2_veg.txt'
)


get_dicty_data <- function(){
  for (file_name in my_files){
    spdat <- read_delim(paste0(data_dir, file_name), col_names=F, delim="\t", skip=1)
    names(spdat) <- c('long_id','uniprot','mw','is_grouping','spectral_count')
    
    exp_name <- gsub(file_name, pattern="Chemotaxis Dicty ", replace="") %>% gsub(pattern="\\.txt", replace="")
    split_expname <- strsplit(exp_name, split = "_")
    
    # will introduce na's as there are ? for kDa
    spdat <- spdat %>% mutate(mw = as.numeric(gsub("kDa", "", mw)))
    
    # split bait and condition, i.e. Galpha2_veg
    spdat <- spdat %>% mutate(bait = split_expname[[1]][1])
    
    # when no condition could be split, the condition was normal
    spdat <- spdat %>% mutate(condition = split_expname[[1]][2]) %>% replace_na(list(condition = "Normal"))
    
    data_list[[file_name]]<- spdat %>% select(long_id, uniprot, mw, is_grouping, spectral_count, bait, condition)
  }

  all_data <- bind_rows(data_list)
}

all_data <- get_dicty_data()
