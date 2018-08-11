

clean_who_number_spaces <- function(x){
  ## Change formating of numbers from xxx xxx to xxxxxx
  
  clean_triplet_pair <- 
    function(x) str_replace(x, "([0-9]{1,3}) ([0-9]{3})", "\\1\\2")
  number_of_iterations <- max(map_int(x, ~ str_count(., " ")))
  for (i in 1:number_of_iterations){
    x <- clean_triplet_pair(x)
  }
}

fix_who_column <- function(df, col1, col2, col3){
  
  ## Take col1 as input and output 3 column dataframe
  
  library(tidyverse)
  library(stringr)
  library(dplyr)
  
  pos_openbraces <- str_locate(df[[col1]], "\\[")
  pos_closebraces <- str_locate(df[[col1]], "\\]")
  pos_dash <- str_locate(df[[col1]], "-")
  
  pos_openbraces <- pos_openbraces[,1]
  pos_closebraces <- pos_closebraces[,1]
  pos_dash <- pos_dash[,1]
  
  
  df[[col2]] <- ifelse(pos_openbraces != "NA", str_sub(df[[col1]], pos_openbraces + 1, pos_dash - 1), "NA")
  
  df[[col3]] <- ifelse(pos_openbraces != "NA", str_sub(df[[col1]], pos_dash + 1, pos_closebraces - 1), "NA")
  
  df[[col1]] <- ifelse(pos_openbraces != "NA", str_sub(df[[col1]], end = pos_openbraces - 1), "NA")
  
  clean_who_number_spaces(df[[col1]])   
  clean_who_number_spaces(df[[col2]])
  clean_who_number_spaces(df[[col3]])
  
  result <- data.frame("Patient.count" = df[[col1]], "lower.bound" = df[[col2]], "upper.bound" = df[[col3]])
  result
}


