library(tidyverse)

excelPath <- "Helmikuu 2022/"
fullPath <- paste0("./", excelPath)
excelData <- list.files(path = fullPath, pattern = "*.xlsx") %>%
  map(function(thisFile) {
    path <- paste0("./", excelPath, "/", thisFile)
    show(path)
    return(readxl::read_xlsx(path))
  }) %>%
  reduce(rbind)
