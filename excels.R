library(tidyverse)
source("./utils.R")

excelPath <- "Helmikuu 2022/"
fullPath <- paste0("./", excelPath)
excelData <- list.files(path = fullPath, pattern = "*.xlsx") %>%
  map(function(thisFile) {
    path <- paste0("./", excelPath, "/", thisFile)
    show(path)
    return(readxl::read_xlsx(path))
  }) %>%
  reduce(rbind) %>%
  mutate(goals = sapply(`Total Score`, getGoals)) %>%
  mutate(parsedResult = sapply(`Total Score`, getGoals, asString = TRUE)) %>%
  mutate(homeGoals = as.numeric(gsub("^(\\d+).*", "\\1", parsedResult))) %>%
  mutate(awayGoals = as.numeric(gsub(".*-(\\d+)$", "\\1", parsedResult))) %>%
  mutate(parsedTime = gsub("^(\\d+)\\w+", "\\1", Date)) %>%
  mutate(parsedTime = gsub(" [A-Z]* \\(.*", "", parsedTime)) %>%
  mutate(parsedTime = parse_datetime(parsedTime, "%d %b %Y, %H:%M")) %>%
  mutate(htGoals = sapply(`1st Half Score`, getGoals)) %>%
  mutate(HTparsedResult = sapply(`1st Half Score`, getGoals, asString = TRUE)) %>%
  mutate(HThomeGoals = as.numeric(gsub("^(\\d+).*", "\\1", HTparsedResult))) %>%
  mutate(HTawayGoals = as.numeric(gsub(".*-(\\d+)$", "\\1", HTparsedResult)))



# getFirstNForDay(excelData, 1)
