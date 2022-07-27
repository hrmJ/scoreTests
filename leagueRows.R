# install.packages("pbapply")
# install.packages("plotly")

library(tidyverse)
library(pbapply)

getStreak <- function(allMatches, index, prevRow, streakLength) {
  if (is.null(prevRow)) {
    prevRes <- NULL
  } else {
    prevRes <- as.character(prevRow$success)
  }
  if (index < 1) {
    thisRow <- allMatches[1, ]
    return(list(streakLength = streakLength, streakType = prevRes, index = index, parsedTime = thisRow$parsedTime))
  }
  thisRow <- allMatches[index, ]
  currentRes <- as.character(thisRow$success)
  sameResult <- FALSE
  noPrevResult <- is.null(prevRes)
  if (!noPrevResult) {
    sameResult <- currentRes == prevRes
  }
  if (noPrevResult | sameResult) {
    return(getStreak(allMatches, index - 1, thisRow, streakLength + 1))
  }
  return(list(streakLength = streakLength, streakType = prevRes, index = index, parsedTime = prevRow$parsedTime))
}

getAllStreaks <- function(allMatches, index, leagueName, allStreaks = tibble(streakLength = NA, streakType = "", parsedTime = NA, league = "")) {
  matchesLeft <- head(allMatches, index)
  if (index > 0 & nrow(matchesLeft) > 0) {
    streak <- getStreak(matchesLeft, nrow(matchesLeft), NULL, 0)
    return(getAllStreaks(matchesLeft, streak$index, leagueName, allStreaks %>% add_row(streakLength = streak$streakLength, streakType = streak$streakType, parsedTime = streak$parsedTime, league = leagueName)))
  }
  return(allStreaks[-1, ])
}


getStreakData <- function(sourceData, earliest = "2021-11-01 00:00", asList = T, filteredLeagues = NULL) {
  tempData <- sourceData %>%
    mutate(success = case_when(
      forebet == "1" & homeGoals > awayGoals ~ "H",
      forebet == "X" & homeGoals == awayGoals ~ "X",
      forebet == "2" & homeGoals < awayGoals ~ "A",
      TRUE ~ "FAIL"
    )) %>%
    mutate(success = as.factor(success))

  leagues <- sourceData %>%
    distinct(countryAndLeague) %>%
    pull(countryAndLeague)

  if(!is.null(filteredLeagues)){
    leagues <- leagues[leagues %in% filteredLeagues]
  }

  inTimeFrame <- tempData %>% filter(parsedTime > earliest)


  streakResults <- pblapply(leagues, function(l) {
    allMatches <- inTimeFrame %>%
      filter(countryAndLeague == l) %>%
      arrange(desc(parsedTime))
    if (nrow(allMatches) == 0) {
      return(NULL)
    }
    allStreaks <- getAllStreaks(allMatches, nrow(allMatches), l)
    return(allStreaks)
  })

  streakResultsFlat <- streakResults %>% bind_rows() 

  if (asList) {
    return(streakResults)
  }
  return(streakResultsFlat)
}
