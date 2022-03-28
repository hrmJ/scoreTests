
getGoals  <- function(val, asString = FALSE){
  tryCatch({
    expr={
      gres <- NULL
      cleanedval <- gsub('(\\d{3,}|U\\d{2})', '', val)
      if(cleanedval == ''){
        return(0)
      }
      gres <- str_extract_all(cleanedval, '\\d{1,2}')[[1]]
      if(asString){
        return(paste0(gres[[1]], '-', gres[[2]]))
      }
      res <- sum(as.numeric(gres[[1]]), as.numeric(gres[[2]]))
      return (res)
    }
  },
  error = function(em){
    return(0)
  }
  )
}
