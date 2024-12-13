#' Add dayvar and beepvar to time series dataframes
#' @import tidyverse
#' @import sjmisc
#' @import purrr
#' @param df dataframe with time series data.
#' @param idvar The name of the column that contains participant ID variable
#' @param datetimevar Column name with date and time. Usually generated from Ethica or REDCap
#' @return Dataframe with columns numbering the dates (day), times (beep), and consecutive beeps (beep_consec). To save this dataframe point it toward an object.
#' @export
#'
add_daybeep <- function(df, idvar, datetimevar){
  object_name <- deparse(substitute(df))
  #datetimevar <- deparse(substitute(datetimevar))

  # input check
  if(missing(df)) stop("Enter the dataframe that needs a numbered day column and numbered beep column")
  if(missing(idvar)) stop("Enter the name of the column that contains participant ID variable in single quotes, e.g., 'id'")
  if(missing(datetimevar)) stop("Enter the name of the column that contains the date and time field in single quotes, e.g., 'Scheduled.Time'")

  df[["startdate"]] <- df[[datetimevar]]
  # df[["startdate"]] <- df[[datetimevar]]  # copy datetime var into new col startdate
  # df <- df %>% dplyr::relocate(datetimevar, .before= startdate) # move startdate after datetime

  # remove timestamp specifier if it exists
  df$startdate <- ifelse(grepl("\\D$", df[["startdate"]]), gsub("\\s*\\w*$", "", df[["startdate"]]), df[["startdate"]])

  df <- df[order(df[[idvar]], df[["startdate"]]), ] # order by id and datetime

  df[["starttime"]] <- format(as.POSIXct(df[["startdate"]],format="%Y-%m-%d %H:%M:%S"),"%H:%M:%S") # split into time only
  df[["startdate"]] <- format(as.POSIXct(df[["startdate"]],format="%Y-%m-%d %H:%M:%S"),"%Y-%m-%d") # split into date only

  list <- split(df, df[[idvar]])
  for(i in seq_along(list)){
    #### day ####
    list[[i]]$startdate <- lubridate::ymd(list[[i]]$startdate)
    list[[i]]$day <- (list[[i]]$startdate-first(list[[i]]$startdate))+1
    list[[i]] <- list[[i]] %>% relocate(day, .after = starttime)
    list[[i]]$day <- gsub(" days", "", list[[i]]$day)
    list[[i]]$day <- as.numeric(list[[i]]$day)

    #### beep ####
    list[[i]] <- list[[i]] %>%
      group_by(day) %>%
      mutate(beep=1:n()) %>%
      ungroup() %>%
      relocate(beep, .after=day)

    #### beep consecutive ####
    list[[i]]$beep_consec <- 1:nrow(list[[i]])
  }

  df <- do.call("rbind", list) # list to df
  row.names(df) <- NULL

  # datetime=deparse(substitute(datetime))
  df <- df %>% sjmisc::move_columns(startdate, starttime, day, beep, beep_consec, .after={{datetimevar}})

  return(df)
  # assign(object_name, df, envir = globalenv())
}
