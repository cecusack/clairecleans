#' Add dayvar and beepvar to time series dataframes
#' @import tidyverse
#' @import sjmisc
#' @import purrr
#' @param df dataframe with time series data.
#' @param id The name of the column that contains participant ID variable
#' @param datetime Column name with date and time. Usually generated from Ethica or REDCap
#' @return Your dataframe with columns numbering the dates (dayvar), times (beepvar), and consecutive beeps (beepconsec).
#' @export
#'
add_daybeep <- function(df, id, datetime){
  object_name <- deparse(substitute(df))

  # input check
  if(missing(df)) stop("Enter the dataframe that needs a numbered day column and numbered beep column")
  if(missing(id)) stop("Enter the name of the column that contains participant ID variable in quotes, e.g., 'id'")
  if(missing(datetime)) stop("Enter the name of the column that contains the date and time field in quotes, e.g., 'Scheduled.Time'")

  df[[id]]=factor(df[[id]])
  df$date <- df[[datetime]]
  df <- df %>% separate(date, c("date","time"), sep = " ", fill = "right")
  df$date <- as.Date(df$date, "%Y-%m-%d")

  if(length(unique(df[[id]]))>1){
    list <- lapply(split(df, df[[id]]), data.frame, stringsAsFactors = FALSE)
    list <- lapply(list, function(df){df[order(df[[datetime]]),]}) # put rows in order by time

    # add a beep var across list
    list <- map(list, ~ .x %>%
                  group_by(date) %>%
                  mutate(beepvar = seq(1:n())) %>%
                  ungroup())

    # add a day var across list
    for(i in seq_along(list)){
      list[[i]]$dayvar <- floor(as.numeric(difftime(list[[i]]$date, list[[i]]$date[1], units="days")))+1}

    # rbind list back into one dataframe
    df <- do.call("rbind", list)
    # move columns to a place I think makes more sense

    df$num<-1:nrow(df)
    df$beepconsec <-ave(df$num,df[[id]],FUN = seq_along)
    df <- df[-which(colnames(df)=="num")]

    datetime=deparse(substitute(datetime))
    df <- df %>% sjmisc::move_columns(date, time, dayvar, beepvar, .after=datetime)
    assign(object_name, df, envir = globalenv())

  } else{

    df <- df[order(df[[datetime]]),]
    # add beep var
    df <- df %>% group_by(date) %>%
      mutate(beepvar = seq(1:n())) %>%
      ungroup()
    # add consecutive beeps
    df <- df %>% dplyr::mutate(beepconsec = seq(1:n())) %>% relocate(beepconsec, .after = beepvar)
    # add day var
    df$dayvar <- floor(as.numeric(difftime(df$date, df$date[1], units="days")))+1
    assign(object_name, df, envir = globalenv())
  }
  return(df %>% select(id, dayvar, beepvar, beepconsec))
}
