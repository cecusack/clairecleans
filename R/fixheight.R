# wow, we have problems with height. data validation up front is easier but here's a function that will solve most problems
#' @import measurements
#' @import mgsub
#' @import tidyverse
#' Wrangle height data
#'
#' @param df A dataframe.
#' @param var Column name containing messy height data.
#' @return
#' \itemize{
#'  \item Your dataframe containing a new column with fixed heights, named height.
#'  \item A new dataframe titled heightflag that contains rows with heights that should be investigated for plausible values, errors, etc.
#' }
#' @examples
#' fixheight(dat, "height")
#' fixheight(fake_data, "demo_9")
#' @export
fixheight = function(df, var){
  object_name <- deparse(substitute(df))
  # input check
  if(missing(var)) stop("Specify the variable name in quotations that contains height data to be fixed. e.g., fixheight(dat, 'var')")

  df <- df %>% dplyr::rename(height_original=var) # rename var as height_original
  df$height <- df$height_original # copy height_original to reference column/not overwrite
  df <- df %>% dplyr::relocate(height, .after=height_original) # move it after height col for easy viewing

  df$height_is_standard <- NA # place holder for logical standard or metric unit
  # df$height_is_standard <- grepl("\\d*\\D\\d|\\d*\\D", df$height)

  df <- df %>%
    dplyr::mutate(df, height_is_standard = # fill in standard or metric T/F
             ifelse(grepl("^\\d{3}$|cm|m", height), FALSE, TRUE)) # if people wrote cm or m, height_is_standard == FALSE

  # two numbers American standard (e.g., "6'3', 6'3'')
  df[which(df$height_is_standard==TRUE), "height"] <- 12 * as.numeric(gsub("\\D.*","",df[which(df$height_is_standard==TRUE), "height"])) +
    as.numeric(gsub("^\\d+\\D*(\\d*)\\D*","\\1",df[which(df$height_is_standard==TRUE), "height"]))

  # people who spelled out numbers (e.g., "five foot two inches")
  df$height <- mgsub::mgsub(df$height_original, c("one", "two", "three", "four", "five", "six", "seven", "eight", "nine", "ten", "eleven"),
                            c("1", "2", "3", "4", "5", "6", "7", "8", "9", "10", "11"))

  df[which(df$height_is_standard==TRUE), "height"] <- 12 * as.numeric(gsub("\\D.*","",df[which(df$height_is_standard==TRUE), "height"])) +
    as.numeric(gsub("^\\d+\\D*(\\d*)\\D*","\\1", df[which(df$height_is_standard==TRUE), "height"]))

  # people who wrote height in american standard but included a half inch (e.g., "5'1.5")
  df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]", df$height_original)), "height"] <-
    12 *as.numeric(gsub("\\D.*", "", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]", df$height_original)), "height_original"]))+
    as.numeric(gsub("\\d+\\D*(\\d*)(\\D*\\d)", "\\1", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]", df$height_original)), "height_original"]))+
    as.numeric(gsub("(^\\d+\\D*)(\\d*)(\\D*\\d)", "\\3", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]", df$height_original)), "height_original"]))

  # people who wrote one number for height (e.g., 5)
  df[which(grepl("^[0-9]${1}", df$height_original)), "height"] <- 12*as.numeric(df[which(grepl("^[0-9]${1}", df$height_original)), "height_original"])

  # people who wrote one number for height followed by foot or feet or some other word (e.g., "5 foot)
  df[which(df$height_is_standard==TRUE & grepl("^\\d+?\\s\\w+$", df$height_original)), "height"] <- 12*as.numeric(gsub("\\D.*", "", df[which(df$height_is_standard==TRUE & grepl("^\\d+?\\s\\w+$", df$height_original)), "height_original"]))

  # people who report height in cm (e.g., "168 cm")
  df[which(df$height_is_standard==FALSE), "height"] <- conv_unit(as.numeric(gsub("\\D", "", df[which(df$height_is_standard==FALSE), "height"])), from = "cm", to= "inch")
  # df[which(grepl("[0-9]{3}$", df$height_original)), "height"] <- conv_unit(as.numeric(gsub("\\D", "", df[which(grepl("[0-9]{3}$", df$height_original)), "height"])), from = "cm", to= "inch")

  # people who write their height as "5 8" so like no punctuation
  df[which(df$height_is_standard==TRUE & grepl("^\\d+?\\s\\d", df$height_original)), "height"] <- 12*as.numeric(gsub("\\D.*", "", df[which(df$height_is_standard==TRUE & grepl("^\\d+?\\s\\d", df$height_original)), "height_original"])) +
    as.numeric(gsub("^\\d+?\\s(\\d)", "\\1", df[which(df$height_is_standard==TRUE & grepl("^\\d+?\\s\\d", df$height_original)), "height_original"]))

  # people who wrote height like 68 inches
  df[which(df$height_is_standard==TRUE & grepl("^\\d{2}", df$height_original)), "height"] <- gsub(" .*$", "", df[which(df$height_is_standard==TRUE & grepl("^\\d{2}", df$height_original)), "height_original"])

  # people who wrote their height like 5'
  df[which(df$height_is_standard==TRUE & grepl("^\\d\\'$", df$height_original)), "height"] <- 12* as.numeric(gsub("'.*$", "", df[which(df$height_is_standard==TRUE & grepl("^\\d\\'$", df$height_original)), "height_original"]))

  # people who wrote their height like 5'2''
  df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]\\D", df$height_original)), "height"] <- 12 *as.numeric(gsub("\\D.*", "", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]\\D", df$height_original)), "height_original"]))+
    as.numeric(gsub("\\d\\D(.*)\\..*", "\\1", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]\\D", df$height_original)), "height_original"]))+
    as.numeric(gsub("\\d+\\D*(\\d*)(\\D*\\d)(\\D*)", "\\2", df[which(grepl("[0-9]'[0-9]{1,2}[.][0-9]\\D", df$height_original)), "height_original"]))

  # people who write their height like 5'3 1/2 WHYYYYY
  df[which(grepl("^\\d'(\\d )(\\d\\D\\d$)", df$height_original)), "height"] <- gsub(" 1/2", ".5", df[which(grepl("^\\d'(\\d )(\\d\\D\\d$)", df$height_original)), "height_original"])

  # step two multiple and add
  df[which(grepl("^\\d'(\\d )(\\d\\D\\d$)", df$height_original)), "height"] <- 12*as.numeric(gsub("\\D.*", "\\1", df[which(grepl("^\\d'(\\d )(\\d\\D\\d$)", df$height_original)), "height_original"])) +
    as.numeric(gsub("^\\d'(\\d.*)", "\\1", df[which(grepl("^\\d'(\\d )(\\d\\D\\d$)", df$height_original)), "height"]))

  # remove height_is_standard column
  df <- df[,-which(colnames(df)=="height_is_standard")]
  # tidy up
  df$height <- mgsub::mgsub(df$height, c("'", "in"),
                            c("", ""))
  df$height <- as.numeric(df$height) # make numeric
  df$height <- round(df$height, digits = 2) # round 2 decimal places


  assign(object_name, value = df, envir = globalenv()) # save this df to environment so it includes the new height column
  # df$height <- ifelse(df$height < 40 | df$height > 200, "implausible value", df$height )
  # if( any(df$height=="implausible value") ) warning('you have implausible heights')

  # create df with rows flagged for possible height problems
  df$heightflag = NA
  df <- df %>%
    dplyr::mutate(df, heightflag =
             ifelse(is.na(height) & !is.na(height_original), TRUE, # if original height col had info but new height col is NA
                    ifelse(height < 48, TRUE, # if under 4 ft
                           ifelse(height > 77, TRUE, FALSE)))) # if over 6'5
  heightflag <- df[which(df$heightflag==TRUE), ] # keep people with possible probs
  heightflag <- heightflag[,-which(colnames(heightflag)=="heightflag")] # remove height flag col

  # add to df with iffy heights to global env
  assign("heightflag", heightflag, envir = globalenv())

  # if there are no height flagged rows, remove height flag df from env
  to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && nrow(x) < 1))
  rm(list = names(to.rm)[to.rm], envir = .GlobalEnv)

  results <- list()
  results$conditionalmessage <- ifelse(nrow(heightflag)>0,
                                       "These are the values for height. In your global environment, there is a new dataframe called 'heightflag' that contains rows with implausible height values or should be looked into further",
                                       "These are the values for height. No rows had height values identified as implausible. That said, you should still check height with original heights to make sure all make sense.")
  results$flaggedheights <- heightflag
  results$dat <- df %>% dplyr::select(height_original, height)
  return(results)

}
