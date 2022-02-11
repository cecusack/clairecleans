#' @import measurements
#' @import mgsub
#' @import tidyverse
#' Wrangle weight data
#'
#' @param df A dataframe.
#' @param var Column name containing messy weight data.
#' @return
#' \itemize{
#'  \item Your dataframe containing a new column with fixed weights, named weight.
#'  \item A new dataframe titled weightflag that contains rows with weights that should be investigated for plausible values, errors, etc.
#' }
#' @examples
#' fixheight(dat, "weight")
#' fixheight(fake_data, "demo_10")
#' @export
#'
fixweight = function(df, var){
  object_name <- deparse(substitute(df))
  # input check
  if(missing(var)) stop("Specify the variable name in quotations that contains weight data to be fixed. e.g., fixweight(dat, 'var')")

  df <- df %>% dplyr::rename(weight_original=var) # rename var as weight_original
  df$weight <- df$weight_original # copy weight_original to reference column/not overwrite
  df <- df %>% dplyr::relocate(weight, .after=weight_original) # move it after weight col for easy viewing

  # vector of common issues
  pattern <- c("lbs", " lbs\\.", "pounds", "ish", "\\`", "\\~", "lb", "\\?", "\\>", " \\(estimate\\)", "\\(I don't weigh myself anymore\\)", " ", "around", "IB", "LBS","LB", "POUNDS", "ibs", ";bs", "Lbs", "ib", "ibs\\.", "\\+", "Ibs", "lB", "Ib", " \\;bs",  "\\;")
  fixed(pattern, ignore_case = TRUE)
  # remove pattern
  df$weight <- str_remove_all(df$weight, paste(pattern, collapse = "|"))

  # responses with "Kg", "KG", "kg"
  df[which(grepl("kg|Kg|KG|kilograms", df$weight)), "weight"] <-
    as.character(2.205*as.numeric(gsub("([0-9]+).*$", "\\1",df[which(grepl("kg|Kg|KG|kilograms", df$weight)), "weight"])))

  # stones. 1 stone=14 pounds
  df[which(grepl("stones$", df$weight)), "weight"] <-
    as.character(14 * as.numeric(gsub("([0-9]+).*$", "\\1",df[which(grepl("stones$", df$weight)),
                                                              "weight"])))

  # take the average of the range
  df$weight <- sapply(strsplit(gsub("[\\[\\]]","",df$weight,perl=T),"-"),function(x) mean(as.numeric(x)))

  # save this df to environment so it includes the new weight col
  assign(object_name, value = df, envir = globalenv())

  # create df with rows flagged for possible weight problems
  df$weightflag = NA
  df <- df %>%
    dplyr::mutate(df, weightflag =
                    ifelse(is.na(weight) & !is.na(weight_original), TRUE, # if original weight col had info but new weight col is NA
                           ifelse(weight < 75, TRUE, # if under 4 ft
                                  ifelse(weight > 400, TRUE, FALSE)))) # if over 6'5
  weightflag <- df[which(df$weightflag==TRUE), ] # keep people with possible probs
  weightflag <- weightflag[,-which(colnames(weightflag)=="weightflag")] # remove weight flag col

  # add to df with iffy weights to global env
  assign("weightflag", weightflag, envir = globalenv())

  # if there are no weight flagged rows, remove weight flag df from env
  to.rm <- unlist(eapply(.GlobalEnv, function(x) is.data.frame(x) && nrow(x) < 1))
  rm(list = names(to.rm)[to.rm], envir = .GlobalEnv)

  results <- list()
  results$conditionalmessage <- ifelse(nrow(weightflag)>0,
                                       "These are the values for weight. In your global environment, there is a new dataframe called 'weightflag' that contains rows with implausible weight values or should be looked into further.",
                                       "These are the values for weight. No rows had weight values identified as implausible. That said, you should still check weight with original weights to make sure all make sense. ")
  results$flaggedweights <- weightflag
  results$dat <- df %>% filter(!is.na(weight)) %>% dplyr::select(weight_original, weight)
  return(results)

}

