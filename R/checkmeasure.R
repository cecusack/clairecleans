#' Check the range of responses
#' @import measurements
#' @import mgsub
#' @import tidyverse
#' @param x Measure/s you want to check.
#' @param min Minimum value for the item/s
#' @param max Minimum value for the item/s
#' @return The mean, standard deviation, minimum, and maximum value for items. A message indicating which items have observations outside of the possible range of values for that item. "Pass" in columns titled min_check and max_check indicate all values are at or above the minumum value and at or below the maximum value. "CHECK" indicates that values fall outside of the possible range.
#' @examples
#' check_measure(fake_data$edeq6_1, 0, 6)
#' check_measure(fake_data[,grep('bdi_[0-9]{1,2}_clinical', colnames(fake_data))],0,6)
#' check_measure(fake_data[,c(41,48)], 0,3)
#' check_measure(fake_data[,c(which(colnames(fake_data)=="edeq6_1"), which(colnames(fake_data)=="ede6_6"))], 0,6)
#' @export
#'
check_measure=function(x, min, max){
  # input check
  if(missing(x)) stop("Specify the variable(s) you want to check (e.g., dat$var).")
  if(missing(min)) stop("Specify the minimum possible value for this variable.")
  if(missing(max)) stop("Specify the maximum possible value for this variable.")

  { valid <- function(x) {  sum(!is.na(x))}

    if(is.vector(x)){
      stats = data.frame(matrix(rep(NA,5),ncol=5) )   #create a temporary arraystats[1, 1] <-  mean(x, na.rm=na.rm )
      stats[1,1]=""
      stats[1, 2]=round(mean(x,na.rm=TRUE),2)
      stats[1, 3]=round(sd(x,na.rm=TRUE),2)
      stats[1, 4]=min(x,na.rm=TRUE)
      stats[1, 5]=max(x,na.rm=TRUE)
      names(stats)=c("", "mean", "sd", "min", "max")
      #round(stats)
    } else {
      statnames=names(x)
      len = dim(x)[2]     #do it for matrices or data.frames
      stats = data.frame(matrix(ncol=5,nrow=as.numeric(len)))
      names(stats)=c("var", "mean", "sd", "min", "max")
      for (i in 1:len) {
        stats[i, 2] <-  round(mean(x[,i], na.rm=TRUE),2)
        stats[i, 3] <- round(sd(x[,i], na.rm=TRUE),2)
        stats[i, 4] <-  min(x[,i], na.rm=TRUE)
        stats[i, 5] <-  max(x[,i], na.rm=TRUE)
        stats[i, 1] = statnames[i]
      }
    }}

  stats$min_check=ifelse(stats$min<min, "CHECK", "Pass")
  stats$max_check=ifelse(stats$max>max, "CHECK", "Pass")


  descript=stats

  results=list()
  results$descriptives=descript
  results$conditionalmessage=ifelse(any(descript$min<min|descript$max>max),
                                    paste0("these are the variables to check: ", paste0(unlist(stats[which(stats$max_check=="CHECK")|which(stats$max_check=="CHECK"),1]), collapse = ", ")),
                                    "All observations are within the possible range for this variable.")
  return(results)
}
