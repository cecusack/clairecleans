#' KPSS data frame function
#' @param df dataframe
#' @param vars option argument to specify a list of which vars to test
#' @return A dataframe containing the kpss test statistic, p value, and logical column of true or false where true indicates stationairyt met, and false means the stationarity assumption is potentially violated.
#' @export

kpss_df <- function(df, vars=NULL){
    if(is.null(vars)) {df = df} else {df = df[,vars]}
    df_kpss <- data.frame(vars = if(is.null(vars)) {vars = names(df) } else {vars},  # pull names
                        kpss_stat = sapply(df, function(v) kpss.test(ts(v), null = "Trend")$statistic), # kpss_stat
                        kpss_pval = sapply(df, function(v) kpss.test(ts(v), null = "Trend")$p.value)) # pval
    df_kpss$assump <- df_kpss$kpss_pval > 0.05 # true = assumption met, false = violated
    row.names(df_kpss) <- c()
    df_kpss
    }

