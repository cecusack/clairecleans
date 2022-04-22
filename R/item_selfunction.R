#' Select variables based on means and sds
#' @param df dataframe.
#' @param num Number of top means/sds to pull.
#' @return A list of the variables with the highest means and highest standard deviations.
#' @export
#'
item_sel <- function(df, num) {
  means <- colMeans(df, na.rm = TRUE)

  # create column for standard deviations
  sds <- apply(df, 2, sd, na.rm = TRUE)

  # organize in dataframe
  data_itemselection <- data.frame(means, sds)

  # sort by means first in descending order
  data_itemselection <- data_itemselection[order(-data_itemselection$means),]

  # select top row names
  topmeans_df <- data_itemselection %>% rownames_to_column() %>% top_n(num, means)
  topmeans <- data_itemselection %>% rownames_to_column() %>% top_n(num, means) %>% pull(rowname)
  topmeans # call to fill out DMS individual network log
  .GlobalEnv$names.means <- topmeans
  # sort by standard deviation
  data_itemselection <- data_itemselection[order(-data_itemselection$sds),]

  # select top sd row names
  topsds_df <- data_itemselection %>% rownames_to_column() %>% top_n(num, sds)
  topsds <- data_itemselection %>% rownames_to_column() %>% top_n(num, sds) %>% pull(rowname)
  topsds # call to fill out DMS individual network log
  .GlobalEnv$names.sd <- topsds

  onedf = cbind(topmeans_df, topsds_df)
  return(list(onedf, topmeans = topmeans, topsds = topsds))
}
