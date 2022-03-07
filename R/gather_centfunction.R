#' Gather centrality estimates from contemporaneous and temporal
#' @param pcc_object object containing partial contemporaneous correlations
#' @param pdc_object object containing partial directed correlation
#' @return a dataframe with centrality estimates from both pcc and pdc networks
#' @export
#'
gather_cent <- function(pcc_object, pdc_object) {

  PCC_cent <- centralityTable(pcc_object) %>% filter(measure=="Strength")
  PCC_cent$type <- gsub("type 1", "PCC", PCC_cent$type)

  PDC_cent <- centralityTable(pdc_object) %>% filter(measure=="InStrength"|measure=="OutStrength")
  PDC_cent$type <- gsub("type 1", "PDC", PDC_cent$type)

  centr <- rbind(PCC_cent, PDC_cent)
  .GlobalEnv$centr <- centr
  centr
}
