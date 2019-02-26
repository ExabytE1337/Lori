#' Compare 2-row grouped subsets of distinct dataframe
#'
#' @param data
#' @param g_by
#' @param distinct
#' @import tibble
#' @import dplyr
#' @import crayon
#' @export
comp2_distinct <- function(data = NULL, g_by = NULL, distinct = F){
  if(distinct) df <- data %>% distinct
  else df <- data
  output <- NULL
  cat("Comparing first two rows from sub_dataframes grouped by
      variable:",red(g_by),".\n")
  output$df <- df %>%
    group_by((!!as.symbol(g_by))) %>%
    filter(n()==2) %>%
    do(comp_jk(.,g_by))
  output$diffs <- colnames(data)[unname(colSums(output$df)<nrow(output$df))[-1]]
  return(output)
}
