#' Compare  two rows and output as tibble
#'
#' @import tibble
#' @export
comp_jk <- function(df = NULL,var = NULL){
  if(nrow(df)>2) cat("too many rows: ",nrow(df),"\n")
  a <- compare::compare(df[1,],df[2,])
  a <- a$detailedResult
  a <- data.frame(t(unlist(a)))
  a <- as_tibble(a)
  a[[var]] <- df[[var]][1]
  return(a)
}
