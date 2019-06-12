prepare_rpart4code <- function(tree){
  frame <- tree$frame
  final_tree_output <- NULL
  # the depth of each node, giving the final tree structure
  depth <- rpart:::tree.depth(as.numeric(row.names(frame)))
  frame$row_name <- as.numeric(row.names(frame))
  frame <- as_tibble(frame) %>% select(row_name, var, yval, ncompete, nsurrogate)
  frame$depth <- depth
  frame <- frame %>% mutate(yval = yval -1)
  splits <- labels(tree, digits = getOption("digits"), minlength = 0L)
  # now we will replace the operators to make them separated
  splits <-
    splits %>%
    str_replace("<"," <") %>%
    str_replace(">"," >") %>%
    str_replace("="," = ") %>%
    str_replace("> =",">=") %>%
    str_replace("< =","<=")
  frame$split <- splits
  # its crucial that the different levels of categories don't have
  # we want to determine which ones are leafs(terminal nodes) and get all the rules that lead there

  frame$row_order <- 0:(nrow(frame)-1)
  frame <- filter(frame,row_order != 0)
  frame <- mutate(frame, terminal = (var == "<leaf>"))

  #now we need to separate the variable, the values and the sign
  vars_and_vals <-
    frame$split %>%
    str_split(pattern = " = | >= | <= | > | < ") %>%
    lapply(function(x) tibble(variable = x[1], value = x[2])) %>%
    bind_rows()
  frame <-
    frame %>%
    bind_cols(vars_and_vals)

  frame <-
    frame %>%
    mutate(sign = as.character(str_match(split,pattern = " = | >= | <= | > | < ")))
  return(frame)
}
