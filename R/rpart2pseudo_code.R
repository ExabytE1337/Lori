#' Metaprogramming function to return java code from rpart object
#'
#' @param frame This should be rpart$frame that has been supplied by prepare_rpart4code.R
#' @param f_depth Please don't change this. It's only meant to change itself during recursion.
#' @param f_row_order Please don't change this. It's only meant to change itself during recursion.
#' @examples
#' prepare_rpart4code(tree_2010_2015) %>% rpart2pseudo_code() %>% message()
rpart2pseudo_code <- function(frame, f_depth = 1, f_row_order = 1, sep_char = " ", english_output = c("not-MCU","MCU")){
  frame_subset_1 <-
    frame %>%
    filter(depth == f_depth, row_order >= f_row_order) %>%
    slice(1)

  frame_subset_2 <-
    frame %>%
    filter(depth == f_depth, row_order >= f_row_order) %>%
    slice(2)
  if(length(as.vector(tree_2010_2015$obsLevels))>2) english_output <- 1:length(as.vector(tree_2010_2015$obsLevels))

  output_row_1 <- paste0(paste(rep(sep_char,(f_depth-1)*2),collapse = ""),"if(",frame_subset_1$split,"){\n")
  output_row_3 <- paste0(paste(rep(sep_char,(f_depth-1)*2),collapse = ""),"} else{\n")

  if(frame_subset_1$terminal){
    output_row_2 <- paste0(paste(rep(sep_char,f_depth*2),collapse = ""),"return ",english_output[frame_subset_1$yval + 1],";\n")
  } else{
    #output_row_2 <- paste0(paste(rep(".",f_depth*2),collapse = ""),"recursion }\n")
    #recursive call with increased depth and increased row_order based on the larger number than that row_order
    output_row_2 <- rpart2pseudo_code(frame, f_depth = f_depth + 1, f_row_order = frame_subset_1$row_order)
  }

  if(frame_subset_2$terminal){
    output_row_4 <- paste0(paste(rep(sep_char,f_depth*2),collapse = ""),"return ",english_output[frame_subset_2$yval + 1],";\n")
  } else{
    #output_row_4 <- paste0(paste(rep(".",f_depth*2),collapse = ""),"recursion2 }\n")
    #recursive call with increased depth and increased row_order based on the larger number than that row_order
    output_row_4 <- rpart2pseudo_code(frame, f_depth = f_depth + 1, f_row_order = frame_subset_2$row_order)
  }
  output_row_5 <- paste0(paste(rep(sep_char,(f_depth-1)*2),collapse = ""),"}\n")
  return(paste0(output_row_1,output_row_2,output_row_3,output_row_4,output_row_5))
}
