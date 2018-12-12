#' For splitting strings using a vector of several possible splitting points.
#'
#' An adaptation of the strsplit function, which is only able to take a single character string for its split argument (when fixed = TRUE).
#'
#' @author Will Haese-Hill
#' @param x character vector, each element of which is to be split. Other inputs, including a factor, will give an error.
#' @param splits character vector containing regular expressions to use for splitting.
#' @return A character vector containing all characters in \eqn{x} that were split using \eqn{splits}.
#' @note Applies fixed = TRUE to underlying strsplit function. See strsplit documentation for explanation of this argument.
#' @export
#' @examples
#' x = c("a+b-c", "d*e/f")
#' multi_strsplit(x, splits=c('+', '-', '*', '/'))
#' # [1] "a" "b" "c" "d" "e" "f"



######################################################################################################
####                                      multi_strsplit                                               ####
######################################################################################################
# An extension of the strsplit function, allowing you to split a character string by a list of
# predefined splits, sequentially.
#
# Example of use:
#
        # x = c("a+b-c","d*e/f")
        # multi_strsplit(x,splits=c('+','-','*','/'))
        # [1] "a" "b" "c" "d" "e" "f"
#
#####################################################################################################

multi_strsplit <- function(x, splits){

  for (split in splits){
    x <- unlist(strsplit(x, split, fixed = TRUE))
  }
  return(x[!x == ""]) # Remove empty values

}

##############################################################
################ END OF multi_strsplit function ###################
##############################################################
