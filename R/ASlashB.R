#' Returns elements of vector \eqn{x} that are not contained within vector \eqn{y}.
#'
#' @author Will Haese-Hill
#' @param x vector with values to be matched.
#' @param y vector with values to match against vector \eqn{x}.
#' @return A vector of all values contained in vector \eqn{x} that could not be located in vector \eqn{y}.
# @seealso \code{\link{"%in%"}} which this function is virtually opposite to.
#' @examples
#' union_jack = c("red","white","blue")
#' rainbow = c("red","orange","yellow","green","blue","indigo","violet")
#' union_jack %a/b% rainbow # gives "white".
#' # i.e. the only colour in the Union Jack that is not contained in the rainbow is "white".
#' @usage x \%a/b\% y
#' @rdname ASlashB
#' @export



##################################################################################################
####                                  %a/b%                                                   ####
##################################################################################################
# Simple function to find which elements of x are NOT contained in y (opposite of %in%)
#
# Example usage:
#
#
#     union_jack = c("red","white","blue")
#     rainbow = c("red","orange","yellow","green","blue","indigo","violet")
#
#     union_jack %a/b% rainbow # gives "white".
#     rainbow %a/b% union_jack
#
#     * Therefore, the only colour in the Union Jack that is not contained in the rainbow is "white".
#
##################################################################################################

"%a/b%" <- function(x, y){

  x[!x %in% y]

}


##############################################################
################ END OF %a/b% function #######################
##############################################################
