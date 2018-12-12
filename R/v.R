#' Invoke a Data Viewer.
#'
#' In RStudio sometimes you want to see all the data, but View is introduced in a different namespace.
#'
#' @author Aaron Hayman
#' @param x an R object which can be coerced to a data frame with non-zero numbers of rows and columns.
#' @param title title for viewer window. Defaults to name of x prefixed by Data:.
#' @return Invisible NULL. The function puts up a window and returns immediately: the window can be closed via its controls or menus.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
# @examples



#################################################################################################
####                                            view                                         ####
#################################################################################################

v = function (x, title)
{
  utils::View(x,title)		# in R studio sometimes you want to see all the data, but View is introduced in a differenct namespace
}


##############################################################
#################### END OF view FUNCTION ####################
##############################################################
