#' Gives details on consecutive repeats within a vector.
#'
#' @author Aaron Hayman
#' @param col a vector.
#' @return A data frame with columns 'value', 'num_rep', 'start_index' and 'end_index':
#'
#' 'value' is the elements of the vector excluding any consecutive repeats,
#'
#' 'num_rep' is the number of times each value consecutively occurs,
#'
#' 'start_index' is the start index of each value within the original vector,
#'
#' 'end_index' is the end index of each value within the original vector.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
# @examples





#################################################################################################
####                                            get_info                                         ####
#################################################################################################
# input is a vector
# returns a data frame with columns 'val', 'fr', 'st' and 'e'
# val is the elements of the vector excluding any consecutively repeats
# fr is the number of times each val consecutively occurs
# st is the start index of each val withing the original vector
# e is the end index of each val withing the original vector
# rep(val,fr)  will return the original vector

get_info=function(col)
{
  na=which(is.na(col))				# checks for NA
  if(length(na)>0)
  {
    col=round(col,6)			# if NA exists the values in the column are rounded to 6 dp
    col[na]=0.14121356			# sets NA values to sqrt(2) correct to 8 dp, since this cannot be matched in the data due to having so many decimal places
  }
  end_index=c(which(c(col[-1],NA)!=col),length(col))	# finds where the next element is not the same
  num_rep=diff(c(0,end_index))					# number of elements the same in each section
  start_index=end_index-num_rep+1					# start of new groups
  if(length(na)>0)
  {
    col[na]=NA				# if there were NAs they are put back in
  }
  value=col[end_index]					# value from column
  return(data.frame(value,num_rep,start_index,end_index))
}

##############################################################
#################### END OF info FUNCTION ####################
##############################################################
