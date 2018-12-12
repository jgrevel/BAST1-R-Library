
#################################################################################################
####                                            LCM (not exported)                           ####
#################################################################################################
# finds the lowest common multiple of x and y
# x and y should both be positive integers of length one

LCM=function(x,y)
{
  x=abs(x)
  y=abs(y)
  z=(x*1:y)[(x*1:y)%in%(y*1:x)]	# finds multiples of x from x to xy, and checks which occur in series y to yx
  return((z)[1])			# returns first (lowest) value
}
#################################################################################################




#' Like R function ' : ' but can be applied to vectors.
#'
#' x\%::\%y is similar to x:y, except x and y are allowed to be vectors.
#'
#' @author Aaron Hayman
#' @param x numeric vector.
#' @param y numeric vector.
#' @return A numeric vector of the form \deqn{c(x[1]:y[1], x[2]:y[2], ...).}
#'  If x and y have lengths nx and ny, then x and y are recycled until \deqn{x[nx]:y[ny]} is performed.
#' @examples
#'   1%::%c(8,7)
#'   c(1:8,1:7) # are both equal
#'
#'   c(0,1)%::%c(4,7)
#'   c(0:4,1:7) # are both equal
#'
#'   c(0,1)%:%c(5,4,-3)
#'   c(0:5,1:4,0:-3,1:5,0:4,1:-3) # are both equal
#' @usage x \%::\% y
# @aliases %:%
#' @rdname ColonColon
#' @export


#################################################################################################
####                                            %::%                                          ####
#################################################################################################
# x%:%y is similar to x:y, except x and y are allowed to be vectors. Returns something like c(x[1]:y[1],x[2]:y[2]...)
# If x and y have lengths nx and ny, x and y are recycled until ...x[nx]:y[ny] is performed.
#
# e.g. 1%:%c(8,7)
#	> 1 2 3 4 5 6 7 8 1 2 3 4 5 6 7			is same as c(1:8,1:7)
#
# e.g. c(0,1)%:%c(4,7)
#	> 0 1 2 3 4 1 2 3 4 5 6 7			is same as c(0:4,1:7)
#
# e.g. c(0,1)%:%c(5,4,-3)
#	> 0 1 2 3 4 5 1 2 3 4 0 -1 -2 -3 1 2 3 4 5 0 1 2 3 4 1 0 -1 -2 -3		is same as c(0:5,1:4,0:-3,1:5,0:4,1:-3)


"%::%"=function(x,y)
{
  lx=length(x)				# checks length of x
  ly=length(y)				# checks length of y
  m=LCM(lx,ly)				# finds lowest common multiple
  x=as.integer(rep(x,m/lx))		# recycle x to length of lowest common multiple
  y=as.integer(rep(y,m/ly))		# recycle y to length of lowest common multiple
  tot=sum(abs(x-y))+m			# length of output vector
  d=abs(y-x)+1L				# section lengths
  dl=y<x					# logic is counting forward or backward, forward=TRUE
  R=1:tot-rep(cumsum(c(0L,d)[1:m]),d)-1L	# numbers to add assuming counting forward
  if(sum(dl)>0L)
  {
    logic=rep(dl,d)
    R[logic]=-R[logic]
  }			# final numbers to add taking backward counting into account
  x=rep(x,d)+R				# result
  return(x)
}


##############################################################
##################### END OF %::% FUNCTION ####################
##############################################################
