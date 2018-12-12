#' For testing equivalence of numbers contained anywhere within a vector.
#'
#' Similar to \%==\% but for testing if numbers in vector \eqn{a} are contained in vector \eqn{b}.
#'
#' @author Aaron Hayman
#' @param a numeric vector to be tested for equivalence.
#' @param b numeric vector to be tested against \eqn{a} for equivalence.
#' @return TRUE for each element of \eqn{b} if all but the insignificant parts are identical to an element in \eqn{a}.
#' @details This operator works with the option insigbits, which should be an integer between 1 and 8. The option insigbits defines how many bits
#'          (from least significant) will be considered as insignificant in deciding upon equality.
#' @note Default for insigbits is 6, it can be changed by using \deqn{options(insigbits = #),} where \eqn{#} is the desired value.
#' @examples
#' a = c(900,90)
#' b = c(100*(0.95 - 0.05), 1000*(0.95 - 0.05))  # c(90,900)
#' a %in% b  # FALSE FALSE
#' a %==% b  # FALSE FALSE
#' a[2] - b[1]  # 1.421085e-14
#' a[1] - b[2]  # 1.136868e-13
#' a %INN% b  # TRUE TRUE
#' @usage a \%INN\% b
#' @rdname INN
#' @export



######################################################################################################
####                                         %INN%                                                  ####
######################################################################################################
# Operator that is similar to ==, for comparing two numeric vectors.
# This operator works with the option insigbits, which should be an integer between 1 and 8. The option
# insigbit defines how many bits (from least significant) will be considered as insignificant in deciding upon equallity.
#
# Operator returns TRUE if all but the insignificant bits are identical.
# Default for insigbits is 6, it can be chaged by using options(insigbits= )


`%INN%` =function(a,b)
{

  if(mode(a)!='numeric' | mode(b)!='numeric' )
  { return(a%in%b)}
  a=as.numeric(a)					# a and b are converted to numeric
  b=as.numeric(b)

  if(is.null(options()$sigfigs)) options(sigfigs=13)

  # new code
  a=signif(a,options()$sigfigs)
  b=signif(b,options()$sigfigs)
  return(a%in%b)

  # previous code below has problems
  #ra=raw()					# empty raw vectors are prepared
  #rb=raw()
  #ra=writeBin(a,ra)				# a and b are converted to raw (byte) format
  #rb=writeBin(b,ra)
  #ind=seq(1L,length(ra),by=8L)			# index for every first byte in numbers, (R uses little endian format, numerics are 8 bytes long)
  #ra[ind]=ra[ind]|as.raw(2^options()$insigbits-1) # first bytes of a and b have the insignifant bits all changed to 1
  #rb[ind]=rb[ind]|as.raw(2^options()$insigbits-1)					# or operator, |, also and operator, &, have very interesting behaviours with raw vectors
  #tr=readBin(ra,'numeric',length(a))%in%readBin(rb,'numeric',length(b))	# raw vectors are coverted back to numbers and checked for equality
  #return(tr)
}

###################################################################
###################### END OF %INN% operator ########################
###################################################################
