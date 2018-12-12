#' Get summary statistics on a vector of numbers, stratified by levels of a vector of factors.
#'
#' Gives basic statistics for a data item da (ignores NAs), within different levels id. \cr
#' WARNING - If data is very large, function can encounter serious rounding issues!
#'
#' @author Aaron Hayman
#' @param id vector of any type (except factor) to describe the various levels, with same length as da.
#' @param da numeric vector of data.
#' @param quantiles numeric. Between 0 and 1 to determine 0\% - 100\% quantile for each id.
#' @return A data frame with a row for each level (see Details).
#' @details Returned data frame contains the following:
#' \describe{
#'    \item{id}{level identifier.}
#'    \item{n}{number of items in level.}
#'    \item{min}{minimum value of da in level.}
#'    \item{max}{maximum value of da in level.}
#'    \item{sum}{sum of values of da in level.}
#'    \item{mean}{mean value of da in level.}
#'    \item{median}{median value of da in level.}
#'    \item{SD}{standard deviation value of da in level.}
#'    \item{pct_n\%}{specified quantiles of levels.}
#'  }
#' @examples
#'  # Want to compare weights of different animals from the following data frame:
#'
#'  # ANIMAL   WEIGHT
#'  # Chicken  3.0
#'  # Cat	    4.2
#'  # Cat	    3.1
#'  # Dog	    12.8
#'  # Cat	    6.1
#'  # Dog	    20.4
#'  # Cat	    2.3
#'
#'  id = c("Chicken", "Cat", "Cat", "Dog", "Cat", "Dog", "Cat")
#'  da = c(3.0, 4.2, 3.1, 12.8, 6.1, 20.4, 2.3)
#'  strat_stats(id,da)
#'
#'  #        id n  min  max  sum   mean median       SD
#'  # 1     Cat 4  2.3  6.1 15.7  3.925   3.65 1.645955
#'  # 2 Chicken 1  3.0  3.0  3.0  3.000   3.00      NaN
#'  # 3     Dog 2 12.8 20.4 33.2 16.600  16.60 5.374012
#' @export



##################################################################################################
####                                          strat_stats                                     ####
##################################################################################################
# Gives basic statistic for a data item da (ignores NAs), within different levels id.
# Returns a data frame with a row for each level, containing: level identifier, id; number of items in level,n;
# minimum value of da in level, min; maximum value of da in level, max; sum of values of da in level, sum;
# mean value of da in level, mean; median value of da in level, median; standard deviation value of da in level, SD;
# and any specified quantiles of levels, pct_n%
#
# quantiles should be specified as numbers between (and including), 0 and 1. (other numbers may not return error or warning but results obtained have no meaning)

strat_stats=function(id,da,quantiles=NULL)
{
  id_class=class(id)                                            # check class of id
  id=as.factor(id)                                              # convert id to factor
  allNA=all(is.na(id))
  if(allNA) id=factor(numeric(length(id)),0)                    # cppStats function cannot cope with the first id being NA, therefore it is changed to 0
  lev=data.frame(id=levels(id),stringsAsFactors = FALSE)        # Put factor levels into a data frame,
  # this is so it can be combined using cbind later

  if(anyNA(id)) lev=rbind(lev,NA_character_)                    # if any NAs are used in id, they were assigned a level, so it is added on now
  if(id_class!='factor') class(lev$id)=id_class                 # ensure the original class of id is conserved.
  # class cannot be assigned this way for factor, so if statement is required

  d=data.frame(id=as.integer(id[!is.na(da)]),da=da[!is.na(da)])	#remove and rows where da is NA
  d=d[order(d$id,d$da),]									                    	# order by identifier then da
  m=cppStats(d$id,d$da)                                         # get basic statistical information about da, stratified by id.
  if(length(quantiles)>0)										                    # if quantiles specified
  {
    qs=quantiles										                                                           # shorter names
    ind=rep(m$n-1,each=length(qs))*qs+rep(m$starts+1,each=length(qs))		                       # quantile positions
    q=matrix(d$da[floor(ind)]*(1-ind%%1)+d$da[ceiling(ind)]*ind%%1,ncol=length(qs),byrow=TRUE) # category quantiles
    colnames(q)=paste0('pct_',round(qs*100,2))					                                       # quantile names for output
    m=cbind(m,q)										                                                           # join quantile data to output data
  }
  m$starts=NULL                                                 # starts is not required
  m=cbind(lev,m)                                                # include id column at strat of data frame
  if(allNA) m$id=NA                                             # if NA was changed to 0, it is changed back prior to return
  return(m)
}
##############################################################
################### END OF strat_stats function ##############
##############################################################
