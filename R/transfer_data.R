#' Transfer data between source and destination data frames.
#'
#' Transfer data between source and destination data frames using matching of identifiers.
#'
#' @author Aaron Hayman
#' @param a1 name of identifier column in source data frame 'a'.
#' @param a2 name of data column in the source data frame 'a' (to be transferred to the destination data frame 'b').
#' @param b1 name of identifier column in destination data frame 'b' to be matched with a1 in data frame 'a'.
#' @param b2 (optional) name of data column in destination data frame 'b' (where data from a2 in source data frame 'a' will be transferred to).
#' @details Moves data from data frame 'a' to data frame 'b'.
#' Finds matches between column a1 and b1 and then transfers data from a2 to b2 where a1 and b1 match.
#'
#' If b2 is omitted from the function argument then data from a2 will be added to a new column.
#' If the b2 argument is used then data from a2 will be merged into existing column.
#'
#' If there is ever a situation where more than one row in the source data frame has matched a row in the destination data frame and the data
#' for transfer within those multiple rows is not all identical, then the data from the first of the matching rows will have been transferred
#' and a warning will be issued.
#' @return Returns column b2.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' \donttest{
#' friends
#' > 	name	occupation	birthday
#' > 1	bob	banker		1/2/89
#' > 2	tom	builder		4/6/75
#' > 3	ted	driver		26/7/81
#' family
#' > 	name	relation	birthday
#' > 1	mavis	mum		5/3/56
#' > 2	pete	dad		12/9/58
#' > 3	bob	cousin		forgotten
#' family$birthday = transfer(friends$name,friends$birthday,family$name,family$birthday)
#' family
#' > 	name	relation	birthday
#' > 1	mavis	mum		5/3/56
#' > 2	pete	dad		12/9/58
#' > 3	bob	cousin		1/2/89
#' }


#################################################################################################
####                                          transfer_data                                       ####
#################################################################################################
# moves data from data frame 'a' to data frame 'b'
# finds matches between column a1 and b1 and then tranfers data from a2 to b2 where a1 and b1 match
#
# i.e. if an animal exists in two data sets, information can be drawn from one dataset to the other
# without worrying about format.
#
#	e.g.	friends
#		> 	name	occupation	birthday
#		> 1	bob	banker		1/2/89
#		> 2	tom	builder		4/6/75
#		> 3	ted	driver		26/7/81
#		family
#		> 	name	relation	birthday
#		> 1	mavis	mum		5/3/56
#		> 2	pete	dad		12/9/58
#		> 3	bob	cousin		forgotten
#		family$birthday=transfer(friends$name,friends$birthday,family$name,family$birthday)
#		family
#		> 	name	relation	birthday
#		> 1	mavis	mum		5/3/56
#		> 2	pete	dad		12/9/58
#		> 3	bob	cousin		1/2/89

transfer_data=function(a1,a2,b1,b2) {
  #a1 is from identifier
  #a2 is from data
  #b1 is to identifier
  if(missing(b2)){b2=NA}
  if(length(b2)==1){b2=rep(b2,length(b1))}
  if(any(duplicated(unique(data.frame(a1,a2))$a1)))
  {
    warning( paste0(deparse(substitute(a1)),' contains non-unique values with ambiguous terms in '
                    ,deparse(substitute(a2)),',\n  first value(s) used. Check this is appropriate')
    )
    ain=get_info(a1)
    if(any(duplicated(ain$value)))
    {
      ain=ain[ain$value %in% ain$value[duplicated(ain$value)] , ]
      ain=ain[order(ain$value),]
      ind=ain$start_index
      ind[ain$num_rep>1]=paste0(ind[ain$num_rep>1],'-',ain$end_index[ain$num_rep>1])
      seps=rep(', ',length(ind))
      inin=get_info(ain$value)
      seps[inin$end_index]='\n'
      ind = unlist(strsplit(paste(paste0(ind,seps),collapse=''),'\n'))
      length(ind) = sum(1:length(ind)<5)
      ind=paste0('    identifier "',inin$value[1:length(ind)],'" occurs at indices: ',ind)
      if(length(ind)==4) ind[4]='    ...'
      ind=paste(ind,collapse='\n')
      mess=paste0('Identifiers from ',deparse(substitute(a1)),' are reused non-consecutively:\n',ind
                  ,'\n  check the identifier used is appropriate')
      stop(mess)
    }
  }
  b2[!is.na(match(b1,a1))]=a2[match(b1,a1)[!is.na(match(b1,a1))]]
  return(b2)
}



##############################################################
################## END OF transfer_data FUNCTION #############
##############################################################
