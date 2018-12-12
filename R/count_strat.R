#' Function for performing contingency analysis of data counts, with stratification by one or two factors.
#' 
#' @author Rupert Austin
#' @description Useful for producing stratified sample counts when producing tables for M&S reports.
#' The function uses the base R table() function followed by lots of reformatting and renaming to produce a more useful output.
#' @param data data for analysis as a data frame.
#' @param strat_col1 name of first column for result stratification.
#' @param strat_col2 name of second column for result stratification. can be set to '' if second stratification is not required.
#' @param pct either 'by_col' or 'by_row'. Indicates how count percentages should be calculated.
#'
#' @return Results are returned as a nicely formatted data frame.
#' @export
#'
#' 
count_strat=function(data,strat_col1,strat_col2='',pct='by_col'){
  data[,strat_col1]=as.character(data[,strat_col1])
  if(strat_col2!=''){
    data[,strat_col2]=as.character(data[,strat_col2])
  }
  if(strat_col2==''){
     pct='by_row'  #  when there is no second level of stratification pecentages can only be calculated by row
  }
  # first stratify counts by data$strat_col1
  # then stratify counts by data$strat_col2 (strat_col2 can be set to '' if no stratification is required)
  #
  # stratify counts by data$strat_col1
  r=data.frame(table(data[,strat_col1]))
  names(r)[1]=strat_col1
  names(r)[2]='Row_Sum'
  if(strat_col2!=''){
    # now add counts stratified by data$data_col
    r1=table(data[,strat_col1],data[,strat_col2])
    for(i in 1:ncol(r1)){
      nm=attr(r1,'dimnames')[[2]][i]
      nm1=paste0(strat_col2,'_',nm)
      r[,nm1]=r1[,nm]
    }
  }
  # add total count in each level of strat_col2
  r[,strat_col1]=as.character(r[,strat_col1])
  r[nrow(r)+1,strat_col1]='Col_Sum'
  if(strat_col2!=''){
     r[nrow(r),2:ncol(r)]=round(as.numeric(colSums(r[1:(nrow(r)-1),2:ncol(r)])),0)
  }else{
     r[nrow(r),'Row_Sum']=round(as.numeric(sum(r[1:(nrow(r)-1),'Row_Sum'])),0)
  }
  # now calculate percentages
  if(pct=='by_row'){
     for(i in 2:ncol(r)){
       cname=names(r)[i]
       new_col_name=paste0(cname,'_pct')
       if(i==2){
         new_col_name=paste0(strat_col1,'_pct')
       }
       r[1:(nrow(r)-1),new_col_name]=round(100*r[1:(nrow(r)-1),cname]/sum(r[1:(nrow(r)-1),cname]),3)
       r[nrow(r),new_col_name]=100
     }
  }
  if(pct=='by_col'){
     for(i in 2:ncol(r)){
        cname=names(r)[i]
        new_col_name=paste0(cname,'_pct')
        if(i==2){
           new_col_name=paste0(strat_col1,'_pct')
           r[1:(nrow(r)-1),new_col_name]=round(100*r[1:(nrow(r)-1),cname]/sum(r[1:(nrow(r)-1),cname]),3)
           r[nrow(r),new_col_name]=100
        }else{
           r[,new_col_name]=round(100*r[,cname]/r[,'Row_Sum'],3)
        }
     }
  }
  return(r)
}
###################################################################################################################################################
###################################################################################################################################################



