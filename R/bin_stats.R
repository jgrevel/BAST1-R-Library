#' Split data into bins (usually time bins), then calculate percentiles of data in each bin.
#'
#' Essentially the part of VPC that deals with observed data. Good for finding trends in large data sets.
#'
#' @author Rupert Austin
#' @param data data frame. Suitably filtered and containing the data to analyse.
#' @param bins numeric vector giving the cut off values for the time bins (binning doesn't have to based on time - see ind_var_col).
#' @param dv_col column name (default value 'DV') in \eqn{data} containing the experimental data (default value is 'DV')
#' @param ind_var_col column name (default value 'TIME') in \eqn{data} containing the variable to use for binning.
#' @param pct numeric vector of percentiles (default value c(10,50,90)) to calculate for the dv_col data within each bin.
#' @return A matrix object with one column for each bin, and the following rows of summary statistics for the dv_col data within each bin
#' (see sum_stats function documentation for descriptions):
#' \itemize{
#'    \item min_ind_var_col
#'    \item max_ind_var_col
#'    \item n
#'    \item min
#'    \item max
#'    \item median
#'    \item arith_mean
#'    \item arith_std_dev
#'    \item arith_CV
#'    \item geo_mean
#'    \item geo_std_dev
#'    \item geo_CV
#'    \item requested percentiles (pct)
#'  }
#' @note For a bin with limits (LOWER,UPPER), an observation will go into that bin if it is >=LOWER and <UPPER.
#' @export




################################################################################
################################################################################
# function to bin experimental data and summarize it for use in plots and tables
# of binned experimental statistics.
#
# For a bin with limits (LOWER,UPPER) an observation will go into that bin if it is >=LOWER and <UPPER.
#
# function inputs:
#
#	data=suitably filtered data frame containing the data to analyse
#	bins=vector giving the cut off values for the time bins (binning doesn't have to based on time - see ind_var_col)
#	dv_col=column name in 'data' containing the experimental data (defualt value is 'DV')
#	ind_var_col=column name in 'data' containing the variable to use for binning (default value is 'TIME')
#       pct=vector of percentiles to calculate for the dv_col data within each bin (defualt value is c(10,50,90))
#
# function outputs:
#
# returns a matrix object with one column for each bin, and the following rows of summary statistics
# for the dv_col data within each bin:
#
# min_ind_var_col
# max_ind_var_col
# n
# min
# max
# median
# arith_mean
# arith_std_dev
# arith_CV
# geo_mean
# geo_std_dev
# geo_CV
# requested percentiles
#
################################################################################
################################################################################
bin_stats=function(data,bins,dv_col='DV',ind_var_col='TIME',pct=c(10,50,90)){

  #.................................................................
  # function for calculating summary statistics on a vector of data
  s_stats=function(data){
    n_tot=length(data)
    data=data[is.na(data)==F]
    n_val=length(data)
    num_NA=n_tot-n_val

    arith_mean=mean(data)
    median=median(data)
    min_val=min(data)
    max_val=max(data)
    stdev=sd(data)
    arith_CV=stdev/arith_mean

    # Geometric statistics. First check all data are positive
    geo_mean=NA
    geo_sd=NA
    geo_CV=NA
    if(all(data>0)){
      ln_data=log(data)
      geo_mean=exp(mean(ln_data))
      geo_sd=exp(sd(ln_data))
      ln_geo_sd2=(log(geo_sd))^2
      geo_CV=(exp(ln_geo_sd2)-1)^0.5
    }
    res=c(n=n_val,n_NA=num_NA,min=min_val,max=max_val,median=median,arith_mean=arith_mean,std_dev=stdev,arith_CV=arith_CV,geo_mean=geo_mean,geo_std_dev=geo_sd,geo_CV=geo_CV)
    return(res)
  }
  #...................................................................#


  # check for NA within the data
  if(any(is.na(data[,dv_col]))==T){
    stop("There is at least one NA values in the DV column. You must first remove all NA values.")
  }
  if(any(is.na(data[,ind_var_col]))==T){
    stop("There is at least one NA values in the independent variable column. You must first remove all NA values.")
  }

  num_bins=length(bins)-1

  # assign data to bins
  bi=rowSums(matrix(data[,ind_var_col]>=rep(bins,each=nrow(data)),nrow=nrow(data)))

  # count number of data points in each bin
  bin_nums=rep(0,num_bins)
  for(i in 1:num_bins){
    bin_nums[i]=sum(bi==i)
  }

  # build a matix for storing results
  result=matrix(data=NA,nrow=12+length(pct),ncol=num_bins)
  rownames(result)=c(paste0('min_',ind_var_col),paste0('max_',ind_var_col),'n','min','max','median','arith_mean','arith_std_dev','arith_CV','geo_mean','geo_std_dev','geo_CV',paste0('pct_',pct))
  colnames(result)=paste0('bin_',1:num_bins)

  # calculate results and tabulate
  for(i in 1:num_bins){
    if(bin_nums[i]>0){
      ind_var_dat=data[bi==i,ind_var_col]
      dv_dat=data[bi==i,dv_col]

      result[1,i]=min(ind_var_dat)
      result[2,i]=max(ind_var_dat)
      result[3,i]=bin_nums[i]
      q=s_stats(dv_dat)
      result[4,i]=q[['min']]
      result[5,i]=q[['max']]
      result[6,i]=q[['median']]
      result[7,i]=q[['arith_mean']]
      result[8,i]=q[['std_dev']]
      result[9,i]=q[['arith_CV']]
      result[10,i]=q[['geo_mean']]
      result[11,i]=q[['geo_std_dev']]
      result[12,i]=q[['geo_CV']]

      # get percentiles
      q1=quantile(dv_dat,probs=pct/100)
      for(k in 1:length(pct)){
        result[12+k,i]=as.numeric(q1[k])
      }
    }else{
      result[3,i]=0
    }
  }
  # return the result
  return(result)
}
################################################################################
############  END OF bin_stats FUNCTION  #######################################
################################################################################


