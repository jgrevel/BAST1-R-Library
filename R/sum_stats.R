#' For calculating summary statistics on a vector.
#'
#' Calculates arithmetic and geometric summary statistics for an input vector.
#'
#' @author Rupert Austin
#' @param data numeric vector to compute summary statistics on.
#' @param pct defaults to \eqn{c(5, 95)}. Up to 3 different percentiles can be calculated. For example: \deqn{sum_stats(data, pct = c(5, 50, 95)).}
#'        If percentiles are not required then set: \deqn{pct = NA.}
#' @return A vector of named elements (see Details).
#' @details Vector of summary statistics contains the following:
#'          \describe{
#'            \item{n}{number of elements.}
#'            \item{n_NA}{number of NA elements.}
#'            \item{min}{minimum value.}
#'            \item{max}{maximum value.}
#'            \item{median}{median value.}
#'            \item{arith_mean}{arithmetic mean value.}
#'            \item{std_dev}{standard deviation.}
#'            \item{arith_CV}{arithmetic coefficient of variation.}
#'            \item{geo_mean}{geometric mean value.}
#'            \item{geo_std_dev}{geometric standard deviation.}
#'            \item{geo_CV}{geometric coefficient of variation.}
#'          }
#' @export



###########################
#### sum_stats ############
###########################
# calculates arithmetic and geometric summary statistics for an input vector
# the vector can contain NA values
# function returns a vector of named elements...min, max, median, arith_mean etc.
# up to 3 different percentiles can be calculates with optional argument pct. For example: sum_stats(1:50,pct=c(5,50,95))
# if percentiles are not required then set: pct=NA
###########################
sum_stats=function(data,pct=c(5,95)){
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
  sum_val=sum(data)

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

  # calculate percentiles if requested. Only calculate up to 3 percentiles
  if(is.na(pct[1])==F){
    qnt=quantile(data,probs=pct/100)
    for(i in 1:max(3,length(pct))){
      if(i==1){
        pct1=as.numeric(qnt[i])
        pct1_name=paste0('pct_',pct[i])
      }
      if(i==2){
        pct2=as.numeric(qnt[i])
        pct2_name=paste0('pct_',pct[i])
      }
      if(i==3){
        pct3=as.numeric(qnt[i])
        pct3_name=paste0('pct_',pct[i])
      }
    }
  }

  # build results object
  if(is.na(pct[1])==T){
    res=c(n=n_val,n_NA=num_NA,min=min_val,max=max_val,sum=sum_val,median=median,arith_mean=arith_mean,std_dev=stdev,arith_CV=arith_CV,geo_mean=geo_mean,geo_std_dev=geo_sd,geo_CV=geo_CV)
  }else{
    if(length(pct)==1){
      res=c(n=n_val,n_NA=num_NA,min=min_val,max=max_val,sum=sum_val,median=median,arith_mean=arith_mean,std_dev=stdev,arith_CV=arith_CV,geo_mean=geo_mean,geo_std_dev=geo_sd,geo_CV=geo_CV,pct1)
      names(res)[length(res)]=pct1_name
    }
    if(length(pct)==2){
      res=c(n=n_val,n_NA=num_NA,min=min_val,max=max_val,sum=sum_val,median=median,arith_mean=arith_mean,std_dev=stdev,arith_CV=arith_CV,geo_mean=geo_mean,geo_std_dev=geo_sd,geo_CV=geo_CV,pct1,pct2)
      names(res)[length(res)-1]=pct1_name
      names(res)[length(res)]=pct2_name
    }
    if(length(pct)>=3){
      res=c(n=n_val,n_NA=num_NA,min=min_val,max=max_val,sum=sum_val,median=median,arith_mean=arith_mean,std_dev=stdev,arith_CV=arith_CV,geo_mean=geo_mean,geo_std_dev=geo_sd,geo_CV=geo_CV,pct1,pct2,pct3)
      names(res)[length(res)-2]=pct1_name
      names(res)[length(res)-1]=pct2_name
      names(res)[length(res)]=pct3_name
    }
  }
  return(res)
}
##############################################################
#################### END OF sum_stats function ###############
##############################################################
