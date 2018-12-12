#' Calculation of range VPC.
#'
#' Runs calculations for a more simple version of a VPC where typically, within each bin, the 5th, 50th and 95th percentiles of the experimental
#' data are calculated along with the central 90\% range of the simulated data (90\% prediction interval). Calculations are run as
#' non-prediction-corrected and prediction-corrected.
#'
#' @author Rupert Austin
#' @param exp_data data frame containing experimental data.
#' @param sim_data data frame containing all simulated data.
#' @param bins vector of limits of the time bins.
#' @param dv_col name of column containing DV, for both exp_data and sim_data.
#' @param ind_var_col name of column containing independent variable (e.g. TAD or TIME), for both exp_data and sim_data.
#' @param exp_quantiles vector of required quantiles of experimental DV in each bin.
#' @param prediction_interval numeric. Percentage size of prediction interval calculated around all data in each bin of simulated data.
#' @param min_bin_width numeric. Width of the smallest possible unrestricted bin (bins will be restricted in width by neighbouring bins). It can be
#'        adjusted to improve the look of the VPC plot.
#' @return A list containing all necessary range VPC parameters to be used in plot_rng_VPC function.
#' @note This function has not been designed for bins containing zero points, and may fail or produce strange results if zero points are detected.
#' @export


##################################################################################################
####                                    calc_rng_VPC                                          ####
##################################################################################################
# runs calculations for a more simple version of a VPC where typically, within each bin, the
# 5th, 50th and 95th percentiles of the experimental data are calculated along with the central 90%
# range of the simulated data (90% prediction interval). Calculations are run as non prediction-corrected
# and prediction-corrected.
#
# exp_data is the experimental data
# sim_data is the simulated data
# bins is the limits of bins (typically TIME or TIME_AFTER_DOSE dose bins)
# dv_col (default='DV') and ind_var_col (default='TAD') are the names of columns with DV and independent variable (typically TAD), for both exp_data and sim_data
# exp_quantiles a vector of required quantiles of experimental DV in each bin
# prediction_interval is size of percent prediction interval calculated around all data in each bin of simulated data
# min_bin_width is the width of the smallest possible unrestricted bin (bins will be restricted in width by neighbouring bins). It can be adjusted to improve the look of the VPC plot
#
# this function has not been designed for bins containing zero points, and may fail or produce strange results if this is done
calc_rng_VPC=function(exp_data,sim_data,bins,dv_col='DV',ind_var_col='TAD',exp_quantiles=c(0.05,0.5,0.95),prediction_interval=90,min_bin_width=0.5){
  # check that information in prediction_interval and exp_quantiles are consistent
  CK=TRUE
  if(length(exp_quantiles)!=3){
    CK=FALSE
  }
  if((exp_quantiles[1]+exp_quantiles[3])!=1){
    CK=FALSE
  }
  if(round(100*(exp_quantiles[3]-exp_quantiles[1]) - prediction_interval,3)!=0){
    CK=FALSE
  }
  if((1-exp_quantiles[1])!=exp_quantiles[3]){
    CK=FALSE
  }
  if(CK==FALSE){
    print("Range VPC will be calculated, but there are inconsistensies in the entered values of exp_quantiles and prediction_interval. Use results with caution.")
  }

  # check if dosing records are accidentally included
  if('EVID'%in%names(exp_data)){
    ed1=exp_data[exp_data$EVID!=0,]
    nr=nrow(ed1)
    if(nr!=0){
      stop('Non-observation records have been found within exp_data. Please remove non-observation records then re-run.')
    }
  }
  if('EVID'%in%names(sim_data)){
    sd1=sim_data[sim_data$EVID!=0,]
    nrs=nrow(sd1)
    if(nrs!=0){
      stop('Non-observation records have been found within sim_data. Please remove non-observation records then re-run.')
    }
  }

  # check for existence of REPL column and add if necessary
  cnms=names(sim_data)
  if(!('REPL' %in% cnms)){
    reps=nrow(sim_data)/nrow(exp_data)
    sim_data$REPL=rep(1:reps,each=nrow(exp_data))
  }
  # first we copy PRED colum from sim_data to exp_data
  exp_data$PRED=sim_data[sim_data$REPL==1,'PRED']
  # calculate the PRED_corrected experimental DV using median PRED in each bin
  exp_data$ASSIGNED_BIN=''
  exp_data$median_PRED_in_bin=0
  exp_data$DV_PRED_CORRECTED=0
  for(i in 1:(length(bins)-1)){
    med_pred=median(exp_data$PRED[exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]])  # median PRED in current bin
    exp_data$DV_PRED_CORRECTED[exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]]=med_pred*exp_data[,dv_col][exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]]/exp_data$PRED[exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]]
    exp_data$median_PRED_in_bin[exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]]=med_pred
    exp_data$ASSIGNED_BIN[exp_data[,ind_var_col]<bins[i+1] & exp_data[,ind_var_col]>=bins[i]]=paste0(bins[i],'_',bins[i+1])
  }
  # calculate the PRED_corrected simulated DV using median PRED in each bin
  sim_data$DV_PRED_CORRECTED=0
  for(i in 1:(length(bins)-1)){
    med_pred=median(sim_data$PRED[sim_data[,ind_var_col]<bins[i+1] & sim_data[,ind_var_col]>=bins[i]])  # median PRED in current bin
    sim_data$DV_PRED_CORRECTED[sim_data[,ind_var_col]<bins[i+1] & sim_data[,ind_var_col]>=bins[i]]=med_pred*sim_data[,dv_col][sim_data[,ind_var_col]<bins[i+1] & sim_data[,ind_var_col]>=bins[i]]/sim_data$PRED[sim_data[,ind_var_col]<bins[i+1] & sim_data[,ind_var_col]>=bins[i]]
  }

  # create a matrix, initially full of zeros, with 8 rows and (length(bins)-1) columns.
  # this matrix will hold the min(time) in bin, max(time) in bin, number of experimental values in bin, lower percentile DV,
  # 50th percentile DV, upper percentile DV, lower end of simulated interval, median of simulated data, upper end of simulated interval,
  # then prediciton corrected experimental and simulated quantiles

  res=rep(0,15*(length(bins)-1))
  dim(res)=c(15,length(bins)-1)
  a1=paste0('exp_',exp_quantiles[1]*100,'%')
  a2=paste0('exp_',exp_quantiles[2]*100,'%')
  a3=paste0('exp_',exp_quantiles[3]*100,'%')
  a4=paste0('sim_',(100-prediction_interval)/2,'%')
  a5=paste0('sim_',100-(100-prediction_interval)/2,'%')
  a6=paste0('PC_exp_',exp_quantiles[1]*100,'%')
  a7=paste0('PC_exp_',exp_quantiles[2]*100,'%')
  a8=paste0('PC_exp_',exp_quantiles[3]*100,'%')
  a9=paste0('PC_sim_',(100-prediction_interval)/2,'%')
  a10=paste0('PC_sim_',100-(100-prediction_interval)/2,'%')
  rownames(res)=c('min_time','max_time','n_bin',a1,a2,a3,a4,'sim_50%',a5,a6,a7,a8,a9,'PC_sim_50%',a10)
  colnames(res)=paste0('Bin_',1:(length(bins)-1))

  # now do VPC calculations
  sim_quantiles=c(0.01*(100-prediction_interval)/2,0.5,0.01*(100-(100-prediction_interval)/2))
  for(i in 1:(length(bins)-1)){
    vals_s=sim_data[,dv_col][sim_data[,ind_var_col]>=bins[i] & sim_data[,ind_var_col]<bins[i+1]]
    qs=quantile(vals_s,probs=sim_quantiles,na.rm=T)
    vals_e=exp_data[,dv_col][exp_data[,ind_var_col]>=bins[i] & exp_data[,ind_var_col]<bins[i+1]]
    qe=quantile(vals_e,probs=exp_quantiles,na.rm=T)
    res[4,i]=qe[1]
    res[5,i]=qe[2]
    res[6,i]=qe[3]
    res[1,i]=min(sim_data[sim_data[,ind_var_col]>=bins[i] & sim_data[,ind_var_col]<bins[i+1],ind_var_col])
    res[2,i]=max(sim_data[sim_data[,ind_var_col]>=bins[i] & sim_data[,ind_var_col]<bins[i+1],ind_var_col])
    res[3,i]=length(exp_data[exp_data[,ind_var_col]>=bins[i] & exp_data[,ind_var_col]<bins[i+1],ind_var_col])
    res[7,i]=qs[1]
    res[8,i]=qs[2]
    res[9,i]=qs[3]
    vals_s=sim_data$DV_PRED_CORRECTED[sim_data[,ind_var_col]>=bins[i] & sim_data[,ind_var_col]<bins[i+1]]
    qs=quantile(vals_s,probs=sim_quantiles,na.rm=T)
    vals_e=exp_data$DV_PRED_CORRECTED[exp_data[,ind_var_col]>=bins[i] & exp_data[,ind_var_col]<bins[i+1]]
    qe=quantile(vals_e,probs=exp_quantiles,na.rm=T)
    res[10,i]=qe[1]
    res[11,i]=qe[2]
    res[12,i]=qe[3]
    res[13,i]=qs[1]
    res[14,i]=qs[2]
    res[15,i]=qs[3]

  }
  # now check bin widths and modify according to value of min_bin_width
  for(i in 1:(length(bins)-1)){
    if(res[2,i]-res[1,i]<min_bin_width){
      res[2,i]=res[2,i]+(min_bin_width-(res[2,i]-res[1,i]))
    }
  }
  for(i in 2:(length(bins)-1)){
    if(res[1,i]<res[2,i-1]){
      res[2,i-1]=res[2,i-1] - 1.05*(res[2,i-1]-res[1,i])
    }
  }
  # return results
  print(res)
  ret=list(VPC_results=res,exp_data_modified=exp_data,dv_col=dv_col,ind_var_col=ind_var_col)
  return(ret)
}
##############################################################
################## END OF calc_rng_VPC function ##############
##############################################################
