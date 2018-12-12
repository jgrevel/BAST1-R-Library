




################################################################################
#' FUNCTION FOR PRODUCING GOF PLOTS
#'
#' @author Rupert Austin
#' @description  Function to generate GOF plots for NONMEM models
#' Similar to the standard GOF plots in BAST1 library except:
#'     Colours are all grayscale or black
#'     Units can be added to axis labels,
#'     LOWESS smoothed lines can be removed (set fval=NA)
#'     DV, PRED, and IPRED can be set to display on log scale
#'     Mean residuals in bins can be used instead of LOWESS smooths (set bin limits using bins_PRED and bins_ind_var arguments)
#'     Alternative layout can be resquested for IWRES plots
#'
#' Extensive examples of use are available in this location:
#'
#' \\\\BAST_SERVER\\BAST_Shared_Files\\QA Documents\\BAST Computer Systems Validation\\final_documents\\BAST_NONMEM_USERS_MANUAL\\GOF_VPC_examples\\enhanced_GOF\\run100
#'
#' @param data Data frame containing output from NONMEM run
#' @param type Defines type of plots to return: CWRES / IWRES / NPDE (default='CWRES')
#' @param ind_var Name of the independent variable column (default='TAD')
#' @param log_ind_var FALSE/TRUE for setting logarithmic x-axis in some of the plots (default=FALSE)
#' @param bks Defines number of bins in histograms (default=30)
#' @param fval Smoothing setting for LOWESS trend lines (larger values give smore smoothing, default=0.66). Set to NA to exclude trend lines.
#' @param bins_PRED  Defines bin limits for PRED/IPRED if binned residual display is required  (default=NA)
#' @param bins_ind_var Defines bin limits for ind_var if binned residual display is required (default=NA)
#' @param dv_units Units of the observations for axis labelling (default='ug/L')
#' @param dv_label Axis label for DV in plots (default='DV')
#' @param ind_var_units Units of the independent variable for axis labelling (default='hour')
#' @param loglog TRUE/FALSE. If TRUE then DV vs PRED and DV vs IPRED are presented on log-log scales, and CWRES/IWRES vs. PRED/IPRED has log x-axis (default=FALSE)
#' @param pt_col Colour of points in all plots (default='gray70')
#' @param alt_plots Specifies if alternative layout of IWRES plots should be returned FALSE/TRUE (default=FALSE)
#'
#' @details If alt_plots=FALSE then output will be:
#'      CWRES: DV VS. PRED, DV VS. IPRED, CWRES VS. PRED, CWRES VS. ind_var, QQ plot of CWRES, histogram of CWRES
#'      IWRES: IWRES VS. IPRED, IWRES VS. ind_var, |IWRES| VS. IPRED, |IWRES| VS. ind_var, QQ plot of IWRES, histogram of IWRES
#'      NPDE:  NPDE VS. ind_var, NPDE VS. PRED, QQ plot of NPDE, histogram of NPDE
#'
#'      If alt_plots=TRUE then IWRES output will be changed to:
#'      IWRES: DV VS. PRED, DV VS. IPRED, IWRES VS. PRED, IWRES VS. ind_var, QQ plot of IWRES, histogram of IWRES
#'
#' @return Produces a panel of GOF plots
#' @export
#'
plot_eGOF=function(data,type='CWRES',ind_var='TAD',log_ind_var=FALSE,bks=30,fval=0.66,bins_PRED=NA,bins_ind_var=NA,dv_units='ug/L',dv_label='DV',ind_var_units='hour',loglog=FALSE,pt_col='gray70',alt_plots=FALSE){

   lv=FALSE
   if(log_ind_var==TRUE){
      lv=TRUE
   }
   if(type=='CWRES'){
      par(mfrow=c(3,2))
      par(mar=c(4.5,4.5,2,1))
      par(oma=c(0,0,0,0))
      par(cex.lab=1.5)
      par(cex.main=1)
      par(cex.axis=1.3)
      pCWRES(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col)
   }
   if(type=='IWRES' & alt_plots==FALSE){
      par(mfrow=c(3,2))
      par(mar=c(4.5,4.5,2,1))
      par(oma=c(0,0,0,0))
      par(cex.lab=1.5)
      par(cex.main=1)
      par(cex.axis=1.3)
      pIWRES(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col)
   }
   if(type=='IWRES' & alt_plots==TRUE){
      par(mfrow=c(3,2))
      par(mar=c(4.5,4.5,2,1))
      par(oma=c(0,0,0,0))
      par(cex.lab=1.5)
      par(cex.main=1)
      par(cex.axis=1.3)
      pIWRES_alt(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col)
   }
   if(type=='NPDE'){
      par(mfrow=c(2,2))
      par(mar=c(4.5,4.5,2,1))
      par(oma=c(0,0,0,0))
      par(cex.lab=1.5)
      par(cex.main=1)
      par(cex.axis=1.3)
      pNPDE(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,pt_col)
   }
   par(mfrow=c(1,1))
   par(mar=c(5,5,2,1.5))
   par(oma=c(0,0,0,0))
   par(cex.lab=1)
   par(cex.main=1)
   par(cex.axis=1)
}
################################################################################
################################################################################


#' plots: DV VS. PRED, DV VS. IPRED, CWRES VS. PRED, CWRES VS. ind_var, QQ plot of CWRES, histogram of CWRES
#'
#' @author Rupert Austin
#' @param data a data frame
#' @param ind_var character, name of the column containing the independent variable
#' @param log_ind_var logical. Should the independent variable be plotted on a log scale
#' @param bks breaks to be put in a histogram can be single integer, or vector of break points
#' @param fval f for lowess smoothing on qq plot
#' @param bins_PRED numeric vector bin boundaries for PRED
#' @param bins_ind_var numeric vector bin boundaries for independent variable
#' @param dv_units character, units of DV
#' @param ind_var_units character, units of independent variable
#' @param loglog logical whether to plot on log log scale
#' @param dv_label character, label for dv in plots
#' @param pt_col colour of points
#'

pCWRES=function(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col){
  # retain only the observation records
  col_names=names(data)
  if('MDV'%in%col_names){
    d=data[data[,'MDV']==0,]
  }
  if('EVID'%in%col_names){
    d=data[data[,'EVID']==0,]
  }
  if(!('EVID'%in%col_names) & !('EVID'%in%col_names)){
    d=data
  }

  ### PLOT OF DV VS. PRED
  x_min=min(d[,'PRED'],d[,'DV'])*0.95
  x_max=max(d[,'PRED'],d[,'DV'])*1.05
  if(loglog==FALSE){
    plot(y=d[,'DV'],x=d[,'PRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('PRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',col=pt_col)
  }else{
    plot(y=d[,'DV'],x=d[,'PRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('PRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',log='xy',col=pt_col)
  }
  abline(a=0,b=1,col='black',lty=2)

  ### PLOT OF DV VS. IPRED
  x_min=min(d[,'IPRED'],d[,'DV'])*0.95
  x_max=max(d[,'IPRED'],d[,'DV'])*1.05
  if(loglog==FALSE){
    plot(y=d[,'DV'],x=d[,'IPRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('IPRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',col=pt_col)
  }else{
    plot(y=d[,'DV'],x=d[,'IPRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('IPRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',log='xy',col=pt_col)
  }
  abline(a=0,b=1,col='black',lty=2)

  ### PLOT OF CWRES VS. PRED
  if(loglog==FALSE){
    plot(x=d[,'PRED'],y=d[,'CWRES'],type='p',xlab=paste0('PRED (',dv_units,')'),ylab='CWRES',main='',col=pt_col)
  }else{
    plot(x=d[,'PRED'],y=d[,'CWRES'],type='p',xlab=paste0('PRED (',dv_units,')'),ylab='CWRES',main='',log='x',col=pt_col)
  }
  abline(h=0,col='black',lty=2)
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,'PRED'],y=d[,'CWRES'],f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_PRED[1])==FALSE){
    q=bin_stats(d,bins=bins_PRED,dv_col='CWRES',ind_var_col='PRED')
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF CWRES VS. ind_var
  if(log_ind_var==TRUE) {
    plot(x=d[,ind_var],y=d[,'CWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='CWRES',main='',log='x',col=pt_col)
    abline(h=0,col='black',lty=2)
  }else{
    plot(x=d[,ind_var],y=d[,'CWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='CWRES',main='',col=pt_col)
    abline(h=0,col='black',lty=2)
  }
  if(is.na(fval)==FALSE){
    rr=lowess(x=d[,ind_var],y=d[,'CWRES'],f=fval,iter=10)
    points(x=rr$x,y=rr$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_ind_var[1])==FALSE){
    q=bin_stats(d,bins=bins_ind_var,dv_col='CWRES',ind_var_col=ind_var)
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### QQ plot of CWRES
  qqnorm(d$CWRES,main='',xlab='CWRES theoretical quantiles',ylab='CWRES sample quantiles',col=pt_col)
  lines(c(-100,100),c(-100,100),col='black',lty=2)

  ### histogram of CWRES
  hist(d$CWRES,main='',xlab='CWRES',breaks=bks)
  #####################################################################

}
#################### END OF pCWRES ###########################


################################################################################
#' plots: IWRES VS. IPRED, IWRES VS. ind_var, |IWRES| VS. IPRED, |IWRES| VS. ind_var, QQ plot of IWRES, histogram of IWRES
#'
#' @author Rupert Austin
#'
#' @param data a data frame
#' @param ind_var character, name of the column containing the independent variable
#' @param log_ind_var logical. Should the independent variable be plotted on a log scale
#' @param bks breaks to be put in a histogram can be single integer, or vector of break points
#' @param fval f for lowess smoothing on qq plot
#' @param bins_PRED numeric vector bin boundaries for PRED
#' @param bins_ind_var numeric vector bin boundaries for independent variable
#' @param dv_units character, units of DV
#' @param ind_var_units character, units of independent variable
#' @param loglog logical whether to plot on log log scale
#' @param dv_label character, label for dv in plots
#' @param pt_col colour of points
#'
pIWRES_alt=function(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col){
  # retain only the observation records
  col_names=names(data)
  if('MDV'%in%col_names){
    d=data[data[,'MDV']==0,]
  }
  if('EVID'%in%col_names){
    d=data[data[,'EVID']==0,]
  }
  if(!('EVID'%in%col_names) & !('EVID'%in%col_names)){
    d=data
  }

  ### PLOT OF IWRES VS. IPRED
  if(loglog==FALSE){
    plot(x=d[,'IPRED'],y=d[,'IWRES'],type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='IWRES',main='',col=pt_col)
  }else{
    plot(x=d[,'IPRED'],y=d[,'IWRES'],type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='IWRES',main='',log='x',col=pt_col)
  }
  abline(h=0,col='black',lty=2)
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,'IPRED'],y=d[,'IWRES'],f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_PRED[1])==FALSE){
    q=bin_stats(d,bins=bins_PRED,dv_col='IWRES',ind_var_col='IPRED')
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF IWRES VS. ind_var
  if(log_ind_var==TRUE) {
    plot(x=d[,ind_var],y=d[,'IWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='IWRES',main='',log='x',col=pt_col)
    abline(h=0,col='black',lty=2)
  }else{
    plot(x=d[,ind_var],y=d[,'IWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='IWRES',main='',col=pt_col)
    abline(h=0,col='black',lty=2)
  }
  if(is.na(fval)==FALSE){
    rr=lowess(x=d[,ind_var],y=d[,'IWRES'],f=fval,iter=10)
    points(x=rr$x,y=rr$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_ind_var[1])==FALSE){
    q=bin_stats(d,bins=bins_ind_var,dv_col='IWRES',ind_var_col=ind_var)
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF |IWRES| VS. IPRED
  if(loglog==FALSE){
    plot(x=d[,'IPRED'],y=abs(d[,'IWRES']),type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='|IWRES|',main='',col=pt_col)
  }else{
    plot(x=d[,'IPRED'],y=abs(d[,'IWRES']),type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='|IWRES|',main='',log='x',col=pt_col)
  }
  abline(h=0,col='black',lty=2)
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,'IPRED'],y=abs(d[,'IWRES']),f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_PRED[1])==FALSE){
    d$absIWRES=abs(d$IWRES)
    q=bin_stats(d,bins=bins_PRED,dv_col='absIWRES',ind_var_col='IPRED')
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF |IWRES| VS. ind_var
  if(log_ind_var==TRUE) {
    plot(x=d[,ind_var],y=abs(d[,'IWRES']),type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='|IWRES|',main='',log='x',col=pt_col)
    abline(h=0,col='black',lty=2)
  }else{
    plot(x=d[,ind_var],y=abs(d[,'IWRES']),type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='|IWRES|',main='',col=pt_col)
    abline(h=0,col='black',lty=2)
  }
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,ind_var],y=abs(d[,'IWRES']),f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_ind_var[1])==FALSE){
    d$absIWRES=abs(d$IWRES)
    q=bin_stats(d,bins=bins_ind_var,dv_col='absIWRES',ind_var_col=ind_var)
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### QQ plot of IWRES
  qqnorm(d$IWRES,main='',xlab='IWRES theoretical quantiles',ylab='IWRES sample quantiles',col=pt_col)
  lines(c(-100,100),c(-100,100),col='black',lty=2)

  ### histogram of IWRES
  hist(d$IWRES,main='',xlab='IWRES',breaks=bks)
  #####################################################################

}
#################### END OF pIWRES ###########################


################################################################################
#' plots: DV VS. PRED, DV VS. IPRED, IWRES VS. PRED, IWRES VS. ind_var, QQ plot of IWRES, histogram of IWRES
#'
#' @author Rupert Austin
#'
#' @param data a data frame
#' @param ind_var character, name of the column containing the independent variable
#' @param log_ind_var logical. Should the independent variable be plotted on a log scale
#' @param bks breaks to be put in a histogram can be single integer, or vector of break points
#' @param fval f for lowess smoothing on qq plot
#' @param bins_PRED numeric vector bin boundaries for PRED
#' @param bins_ind_var numeric vector bin boundaries for independent variable
#' @param dv_units character, units of DV
#' @param ind_var_units character, units of independent variable
#' @param loglog logical whether to plot on log log scale
#' @param dv_label character, label for dv in plots
#' @param pt_col colour of points
#'
pIWRES=function(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,loglog,dv_label,pt_col){
  # retain only the observation records
  col_names=names(data)
  if('MDV'%in%col_names){
    d=data[data[,'MDV']==0,]
  }
  if('EVID'%in%col_names){
    d=data[data[,'EVID']==0,]
  }
  if(!('EVID'%in%col_names) & !('EVID'%in%col_names)){
    d=data
  }

  ### PLOT OF DV VS. PRED
  x_min=min(d[,'PRED'],d[,'DV'])*0.95
  x_max=max(d[,'PRED'],d[,'DV'])*1.05
  if(loglog==FALSE){
    plot(y=d[,'DV'],x=d[,'PRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('PRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',col=pt_col)
  }else{
    plot(y=d[,'DV'],x=d[,'PRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('PRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',log='xy',col=pt_col)
  }
  abline(a=0,b=1,col='black',lty=2)

  ### PLOT OF DV VS. IPRED
  x_min=min(d[,'IPRED'],d[,'DV'])*0.95
  x_max=max(d[,'IPRED'],d[,'DV'])*1.05
  if(loglog==FALSE){
    plot(y=d[,'DV'],x=d[,'IPRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('IPRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',col=pt_col)
  }else{
    plot(y=d[,'DV'],x=d[,'IPRED'],type='p',ylab=paste0(dv_label,' (',dv_units,')'),xlab=paste0('IPRED (',dv_units,')'),xlim=c(x_min,x_max),ylim=c(x_min,x_max),main='',log='xy',col=pt_col)
  }
  abline(a=0,b=1,col='black',lty=2)

  ### PLOT OF IWRES VS. IPRED
  if(loglog==FALSE){
    plot(x=d[,'IPRED'],y=d[,'IWRES'],type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='IWRES',main='',col=pt_col)
  }else{
    plot(x=d[,'IPRED'],y=d[,'IWRES'],type='p',xlab=paste0('IPRED (',dv_units,')'),ylab='IWRES',main='',log='x',col=pt_col)
  }
  abline(h=0,col='black',lty=2)
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,'IPRED'],y=d[,'IWRES'],f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_PRED[1])==FALSE){
    q=bin_stats(d,bins=bins_PRED,dv_col='IWRES',ind_var_col='IPRED')
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF IWRES VS. ind_var
  if(log_ind_var==TRUE) {
    plot(x=d[,ind_var],y=d[,'IWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='IWRES',main='',log='x',col=pt_col)
    abline(h=0,col='black',lty=2)
  }else{
    plot(x=d[,ind_var],y=d[,'IWRES'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='IWRES',main='',col=pt_col)
    abline(h=0,col='black',lty=2)
  }
  if(is.na(fval)==FALSE){
    rr=lowess(x=d[,ind_var],y=d[,'IWRES'],f=fval,iter=10)
    points(x=rr$x,y=rr$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_ind_var[1])==FALSE){
    q=bin_stats(d,bins=bins_ind_var,dv_col='IWRES',ind_var_col=ind_var)
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### QQ plot of IWRES
  qqnorm(d$IWRES,main='',xlab='IWRES theoretical quantiles',ylab='IWRES sample quantiles',col=pt_col)
  lines(c(-100,100),c(-100,100),col='black',lty=2)

  ### histogram of IWRES
  hist(d$IWRES,main='',xlab='IWRES',breaks=bks)
  #####################################################################

}
#################### END OF pIWRES_alt #######################


################################################################################
#' plots: NPDE VS. ind_var, NPDE VS. PRED, QQ plot of NPDE, histogram of NPDE
#'
#' @author Rupert Austin
#'
#' @param data a data frame
#' @param ind_var character, name of the column containing the independent variable
#' @param log_ind_var logical. Should the independent variable be plotted on a log scale
#' @param bks breaks to be put in a histogram can be single integer, or vector of break points
#' @param fval f for lowess smoothing on qq plot
#' @param bins_PRED numeric vector bin boundaries for PRED
#' @param bins_ind_var numeric vector bin boundaries for independent variable
#' @param dv_units character, units of DV
#' @param ind_var_units character, units of independent variable
#' @param pt_col colour of points
#'
pNPDE=function(data,ind_var,log_ind_var,bks,fval,bins_PRED,bins_ind_var,dv_units,ind_var_units,pt_col){
  # retain only the observation records
  col_names=names(data)
  if('MDV'%in%col_names){
    d=data[data[,'MDV']==0,]
  }
  if('EVID'%in%col_names){
    d=data[data[,'EVID']==0,]
  }
  if(!('EVID'%in%col_names) & !('EVID'%in%col_names)){
    d=data
  }

  ### PLOT OF NPDE VS. ind_var
  if(log_ind_var==TRUE) {
    plot(x=d[,ind_var],y=d[,'NPDE'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='NPDE',main='',log='x',col=pt_col)
    abline(h=0,col='black',lty=2)
  }else{
    plot(x=d[,ind_var],y=d[,'NPDE'],type='p',xlab=paste0(ind_var,' (',ind_var_units,')'),ylab='NPDE',main='',col=pt_col)
    abline(h=0,col='black',lty=2)
  }
  if(is.na(fval)==FALSE){
    rr=lowess(x=d[,ind_var],y=d[,'NPDE'],f=fval,iter=10)
    points(x=rr$x,y=rr$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_ind_var[1])==FALSE){
    q=bin_stats(d,bins=bins_ind_var,dv_col='NPDE',ind_var_col=ind_var)
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### PLOT OF NPDE VS. PRED
  plot(x=d[,'PRED'],y=d[,'NPDE'],type='p',xlab=paste0('PRED (',dv_units,')'),ylab='NPDE',main='',col=pt_col)
  abline(h=0,col='black',lty=2)
  if(is.na(fval)==FALSE){
    qq=lowess(x=d[,'PRED'],y=d[,'NPDE'],f=fval,iter=10)
    points(x=qq$x,y=qq$y,col='black',type='l',lwd=1)
  }
  if(is.na(bins_PRED[1])==FALSE){
    q=bin_stats(d,bins=bins_PRED,dv_col='NPDE',ind_var_col='PRED')
    for(i in 1:ncol(q)){
      lines(x=c(q[1,i],q[2,i]),y=c(q[7,i],q[7,i]),lwd=2,col='black')
    }
  }

  ### QQ plot of NPDE
  qqnorm(d$NPDE,main='',xlab='NPDE theoretical quantiles',ylab='NPDE sample quantiles',col=pt_col)
  lines(c(-100,100),c(-100,100),col='black',lty=2)

  ### histogram of NPDE
  hist(d$NPDE,main='',xlab='NPDE',breaks=bks)
  #####################################################################

}
################################################################################

#################### END OF pNPDE ############################



