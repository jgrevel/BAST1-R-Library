#' @export

################################################################################
################################################################################
# function to generate general GOF plots for NONMEM models
#
# function inputs:
#	data=data frame containing output from NONMEM run
#	ind_var=name of the independent variable column, usually TIME or TIME_AFTER_DOSE.
#
# function outputs:
#	returns a panel of 8 different diagnostic plots
################################################################################
################################################################################
GOFplotter = function(data,ind_var,log_ind_var,bks,fval){

  par(mfrow=c(2,3))
  par(mar=c(4.2,4.3,3,1))
  par(oma=c(0,0,0,0))
  par(cex.lab=1.5)
  par(cex.main=1)
  par(cex.axis=1.5)

  #remove dosing records to leave just observation records
  col_names=names(data)
  if('MDV' %in% col_names){
    data1=data[data[,'MDV']==0,]
  } else {
    print('no MDV column in data file')
    stop()
  }

  #create copy of the DV column and call it OBS
  data1[,'OBS']=data1[,'DV']

  #create copy of the PRED column and call it PREDS
  data1[,'PREDS']=data1[,'PRED']

  #create y axis label
  y1='DV'
  #####################################################################

  ### PLOT OF DV VS. PRED
  x_min=min(data1[,'PRED'],data1[,'DV'])*0.95
  x_max=max(data1[,'PRED'],data1[,'DV'])*1.05
  plot(y=data1[,'DV'],x=data1[,'PRED'],type="p",ylab='DV',xlab='PRED',cex=1.3,cex.axis=1.4,cex.lab=1.4,xlim=c(x_min,x_max),ylim=c(x_min,x_max))
  abline(a=0,b=1,col='red',lty=2)

  ### PLOT OF DV VS. IPRED
  x_min=min(data1[,'IPRED'],data1[,'DV'])*0.95
  x_max=max(data1[,'IPRED'],data1[,'DV'])*1.05
  plot(y=data1[,'DV'],x=data1[,'IPRED'],type="p",ylab='DV',xlab='IPRED',cex=1.3,cex.axis=1.4,cex.lab=1.4,xlim=c(x_min,x_max),ylim=c(x_min,x_max))
  abline(a=0,b=1,col='red',lty=2)


  ### QQ plot of CWRES
  qqnorm(data1$CWRES,main='',xlab='CWRES theoretical quantiles',ylab='CWRES sample quantiles')
  lines(c(-10,10),c(-10,10),col='red',lty=2)

  # calculate smoothed curve for plot of CWRES vs. PRED
  qq=lowess(x=data1[,'PRED'],y=data1[,'CWRES'],f=fval,iter=10)

  # calculate smoothed curve for plot of CWRES vs. ind_var
  rr=lowess(x=data1[,ind_var],y=data1[,'CWRES'],f=fval,iter=10)


  ### PLOT OF CWRES VS. PRED
  plot(x=data1[,'PRED'],y=data1[,'CWRES'],type="p",xlab='PRED',ylab='CWRES',cex=1.3,cex.axis=1.4,cex.lab=1.4)
  abline(h=0,col='red',lty=2)
  points(x=qq$x,y=qq$y,col='blue',type='l')

  if(log_ind_var==TRUE) {
    ### PLOT OF CWRES VS. independent variable
    plot(x=data1[,ind_var],y=data1[,'CWRES'],type="p",xlab=ind_var,ylab='CWRES',cex=1.3,cex.axis=1.4,cex.lab=1.4,log='x')
    abline(h=0,col='red',lty=2)
  } else {
    ### PLOT OF CWRES VS. independent variable
    plot(x=data1[,ind_var],y=data1[,'CWRES'],type="p",xlab=ind_var,ylab='CWRES',cex=1.3,cex.axis=1.4,cex.lab=1.4)
    abline(h=0,col='red',lty=2)
  }
  points(x=rr$x,y=rr$y,col='blue',type='l')

  ### histogram of CWRES
  hist(data1$CWRES,main='',xlab='CWRES',breaks=bks)
  #####################################################################

}
##############################################################
#################### END OF GOFplotter ######################
##############################################################
