#' @export

################################################################################
################################################################################
# function to generate general NPDE plots for NONMEM models
#
# function inputs:
# data=data frame containing output from NONMEM run
#	ind_var=name of the independent variable column, usually TIME or TIME_AFTER_DOSE.
#
# function outputs:
#	returns a panel of 4 different diagnostic NPDE plots
################################################################################
################################################################################
NPDEplotter=function(data,ind_var,log_ind_var,bks,fval){

  par(mfrow=c(2,2))
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

  # calculate smoothed curve for plot of NPDE vs. PRED
  qq=lowess(x=data1[,'PRED'],y=data1[,'NPDE'],f=fval,iter=10)

  # calculate smoothed curve for plot of NPDE vs. ind_var
  rr=lowess(x=data1[,ind_var],y=data1[,'NPDE'],f=fval,iter=10)

  if(log_ind_var==TRUE) {
    ### PLOT OF NPDE VS. independent variable
    plot(x=data1[,ind_var],y=data1[,'NPDE'],type="p",xlab=ind_var,ylab='NPDE',cex=1.3,cex.axis=1.4,cex.lab=1.4,log='x')
    abline(h=0,col='red',lty=2)
  } else {
    ### PLOT OF NPDE VS. independent variable
    plot(x=data1[,ind_var],y=data1[,'NPDE'],type="p",xlab=ind_var,ylab='NPDE',cex=1.3,cex.axis=1.4,cex.lab=1.4)
    abline(h=0,col='red',lty=2)
  }
  points(x=rr$x,y=rr$y,col='blue',type='l')


  ### PLOT OF NPDE VS. PRED
  plot(x=data1[,'PRED'],y=data1[,'NPDE'],type="p",xlab='PRED',ylab='NPDE',cex=1.3,cex.axis=1.4,cex.lab=1.4)
  abline(h=0,col='red',lty=2)
  points(x=qq$x,y=qq$y,col='blue',type='l')

  ### QQ plot of NPDE
  qqnorm(data1$NPDE,main='',xlab='NPDE theoretical quantiles',ylab='NPDE sample quantiles')
  lines(c(-10,10),c(-10,10),col='red',lty=2)

  ### histogram of NPDE
  hist(data1$NPDE,main='',xlab='NPDE',breaks=bks)
  #####################################################################

}
##############################################################
#################### END OF NPDEplotter #####################
##############################################################
