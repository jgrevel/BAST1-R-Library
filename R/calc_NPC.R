#' Calculate and plot NPC.
#'
#' Function for performing a Numeric Predictive Check (NPC).
#'
#' @author Rupert Austin, Aaron Hayman
#' @param exp_data data frame containing experimental data.
#' @param sim_data data frame containing all simulated data. Should have the same data points as exp_data, simply multiple simulations of them.
#' @param pct vector of the sizes (in \%) of the prediction intervals to be tested.
#' @param nreps numeric. The number of simulations performed.
#' @param dv_col name of column containing DV, for both exp_data and sim_data.
#' @param full TRUE (default) or FALSE. If TRUE, then the function returns maximum output and plots (see Details).
#' @param save_path path for folder to save plots.
#' @param ind_var_col name of column for independent variable for normalised NPC plots (normally TIME or TAD), for both exp_data and sim_data.
#' @param bins numeric vector defining bins for the normalised NPC plots. If set to FALSE then normalised NPC plots not generated.
#' @param name_stub string to be added to the file names of the 3 plots that are saved by the function.
#'
#' @details If full==FALSE: \cr
#'          This function will return a named numeric vector, the same length as pct indicating what \% of the experimental
#'          data lies within each prediction interval tested.
#'
#'          If full==TRUE: \cr
#'          This function will return a list with 8 elements:
#'          \itemize{
#'            \item $exp_count
#'            \item $df_NPC
#'            \item $pct_within_interval
#'            \item $pct_above_interval
#'            \item $pct_below_interval
#'            \item $n_within_interval
#'            \item $n_above_interval
#'            \item $n_below_interval
#'          }
#'          $exp_count is simply the number of experimental data points. \cr
#'          $df_NPC is a copy of exp_data with some columns added: 5th, 50th, 95th percentiles of simulated data; normalised_NPC, defined as
#'          \deqn{normalised_NPC = (DV - median of simulations) / abs(5th OR 95th percentile of simulations - median of simulations),}
#'          where 95th percentile is used if \eqn{DV > median} and 5th percentile is used if \eqn{DV < median}. \cr
#'          If \eqn{normalised_NPC = 0}, then observation = median of simulations. \cr
#'          If \eqn{normalised_NPC = +1}, then observation = 95th percentile of simulations. \cr
#'          If \eqn{normalised_NPC = -1}, then observation = 5th percentile of simulations. \cr
#'          Note that \deqn{$pct_within_interval + $pct_above_interval + $pct_below_interval = 100.}
#'
#'
#' @return A series of NPC plots, as well as a list or vector of information (see Details).
#' @note Deciding if a number is within the prediction interval is inclusive at both ends. i.e. if the number is equal to either of the quantile
#'       limits, it will be considered to be in the interval.
#' @export



######################################################################################################
####                                         calc_NPC                                             ####
######################################################################################################
#
#	Function for performing numeric predictive check
#	Args:
#		exp_data:	    the experimental data frame
#		sim_data:	    the simulated data frame should have the same data points as the exp_data, simply multiple simulations of them
#		pct:		      the sizes (in percent) of the prediction intervals to be tested (an optional vector of numbers)
#		nreps:		    the number of simulations performed
#		dv_col:		    the column to be compared, normally is DV
#   full:         if full==TRUE then the function returns maximum output and plots
#   save_path:    Path for folder to save plot
#   ind_var_col:  Name of column for independent variable for normalised NPC plots (normally TIME or TAD)
#   bins:         numeric vector defining bins for the normalised NPC plots. If set to FALSE then normalised NPC plots not generated
#   name_stub:    A text string to be added to the file names of the 3 plots that are saved by the function
#
# 	If full==FALSE This function will return a named numeric vector, the same length as pct
#	  indicating what percent of the experimental data lies within each prediction interval tested
# 	If full==TRUE This function will return a list with 8 elements: $exp_count, $df_NPC, $pct_within_interval, $pct_above_interval,
#   $pct_below_interval, $n_within_interval, $n_above_interval, $n_below_interval
#   $exp_count is simply the number of experimental data points
#   $df_NPC is a copy of exp_data with some columns added: 5th, 50th, 95th percentiles of simulated data, normalised NPC
#   Normalised NPC = (DV - median of simulations)/abs(5th or 95th percentile of simulations - median of simulation) where 95th percentile is used if DV>median and 5th percentile is used if DV<median
#   If normalised_NPC=0 then observation = median of simulation
#   If normalised_NPC=+1 then observation = 95th percentile of simulations
#   If normalised_NPC=-1 then observation = 5th percentile of simulations
#
#   Note that $pct_within_interval+$pct_above_interval+$pct_below_interval=100
#
#	  deciding if a number is within the prediction interval is inclusive at both ends. i.e. if the number is equal to either of the quantile
#	  limits it will be considered to be in the interval
#
#####################################################################################################
calc_NPC=function(exp_data,sim_data,pct=c(10,20,30,40,50,60,70,75,80,85,90),nreps,dv_col='DV',full=TRUE,save_path='',ind_var_col='TAD',bins=FALSE,name_stub=''){
  if(nrow(sim_data)!=nreps*nrow(exp_data))			# each sim from sim_data should match the rows of exp_data
  {								# function makes this simple check if the numbers suggest this is true
    stop('sim_data and exp_data rows do not match.')	# if this is not the case an error is thrown
  }
  col=dv_col
  ind_var=ind_var_col

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
  # check if BLQ records are accidentally included
  if('LLOQFLAG'%in%names(exp_data)){
    ed1=exp_data[exp_data$LLOQFLAG!=0,]
    nr=nrow(ed1)
    if(nr!=0){
      stop('BLQ records have been found within exp_data. Please remove BLQ records then re-run.')
    }
  }
  if('LLOQFLAG'%in%names(sim_data)){
    sd1=sim_data[sim_data$LLOQFLAG!=0,]
    nr=nrow(sd1)
    if(nr!=0){
      stop('BLQ records have been found within sim_data. Please remove BLQ records then re-run.')
    }
  }

  print(paste0('Prediction intervals used: ',paste0(pct,collapse=',')))

  # check for existence of REPL column and add if necessary
  cnms=names(sim_data)
  if(!('REPL' %in% cnms)){
    sim_data$REPL=rep(1:nreps,each=nrow(exp_data))
  }

  if(nchar(name_stub)>0 & substr(name_stub,1,1)!="_"){
    name_stub=paste0("_",name_stub)
  }

  ###################################################
  # now do normalised NPC calcs
  qs=c(0.95,0.5,0.05)						# for normalized NPC this is quantiles 0.95,0.5,0.05
  n=nrow(exp_data)						# rows in exp_data
  dv=matrix(sim_data[,col],ncol=n,byrow=TRUE)			# sim_data DV col arranged into a matrix such that each column is all the simulations of a particular data point
  r=nrow(dv)							# this will be equal to nreps
  dv[]=dv[order(rep(1:n,each=r),dv)]				# the content of each column is sorted.
  ind=rep(r-1,each=length(qs))*qs+1 				# theoretical row indices (non-integer) for quantiles
  qdv=t(dv[floor(ind),]*(1-ind%%1)+dv[ceiling(ind),]*ind%%1)	# matrix of quantile values for each (repeated) data point in simulations
  # 1st column is 95th percentile, 2nd column is median, 3rd column is 5th percentile
  exp_vals=exp_data[,col] # get observations
  qdv=cbind(qdv,exp_vals) # add observations to qdv
  qdv=data.frame(qdv)
  names(qdv)=c('pct95','pct50','pct5','obs')

  # now do the normalised NPC calculation
  # Normalised NPC = (DV - median of simulations)/|5th or 95th percentile of simulations - median of simulation| where 95th percentile is used if DV>median and 5th percentile is used if DV<median
  qdv$MED_GT_OBS=qdv$obs>qdv$pct50
  qdv$limit=ifelse(qdv$MED_GT_OBS==TRUE,qdv$pct95,qdv$pct5)
  qdv$NPC_norm=(qdv$obs - qdv$pct50)/abs(qdv$limit - qdv$pct50)
  # now add results to exp_data
  exp_data$percentile_5_of_simulations=qdv$pct5
  exp_data$percentile_50_of_simulations=qdv$pct50
  exp_data$percentile_95_of_simulations=qdv$pct95
  exp_data$normalised_NPC=qdv$NPC_norm

  # now calculate the corrected normalised NPC values
  cn_NPC=exp_data$normalised_NPC
  cn_NPC[cn_NPC<(-1)]=-1.2
  cn_NPC[cn_NPC>1]=1.2
  exp_data$corrected_normalised_NPC=cn_NPC

  # calculate pct above, below, within -1/+1 range
  pct_above=round(100*length(exp_data$corrected_normalised_NPC[exp_data$corrected_normalised_NPC==1.2])/nrow(exp_data),2)
  pct_below=round(100*length(exp_data$corrected_normalised_NPC[exp_data$corrected_normalised_NPC==-1.2])/nrow(exp_data),2)
  pct_within=100-pct_above-pct_below
  plot_text=paste0(pct_above,'% > +1  :  ',pct_below,'% < -1  :  ',pct_within,'% between -1 and +1')
  ###################################################

  ###################################################
  # now calculate percentages of observations below, within and above prediciton intervals
  qs=c(pct,-pct)/200+0.5						# quantiles needed to calculate the confidence intevals
  n=nrow(exp_data)						# rows in exp_data
  dv=matrix(sim_data[,col],ncol=n,byrow=TRUE)			# sim_data DV col arranged into a matrix such that each column is all the simulations of a particular data point
  r=nrow(dv)							# this will be equal to nreps
  dv[]=dv[order(rep(1:n,each=r),dv)]				# the content of each column is sorted.
  ind=rep(r-1,each=length(qs))*qs+1 				# theoretical row indices (non-integer) for quantiles
  qdv=t(dv[floor(ind),]*(1-ind%%1)+dv[ceiling(ind),]*ind%%1)	# matrix of quantile values for each (repeated) data point in simulations
  udv=qdv[,1:length(pct)]						# upper quantiles
  ldv=qdv[,-(1:length(pct))]					# lower quantiles
  ret=colMeans(exp_data[,col]>=ldv & exp_data[,col]<=udv)*100	# percents of data within (inclusive at both ends) prediction intervals
  names(ret)=paste0('CI_',pct,'%')
  ###################################################

  if(full==FALSE){
    return(ret)
  } else {
    ret_n=colMeans(exp_data[,col]>=ldv & exp_data[,col]<=udv)*n  # counts of data within (inclusive at both ends) prediction intervals
    names(ret_n)=paste0('n_CI_',pct,'%')
    ret_pct_above=colMeans(exp_data[,col]>udv)*100  # percents of data above top end of prediction interval
    names(ret_pct_above)=paste0('>CI_',pct,'%')
    ret_n_above=colMeans(exp_data[,col]>udv)*n  # counts of data above top end of prediction interval
    names(ret_n_above)=paste0('n>CI_',pct,'%')
    ret_pct_below=colMeans(exp_data[,col]<ldv)*100  # percents of data below bottom end of prediction interval
    names(ret_pct_below)=paste0('<CI_',pct,'%')
    ret_n_below=colMeans(exp_data[,col]<ldv)*n  # counts of data below bottom end of prediction interval
    names(ret_n_below)=paste0('n<CI_',pct,'%')
    res=list(exp_count=n,df_NPC=exp_data,pct_within_interval=ret,n_within_interval=ret_n,pct_above_interval=ret_pct_above,n_above_interval=ret_n_above,pct_below_interval=ret_pct_below,n_below_interval=ret_n_below)

    ##############################################
    ### produce diagnostic plots
    if(save_path!=''){
      png(file=paste0(save_path,'NPC_plot',name_stub,'.png'),width=1250,height=650)
    }
    par(mfrow=c(1,3))
    if(par('new')) par(mfg=c(1,1))	# line added 15/04/2016 to allow adding plots within an existing window
    par(mar=c(5,5,2,1))
    par(cex.lab=1.8)
    par(cex.main=1.8)
    par(cex.axis=1.8)
    cx=2
    cx1=1.2
    pct_vals=pct
    q=res
    # plot basic NPC plot showing percentage of observations within each prediction interval
    plot(x=pct_vals,y=q$pct_within_interval,xlim=c(0,100),xaxt='n',yaxt='n',ylim=c(-5,100),xlab='Prediction interval (%)',ylab='Percent of observations within prediction interval',cex=cx)
    lines(x=c(-100,100),y=c(-100,100),col='black',lty=2)
    abline(h=0)
    xv1=c(0,10,20,30,40,50,60,70,80,90,100)
    axis(1,xv1,as.character(xv1),las=0)
    axis(2,xv1,as.character(xv1),las=0)
    text(x=0,y=95,paste0('Total observations = ',n),cex=1.2,pos=4)
    text(x=-2,y=-3,'n',cex=cx1,pos=1)
    text(x=pct_vals,y=rep(c(-1,-5),length.out=length(pct_vals)),q$n_within_interval,pos=1,cex=cx1)

    ylm=c(min(2,0.95*q$pct_below_interval[q$pct_below_interval>0],0.95*q$pct_above_interval[q$pct_above_interval>0]),max(50,1.05*q$pct_below_interval[q$pct_below_interval>0],1.05*q$pct_above_interval[q$pct_above_interval>0]))
    xlm=c(0.95*min(pct_vals),100)
    plot(x=pct_vals[q$pct_below_interval>0],y=q$pct_below_interval[q$pct_below_interval>0],col='blue',xaxt='n',xlim=xlm,xlab='Prediction interval (%)',ylab='Percent of observations above and below prediction interval',cex=cx,ylim=ylm,log='y')
    points(x=pct_vals[q$pct_above_interval>0],y=q$pct_above_interval[q$pct_above_interval>0],col='red',cex=cx)
    axis(1,xv1,as.character(xv1),las=0)
    xv=c(0:99,99.9)
    yv=50-0.5*xv
    points(x=xv,y=yv,type='l',lty=2)
    legend('bottomleft',c('Above prediciton interval','Below prediction interval'),pch=c(1,1),col=c('red','blue'),cex=1.3,bty='n')
    # add lines indicating deviations from expected behaviour
    corr_val=0.4
    for(m in 1:length(pct_vals)){
      lines(x=c(pct_vals[m]+corr_val,pct_vals[m]+corr_val),y=c((100-pct_vals[m])/2,q$pct_above_interval[m]),col='red')
      lines(x=c(pct_vals[m],pct_vals[m]),y=c((100-pct_vals[m])/2,q$pct_below_interval[m]),col='blue')
    }

    # now plot percentage of observations outside prediction interval that are above / below the prediction interval
    n_out_of_interval=q$n_below_interval+q$n_above_interval
    y1=100*q$n_above_interval/n_out_of_interval
    y2=100*q$n_below_interval/n_out_of_interval
    inf_test=y1==Inf
    y1=y1[inf_test==FALSE]
    x1=pct_vals[inf_test==FALSE]
    inf_test=y2==Inf
    y2=y2[inf_test==FALSE]
    plot(x=x1,y=y1,type='b',cex=cx,xlim=c(-4,100),ylim=c(-20,100),yaxt='n',xaxt='n',col='red',xlab='Prediction interval (%)',ylab='% of observations outside prediction interval that are above or below the interval')
    points(x=x1,y=y2,type='b',cex=cx,col='blue')
    axis(2,xv1,as.character(xv1),las=0)
    axis(1,xv1,as.character(xv1),las=0)
    abline(h=50,lty=2)
    abline(h=0)
    text(x=-4,y=-1,'n>',cex=cx1,pos=1)
    text(x=pct_vals,y=rep(c(-1,-5),length.out=length(pct_vals)),q$n_above_interval,pos=1,cex=cx1)
    text(x=-4,y=-14,'n<',cex=cx1,pos=1)
    text(x=pct_vals,y=rep(c(-14,-18),length.out=length(pct_vals)),q$n_below_interval,pos=1,cex=cx1)
    legend('topleft',c('Above prediciton interval','Below prediction interval'),pch=c(1,1),col=c('red','blue'),cex=1.3,bty='n')
    if(save_path!=''){
      dev.off()
    }
    ##############################################

    ##############################################
    ### produce copy of main diagnostic plot
    if(save_path!=''){
      png(file=paste0(save_path,'NPC_plot1',name_stub,'.png'),width=750,height=650)
    }
    par(mfrow=c(1,1))
    if(par('new')) par(mfg=c(1,1))	# line added 15/04/2016 to allow adding plots within an existing window
    par(mar=c(5,5,2,1))
    par(cex.lab=1.5)
    par(cex.main=1.5)
    par(cex.axis=1.5)
    cx=2
    cx1=1
    pct_vals=pct
    q=res
    # plot basic NPC plot showing percentage of observations within each prediction interval
    plot(x=pct_vals,y=q$pct_within_interval,xlim=c(0,100),xaxt='n',yaxt='n',ylim=c(-5,100),xlab='Prediction interval (%)',ylab='Percent of observations within prediction interval',cex=cx)
    lines(x=c(-100,100),y=c(-100,100),col='black',lty=2)
    abline(h=0)
    xv1=c(0,10,20,30,40,50,60,70,80,90,100)
    axis(1,xv1,as.character(xv1),las=0)
    axis(2,xv1,as.character(xv1),las=0)
    text(x=0,y=95,paste0('Total observations = ',n),cex=1.2,pos=4)
    text(x=-2,y=-3,'n',cex=cx1,pos=1)
    text(x=pct_vals,y=rep(c(-1,-5),length.out=length(pct_vals)),q$n_within_interval,pos=1,cex=cx1)

    if(save_path!=''){
      dev.off()
    }
    ##############################################

    ##########################################################
    # normalised NPC plot
    if(length(bins)>1){
      if(save_path!=''){
        png(file=paste0(save_path,'normalised_NPC_plot',name_stub,'.png'),width=800,height=650)
      }
      par(mfrow=c(1,1))
      par(mar=c(5,5,2,1))
      par(cex.lab=1.5)
      par(cex.main=1.5)
      par(cex.axis=1.5)

      q1=q$df_NPC

      # calculate median normalised_NPC in each bin
      cdv='normalised_NPC'
      col_time=ind_var
      d=q$df_NPC
      p_res=extract_exp_data(d,bins,cdv,ind_var)

      # now check bin widths and modify according to value of min_bin_width
      min_bin_width=0
      for(i in 1:(length(bins)-1)){
        if(p_res$ex_res[2,i]-p_res$ex_res[1,i]<=min_bin_width){
          p_res$ex_res[2,i]=p_res$ex_res[2,i]*1.05
        }
      }
      for(i in 2:(length(bins)-1)){
        if(p_res$ex_res[1,i]<p_res$ex_res[2,i-1]){
          p_res$ex_res[2,i-1]=p_res$ex_res[2,i-1] - 1.05*(p_res$ex_res[2,i-1]-p_res$ex_res[1,i])
        }
      }

      plot(x=q1[,ind_var],y=q1[,'normalised_NPC'],ylab='Normalised fractional deviation from median',xlab=ind_var)
      abline(h=1,col='black',lty=1)
      abline(h=-1,col='black',lty=1)
      abline(h=0,col='black',lty=1)
      for(i in 1:ncol(p_res$ex_res)){
        lines(x=c(p_res$ex_res[1,i],p_res$ex_res[2,i]),y=c(p_res$ex_res[4,i],p_res$ex_res[4,i]),col='red',lwd=2)
      }

      if(save_path!=''){
        dev.off()
      }
    }
    ##########################################################

    ##########################################################
    # corrected normalised NPC plot
    if(length(bins)>1){
      if(save_path!=''){
        png(file=paste0(save_path,'corrected_normalised_NPC_plot',name_stub,'.png'),width=800,height=650)
      }
      par(mfrow=c(1,1))
      par(mar=c(5,5,2,1))
      par(cex.lab=1.5)
      par(cex.main=1.5)
      par(cex.axis=1.5)

      # calculate median corrected normalised_NPC in each bin
      cdv='corrected_normalised_NPC'
      col_time=ind_var
      d=q$df_NPC
      p_res=extract_exp_data(d,bins,cdv,ind_var)

      # now check bin widths and modify according to value of min_bin_width
      min_bin_width=0
      for(i in 1:(length(bins)-1)){
        if(p_res$ex_res[2,i]-p_res$ex_res[1,i]<=min_bin_width){
          p_res$ex_res[2,i]=p_res$ex_res[2,i]*1.05
        }
      }
      for(i in 2:(length(bins)-1)){
        if(p_res$ex_res[1,i]<p_res$ex_res[2,i-1]){
          p_res$ex_res[2,i-1]=p_res$ex_res[2,i-1] - 1.05*(p_res$ex_res[2,i-1]-p_res$ex_res[1,i])
        }
      }
      set.seed(435671)
      plot(x=q1[,ind_var][q1$corrected_normalised_NPC<=1 & q1$corrected_normalised_NPC>=(-1)],y=q1[,'corrected_normalised_NPC'][q1$corrected_normalised_NPC<=1 & q1$corrected_normalised_NPC>=(-1)],ylab='Corrected normalised fractional deviation from median',xlab=ind_var,ylim=c(-1.5,1.5))
      points(x=q1[,ind_var][q1$corrected_normalised_NPC>1 | q1$corrected_normalised_NPC<(-1)],y=jitter(q1[,'corrected_normalised_NPC'][q1$corrected_normalised_NPC>1 | q1$corrected_normalised_NPC<(-1)],factor=0.25))
      abline(h=1,col='black',lty=1)
      abline(h=-1,col='black',lty=1)
      abline(h=0,col='black',lty=1)
      for(i in 1:ncol(p_res$ex_res)){
        lines(x=c(p_res$ex_res[1,i],p_res$ex_res[2,i]),y=c(p_res$ex_res[4,i],p_res$ex_res[4,i]),col='red',lwd=2)
      }
      text(x=min(q1[,ind_var]),y=1.5,plot_text,pos=4,cex=1.25)
      dev.off()
    }
    ##########################################################

    if(save_path!=''){
      print(paste0('NPC plot(s) saved to folder: ',save_path))
    }

    ##############################################

    return(res)
  }
}
################################################################
################## END OF FUNCTION calc_NPC ####################
################################################################

