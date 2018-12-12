#' Plot range VPC using output of calc_rng_VPC function.
#'
#' Function for production of range VPC plots.
#'
#' @author Rupert Austin
#' @param VPC_results list. Output of calc_rng_VPC function.
#' @param x_lab label for the x axis.
#' @param y_lab label for the y axis.
#' @param add_simulated_median TRUE (default) or FALSE. Add simulated median quantile to bins in plot.
#' @param prediction_corrected TRUE (default) or FALSE. Produce a prediction-corrected range VPC plot.
#' @param plot_obs TRUE (default) or FALSE. Overlay observed data points on plot.
#' @param legend_position FALSE or "bottomright"/"bottom"/"bottomleft"/"left"/"topleft"/"top"/"topright"/"right"/"center". Descriptive position of legend in plot.
#'        FALSE ensures no legend is produced.
#' @param legend_size positive numeric. Typically in range 0.4 to 3 (from cex argument in legend).
#' @param bw TRUE or FALSE (default). If set to TRUE a black and white output is given, which is required for certain submission reports.
#' @param col_scheme defaults to FALSE. Can be used for setting a user-defined colouring of VPC plot elements using the format of: \deqn{col_scheme = c('gray90', 'black', 'orange', 'red'),} where:
#'
#'        1st colour is for shading of prediction interval of simulated data.
#'
#'        2nd colour is for 50th percentile of simulated data.
#'
#'        3rd colour is for outer experimental percentiles.
#'
#'        4th colour is for 50th experimental percentile.
#' @return A range VPC plot. Lines showing percentiles of experimental data in each bin are shown. A prediction interval around all simulated data
#'         in each bin is also shown.
#' @note The user can add standard R plot() arguments such as log='y', xlim=c(0,100), yaxt='n', main='VPC' etc. to customize the VPC plot.
#' @export



#############################################################################################################
#############################################################################################################
# Function for production of range VPC plots
#
# Returns a range VPC plot.
# lines showing percentiles of experimental data in each bin are shown
# A prediction interval around all simulated data in each bin is also shown
#
# function inputs:
#
# VPC_results=output of calc_rng_VPC function
# x_lab=label for the x axis (default='Time after dose (hours)')
# y_lab=label for the y axis (default='Prediction-corrected concentration (ug/L)')
# add_simulated_median=TRUE/FALSE (default=TRUE)
# prediction_corrected=TRUE/FALSE (default=TRUE)
# plot_obs=do you want to add observed data to the VPC plot (default=TRUE)
# legend_position=FALSE or "bottomright"/"bottom"/"bottomleft"/"left"/"topleft"/"top"/"topright"/"right"/"center" (default="topright"). If FALSE then no legend added
# legend_size=positive number typically in range 0.4 to 3 (default=0.7)
# bw=TRUE/FALSE (default=FALSE) If bw=TRUE then a black and white colour scheme is used, which is more suitable for submission reports
# col_scheme (default=FALSE) is for setting a user-defined colouring along the format of: col_scheme=c('gray90','black','orange','red') where:
# First colour is for shading of prediction interval of simulated data.
# Second colour is for 50th percentile of simulated data.
# 3rd colour is for outer experimental percentiles.
# 4th colour is for 50th experimental percentile.
#
# The user can add standard R plot() arguments such as log='y', xlim=c(0,100), yaxt='n' (along with separate axis() command), main='VPC' etc. to customize the VPC plot
#############################################################################################################
#############################################################################################################
plot_rng_VPC=function(VPC_results,x_lab='Time after dose (hours)',y_lab='Prediction-corrected concentration (ug/L)',
                      add_simulated_median=TRUE,prediction_corrected=TRUE,plot_obs=TRUE,bw=FALSE,legend_position='topright',
                      legend_size=0.7,col_scheme=FALSE,...)  {

  print('Default x-axis label is Time after dose (hours) and default y-axis label is Prediction corrected concentration (ug/L)')
  print('These default axis labels can be changed with the x_lab and y_lab arguments')
  print('Standard R plot() arguments such as log=y, xlim=c(0,100), yaxt=n, main=VPC etc. can be used to customize your VPC plot')
  print('If you have chosen to add a legend then you need to save the plot to a graphic file to view the legend correctly')

  dv_col=VPC_results$dv_col
  ind_var_col=VPC_results$ind_var_col
  exp_data_modified=VPC_results$exp_data_modified
  VPC_results=VPC_results$VPC_results

  # row numbers to use in VPC_results
  if(prediction_corrected==TRUE){
    vals=c(10:15)
  } else {
    vals=c(4:9)
  }

  # setup the colours. VPC_cols is setup like this:
  # First colour is for shading of prediction interval of simulate data.
  # Second colour is for 50th percentile of simulated data.
  # 3rd colour is for outer experimental percentiles.
  # 4th colour is for 50th experimental percentile.
  if(bw==FALSE){
    VPC_cols=c('gray90','black','orange','red') # For standard colour version.
  }
  if(bw==TRUE){
    VPC_cols=c('gray90','black','gray65','black') # For standard black and white version.
  }
  if(col_scheme[1]!=FALSE & length(col_scheme)==4){
    VPC_cols=col_scheme
  }

  # generate main plot
  par(mfrow=c(1,1))
  par(mar=c(4.5,4.5,2,1))
  par(oma=c(0,0,0,0))
  par(cex.lab=1.5)
  par(cex.main=1.5)
  par(cex.axis=1.5)
  plot(x=c(VPC_results[1,],VPC_results[2,]),y=c(VPC_results[vals[1],],VPC_results[vals[3],]),pch=25,type="n",ylab=y_lab,xlab=x_lab,...)

  # add lines and boxes
  t=VPC_results
  for (i in 1:ncol(t)) {
    # prediction interval for simulated data
    polygon(x=c(t[1,i],t[2,i],t[2,i],t[1,i]),y=c(t[vals[4],i],t[vals[4],i],t[vals[6],i],t[vals[6],i]),col=VPC_cols[1],border=VPC_cols[1],lwd=2)
    # add experimental percentiles
    lines(x=c(t[1,i],t[2,i]),y=c(t[vals[2],i],t[vals[2],i]),col=VPC_cols[4],lwd=2.5)
    lines(x=c(t[1,i],t[2,i]),y=c(t[vals[1],i],t[vals[1],i]),col=VPC_cols[3],lwd=2.5)
    lines(x=c(t[1,i],t[2,i]),y=c(t[vals[3],i],t[vals[3],i]),col=VPC_cols[3],lwd=2.5)
    # add simulated median
    if(add_simulated_median==TRUE) {
      lines(x=c(t[1,i],t[2,i]),y=c(t[vals[5],i],t[vals[5],i]),col=VPC_cols[2],lwd=1.5,lty=3)
    }
  }

  # add observed data
  if(plot_obs==TRUE & prediction_corrected==TRUE){
    points(x=exp_data_modified[,ind_var_col],y=exp_data_modified[,'DV_PRED_CORRECTED'],cex=0.5)
  }
  if(plot_obs==TRUE & prediction_corrected==FALSE){
    points(x=exp_data_modified[,ind_var_col],y=exp_data_modified[,dv_col],cex=0.5)
  }

  # add legend
  if(legend_position!=FALSE){
    if(!legend_position%in%c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center")){
      stop("Legend position not recognised try one of the following: bottomright,bottom,bottomleft,left,topleft,top,topright,right,center")
    } else {
      # add the legend
      mid_pct=as.numeric(substr(rownames(VPC_results)[5],5,nchar(rownames(VPC_results)[5])-1))
      low_pct=as.numeric(substr(rownames(VPC_results)[4],5,nchar(rownames(VPC_results)[4])-1))
      hgh_pct=as.numeric(substr(rownames(VPC_results)[6],5,nchar(rownames(VPC_results)[6])-1))
      conf_int=hgh_pct-low_pct

      if(prediction_corrected==TRUE){
        txt1=paste0(low_pct,'th and ',hgh_pct,'th percentiles of prediction-corrected observations in bin')
        txt2=paste0(mid_pct,'th percentile of prediction-corrected observations in bin')
        txt3=paste0(mid_pct,'th percentile of prediction-corrected simulated values in bin')
        txt4=paste0(conf_int,'% prediction interval of prediction-corrected simulated values in bin')
        txt5='Prediction-corrected observation'
      } else {
        txt1=paste0(low_pct,'th and ',hgh_pct,'th percentiles of observations in bin')
        txt2=paste0(mid_pct,'th percentile of observations in bin')
        txt3=paste0(mid_pct,'th percentile of simulated values in bin')
        txt4=paste0(conf_int,'% prediction interval of simulated values in bin')
        txt5='Observation'
      }
      if(add_simulated_median==TRUE & plot_obs==TRUE){
        legend(legend_position,c(txt1,txt2,txt3,txt4,txt5),
               col=c(VPC_cols[3],VPC_cols[4],VPC_cols[2],VPC_cols[1],'black'),
               pch=c(NA,NA,NA,15,1),pt.cex=c(NA,NA,NA,2,1),lty=c(1,1,3,NA,NA),lwd=c(2.5,2.5,1.5,NA,NA),cex=legend_size)
      }
      if(add_simulated_median==TRUE & plot_obs==FALSE){
        legend(legend_position,c(txt1,txt2,txt3,txt4),
               col=c(VPC_cols[3],VPC_cols[4],VPC_cols[2],VPC_cols[1]),
               pch=c(NA,NA,NA,15),pt.cex=c(NA,NA,NA,2),lty=c(1,1,3,NA),lwd=c(2.5,2.5,1.5,NA),cex=legend_size)
      }
      if(add_simulated_median==FALSE & plot_obs==TRUE){
        legend(legend_position,c(txt1,txt2,txt4,txt5),
               col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1],'black'),
               pch=c(NA,NA,15,1),pt.cex=c(NA,NA,2,1),lty=c(1,1,NA,NA),lwd=c(2.5,2.5,NA,NA),cex=legend_size)
      }
      if(add_simulated_median==FALSE & plot_obs==FALSE){
        legend(legend_position,c(txt1,txt2,txt4),
               col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1]),
               pch=c(NA,NA,15),pt.cex=c(NA,NA,2),lty=c(1,1,NA),lwd=c(2.5,2.5,NA),cex=legend_size)
      }
    }
  }
}
################################################################################
############  END OF plot_rng_VPC FUNCTION  ####################################
################################################################################


