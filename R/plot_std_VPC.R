#' Plot VPC using output of calc_std_VPC function.
#'
#' Function for production of standard VPC plots.
#'
#' @author Rupert Austin
#' @param VPC_results list. Output of calc_std_VPC function.
#' @param x_lab label for the x axis.
#' @param y_lab label for the y axis.
#' @param add_simulated_median TRUE (default) or FALSE. Add simulated median quantile to bins in plot.
#' @param prediction_corrected TRUE (default) or FALSE. Produce a prediction-corrected VPC plot.
#' @param plot_outer TRUE (default) or FALSE. If FALSE then the outer percentiles (typically 10 and 90) are omitted from the VPC. Important with small experimental datasets.
#' @param plot_obs TRUE (default) or FALSE. Overlay observed data points on plot.
#' @param legend_position FALSE or "bottomright"/"bottom"/"bottomleft"/"left"/"topleft"/"top"/"topright"/"right"/"center". Descriptive position of legend in plot.
#'        FALSE ensures no legend is produced.
#' @param legend_size positive numeric. Typically in range 0.4 to 3 (from cex argument in legend).
#' @param bw TRUE or FALSE (default). If set to TRUE a black and white output is given, which is required for certain submission reports.
#' @param col_scheme   defaults to FALSE. Can be used for setting a user-defined colouring of VPC plot elements using the format of:
#'        \deqn{col_scheme = c('gray90', 'black', 'orange', 'red'),} where:
#'
#'        1st colour is for shading of outer confidence intervals of simulated data.
#'
#'        2nd colour is for 50th percentiles of simulated data in outer percentiles and median.
#'
#'        3rd colour is for outer experimental percentiles.
#'
#'        4th colour is for 50th experimental percentile.
#' @return A standard VPC plot. By default, lines showing 10th percentile, 50th percentile and
#'         90th percentile of experimental and simulated data are shown.
#'         95\% confidence intervals around the simulated 10, 50, 90 percentiles are shown as shaded boxes.
#' @note The user can add standard R plot() arguments such as log='y', xlim=c(0,100), yaxt='n', main='VPC' etc. to customize the VPC plot.
#' @export



#############################################################################################################
#############################################################################################################
# function for production of standard VPC plots
#
# Returns a standard VPC plot.  By default, lines showing 10th percentile, 50th percentile and
# 90th percentile of experimental and simulated data are shown.
# 95% confidence intervals around the simulated 10, 50, 90 percentiles are shown as shaded boxes
#
# function inputs:
#	VPC_results=output of fast_VPC function
#	x_lab=label for the x axis
#	y_lab=label for the y axis
# add_simulated_median=TRUE/FALSE (default=TRUE)
# prediction_corrected=TRUE/FALSE (default=TRUE)
#	plot_outer=TRUE/FALSE (default=TRUE). If FALSE then the outer percentiles (typically 10 and 90) are omitted from the VPC. Important with small experimental datasets
# plot_obs=do you want to add observed data to the VPC plot (default=TRUE)
# legend_position=FALSE or "bottomright"/"bottom"/"bottomleft"/"left"/"topleft"/"top"/"topright"/"right"/"center" (default="topright")
# legend_size=positive number typically in range 0.4 to 3 (default=0.7)
# bw=TRUE/FALSE (default=FALSE) If bw=TRUE then a black and white colour scheme is used.
# col_scheme (default=FALSE) is for setting a user-defined colouring along the format of: col_scheme=c('gray90','black','orange','red') where:
# 1st colour is for shading of outer confidence intervals of simulated data.
# 2nd colour is for 50th percentiles of simulated data in outer percentiles and median.
# 3rd colour is for outer experimental percentiles.
# 4th colour is for 50th experimental percentile.
#
# The user can add standard R plot() arguments such as log='y', xlim=c(0,100), yaxt='n' (along with separate axis() command), main='VPC' etc. to customize the VPC plot
#############################################################################################################
#############################################################################################################
plot_std_VPC=function(VPC_results,x_lab='Time after dose (hours)',y_lab='Prediction-corrected concentration (ug/L)',
                      add_simulated_median=TRUE,prediction_corrected=TRUE,plot_obs=FALSE,plot_outer=TRUE,
                      legend_position='topright',bw=FALSE,legend_size=0.7,col_scheme=FALSE,...)  {

  print('Default x-axis label is Time after dose (hours) and default y-axis label is Prediction corrected concentration (ug/L)')
  print('These default axis labels can be changed with the x_lab and y_lab arguments')
  print('Standard R plot() arguments such as log=y, xlim=c(0,100), yaxt=n, main=VPC etc. can be used to customize your VPC plot')
  print('If you have chosen to add a legend then you need to save the plot to a graphic file to view the legend correctly')

  dv_col=VPC_results$dv_col
  ind_var_col=VPC_results$ind_var_col
  exp_data_modified=VPC_results$exp_data_analysed
  percentiles=VPC_results$percentiles
  conf_int=VPC_results$conf_int
  e=VPC_results$ex_res
  s=VPC_results$sim_res
  t1=e[1,]
  t2=e[2,]

  # row numbers to use in VPC_results$ex_res and VPC_results$sim_res
  if(prediction_corrected==FALSE){
    ex_vals=3:5
    sim_vals=1:9
  } else {
    ex_vals=6:8
    sim_vals=10:18
  }

  # setup the colours. VPC_cols is setup like this:
  # 1st colour is for shading of outer confidence intervals of simulated data.
  # 2nd colour is for 50th percentiles of simulated data.
  # 3rd colour is for outer experimental percentiles.
  # 4th colour is for 50th experimental percentile.
  if(bw==FALSE){
    VPC_cols=c('gray90','black','orange','red')
  }
  if(bw==TRUE){
    VPC_cols=c('gray90','black','gray65','black')
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
  plot(x=c(e[1,],e[2,]),y=c(s[sim_vals[1],],s[sim_vals[9],]),type="n",ylab=y_lab,xlab=x_lab,...)

  # add lines and shaded regions
  for(i in 1:ncol(e)){
    if(plot_outer==TRUE){
      # shaded region for 10th percentiles
      polygon(c(t1[i],t2[i],t2[i],t1[i]),c(s[sim_vals[1],i],s[sim_vals[1],i],s[sim_vals[3],i],s[sim_vals[3],i]),col=VPC_cols[1],border=VPC_cols[1])
      # observed 10th percentile
      lines(c(t1[i],t2[i]),c(e[ex_vals[1],i],e[ex_vals[1],i]),col=VPC_cols[3],lwd=2.5)
      if(add_simulated_median==TRUE){
        lines(c(t1[i],t2[i]),c(s[sim_vals[2],i],s[sim_vals[2],i]),col=VPC_cols[2],lwd=1.5,lty=3)
      }
      # shaded region for 90th percentiles
      polygon(c(t1[i],t2[i],t2[i],t1[i]),c(s[sim_vals[7],i],s[sim_vals[7],i],s[sim_vals[9],i],s[sim_vals[9],i]),col=VPC_cols[1],border=VPC_cols[1])
      # observed 90th percentile
      lines(c(t1[i],t2[i]),c(e[ex_vals[3],i],e[ex_vals[3],i]),col=VPC_cols[3],lwd=2.5)
      if(add_simulated_median==TRUE){
        lines(c(t1[i],t2[i]),c(s[sim_vals[8],i],s[sim_vals[8],i]),col=VPC_cols[2],lwd=1.5,lty=3)
      }
    }

    # shaded region for 50th percentiles
    polygon(c(t1[i],t2[i],t2[i],t1[i]),c(s[sim_vals[4],i],s[sim_vals[4],i],s[sim_vals[6],i],s[sim_vals[6],i]),col='white',border='black',density=0)
    # observed 50th percentile
    lines(c(t1[i],t2[i]),c(e[ex_vals[2],i],e[ex_vals[2],i]),col=VPC_cols[4],lwd=2)
    if(add_simulated_median==TRUE){
      lines(c(t1[i],t2[i]),c(s[sim_vals[5],i],s[sim_vals[5],i]),col=VPC_cols[2],lwd=1.5,lty=3)
    }
  }

  # add observed data
  if(plot_obs==TRUE & prediction_corrected==TRUE){
    points(x=exp_data_modified[,ind_var_col],y=exp_data_modified[,'DV_Prediction_Corrected'],cex=0.5)
  }
  if(plot_obs==TRUE & prediction_corrected==FALSE){
    points(x=exp_data_modified[,ind_var_col],y=exp_data_modified[,dv_col],cex=0.5)
  }

  # add legend
  if(legend_position!=FALSE){
    sc=0.8
    if(!legend_position%in%c("bottomright","bottom","bottomleft","left","topleft","top","topright","right","center")){
      stop("Legend position not recognised try one of the following: bottomright,bottom,bottomleft,left,topleft,top,topright,right,center")
    } else {
      # add the legend
      mid_pct=percentiles[2]
      low_pct=percentiles[1]
      hgh_pct=percentiles[3]

      if(prediction_corrected==TRUE){
        txt1=paste0(low_pct,'th and ',hgh_pct,'th percentiles of prediction-corrected observations in bin')
        txt2=paste0(mid_pct,'th percentile of prediction-corrected observations in bin')
        txt3=paste0(conf_int,'% confidence interval of ',low_pct,'th and ',hgh_pct,'th percentiles of prediction-corrected simulated values in bin')
        txt4=paste0(conf_int,'% confidence interval of ',mid_pct,'th percentile of prediction-corrected simulated values in bin')
        txt5=paste0('Median of ',low_pct,'th, ',mid_pct,'th and ',hgh_pct,'th percentiles of prediction-corrected simulated values in bin')
        txt6='Prediction-corrected observation'
      } else {
        txt1=paste0(low_pct,'th and ',hgh_pct,'th percentiles of observations in bin')
        txt2=paste0(mid_pct,'th percentile of observations in bin')
        txt3=paste0(conf_int,'% confidence interval of ',low_pct,'th and ',hgh_pct,'th percentiles of simulated values in bin')
        txt4=paste0(conf_int,'% confidence interval of ',mid_pct,'th percentile of simulated values in bin')
        txt5=paste0('Median of ',low_pct,'th, ',mid_pct,'th and ',hgh_pct,'th percentiles of simulated values in bin')
        txt6='Observation'
      }

      if(add_simulated_median==TRUE & plot_obs==TRUE & plot_outer==TRUE){
        leg_text=c(txt1,txt2,txt3,txt4,txt5,txt6)
        leg_col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1],'black',VPC_cols[2],'black')
        leg_pch=c(NA,NA,15,0,NA,1)
        leg_ptcex=c(NA,NA,2.3*sc,2*sc,NA,1)
        leg_lty=c(1,1,NA,NA,3,NA)
        leg_lwd=c(2.5,2.5,NA,NA,1.5,NA)
      }
      if(add_simulated_median==TRUE & plot_obs==TRUE & plot_outer==FALSE){
        txt5=paste0('Median of ',mid_pct,'th percentiles of simulated values in bin')
        leg_text=c(txt2,txt4,txt5,txt6)
        leg_col=c(VPC_cols[4],'black',VPC_cols[2],'black')
        leg_pch=c(NA,0,NA,1)
        leg_ptcex=c(NA,2*sc,NA,1)
        leg_lty=c(1,NA,3,NA)
        leg_lwd=c(2.5,NA,1.5,NA)
      }
      if(add_simulated_median==TRUE & plot_obs==FALSE & plot_outer==FALSE){
        txt5=paste0('Median of ',mid_pct,'th percentiles of simulated values in bin')
        leg_text=c(txt2,txt4,txt5)
        leg_col=c(VPC_cols[4],'black',VPC_cols[2])
        leg_pch=c(NA,0,NA)
        leg_ptcex=c(NA,2*sc,NA)
        leg_lty=c(1,NA,3,NA)
        leg_lwd=c(2.5,NA,1.5)
      }
      if(add_simulated_median==TRUE & plot_obs==FALSE & plot_outer==TRUE){
        leg_text=c(txt1,txt2,txt3,txt4,txt5)
        leg_col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1],'black',VPC_cols[2])
        leg_pch=c(NA,NA,15,0,NA)
        leg_ptcex=c(NA,NA,2.3*sc,2*sc,NA)
        leg_lty=c(1,1,NA,NA,3)
        leg_lwd=c(2.5,2.5,NA,NA,1.5)
      }
      if(add_simulated_median==FALSE & plot_obs==TRUE & plot_outer==TRUE){
        leg_text=c(txt1,txt2,txt3,txt4,txt6)
        leg_col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1],'black','black')
        leg_pch=c(NA,NA,15,0,1)
        leg_ptcex=c(NA,NA,2.3*sc,2*sc,1)
        leg_lty=c(1,1,NA,NA,NA)
        leg_lwd=c(2.5,2.5,NA,NA,NA)
      }
      if(add_simulated_median==FALSE & plot_obs==TRUE & plot_outer==FALSE){
        leg_text=c(txt2,txt4,txt6)
        leg_col=c(VPC_cols[4],'black','black')
        leg_pch=c(NA,0,1)
        leg_ptcex=c(NA,2*sc,1)
        leg_lty=c(1,NA,NA)
        leg_lwd=c(2.5,NA,NA)
      }
      if(add_simulated_median==FALSE & plot_obs==FALSE & plot_outer==FALSE){
        leg_text=c(txt2,txt4)
        leg_col=c(VPC_cols[4],'black')
        leg_pch=c(NA,0)
        leg_ptcex=c(NA,2*sc)
        leg_lty=c(1,NA,1)
        leg_lwd=c(2.5,NA)
      }
      if(add_simulated_median==FALSE & plot_obs==FALSE & plot_outer==TRUE){
        leg_text=c(txt1,txt2,txt3,txt4)
        leg_col=c(VPC_cols[3],VPC_cols[4],VPC_cols[1],'black')
        leg_pch=c(NA,NA,15,0)
        leg_ptcex=c(NA,NA,2.3*sc,2*sc)
        leg_lty=c(1,1,NA,NA)
        leg_lwd=c(2.5,2.5,NA,NA)
      }

      legend(legend_position,leg_text,col=leg_col,pch=leg_pch,pt.cex=leg_ptcex,lty=leg_lty,lwd=leg_lwd,cex=legend_size)
    }
  }


}
################################################################################
############  END OF plot_std_VPC FUNCTION  ####################################
################################################################################
