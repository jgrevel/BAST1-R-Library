#' Plots GOF involving DV and CWRES
#'
#' Generates general GOF plots for NONMEM models.
#'
#' @author Rupert Austin
#' @param filepath character. File path for output GOF plots.
#' @param data data frame containing output from NONMEM run.
#' @param ind_var name of the independent variable column, usually TIME or TIME_AFTER_DOSE.
#' @param log_ind_var logical. If TRUE, plot x-axis on a log scale.
#' @param bks integer. Number of breaks for histogram of CWRES.
#' @param fval f value in lowess function. Larger values give more smoothness.
#' @param name_stub character. Optional string to add to end of image file name.
#' @return A panel of 8 different diagnostic plots.
#' @details Uses BAST function GOFplotter.
#' @export


plot_GOF = function(filepath,data,ind_var,log_ind_var,bks=20,fval=0.33,name_stub=''){

  lv=FALSE
  if(log_ind_var==TRUE){
    lv=TRUE
  }
  if(nchar(name_stub)>0 & substr(name_stub,1,1)!="_"){
    name_stub=paste0("_",name_stub)
  }
  if(filepath!=''){
    # plot GOF with save
    png(paste(filepath,'GOF',name_stub,'.png',sep=''),width=1000,height=700)
    GOFplotter(data,ind_var,lv,bks,fval)
    dev.off()
    # plot GOF but don't save
    GOFplotter(data,ind_var,lv,bks,fval)
  } else {
    # plot GOF but don't save
    GOFplotter(data,ind_var,lv,bks,fval)
  }
  par(mfrow=c(1,1))
  par(mar=c(5,5,2,1.5))
  par(oma=c(0,0,0,0))
  par(cex.lab=1)
  par(cex.main=1)
  par(cex.axis=1)

}
