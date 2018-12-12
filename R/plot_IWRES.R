#' Plots GOF involving IWRES
#'
#' Generates general IWRES plots for NONMEM models.
#'
#' @author Rupert Austin
#' @param filepath character. File path for output IWRES plots.
#' @param data data frame containing output from NONMEM run.
#' @param ind_var name of the independent variable column, usually TIME or TIME_AFTER_DOSE.
#' @param log_ind_var logical. If TRUE, plot x-axis on a log scale.
#' @param bks integer. Number of breaks for histogram of IWRES.
#' @param fval f value in lowess function. Larger values give more smoothness.
#' @param name_stub character. Optional string to add to end of image file name.
#' @return A panel of 4 different diagnostic IWRES plots.
#' @details Uses BAST function IWRESplotter.
#' @export


plot_IWRES = function(filepath,data,ind_var,log_ind_var,bks=20,fval=0.33,name_stub=''){

  lv=FALSE
  if(log_ind_var==TRUE){
    lv=TRUE
  }
  if(nchar(name_stub)>0 & substr(name_stub,1,1)!="_"){
    name_stub=paste0("_",name_stub)
  }
  col_names=names(data)
  if('IWRES' %in% col_names){
    if(filepath!=''){
      # now plot NPDE GOF with save
      png(paste(filepath,'IWRES',name_stub,'.png',sep=''),width=1000,height=700)
      IWRESplotter(data,ind_var,lv,bks,fval)
      dev.off()
      # plot NPDE GOF but don't save
      IWRESplotter(data,ind_var,lv,bks,fval)
    } else {
      # plot NPDE GOF but don't save
      IWRESplotter(data,ind_var,lv,bks,fval)
    }
  } else {
    print('IWRES column does not exist in your data')
  }
  par(mfrow=c(1,1))
  par(mar=c(5,5,2,1.5))
  par(oma=c(0,0,0,0))
  par(cex.lab=1)
  par(cex.main=1)
  par(cex.axis=1)
}
