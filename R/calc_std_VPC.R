#' Fast calculation of standard VPC.
#'
#' A faster version of previous VPC_data, also allows user defined quantiles.
#'
#' @author Aaron Hayman
#' @param exp_data data frame containing experimental data.
#' @param sim_data data frame containing all simulated data.
#' @param use logic vector of rows to use from the exp_data (must match the sim data too).
#' @param bins vector of limits of the time bins.
#' @param dv_col name of column containing DV (or IPRED), for both exp_data and sim_data.
#' @param time_col name of column containing independent variable (e.g. TAD or TIME), for both exp_data and sim_data.
#' @param quantiles vector of quantiles to find between zero and one.
#' @param lower.bound numeric. Lower limit of all DV (-Inf will work).
#' @param confidence.interval numeric. Percentage size of confidence interval calculated around each quantile in simulated data.
#' @param min_bin_width numeric. Width of the smallest possible unrestricted bin (bins will be restricted in width by neighbouring bins).
#' @param T0 numeric. Start time.
#' @return A list containing all necessary VPC parameters to be used in plot_std_VPC function.
#' @note This function has not been designed for bins containing zero points, and may fail or produce strange results if zero points are detected.
#' @export



##################################################################################################
####                                    calc_std_VPC                                          ####
##################################################################################################
# A faster version of VPC_data, also allows user defined quantiles
# exp_data is the experimental data
# sim_data is the all simulated data (sim_data_H is no longer required)
# use is a logic vector of rows to use from the exp_data (must match the sim data too)
# bins is the limits of time bins
# dv_col and time_col are the names of columns with time and dv (or ipred) in them, for both exp data and sim data
# quantiles a vector of quantiles to find between zero and one
# lower.bound the lowest theoretical of DV (dont expect using -Inf will work though)
# confidence.interval is size of percent confidence interval calculated around each quantile in simulated data
# min_bin_width is the width of the smallest possible unrestricted bin (bins will be restricted in width by neighbouring bins)
# T0 is the start time

# this function has not been designed for bins containing zero points, and may fail or produce strange results if this is done

calc_std_VPC=function(exp_data,sim_data,use,bins,dv_col,time_col,quantiles=c(0.1,0.5,0.9),lower.bound=0,confidence.interval=95,min_bin_width=1,T0=0)
{
  qs=quantiles					# vector of quantiles to be calculated, values should be between 0 and 1
  lb=lower.bound					# the theoretical lowest value of dv_col, typically zero
  ci=confidence.interval				# confidence band for each quantile band in the simulated data, between 0 and 100
  sim_stuff=function(d,tb,qs,ci)			# function to find the quantiles of simulated data with confidence bounds and median
  {
    eb=cumsum(tb)						# ends of bins
    sb=eb-tb							# starts of bins
    ind=1+rep(as.numeric(tb-1),each=length(qs))*qs+rep(as.numeric(sb),each=length(qs))		# quantile rows
    simq=d[floor(ind),]*(1-ind%%1)+d[ceiling(ind),]*ind%%1						# rows of quantiles
    simq=matrix(simq[order(rep(1:nrow(simq),sims),as.numeric(simq))],ncol=sims,byrow=TRUE)	# sort each row into ascending numerical order
    cind=1+as.numeric(sims-1)*(0.5+c(-1,0,1)*ci/200)							# confidence bounds columns
    simq=simq[,floor(cind)]*rep((1-cind%%1),each=nrow(simq))+simq[,ceiling(cind)]*rep(cind%%1,each=nrow(simq)) # confidence bounds of quantiles
    return(matrix(t(simq),ncol=length(tb)))									# format and order made to match previous VPC function
  }
  exp_stuff=function(d,tb,qs)				# function to find the quantiles of simulated data
  {
    eb=cumsum(tb)					# ends of bins
    sb=eb-tb						# starts of bins
    ind=1+rep(as.numeric(tb-1),each=length(qs))*qs+rep(as.numeric(sb),each=length(qs))	# quantile positions
    expq=d[floor(ind)]*(1-ind%%1)+d[ceiling(ind)]*ind%%1						# quantiles
    return(matrix(expq,ncol=length(tb)))
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


  dl=sum(use)											# length of data in use
  bi=rowSums(matrix(exp_data[use,time_col]>=rep(bins,each=dl),nrow=dl))	# bin assignments
  tb=table(bi)[2:length(bins)-1L]							# length of bins
  ######## BIN WIDTH SECTION
  times=exp_data[use,time_col][order(bi,exp_data[use,time_col])]		# times ordered into bins and assending time
  Mt=times[cumsum(tb)]									# max times for bins
  mt=times[cumsum(tb)-tb+1]								# min times for bins
  res1=mt						 					# copy of min times
  res2=Mt											# copy of max times
  change=mt+min_bin_width>Mt								# intervals which are smaller than min bin width
  adj=(min_bin_width-(Mt-mt))*0.5							# adjustments required to make everything equal to min bin width
  res1[change]=mt[change]-adj[change]							# adjustmens made only to small bins in copies
  res2[change]=Mt[change]+adj[change]
  if(res1[1]<T0){res1[1]=T0}								# ensure start is not below start time

  change=res1[-1]<res2[-length(tb)]							# which adjustments have caused bins to overlap
  mid=0.5*(mt[-1]+Mt[-length(tb)])							# mid points bewteen true maxes and mins
  range=Mt[length(tb)]-mt[1]								# range of data
  res1[c(FALSE,change)]=mid[change]+0.002*range					# boarders where bins overlap are moved to mid point with a gap 0.4% of the range
  res2[c(change,FALSE)]=mid[change]-0.002*range
  mt=res1											# originals are now replaced by copies
  Mt=res2
  ######## DATA PROCESSING SECTION
  sims=nrow(sim_data)/nrow(exp_data)							# number of simulations
  simdv=sim_data[rep(use,sims),dv_col]						# sim data
  simdv=matrix(simdv[order(rep(1:sims,each=dl),rep(bi,sims),simdv)],ncol=sims)	# ordered to ascending simulations, bins and value
  expdv=exp_data[use,dv_col][order(bi,exp_data[use,dv_col])]			# experimental data ordered to ascending bin then value
  PRED=sim_data$PRED[1:length(use)][use]						# population predctions
  pred=PRED[order(bi,PRED)]								# ordreed to bins then value
  mpred=exp_stuff(pred,tb,0.5)[bi]							# median bin predictions (in order of original used data)
  pcexpdv=lb+ (exp_data[use,dv_col] - lb)*(mpred - lb)/(sim_data$PRED[1:length(use)][use] - lb)	# prediction corrected experimental values
  pcExpdv=pcexpdv[order(bi,pcexpdv)]							# ordered to bin and value
  pcsimdv=lb+ (sim_data[rep(use,sims),dv_col] - lb)*(mpred - lb)/(sim_data$PRED[1:length(use)][use] - lb)	# prediction corrected sim values
  pcsimdv=matrix(pcsimdv[order(rep(1:sims,each=dl),rep(bi,sims),pcsimdv)],ncol=sims)			# odered to sim, bin then value
  simq=rbind(sim_stuff(simdv,tb,qs,ci),sim_stuff(pcsimdv,tb,qs,ci))		# quantiles and confidence bounds for simulated data and pc sim data
  expq=rbind(mt,Mt,exp_stuff(expdv,tb,qs),exp_stuff(pcExpdv,tb,qs))		# quantiles for experimental data and pc experimental data
  ######## SECTION FOR NAMING ROWS
  rownames(expq)=c('min_time','max_time',paste0(rep(c('VPC_','pcVPC_'),each=length(qs)),round(qs*100,2),'%'))
  n_in_bin=tb
  expq=rbind(expq,n_in_bin)   # add number of obs in each bin to expq
  l=50-ci/2
  u=50+ci/2
  rownames(simq)=c(paste0(rep(round(qs*100,2),each=3),c(paste0('%_',l,'%'),'%',paste0('%_',u,'%'))),paste0('pc',rep(round(qs*100,2),each=3),c(paste0('%_',l,'%'),'%',paste0('%_',u,'%'))))
  out=cbind(exp_data[use,],PRED,DV_Prediction_Corrected=pcexpdv)
  rownames(out)=NULL
  print(simq)
  print(expq)
  print('Check the number of observed data in each bin (n_in_bin).')
  print('If any bins have <15-20 data then consider using plot_outer=FALSE when plotting')
  print('Alternatively consider a narrower quantile argument such as quantiles=c(0.2,0.5,0.8)')
  return(list(sim_res=simq,ex_res=expq,bp=bi,exp_data_analysed=out,dv_col=dv_col,ind_var_col=time_col,percentiles=100*quantiles,conf_int=confidence.interval))
}

##############################################################
################## END OF calc_std_VPC function ##############
##############################################################
