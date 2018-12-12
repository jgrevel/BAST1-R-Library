#' Calculates total and individual ETA shrinkage. Plots histograms and qqnorm for each ETA
#'
#' Generates general summaries of shrinkage, individual shrinkage and also produces histograms and qqplots of ETA values.
#'
#' @author Rupert Austin
#' @param mod_folder path to the modelling folder. Typically "\\\\\\\\BAST_SYSN\\\\blah\\\\blah\\\\Modelling\\\\"
#' @param run name of the sub-folder of mod_folder that contains the NONMEM .tab .ext and .phi files. Typically "001" / "002" / "003" etc.
#' @param IOV optional vector containing a list of the ETA numbers that are used for inter-occasion variability. For example,
#'        if ETA3, ETA4 and ETA5 are used for IOV then set IOV=c(3,4,5). This is important as shrinkage calculations are rather
#'        different for ETAs used to describe inter-occasion variability.
#' @param EST_num number indicating which $ESTIMATION to analyze results for. For example, if you ran 3 $EST methods and you want to analyze results
#'        from 3rd method then set EST_num=3. Default value is 1.
#' @return Function outputs a list of the following three items:
#'         \describe{
#'         \item{$ETA_data}{a data frame containing ETA estimates, variances of ETA estimates and individual shrinkages. important columns have titles
#'                          such as Include_ETA_1 which contains TRUE/FALSE for each individual. This is important information for shrinkage calculations
#'                          and indicate if the particular ETA was active for each individual.}
#'         \item{$ETA_summary}{a small data frame giving ETAShrink and EBVShrink values for each ETA and for IOV (if used).}
#'         \item{$PAR_estimates}{a one row data frame containing the estimates of all parameters from the model.}
#'         }
#'
#'         Function also saves three objects to the model folder:
#'         \describe{
#'         \item{ETA_plots.png}{histograms and qqplots of each ETA.}
#'         \item{ETA_data.csv}{same as $ETA_data.}
#'         \item{ETA_shrinkage_summary.csv}{same as $ETA_summary.}
#'         }
#' @note This function tries to be smart and figure out which IDs contribute to each ETA (for IIV and IOV),
#'       but it is worth having a close look at the contents of $ETA_data (particularly the Include_ETA_N columns)
#'       to make sure it all looks OK.
#' @export



################################################################################
################################################################################
# function to generate general summaries of shrinkage, individual shrinkage and
# also produce histograms and qqplots of ETA values
#
# function inputs:
# mod_folder=path to the modelling folder. Typically \\\\BAST_SYSN\\blah\\blah\\Modelling\\
#	run=name of the sub folder of mod_folder that contains the NONMEM .tab .ext and .phi files. Typically 001 / 002 / 003 etc.
# IOV=optional vector containing a list of the ETA numbers that are used for inter occasion variability. For example,
# if ETA3, ETA4 and ETA5 are used for IOV then set IOV=c(3,4,5). This is important as shrinkage calculations are rather
# different for EATs used to describe inter occasion variability.
# EST_num=number indicating which $ESTIMATION to analyze results for. If you ran 3 $EST methods and you want to analyze results from 3rd
# method then set EST_num=3. Default value is 1
#
# function outputs:
#	$ETA_data                     which is a data frame containing EAT estimates, variances of ETA estimates and individual shrinkages
#                               important columns have titles such as Include_ETA_1 which contains TRUE/FALSE for each individual.
#                               This is important information for shrinkage calculations and indicate if the particular ETA was active for each individual
# $ETA_summary                  which is a small data frame giving ETAshrink and EBVShrink values for each ETA and for IOV (if used)
# $PAR_estimates                which is a one row data frame containing the estimates of all parameters from the model
#
# function also save three objects to the model folder:
# ETA_plots.png                 histograms and qqplots of each ETA
# ETA_data.csv                  same as $ETA_data
# ETA_shrinkage_summary.csv     same as $ETA_summary
#
# NOTE: This function tries to be smart and figure out which IDs contribute to each ETA (for IIV and IOV)
#       but it is worth have a close look at the contents of $ETA_data (particularly the Include_ETA_N columns)
#       to make sure it all looks OK
################################################################################
################################################################################
get_ETA_metrics = function(mod_folder,run,IOV=0,EST_num=1){
  library(stringr)

  path=paste0(mod_folder,'run',run,'\\')

  FILEOK=FALSE
  phi_file=''
  ext_file=''
  if(nchar(run)!=3){
    if(nchar(run)>3){
      run=substring(run,1,3)
      FILEOK=TRUE
      phi_file=paste0(path,'run',run,'.phi')
      ext_file=paste0(path,'run',run,'.ext')
    }
  } else {
    FILEOK=TRUE
    phi_file=paste0(path,'run',run,'.phi')
    ext_file=paste0(path,'run',run,'.ext')
  }

  if(FILEOK==FALSE){
    print("Run number not in correct format. The following are acceptable examples:")
    print("001, 015, 124, 999, 001a, 034c, 873z, 152_rerun")
    print("Corresponding to run folder names run001, run015, run999, run001a, run035c, run873z, run152_rerun")
    print("And where the run folders contain .tab  (and .ext and .phi) files with these names:")
    print("run001.tab, run015.tab, run124.tab, run999.tab, run001.tab, run034.tab, run873.tab, run152.tab")
    return(NA)
  } else {
    ###########################################
    # load file containing individual ETA estimates [from .phi file] (ETA.1, ETA.2 etc), and variances of the individual ETA estimates (ETC.1.1, ETC.2.2 etc)

    # count lines in the phi file
    conn=file(phi_file,open="r")
    linn=readLines(conn)
    for (i in 1:length(linn)){
    }
    close(conn)
    # put contents of phi file into file_list
    file_list=vector(mode='list',length=i)
    conn=file(phi_file,open="r")
    linn=readLines(conn)
    for (i in 1:length(linn)){
      file_list[[i]]=linn[i]
    }
    close(conn)

    # now look for rows within the phi file that contain TABLE numbers (possible if more than one estimation method used)
    table_rows=NULL
    for(i in 1:(length(file_list))){
      if(substr(file_list[[i]],1,9)=='TABLE NO.'){
        table_rows=c(table_rows,i)
      }
    }
    table_rows=c(table_rows,length(file_list)+1)

    # now extract the required table entries. First get start and end values of file_list
    if(EST_num==1){
      start_val=2
      if(length(table_rows)>1){
        end_val=table_rows[2]-1
      } else {
        end_val=length(file_list)
      }
    } else {
      start_val=table_rows[EST_num]+1
      end_val=table_rows[EST_num+1]-1
    }
    # now put values into a data frame
    res_vals=NULL
    x1=file_list[[start_val]]
    x1=str_split(x1,' ')
    x1=unlist(x1)
    x1=x1[x1!='']
    df_names=x1
    cnt=0
    for(i in (start_val+1):end_val){
      x1=file_list[[i]]
      x1=str_split(x1,' ')
      x1=unlist(x1)
      x1=x1[x1!='']
      x1=as.numeric(x1)

      if(cnt==0){
        df_phi=data.frame(t(x1))
        names(df_phi)=df_names
        cnt=1
      } else {
        df_phi=rbind(df_phi,x1)
      }
    }
    ETA_est=df_phi
    # finished reading phi file
    ###########################################

    ###########################################
    # load file containing parameter estimates [from .ext file] (THETAS and OMEGAS). The estimates at final iteration are those with larges positive value of "ITERATION"
    # ETA_est=read.table(file=phi_file,sep="",skip=1,header=T)

    # count lines in the ext file
    conn=file(ext_file,open="r")
    linn=readLines(conn)
    for (i in 1:length(linn)){
    }
    close(conn)
    # put contents of phi file into file_list
    file_list=vector(mode='list',length=i)
    conn=file(ext_file,open="r")
    linn=readLines(conn)
    for (i in 1:length(linn)){
      file_list[[i]]=linn[i]
    }
    close(conn)

    # now look for rows within the ext file that contain TABLE numbers (possible if more than one estimation method used)
    table_rows=NULL
    for(i in 1:(length(file_list))){
      if(substr(file_list[[i]],1,9)=='TABLE NO.'){
        table_rows=c(table_rows,i)
      }
    }
    table_rows=c(table_rows,length(file_list)+1)

    # now extract the required table entries. First get start and end values of file_list
    if(EST_num==1){
      start_val=2
      if(length(table_rows)>1){
        end_val=table_rows[2]-1
      } else {
        end_val=length(file_list)
      }
    } else {
      start_val=table_rows[EST_num]+1
      end_val=table_rows[EST_num+1]-1
    }
    # now put values into a data frame
    res_vals=NULL
    x1=file_list[[start_val]]
    x1=str_split(x1,' ')
    x1=unlist(x1)
    x1=x1[x1!='']
    df_names=x1
    cnt=0
    for(i in (start_val+1):end_val){
      x1=file_list[[i]]
      x1=str_split(x1,' ')
      x1=unlist(x1)
      x1=x1[x1!='']
      x1=as.numeric(x1)

      if(cnt==0){
        df_phi=data.frame(t(x1))
        names(df_phi)=df_names
        cnt=1
      } else {
        df_phi=rbind(df_phi,x1)
      }
    }
    PAR_est=df_phi
    # finished reading phi file
    ###########################################


    # need to extract THETA and OMEGA values from PAR_est, so we need to find the data from the appropriate iteration
    # For an optimization run the correct iteration is the last one with positive value
    # For a MAXEVAL=0 type of evaluation run the correct iteration is the first negative one
    IT_vals=PAR_est$ITERATION
    if(max(IT_vals)>0){
      ext <- PAR_est[PAR_est$ITERATION>=0,]     # This command leaves only the positive iteration results
      ext=ext[nrow(ext),]                       # keep only last row, which is parameter estimates after final iteration
      PAR_est=ext
    } else {
      # need to find the first iteration that has a negative value so THETA and OMEGA values can be extracted
      IT_VAL=max(PAR_est$ITERATION[PAR_est$ITERATION<0])
      PAR_est=PAR_est[PAR_est$ITERATION==IT_VAL,]
      ext=PAR_est
    }

    ext=data.frame(ext[,grep('OMEGA',names(ext))])      # only want omega values from ext. Next we will have to find only the diagonal omega values
    n=1:(sqrt(2*ncol(ext)+0.25)-0.5)          # find number of diagonal ETAs
    if(length(n)>1){
      ext=ext[,n*(n+1)/2]                       # extract only the diagonal omegas
    }
    ETA_numbers=1:ncol(ext)

    result=ETA_est[,1:(ncol(ext)+2)]          # start building the final table of results
    names(result)[3:ncol(result)]=paste0('ETA_',1:ncol(ext))   # tidy up the column names

    # create columns indicating if each ETA was used or not on each patient
    ETA_include=data.frame(result[,3:ncol(result)])
    ETA_include[,]=FALSE
    names(ETA_include)=paste0('Include_ETA_',1:ncol(ext))  # tidy up the column names
    result=cbind(result,ETA_include)

    # Add ETC values, which are variances of the individual ETA estimates
    hh_ETC=grep('ETC',names(ETA_est))
    hh_PHC=grep('PHC',names(ETA_est))
    no_result=0
    if(length(hh_ETC)>0){
      hh=hh_ETC
    } else {
      hh=hh_PHC
      no_result=1
    }

    data=data.frame(ETA_est[,hh])
    if(length(n)>1){
      data=data[,n*(n+1)/2]                     # filter to ETC values for diagonal terms only
    }
    names(data)=paste0('Variance_ETA_',1:ncol(ext))
    result=cbind(result,data)

    # Add OMEGA estimates
    ext=data.frame(ext[rep(1,nrow(data)),])               # expand omegas to number of IDs
    rownames(ext)=NULL
    colnames(ext)= paste0('OMEGA_',1:ncol(ext)) # set colnames
    result=cbind(result,ext)

    # now, for each ETA, check each individual to see if it was used (essential for correctly calculating shrinkage)
    for(i in ETA_numbers){
      include_col=paste0('Include_ETA_',i)
      variance_col=paste0('Variance_ETA_',i)
      omega_col=paste0('OMEGA_',i)
      ETA_col=paste0('ETA_',i)
      # determine if each patient contributed to each ETA
      result[result[,ETA_col]!=0,include_col]=TRUE   # if ETA<>0 then that patient definitely contributed to the ETA
      result[result[,ETA_col]==0 & signif(result[,variance_col],digits=3)!=signif(result[,omega_col],digits=3) & result[,variance_col]!=0,include_col]=TRUE # if ETA=0 and variance of ETA <> OMEGA and variance of ETA<>0 the patient definitely contributed to ETA
      result[result[,ETA_col]==0 & result[,variance_col]==0,include_col]=FALSE   # if ETA=0 and variance of ETA=0 then that patient definitely did not contribute to the ETA
    }

    # now, for each ETA, calculate individual shrinkages
    for(i in ETA_numbers){
      include_col=paste0('Include_ETA_',i)
      variance_col=paste0('Variance_ETA_',i)
      omega_col=paste0('OMEGA_',i)
      ishrink_col=paste0('iShrink_ETA_',i)
      result[,ishrink_col]=NA
      result[result[,include_col]==TRUE,ishrink_col]=100*(1 - sqrt(1 - result[result[,include_col]==TRUE,variance_col]/result[result[,include_col]==TRUE,omega_col]))
    }

    # now calculate ETAShrink and EBVShrink
    Name=c('Estimate','ETAShrink','EBVShrink')
    shrink=data.frame(Name,ext[1:3,])
    names(shrink)[2:ncol(shrink)]=paste0('ETA_',1:(ncol(shrink)-1))

    for(i in ETA_numbers){
      omega_name=paste0('OMEGA_',i)
      eta_name=paste0('ETA_',i)
      shk_name=paste0('iShrink_ETA_',i)
      include_col=paste0('Include_ETA_',i)
      shrink[2,i+1]=100*(1 - sqrt(var(result[result[,include_col],eta_name])/result[result[,include_col],omega_name][1]))
      shrink[3,i+1]=mean(result[result[,include_col],shk_name])
    }

    # now have to make some alterations if any of the ETAs represent interoccasion variability
    if(IOV[1]!=0){
      IOV_cols=paste0('ETA_',IOV)
      ETA_IOV=shrink[,IOV_cols[1]]
      for(i in IOV_cols){
        shrink[,i]=NULL
      }
      shrink$ETA_IOV=ETA_IOV
      # first calculate ETAShrink
      # get a list of estimated ETAs for each valid occasion for each patient
      include_cols=paste0('Include_ETA_',IOV)
      IOV_ETA_list=NULL
      for(i in 1:length(IOV)){
        vals=result[result[,include_cols[i]],IOV_cols[i]]
        IOV_ETA_list=c(IOV_ETA_list,vals)
      }
      omega_col=paste0('OMEGA_',IOV[1])
      IOV_ETA_shrink_val=100*(1 - sqrt(var(IOV_ETA_list)/result[1,omega_col]))
      shrink[2,'ETA_IOV']=IOV_ETA_shrink_val

      # now calculate EBVShrink
      # get a list of individual shrinkage for each valid occasion for each patient
      IOV_iShrink_list=NULL
      ishrink_cols=paste0('iShrink_ETA_',IOV)
      for(i in 1:length(IOV)){
        vals=result[result[,include_cols[i]],ishrink_cols[i]]
        IOV_iShrink_list=c(IOV_iShrink_list,vals)
      }
      shrink[3,'ETA_IOV']=mean(IOV_iShrink_list)
    }


    ##########################################
    # now produce and save plots of ETA values
    etas_to_plot=ETA_numbers
    IOV_etas_to_plot=0
    plot_IOV=0
    if(IOV[1]!=0){
      etas_to_plot=etas_to_plot[!(etas_to_plot%in%IOV)]
      IOV_etas_to_plot=IOV
      plot_IOV=1
    }
    # check there are some valid ETA values for each of the ETAS that needs to be plotted
    etas_to_plot_adj=etas_to_plot
    for(i in etas_to_plot){
      eta_col=paste0('ETA_',etas_to_plot[i])
      include_col=paste0('Include_ETA_',etas_to_plot[i])
      vals=result[result[,include_col],eta_col]
      if(length(vals)==0){
        # no values for this ETA so remove from etas_to_plot
        etas_to_plot_adj=etas_to_plot_adj[etas_to_plot_adj!=etas_to_plot[i]]
      }
    }
    etas_to_plot=etas_to_plot_adj

    total_num_etas_to_plot=length(etas_to_plot)+plot_IOV

    if(length(etas_to_plot)==0){
      etas_to_plot=0
    }

    if(no_result==1){
      etas_to_plot=0
    }


    wd=600
    ht=(total_num_etas_to_plot)*250

    if(etas_to_plot[1]>0){
      png(file=paste0(path,'ETA_plots.png'),width=wd,height=ht)
    }

    sz=min((ht+1000)/1000-0.25,2)
    par(mfrow=c(total_num_etas_to_plot,2))
    par(mar=c(4.5,4.5,2,1))
    par(oma=c(0,0,0,0))
    par(cex.lab=sz)
    par(cex.main=sz)
    par(cex.axis=sz)
    bks='Sturges'

    if(etas_to_plot[1]!=0){
      for(i in 1:length(etas_to_plot)){
        eta_col=paste0('ETA_',etas_to_plot[i])
        include_col=paste0('Include_ETA_',etas_to_plot[i])
        vals=result[result[,include_col],eta_col]
        title=paste0('Histogram of ',eta_col)
        sh=round(shrink[2,eta_col],2)
        xlb=paste0(eta_col,' (shrinkage=',sh,'%)')
        hist(vals,main=title,xlab=xlb,breaks=bks)
        qqnorm(vals/sd(vals),main=paste0('QQ plot of ',eta_col,'/sd(',eta_col,')'))
        lines(x=c(-100,100),y=c(-100,100),col='red',lty=2)
      }
    }

    if(plot_IOV==1 & etas_to_plot[1]>0){
      vals=IOV_ETA_list
      eta_names=paste(IOV,collapse=' ')
      sh=round(shrink[2,'ETA_IOV'],2)
      # xlb=paste0(eta_col,' (shrinkage=',sh,'%)')
      xlb=paste0('ETA_IOV (shrinkage=',sh,'%)')
      hist(vals,main=paste0('Histogram of ETAs for IOV (',eta_names,')'),xlab=xlb,breaks=bks)
      qqnorm(vals/sd(vals),main=paste0('QQ plot of ETA_IOV/sd(ETA_IOV)'))
      lines(x=c(-100,100),y=c(-100,100),col='red',lty=2)
    }

    if(etas_to_plot[1]>0){
      dev.off()
    }

    sz=min((ht+1000)/1000-0.25,2)
    par(mfrow=c(total_num_etas_to_plot,2))
    par(mar=c(4.5,4.5,2,1))
    par(oma=c(0,0,0,0))
    par(cex.lab=sz)
    par(cex.main=sz)
    par(cex.axis=sz)
    bks='Sturges'

    if(etas_to_plot[1]!=0){
      for(i in 1:length(etas_to_plot)){
        eta_col=paste0('ETA_',etas_to_plot[i])
        include_col=paste0('Include_ETA_',etas_to_plot[i])
        vals=result[result[,include_col],eta_col]
        title=paste0('Histogram of ',eta_col)
        sh=round(shrink[2,eta_col],2)
        xlb=paste0(eta_col,' (shrinkage=',sh,'%)')
        hist(vals,main=title,xlab=xlb,breaks=bks)
        qqnorm(vals/sd(vals),main=paste0('QQ plot of ',eta_col,'/sd(',eta_col,')'))
        lines(x=c(-100,100),y=c(-100,100),col='red',lty=2)
      }
    }

    if(plot_IOV==1 & etas_to_plot[1]>0){
      vals=IOV_ETA_list
      eta_names=paste(IOV,collapse=' ')
      sh=round(shrink[2,'ETA_IOV'],2)
      # xlb=paste0(eta_col,' (shrinkage=',sh,'%)')
      xlb=paste0('ETA_IOV (shrinkage=',sh,'%)')
      hist(vals,main=paste0('Histogram of ETAs for IOV (',eta_names,')'),xlab=xlb,breaks=bks)
      qqnorm(vals/sd(vals),main=paste0('QQ plot of ETA_IOV/sd(ETA_IOV)'))
      lines(x=c(-100,100),y=c(-100,100),col='red',lty=2)
    }

    if(no_result==0){

      # save the ETA data (result)
      file_path=paste0(path,'ETA_data.csv')
      write.table(result,file=file_path,col.names=TRUE,quote=FALSE,row.names=FALSE,sep=",")

      # save the ETA shrinkage summary (shrink)
      file_path=paste0(path,'ETA_shrinkage_summary.csv')
      write.table(shrink,file=file_path,col.names=TRUE,quote=FALSE,row.names=FALSE,sep=",")

      ret=list(ETA_data=result,ETA_summary=shrink,PAR_estimates=PAR_est)
      return(ret)

    } else {
      print("No results returned because .phi file does not contain ETA values for requested $ESTIMATION type")
      print("You will have to extract ETA values from $TABLE file instead")
      ret=NULL
      return(ret)
    }
  } # end of else
}
##############################################################
#################### END OF get_ETA_metrics ##################
##############################################################
