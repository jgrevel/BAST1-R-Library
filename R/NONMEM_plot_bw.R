#' NONMEM_visualising_tool
#'
#' @author Rupert Austin
#' @description The tool provides a good overview of the available data for each patient and helps to identify severe outliers or programming errors. 
#'              This can be useful when doing QC work upon building a new NONMEM data file. 
#'              The tool was created to visualise any NONMEM data file that contains continuous type DV. 
#'              The observations could be PK or PD type data, and dosing records don't necessarily have to be present. 
#'              To identify potential outliers, observations for each individual can be superimposed on observations for all patients. 
#'              The tool will plot observations and dosing records vs time, saving one plot for each individual or a panel of eight plots for subject-rich data files. 
#'
#' @param data name of the data frame containing the NONMEM-formatted data
#' @param save_path path to the folder where plots will be saved
#' @param ID_col Name of the column in NONMEM file which contains patient identifiers. Defaultis "ID".
#' @param ind_var_col Name of the column in NONMEM file which contains the variable for use as the x-axis in plots. 'TAD' is the other main alternative for ind_var_col. Default is "TIME"
#' @param DV_col Name of the column in NONMEM file which contains observations for plotting on y-axis. If dose corrected DV is to be used then set DV_col='DV_DOSE_CORRECTED'. Default is "DV".
#' @param x_label Label for x-axis. Default is ""
#' @param y_label  Label for y-axis can be character or expression e.g. expression('Concentration (' * mu * 'g/L)') (see \link[grDevices]{plotmath}). Defalts to ""
#' @param dose_marker Add dose marker at the top of the plot. If the AMT or DOSE column is present in the data file this marker will be coloured according to the dose size. A separate plot Legend.png will be generated with a key to colourin g of different dose sizes. Default is TRUE.
#' @param panel Generate single plots for each ID when the argument is set to FALSE, or panels of 8 plots when the argument is set to TRUE. Default is TRUE
#' @param keep_doses_after_last_obs When set to TRUE the x-axis of each plot will be scaled such that doses after the last observation are visualised. When set to FALSE the x-xis will only reach as far as the last observation. Default is FALSE.
#' @param calc_dose Add DOSE column (showing size of most recent non-zero AMT) to the data when the argument is TRUE. Required if DOSE is not already in the data file and you set either dose_marker=TRUE or DV_col=DV_DOSE_CORRECTED. Default is FALSE.
#' @param calc_occ Add OCC column if the argument is equal to TRUE. Required if OCC is not already in the data file and you want to plot data from different occasions using occ=1 or occ=2 etc. Default is FALSE.
#' @param calc_tad Add TAD column if the argument is equal to TRUE. Required if TAD is not already in the data file and ind_var_col='TAD'. Default is FALSE.
#' @param occ  Plot data for a particular occasion. Default is FALSE which leads to plots including data from all occasions. If the argument is equal to e.g. 2 only data from second occasion is plotted. Default is FALSE.
#' @param LLOQFLAG_col Name of the column in NONMEM file which indicates if an observation is BLQ (1) or has been quantified (0). BLQ observations are plotted as green circles. Set to FALSE if a LLOQFLAG column is not in the data file. Default is FALSE.
#' @param LLOQ_col Name of the column in NONMEM file which contains the values of the limit of quantification. If the argument is a number (rather than text such as 'LLOQ') then the lower limit of quantification for every sample will be set to that number, and the BLQ samples will be plotted as green circles at that concentration. Default is FALSE.
#' @param title_label_col Names of the columns in NONMEM file which will be used for supplying some additional information to plot titles. If title_label_col='' then no additional labels are added. For multiple dose studies the dose shown in the title will correspond to the selected occasion. If all occasions are plotted the value of the dose in the title will correspond to the first occasion.  Default is "".
#' @param colour_shading Sets the relationship between the dose amounts and the colour shades. Important when the range of doses is wide. Try experimenting with different values to see effect on Legend.png and the dose markers. It should be a positive number. If set to 0 then it will instead be calculated as sqrt(max(AMT)/min(AMT))/2. Default is 0.
#' @param colour_range Sets the colour transitions for dose markers and background data. A minimum of 2 colours is required and plot colours will be interpolated between the members of colour_range, depending on the size of DOSE. Default isc('grey90','grey75','grey60','black') 
#' @param background_data If TRUE background data (data from all other individuals) are added to the plot. Default is FALSE.
#' @param plot_IPRED If set to TRUE then IPRED data are added to the plots. Obviously the data file will have to be a NONMEM $TABLE file such that IPRED data are available. Default is FALSE.
#' @param all_same_scale_x Determines if x-axis range should be same for all individuals (TRUE) or different for each individual (FALSE). Default is FALSE.
#' @param all_same_scale_y Determines if y-axis range should be the same for all individuals (TRUE) or different for each individual (FALSE). Default is TRUE.
#' @param file_text Free text that will be added to each filename. Useful when creating lots of different visualisations from the same data. Default is "".
#' @param ind_var_conversion Factor to be used for converting the data in the x-axis. For example set to 1/24 to convert TIME from hours to days. Default is 1.
#' @param extend_dose_marker When set to TRUE the dose markers will be extended as grey dashed lines from top to bottom of each plot. This can help determine if observations close in to time to a dose actually lie before or after the dosing event. Default is TRUE.
#' @param ... additional arguments passed to plot.
#'
#' @details Only records with EVID=0 are plotted 
#' Function will accept standard arguments for the default R plot command, such as log='y'
#' Columns 'ID', 'EVID', 'DV' and 'TIME' are expected to be present. Columns 'TAD', 'AMT', DOSE' and 'OCC' (occasion) are optional. 
#' 'TAD', 'DOSE' and 'OCC' can be added to the data file before plotting by using the following arguments: calc_tad=TRUE, calc_dose=TRUE, calc_occ=TRUE.  
#' It is assumed that DV and IPRED are on a linear scale. If not, then the user should correct this before using the NONMEM_plot function
#' Before running the function, the user is expected to filter data file to records of interest e.g. placebo or biomarker type, through filtering based on columns such as CMT or DVID
#'
#' @return The tool will plot observations and dosing records vs time, saving one plot for each individual or a panel of eight plots for subject-rich data files.
#' @export
#'

NONMEM_plot_bw=function(data,save_path,ID_col='ID',ind_var_col='TIME',DV_col='DV',x_label='',y_label='',dose_marker=TRUE,panel=TRUE,
      keep_doses_after_last_obs=FALSE,calc_dose=FALSE,calc_occ=FALSE,calc_tad=FALSE,occ=FALSE,LLOQFLAG_col=FALSE,LLOQ_col=FALSE,
      title_label_col='',colour_shading=0,colour_range=c('gray90','gray75','gray60','black'),background_data=FALSE,plot_IPRED=FALSE,all_same_scale_x=FALSE,
      all_same_scale_y=TRUE,file_text='',ind_var_conversion=1,extend_dose_marker=TRUE,...){
   
  LOG=FALSE			# check if x axis is logged
  if(!is.null(list(...)$log))
  {
	LOG=grepl('x',list(...)$log)
  }
  plot0=function(x,y,xlimi=NULL,...)	## A function added by Aaron for doing nice looking things with log scale on x axis
  {
	LOG=FALSE
	x0=0
	if(!is.null(list(...)$log))
	{
		LOG=grepl('x',list(...)$log)
	}
	snapx=function(x)
	{
		usr=par()$usr
		opar <- par('xlog','ylog')
		par(xlog=FALSE)
		par(ylog=FALSE)
		w=0.5*strwidth('E')
		h=0.5*strheight('W')
		polygon(x+c(w,w,-w,-w),usr[3]+c(h,-h,-h,h),col='white',lty=0,xpd=NA)
		lines(x+c(w,w,NA,-w,-w),usr[3]+c(h,-h,NA,-h,h),xpd=NA)
		par(opar)
	
	}
	if(LOG) # & any(x<=0) # this part has been excluded from use here so results have consistency
	{
		if(!is.null(xlimi))
		{	
			r=log(xlimi,10)
		}else{
			r=range(log(x[x>0],10),na.rm=TRUE)
		}
		x0=r[1]-0.2*diff(r)
		xr=x
		xr[xr<=0]=10**x0
		plot(xr,y,xaxt='n',xlim=10**c(x0,r[2]),...)
		ax=axisTicks(r+diff(r)*0.05*c(-1,1),log=TRUE)
		axis(1,at=c(10**x0,ax),labels=c(0,ax))
		snapx(0.5*r[1]+0.5*x0)
	}else{
		if(is.null(xlimi)) xlimi=range(x,na.rm=TRUE)
		plot(x,y,xlim=xlimi,...)
	}
	return(invisible(10**x0))
  }
  points0=function(x,y,x0,...)
  {
	if(par()$xlog)
	{
		x[x<=0]=x0
	}
	points(x,y,...)
  }
  lines0=function(x,y,x0,...)
  {
	if(par()$xlog)
	{
		x[x<=0]=x0
	}
	lines(x,y,...)
  }

  ##############
  exp_data=data
  # set axes labels

  # rename some variables
  all_subjects_colours=colour_range
  filter=file_text
  NONMEM_table=plot_IPRED

  if(LLOQFLAG_col!=FALSE & LLOQ_col==FALSE){
    stop("LLOQFLAG_col has been set to a column name but LLOQ_col has been set to FALSE. When LLOQFLAG_col has a name then LLOQ_col must also have a name")
  }

  if(LLOQFLAG_col==FALSE){
    LLOQ_col=FALSE
  }
  
  # if LLOQ_col is a number then it is not a column name but it is an LLOQ value to be applied to all samples where LLOQFLAG==1               
  LIMIT=NA
  if(is.numeric(LLOQ_col)==TRUE){
    LIMIT=LLOQ_col
  }
  
  # if user has specified a particular values of occ to plot, then it makes no sence to have set keep_doses_after_last_obs to TRUE, so set it to FALSE
  if(occ!=FALSE){
    keep_doses_after_last_obs=FALSE
  }
  
  if(!DV_col%in%c('DV','DV_DOSE_CORRECTED')){
    stop('The only permitted values for DV_col are: DV or DV_DOSE_CORRECTED')
  }
  
  plot_counter=0
  legend_counter=0
  
  
  #-----------------------------------------------------------------------------------------------------------------
  
  if(calc_dose==TRUE & !"AMT"%in%names(exp_data)){
    stop("You have requested calculation of DOSE column but the is no AMT column in the data file. DOSE cannot be calculated")
  }   

  # check if dose column is required
  if((dose_marker==TRUE | "DOSE"%in%title_label_col | DV_col=="DV_DOSE_CORRECTED") & !"DOSE"%in%names(exp_data)){
    # dose column is required and is not already in the data
    if(calc_dose==FALSE){
      stop("The function arguments require that a DOSE column available. DOSE is not in the data, so you much set calc_dose to TRUE to continue")
    }
    if(calc_dose==TRUE & !"AMT"%in%names(exp_data)){
      stop("You have requested calculation of DOSE column but the is no AMT column in the data file. DOSE cannot be calculated")
    }    
  }
  
  #---------------------------------------------------- Option to add DOSE column to the database ------------------------------
  if (calc_dose==TRUE){
    exp_data=calc_dose(exp_data,ID_col=ID_col)
  }
  
  #------------------------------------------------------ DOSE column ----------------------------------
  if("DOSE"%in%names(exp_data) & DV_col=="DV"){    
    colours_column=TRUE
    plot_dose_marker=TRUE
  }
  
  if("DOSE"%in%names(exp_data) & DV_col=="DV_DOSE_CORRECTED"){    
    colours_column=TRUE
    plot_dose_marker=TRUE
  }
  
  if(!"DOSE"%in%names(exp_data) & DV_col=="DV"){      #in this case function cannot add colours to the plot based on the dose size
    colours_column=FALSE
    COLOUR_occ='gray'
    COLOUR_all='gray'
    plot_dose_marker=FALSE
  }
  
  #---------------------------------------------------- Option to add TAD column --------------------------
  
  if(calc_tad==TRUE & "EVID"%in%names(exp_data) & ID_col%in%names(exp_data) & "TIME"%in%names(exp_data)){
    exp_data=calc_tad(exp_data,ID_col=ID_col)
  }
  
  if(ind_var_col=="TAD" & !"TAD"%in%names(exp_data)){
    stop("Function arguments require that a TAD column is in the data file. TAD is not in the data file. Try setting calc_tad to TRUE")
  } 
  
  exp_data[,ind_var_col]=exp_data[,ind_var_col]*ind_var_conversion   # Convert independent variable.  Often this will be conversion of TIME from hours to days
  
  #---------------------------------------------------- Option to add OCC column to the databse -------------------------------
  #Each occasion consisits of data records related to administrated dose and all concentrations which were measured afterwards
  #Occasion is equal to zero before administration of the first dose
  if(occ!=FALSE & calc_occ==FALSE & !"OCC"%in%names(exp_data)){
    stop("Function arguments require OCC column to be present in the data file. Try setting calc_occ to TRUE")
  }
  
  if(calc_occ==TRUE & !"AMT"%in%names(exp_data)){
    stop("Calculation of OCC column has been requested but AMT column is not in the data file. Cannot continue")
  }
  
  if(calc_occ==TRUE & "AMT"%in%names(exp_data)){
    exp_data=calc_occ(exp_data,ID_col=ID_col,calc_method='by_obs')
  }
  
  #---------------------------------------------------- IPRED --------------------------
  
  IPRED_col="IPRED"
  if (NONMEM_table==TRUE){
    if (DV_col=='DV_DOSE_CORRECTED'){
      exp_data$IPRED_DOSE_CORRECTED=exp_data$IPRED/exp_data$DOSE #Calculate dose-corrected DV 
      IPRED_col="IPRED_DOSE_CORRECTED"
    }
  }
  
  #------------------------------------------------ Add column with different colours for different doses ---------------------
  if(colours_column==TRUE){
    
    dose_unique=unique(exp_data[,"DOSE"])
    dose_unique_sorted=sort(dose_unique)

    if(colour_shading<0){
      colour_shading=0
    }
    if(colour_shading==0){
      colour_shading=sqrt(max(dose_unique)/min(dose_unique))/2
    }

    #Add column with with different colours for different doses to be later shown on the plot as e.g. background data
    colfunc=colorRampPalette(all_subjects_colours, bias=colour_shading) #Data in the backround to be coloured as shades of pink to identify increase in dose 
    all_dose_colours=colfunc(length(dose_unique_sorted))
    
    exp_data[,"ALL_COLOURS"]='9999'
    
    for(col in 1:length(all_dose_colours)){
      exp_data[which(exp_data[,"DOSE"]==dose_unique_sorted[col]),'ALL_COLOURS']=all_dose_colours[col]
    }
    
  }
  
  
  if("OCC"%in%names(exp_data)){
    exp_data[,"OCC_ORIG"]=exp_data[,"OCC"]                                  #the original occasion column with OCC=0 remaining unchanged 
    exp_data[exp_data$OCC==0,"OCC"]=1  #occasion column used for plotting: records before the first dose are plotted as occasion 1
  }

  #---------------------------------------------------- Calculate dose-corrected DV -------------------------------------------
  #-----------------------------------------------  Handling data below the limit of quantification ---------------------------
  
  
  #For data records below the limit of quantification DV_col is equal to limit of quantification; if column with the value of LOQ does not exist then the limit is equal to LIMIT
  if(LLOQFLAG_col!=FALSE & DV_col=="DV_DOSE_CORRECTED"){
        exp_data$DV_DOSE_CORRECTED=exp_data$DV/exp_data$DOSE #Calculate dose-corrected DV 
        if(is.na(LIMIT)==FALSE){                             
          exp_data[exp_data[,LLOQFLAG_col]==1,DV_col]=LIMIT/exp_data[exp_data[,LLOQFLAG_col]==1,"DOSE"]   # DV_DOSE_CORRECTED: use the BLQ LIMIT value if it has been entered. So the LIMIT gets dose-corrected
        }else{
          exp_data[exp_data[,LLOQFLAG_col]==1,DV_col]=exp_data[exp_data[,LLOQFLAG_col]==1,LLOQ_col]/exp_data[exp_data[,LLOQFLAG_col]==1,"DOSE"]  # DV_DOSE_CORRECTED
        }
  }


  if(LLOQFLAG_col!=FALSE & DV_col=="DV"){
        if(is.na(LIMIT)==FALSE){                             
          exp_data[exp_data[,LLOQFLAG_col]==1,DV_col]=LIMIT   # DV
        }else{
          exp_data[exp_data[,LLOQFLAG_col]==1,DV_col]=exp_data[exp_data[,LLOQFLAG_col]==1,LLOQ_col]  # DV
        }
  }


  if(LLOQFLAG_col==FALSE & DV_col=="DV_DOSE_CORRECTED"){
        exp_data$DV_DOSE_CORRECTED=exp_data$DV/exp_data$DOSE # DV_DOSE_CORRECTED
  }

  if(DV_col=="DV_DOSE_CORRECTED"){
     # correct any calculations of DV_DOSE_CORRECTED that have problems due to zero dose
     exp_data[,DV_col][exp_data$DOSE==0]=exp_data$DV[exp_data$DOSE==0]
  }
  

   zero_obs=exp_data[,DV_col][exp_data$EVID==0 & exp_data[,DV_col]<=0]
   if(length(zero_obs)>0){
     print('WARNING...WARNING...WARNING...')
     print(paste0(length(zero_obs),' observations have values less than or equal to zero. This will cause problems if you have requested a logarithmic y axis'))
     print('If you are not expecting any observations with value of zero, then check that LLOQFLAG_col and LLOQ_col have been correctly defined')
   }
   
   
   # before TAD and TIME get altered, save the exp_data as all_exp_data, which will be returned by the function at the end
   all_exp_data=exp_data
   
   #------------------------- Replace TAD and TIME = 0 with a small value (to avoid problems on a log scale)----------------
   
   ### AARON 1 START ### here we make the adjustments to TIME and TAD to enable plotting with log x-axis. The adjustments are actually used with linear x-axis too, which is a bit lazy really

   #TIME
   #minimum=min(exp_data[exp_data[,"TIME"]!=0,"TIME"])   # we are assuming here that TIME shouldn't normally be negative
   #if(minimum>=0.1){
   #  exp_data[exp_data[,"TIME"]==0,"TIME"]=0.1
   #}else{
   #  exp_data[exp_data[,"TIME"]==0,"TIME"]=minimum
   #}
   #
   #TAD
   #if(ind_var_col=="TAD"){
   #  minimum=min(exp_data[exp_data[,"TAD"]>0,"TAD"])   # remember that TAD could be negative (eg for observations before first dose). For datasets which have records with TAD<0 then log x-axis will be a problem unless the records with negative TAD are filtered before plotting
   #  if(minimum>=0.1){
   #    exp_data[exp_data[,"TAD"]==0,"TAD"]=0.1
   #  }else{
   #    exp_data[exp_data[,"TAD"]==0,"TAD"]=minimum
   #  }
   #}
   
   ### AARON 1 END ###
   
   #-------------------------------------------------------
   dose_time=exp_data[exp_data$EVID%in%c(1,4),] #this set of data will be used later to plot vertical lines when dose was administrated  
   


  ### now check for any subjects who have no observations and remove them
  subject=unique(exp_data[,ID_col])
  IDS_NULL=NULL
  for(g in 1:length(subject)){
   obs_recs=exp_data[exp_data[,ID_col]==subject[g] & exp_data$EVID==0,DV_col]
   if(length(obs_recs)==0){
     IDS_NULL=c(subject[g],IDS_NULL)
   }
  }	
  if(length(IDS_NULL)>0){
    num_null=length(IDS_NULL)
    print(paste0('The following ',num_null,' IDs have no observations and will not be plotted:'))
    print(paste0(IDS_NULL,collapse=' '))
    exp_data=exp_data[!exp_data[,ID_col]%in%IDS_NULL,]
  }



  #---------------------------------------------------- Plot data -------------------------------------------------------------
  #---------------------------------------------------- Single plot or panel of 8 ---------------------------------------------

  subject=unique(exp_data[,ID_col])
  
  print('Generating plots...')    
  
  #Plots
  if (panel==TRUE){                  #panel of 8 plots
    loops=ceiling(length(subject)/8) #number of panels each consisiting of 8 plots
    N=c(2,4)                       #2 rows and 4 columns
    M=c(4,4,0,0)
    plot=8
    WIDTH=1500                     #width of a figure generated
    xl=''                          #for panels labels will be placed using mtext function 
    yl=''
  }else{                             #single plot
    loops=length(subject)
    N=c(1,1)
    M=c(0,0,0,0)
    plot=1
    WIDTH=800
    xl=x_label
    yl=y_label
  } 
  
  
  for(s in 1:loops){         #this loop is necessary when panels of 8 plots are generated; "loops" is equal to the number of subjects when a single plot has to be generated for each subject
    
    if(s!=loops){            #to ensure that panels include in total as many plots as number of subjects and not the extra empty ones
      PLOT=plot              #8 plots
    } else{
      PLOT=length(subject)   #in the last panel as many plots as the number of remaining subjects
    }
    
    if(occ!=FALSE){          #to always display occasion number in the title of the plot
      occ_label=occ
    }else{
      occ_label='all'
    }
    
    if (panel==TRUE){
      filename=paste0('panel_', 'IDs ',subject[1], '-',subject[PLOT],' ', filter,' ', 'OCC_',occ_label) #the name has to be changed accordingly depending if panels or single plots are generated
    }else{
      filename=paste0('ID_', subject[1],' ', filter,' ','OCC_',occ_label)
    }  
    
    
    fnme=paste0(save_path,filename,'.png')
    png(fnme,width=WIDTH,height=800)
    
    par(mfrow=N)
    par(mar=c(4.5,4.5,3,1))            
    par(oma=M)
    par(cex.lab=1.2)    
    par(cex.main=1.2)   
    par(cex.axis=1.2)
    
    
    for(i in 1:PLOT){       #The previous loop is not continued but the new one is started because the changes to TAD column (TAD=0) have to be saved for the whole database to be later plotted
      
      subjectunique=exp_data[exp_data[,ID_col]==subject[i], ]
      
      # add additional plot title label if required
      additional_title_label=''
      if(sum(title_label_col!='')){
        
        for(L in 1:length(title_label_col)){
          
          additional_title_label=paste0(' ',title_label_col[L],'=',subjectunique[,title_label_col[L]][1]) #to later make a vector of titles of disfferent columns and its values for each subject
          
          
          if (occ!=FALSE){                       #multiple dose: dose might change with each occasion
            if(title_label_col[L]=='AMT'){
              amt_unique=dose_time[dose_time[,ID_col]==subject[i] & dose_time$OCC==occ,'AMT']
              additional_title_label=paste0(' ',title_label_col[L],'=',amt_unique[1])
            }
            if(title_label_col[L]=='DOSE'){
              dose_unique=dose_time[dose_time[,ID_col]==subject[i] & dose_time$OCC==occ,'DOSE']
              additional_title_label=paste0(' ',title_label_col[L],'=',dose_unique[1])
            }
            if(title_label_col[L]=='OCC'){
              occ_unique=exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ,'OCC']
              additional_title_label=paste0(' ',title_label_col[L],'=',occ_unique[1])
            }
          }else{
            if(title_label_col[L]=='AMT'){
              amt_unique=dose_time[dose_time[,ID_col]==subject[i],'AMT']
              additional_title_label=paste0(' ',title_label_col[L],'=',amt_unique[1])
            }
            if(title_label_col[L]=='DOSE'){
              dose_unique=dose_time[dose_time[,ID_col]==subject[i],'DOSE']
              additional_title_label=paste0(' ',title_label_col[L],'=',dose_unique[1])
            }
            if(title_label_col[L]=='OCC'){
              additional_title_label=paste0(' ',title_label_col[L],'=','all')
            }
          }
          
          if (L==1){
            additional_title_list=c(paste0('ID=',subject[i]), additional_title_label) #to make a list of column titles to be displayed at the top of each plot
          }else{
            additional_title_list=c(additional_title_list,additional_title_label)
          }
          
        }
        
      }else{
        additional_title_list=paste0('SID=',subjectunique$SID[1])
      }
      
      title_main=paste(as.character(additional_title_list), collapse=", ") #so that column titles are displayed in one line at the top of each plot
      
      #set colours for observation records: red for BLQ, black for all others
      if(LLOQFLAG_col %in% names(subjectunique)){
        subjectunique[(subjectunique$EVID==0 & subjectunique[,LLOQFLAG_col]==0),'COLOUR']='black'
        subjectunique[(subjectunique$EVID==0 & subjectunique[,LLOQFLAG_col]!=0),'COLOUR']='black' 
      } else {
        subjectunique[subjectunique$EVID==0,'COLOUR']='black'      
      }
      
      #Plot data
      
      if(colours_column==TRUE){
        COLOUR_occ=exp_data[,'ALL_COLOURS'][exp_data$OCC==occ & exp_data$EVID==0]
        COLOUR_all=exp_data[,'ALL_COLOURS'][exp_data$EVID==0]
      }
      
      
      
      id_dat=subjectunique[subjectunique$EVID==0,]                                  # get observation records for current patient
      t_last=max(exp_data$TIME)			      
      if(keep_doses_after_last_obs==FALSE & ind_var_col=='TIME'){
        t_last=max(subjectunique[subjectunique$EVID==0,ind_var_col])		      # get TIME of last observation for current patient	   
      }
      
      
      # set y-axis limits
      min_y=min(exp_data[exp_data$EVID==0,DV_col])
      if(NONMEM_table==TRUE){
         min_y=min(min_y,exp_data[exp_data$EVID==0 & is.na(exp_data[,IPRED_col]==FALSE),IPRED_col],exp_data[exp_data$EVID==0 & is.na(exp_data[,'PRED']==FALSE),'PRED'])
      }

      if(all_same_scale_y==FALSE & NONMEM_table==FALSE){
        ylims=c(min(id_dat[,DV_col]),max(id_dat[,DV_col])) 
      }

      if(all_same_scale_y==FALSE & NONMEM_table==TRUE){
        ylims=c(min(min(id_dat[,DV_col]),min(id_dat[is.na(id_dat[,IPRED_col])==FALSE,IPRED_col]),min(id_dat[is.na(id_dat$PRED)==FALSE,'PRED'])),max(max(id_dat[,DV_col]),max(id_dat[is.na(id_dat[,IPRED_col])==FALSE,IPRED_col]),max(id_dat[is.na(id_dat$PRED)==FALSE,'PRED']))) 
      } 

      if(all_same_scale_y==TRUE & NONMEM_table==FALSE){
        ylims=c(min_y,max(exp_data[exp_data$EVID==0,DV_col])) 
      }

      if(all_same_scale_y==TRUE & NONMEM_table==TRUE){
        ylims=c(min_y,max(max(exp_data[,DV_col]),max(exp_data[is.na(exp_data[,IPRED_col])==FALSE,IPRED_col]))) 
      }

      
      ### AARON 2 START ### here we set x-axis limits.

      # set x-axis limits
      if(occ==FALSE){
        if(all_same_scale_x==FALSE){
          xlims=c(min(exp_data[exp_data[,ID_col]==subject[i],ind_var_col]),max(exp_data[exp_data[,ID_col]==subject[i],ind_var_col]))
	  if(LOG)
	  {
		xlims=range(exp_data[exp_data[,ID_col]==subject[i],ind_var_col][exp_data[exp_data[,ID_col]==subject[i],ind_var_col]>0])
	  }
        }
        
        if(all_same_scale_x==TRUE){
          xlims=c(min(exp_data[,ind_var_col]),max(exp_data[,ind_var_col]))
	  if(LOG)
	  {
		xlims=range(exp_data[,ind_var_col][exp_data[,ind_var_col]>0])
	  }
        }
        
        
        if(all_same_scale_x==FALSE & keep_doses_after_last_obs==FALSE & ind_var_col=='TIME'){
          # adjust the x-axis maximum if we dont want to include doses after last observation	
          xlims[2]=max(exp_data[exp_data[,ID_col]==subject[i] & exp_data[,ind_var_col]<=t_last,ind_var_col])   # on reading this again I think xlims[2]=t_last is all that is needed
        }
        
        if(all_same_scale_x==TRUE & keep_doses_after_last_obs==FALSE & ind_var_col=='TIME'){
          max_obs_time=max(exp_data[exp_data$EVID==0,ind_var_col])	
          xlims[2]=max_obs_time
        }
      }
      
      if(occ!=FALSE & all_same_scale_x==FALSE){ 
      ## take care here. This is all to do with plotting data for a particular value of OCC. If a patient has no data for that value of OCC then they will get an empty plot so x-limits still have to be set
        fix=0
        xl1=exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ,ind_var_col] 
        xl2=exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ & exp_data$EVID==0,ind_var_col]
        if(length(xl1)==0){
          # no doses or observations for this OCC for current subject
          xl1=0.1
          fix=1
        }
        if(length(xl2)==0){
          # no observations for this OCC for current subject
          xl2=xl1*2
          fix=1
        }
        if(fix==0){
          xlims=c(min(exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ,ind_var_col]),max(exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ & exp_data$EVID==0,ind_var_col]))
	  if(LOG)
	  {
		xlims=range(exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ,ind_var_col][exp_data[exp_data[,ID_col]==subject[i] & exp_data$OCC==occ,ind_var_col]>0])
	  }

        } else {
          xlims=c(xl1,xl2)
        }
      }
      
      if(occ!=FALSE & all_same_scale_x==TRUE){ 
        xlims=c(min(exp_data[,ind_var_col]),max(exp_data[exp_data$EVID==0,ind_var_col]))
	if(LOG)
	{
		xlims=range(exp_data[,ind_var_col][exp_data[,ind_var_col]>0])
	}
      }
   
      # now make adjustments if axis min = axis max
      if(xlims[2]==xlims[1]){
         xlims[2]=xlims[1] + 0.1*abs(xlims[1]) + 0.001    # add 0.001 just in case xlims[1]=0
      }
      if(ylims[2]==ylims[1]){
         ylims[2]=ylims[1] + 0.1*abs(ylims[1]) + 0.001    # add 0.001 just in case ylims[1]=0
      }     
      
      # print(paste0(subject[i],' : ',xlims[1],' ',xlims[2],' : ',ylims[1],' ',ylims[2]))

      ### AARON 2 END ### 
      


      ### AARON 3 START ### here we build the plots 

if(occ!=FALSE){
  if (background_data==TRUE){
    X=exp_data[,ind_var_col][exp_data$OCC==occ & exp_data$EVID==0]
    x0=plot0(x=X,y=exp_data[,DV_col][exp_data$OCC==occ & exp_data$EVID==0],type='p',col=COLOUR_occ,pch=19, cex=1.0, xlimi=xlims, ylim=ylims, ylab=yl, xlab=xl, main=NULL,...)
    title(title_main, line=2)
    points0(x=subjectunique[,ind_var_col][subjectunique$OCC==occ & subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$OCC==occ & subjectunique$EVID==0],x0=x0,type='p',col=subjectunique[subjectunique$OCC==occ & subjectunique$EVID==0,'COLOUR'],pch=19,cex=1.7)
    lines0(x=subjectunique[,ind_var_col][subjectunique$OCC==occ & subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$OCC==occ & subjectunique$EVID==0],x0=x0,type='l',col='black',lty=2)
    
  }else{
    if(sum(subjectunique$OCC==occ)){ #there might be situation that there are no data records for a certain subject and certain occasion; in this case an empty plot is generated if background data is not requested
      X=subjectunique[,ind_var_col][subjectunique$OCC==occ & subjectunique$EVID==0]
      x0=plot0(x=X,y=subjectunique[,DV_col][subjectunique$OCC==occ & subjectunique$EVID==0],type='p',col=subjectunique[subjectunique$OCC==occ & subjectunique$EVID==0,'COLOUR'],pch=19,cex=1.7, xlimi=xlims, ylim=ylims, ylab=yl, xlab=xl, main=NULL,...)
      title(title_main, line=2)
      lines0(x=subjectunique[,ind_var_col][subjectunique$OCC==occ & subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$OCC==occ & subjectunique$EVID==0],x0=x0,type='l',col='black',lty=2)
      
    }else{
      plot0(0, type='n', xlab="", ylab="", xaxt='n', yaxt='n') #if there is no data present for a specific subject and occasion then an empty window is shown
      title(title_main, line=2)
    }
  }
}else{
  if (background_data==TRUE){ 
    X=exp_data[,ind_var_col][exp_data$EVID==0]
    # plot background data
    x0=plot0(x=X,y=exp_data[,DV_col][exp_data$EVID==0],type='n',col=COLOUR_all, pch=19, cex=1.0, xlimi=xlims, ylim=ylims, ylab=yl,xlab=xl,main=NULL,...)
    if(extend_dose_marker==TRUE & ind_var_col=='TAD'){
       abline(v=0,lty=2,lwd=1,col='gray90')
    }
    points(x=exp_data[,ind_var_col][exp_data$EVID==0 & exp_data$LLOQFLAG==0],y=exp_data[,DV_col][exp_data$EVID==0 & exp_data$LLOQFLAG==0],type='p',col=COLOUR_all, pch=19, cex=1.0)
    points(x=exp_data[,ind_var_col][exp_data$EVID==0 & exp_data$LLOQFLAG==1],y=exp_data[,DV_col][exp_data$EVID==0 & exp_data$LLOQFLAG==1],type='p',col=COLOUR_all, pch=17, cex=1.6)
    title(title_main,line=2)
    ####RPA#### add individual data, cicles for >LLOQ and triangles for <LLOQ
    points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],y=subjectunique[,DV_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],x0=x0,type='p',col=subjectunique[,'COLOUR'][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],pch=19,cex=1.2)
    points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],y=subjectunique[,DV_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],x0=x0,type='p',col=subjectunique[,'COLOUR'][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],pch=17,cex=2)
    
    if(ind_var_col=='TIME'){
      # add lines to DV if TIME is x axis
      points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$EVID==0],x0=x0,type='l',col='black',lty=2)
    }

    if(NONMEM_table==TRUE & ind_var_col=='TIME'){
      # add lines to IPREDS if TIME is x axis
      points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & is.na(subjectunique[,'PRED'])==FALSE],x0=x0,y=subjectunique[,'PRED'][subjectunique$EVID==0 & is.na(subjectunique[,'PRED'])==FALSE],type='l',col='gray80',lty=1,pch=19,cex=1)
      points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & is.na(subjectunique[,IPRED_col])==FALSE],x0=x0,y=subjectunique[,IPRED_col][subjectunique$EVID==0 & is.na(subjectunique[,IPRED_col])==FALSE],type='l',col='black',lty=1)
    }

  }else{
    x0=plot0(x=subjectunique[,ind_var_col][subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$EVID==0],type='n',col=subjectunique[,'COLOUR'][subjectunique$EVID==0],pch=19,cex=1.5, xlimi=xlims, ylim=ylims, ylab=yl,xlab=xl,main=NULL,...)
    
    if (dose_marker==TRUE & length(dose_time[dose_time[,ID_col]==subject[i],"ALL_COLOURS"])>0){          #to plot vertical line when the dose was administrated
       colours=dose_time[dose_time[,ID_col]==subject[i],"ALL_COLOURS"]  #dose colours
       id_dose_times=dose_time[dose_time[,ID_col]==subject[i],ind_var_col] #times when the dose was administarted
       if(LOG) id_dose_times[id_dose_times<=0]=x0
       mark=id_dose_times 
       for(c in 1:length(mark)){   #to make sure that tickmarks have different colours depending on the dose administrated
          axis(side=3, at=mark[c], col.ticks=colours[c], labels=FALSE, lwd.ticks=3, lwd=0, tck=-0.03) #tck - the length of the tickmark (if negative tickamark is plotted outside the plot)
       }
       if(extend_dose_marker==TRUE){
          abline(v=id_dose_times,lty=2,lwd=1,col='gray90')
       }
    }
    
    ####RPA#### when no background data is plotted, here we add individual data, cicles for >LLOQ and triangles for <LLOQ
    points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],y=subjectunique[,DV_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],type='p',col=subjectunique[,'COLOUR'][subjectunique$EVID==0 & subjectunique$LLOQFLAG==0],pch=19,cex=1.2)
    points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],y=subjectunique[,DV_col][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],type='p',col=subjectunique[,'COLOUR'][subjectunique$EVID==0 & subjectunique$LLOQFLAG==1],pch=17,cex=2)
    title(title_main,line=2)
    
    if(ind_var_col=='TIME' & NONMEM_table==FALSE){
       # add lines to DV if TIME is x axis
       points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0],y=subjectunique[,DV_col][subjectunique$EVID==0],x0=x0,type='l',col='black',lty=2)
    }
    
    if(NONMEM_table==TRUE & ind_var_col=='TIME'){
      # add lines to IPREDS if TIME is x axis
       points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & is.na(subjectunique[,'PRED'])==FALSE],x0=x0,y=subjectunique[,'PRED'][subjectunique$EVID==0 & is.na(subjectunique[,'PRED'])==FALSE],type='l',col='gray80',lty=1,pch=19,cex=1)
       points0(x=subjectunique[,ind_var_col][subjectunique$EVID==0 & is.na(subjectunique[,IPRED_col])==FALSE],x0=x0,y=subjectunique[,IPRED_col][subjectunique$EVID==0 & is.na(subjectunique[,IPRED_col])==FALSE],type='l',col='black',lty=1)
    }
  }
}  
      
      
      if (panel==TRUE){
        mtext(y_label, side=2, outer=TRUE, cex=1.5) #to ensure that the plot labels in the panel of 6 plots are only on the outside 
        mtext(x_label, side=1, outer=TRUE, cex=1.5) 
      }
    }
    plot_counter=plot_counter+1
    dev.off()   
    
    if (panel==TRUE){                   #to remove subjects from the vector of all subjects each time a panel of 8 is generated so that new panels are generated for remaining subjects only 
      subject=subject[!subject%in%subject[1:8]]
    }else{
      subject=subject[!subject%in%subject[1]]  #and the same for single plots
    }
    
    if(plot_dose_marker==TRUE & legend_counter==0){
      #Plot legend as a separate plot
      legend_counter=1
      fnme=paste0(save_path,'Legend.png')
      png(fnme,width=800,height=400)
      
      par(mfrow=c(2,1))
      par(cex.lab=1.2)
      par(cex.axis=1.2)
      plot(1,type='n',axes=F,ann=F)
       if(is.na(LIMIT)==TRUE){
         leg1=paste0('Observation <LLOQ. Marker placed at LLOQ for visualisation')
       }else{
         leg1=paste0('Observation <LLOQ. For visualisation of observations <LLOQ the markers are are placed at: ',LIMIT)
       }
      
       print(leg1)
       legend('topleft',c('Quantified observation',leg1),pt.cex=c(1.5,2),pch=c(19,17),col=c('black','black'),bty='n')
       plot(x=dose_unique_sorted,y=rep(1,length(all_dose_colours)),col=all_dose_colours,cex=3,pch=19,ylab='',yaxt='n',xlab="DOSE",main='Legend for colouring of doses',xaxt='n',xlim=range(pretty(dose_unique_sorted,n=8)))
       axis(side=1,at=pretty(dose_unique_sorted,n=8),labels=as.character(pretty(dose_unique_sorted,n=8)))
       dev.off()
    }
     
    if(plot_dose_marker==FALSE & legend_counter==0){
      #Plot legend as a separate plot
      legend_counter=1
      fnme=paste0(save_path,'Legend_no_dose_key.png')
      png(fnme,width=800,height=250)

      par(mfrow=c(1,1))
      par(cex.lab=1.2)
      par(cex.axis=1.2)
      plot(1,type='n',axes=F,ann=F)
      if(is.na(LIMIT)==TRUE){
         leg1=paste0('Observation <LLOQ. Marker placed at LLOQ for visualisation')
      }else{
         leg1=paste0('Observation <LLOQ. For visualisation of observations <LLOQ the markers are are placed at: ',LIMIT)
      }
      print(leg1)
      legend('topleft',c('Quantified observation',leg1),pt.cex=c(1.5,2),pch=19,col=c('black','black'),bty='n')
      dev.off()
    }
    
    
  }
  print(paste0(plot_counter,' files saved to ',save_path))
  return(invisible(all_exp_data))
  #-----------------------------------------------------THE END of FUNCTION ----------------------------------------------------
}
######################################################################################################################################
######################################################################################################################################



