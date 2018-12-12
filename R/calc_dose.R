#' Calculates most recent DOSE size for a NONMEM data file.
#'
#' Calculates size of most recent dose for any NONMEM file that contains dosing records.
#'
#' @author Rupert Austin
#' @param data data frame containing data in NONMEM format.
#' @param ID_col name of column containing patient IDs.
#' @param TIME_col name of column containing TIME.
#' @param DOSE_col name of the new column that will contain DOSE.
#' @param CMT_vals vector of value(s) of CMT for which doses should contribute to the DOSE calculation. If CMT_vals is left as FALSE and a CMT
#'        column exists then all doses will contribute to the calculation of DOSE, irrespective of the value of CMT. As an example, you might
#'        have oral doses with CMT=1 and IV doses with CMT=2 and you only want the IV doses to register as doses in the DOSE calculation, then
#'        set: CMT_vals=2.
#' @return The input data frame with DOSE added in new column with name DOSE_col.
#' @details RULES FOR CALCULATION OF DOSE:
#'
#'          DOSE = first non-zero value of AMT (which has CMT within CMT_vals) for all observations before the first dose. Whenever AMT (which has
#'          CMT within CMT_vals) changes to a new non-zero value, that becomes the new value of DOSE until AMT (which has CMT within CMT_vals)
#'          changes again to a non-zero value.
#' @note If a patient has no qualifying dosing records, then DOSE will be set equal to 0 for all records.
#' @export

#######################
#### calc_dose #############
#######################
# calculates most recent DOSE size for a NONMEM data file.
#
# Function arguments:
#
# data               Dataframe containing data in NONMEM format
#
# ID_col             (optional, default=ID) Name of column containing patient IDs
#
# TIME_col           (optional, default=TIME) Name of column containing TIME
#
# DOSE_col           (optional, default=DOSE) Name of the new column that will contain DOSE
#
# CMT_vals           (optional, default=FALSE) Vector of value(s) of CMT for which doses should contribute to the DOSE
#                    calculation. If CMT_vals is left as FALSE and a CMT column exists then all
#                    doses will contribute to the calculation of DOSE, irrespective of the value of CMT.
#                    As an example, you might have oral doses with CMT=1 and IV doses with CMT=2 and you only want the
#                    IV doses to register as doses in the DOSE calculation, then set: CMT_vals=2.
#
# function returns the input data frame with DOSE added in new column with name DOSE_col
#
# RULES FOR CALCULATION OF DOSE ###
# DOSE=first non-zero value of AMT (which has CMT within CMT_vals) for all observations before the first dose.
# Whenever AMT (which has CMT within CMT_vals) changes to a new non-zero value, that becomes the new value
# of DOSE until AMT (which has CMT within CMT_vals) changes again to a non-zero value
#
# NOTE: if a patient has no qualifying dosing records then DOSE will be set equal to 0 for all records
#
############################################################################################
calc_dose=function(data,ID_col='ID',TIME_col='TIME',DOSE_col='DOSE',CMT_vals=FALSE){

  ### First check for required columns
  nms=names(data)
  required_names=c('AMT','EVID',TIME_col)
  if(!all(required_names%in%nms)){
    stop("Cannot calculate DOSE as one or more required columns (AMT / EVID / TIME) are missing")
  }

  ### FIRST WE MUST SORT THE DATA BY ASCENDING ID THEN ASCENDING TIME THEN DESCENDING EVID
  ### THIS WILL ENSURE THAT WHEN DOSES AND OBSERVATIONS HAVE THE SAME TIME, THE DOSE WILL APPEAR FIRST
  data=data[order(data[,ID_col],data[TIME_col],-data$EVID),]

  data[,DOSE_col]=0
  subject=unique(data[,ID_col])
  for(i in 1:length(subject)){
    s=data[data[,ID_col]==subject[i],]
    if(CMT_vals[1]!=FALSE){
      doserows=which(s[,'EVID']%in%c(1,4) & s$CMT%in%CMT_vals)
    }else{
      doserows=which(s[,'EVID']%in%c(1,4))
    }
    if(length(doserows)>0){
      first_dose=s[doserows[1],'AMT']
      for(k in 1:nrow(s)){
        if(k==1){
          s[k,DOSE_col]=first_dose
          c_dose=first_dose
        }
        if(k>1 & s[k,'AMT']==0){
          s[k,DOSE_col]=c_dose
        }
        if(CMT_vals[1]!=FALSE){
          if(k>1 & s[k,'AMT']!=0 & !s[k,'CMT']%in%CMT_vals){
            s[k,DOSE_col]=c_dose
          }
          if(k>1 & s[k,'AMT']!=0 & s[k,'CMT']%in%CMT_vals){
            c_dose=s[k,'AMT']
            s[k,DOSE_col]=c_dose
          }
        }else{
          if(k>1 & s[k,'AMT']!=0){
            c_dose=s[k,'AMT']
            s[k,DOSE_col]=c_dose
          }
        }
      }
    } else {
      # no dose records
      # leave DOSE=0 for all records
    }
    # copy DOSE from s to data
    data[data[,ID_col]==subject[i],DOSE_col]=s[,DOSE_col]
  } # end of i loop
  return(data)
}
##############################################################
#################### END OF calc_dose function ####################
##############################################################
