#' Calculates Time After most recent Dose for a NONMEM data file.
#'
#' Calculates elapsed time after most recent dose for any NONMEM file that contains dosing records.
#'
#' @author Rupert Austin
#' @param data data frame containing data in NONMEM format.
#' @param ID_col name of column containing patient IDs.
#' @param TIME_col name of column containing TIME.
#' @param TAD_col name of the new column that will contain Time After (most recent) Dose.
#' @param CMT_vals vector of value(s) of CMT for which doses should contribute to the TAD calculation. If CMT_vals is left as FALSE and a
#'        CMT column exists then all doses will contribute to the calculation of TAD, irrespective of the value of CMT. As an example,
#'        you might have oral doses with CMT=1 and IV doses with CMT=2 and you only want the IV doses to register as doses in the TAD
#'        calculation, then set: CMT_vals=2.
#' @param TROUGH_val 1 (default) or 0. Determines how Time After (most recent) Dose is calculated for an observation record when that
#'        obervation record is at the same TIME as a dosing record. If the dosing record is the first qualifying dose (i.e. it's CMT exists
#'        within CMT_vals) then TAD will always be calculated as 0 irrespective of the setting for TROUGH_val. For subsequest qualifying
#'        doses which have observation records at the same TIME, if TROUGH_val=1 then TAD will be calculated as the elapsed TIME since the
#'        previous qualifying dose (default behaviour because this gives much better distinction of the trough concentrations in VPC/GOF plots).
#'        If TROUGH_val=0 then TAD will be calculated as 0.
#' @return The input data frame with Time After last Dose added in new column with name TAD_col.
#' @note If a patient has no qualifying dosing records, then TAD will be set to 0 for all records.
#' @note If CMT_vals<>FALSE then any dosing record with CMT not within CMT_vals will be treated like an observation record, and will likely
#'        have some TAD values >0.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
# @examples


#######################
#### calc_tad ##############
#######################
# calculates Time After (most recent) Dose for a NONMEM data file.
#
# Function arguments:
#
# data               Dataframe containing data in NONMEM format
#
# ID_col             (optional, default=ID) Name of column containing patient IDs
#
# TIME_col           (optional, default=TIME) Name of column containing TIME
#
# TAD_col            (optional, default=TAD) Name of the new column that will contain Time After (most recent) Dose
#
# CMT_vals           (optional, default=FALSE) Vector of value(s) of CMT for which doses should contribute to the TAD
#                    calculation. If CMT_vals is left as FALSE and a CMT column exists then all
#                    doses will contribute to the calculation of TAD, irrespective of the value of CMT.
#                    As an example, you might have oral doses with CMT=1 and IV doses with CMT=2 and you only want the
#                    IV doses to register as doses in the TAD calculation, then set: CMT_vals=2.
#
# TROUGH_val         (optional, default=1, alternative value=0) Determines how Time After (most recent) Dose is calculated for an
#                    observation record when that obervation record is at the same TIME as a dosing record. If the dosing record
#                    is the first qualifying dose (i.e. it's CMT exists within CMT_vals) then TAD will always be calculated
#                    as 0 irrespective of the setting for TROUGH_val. For subsequest qualifying doses which have
#                    observation records at the same TIME, if TROUGH_val=1 then TAD will be calculated as the elapsed
#                    TIME since the previous qualifying dose (default behaviour because this gives much better distinction
#                    of the trough concentrations in VPC/GOF plots). If TROUGH_val=0 then TAD will be calculated as 0.
#
# function returns the input data frame with Time After last Dose added in new column with name TAD_col
#
# NOTE: if a patient has no qualifying dosing records then TAD will be set to 0 for all records
#
# NOTE: If CMT_vals<>FALSE then any dosing record with CMT not within CMT_vals will be treated like an
# observation record, and will likely have some TAD values >0
############################################################################################
calc_tad = function(data,ID_col='ID',TIME_col='TIME',TAD_col='TAD',CMT_vals=FALSE,TROUGH_val=1){

  ### First check for required columns
  nms=names(data)
  required_names=c('AMT','EVID',TIME_col)
  if(!all(required_names%in%nms)){
    stop("Cannot calculate TAD as one or more required columns (AMT / EVID / TIME) are missing")
  }

  ### NOW WE MUST SORT THE DATA BY ASCENDING ID THEN ASCENDING TIME THEN DESCENDING EVID
  ### THIS WILL ENSURE THAT WHEN DOSES AND OBSERVATIONS HAVE THE SAME TIME, THE DOSE WILL APPEAR FIRST
  data=data[order(data[,ID_col],data[,TIME_col],-data$EVID),]

  # first test for presence of EVID column
  if(!('EVID' %in% names(data))){
    stop('EVID not present in NONMEM data file. Calculation not possible')
    res=data
  } else if(!TROUGH_val%in%c(0,1)){
    print('TROUGH_val must be 0 or 1. Calculation not possible')
    res=data
  } else {
    q=data
    q[,TAD_col]=-999

    # set TAD for qualifying dosing records
    if(CMT_vals[1]==FALSE){
      q[q$EVID%in%c(1,4),TAD_col]=0
    } else {
      q[q$EVID%in%c(1,4) & q$CMT%in%CMT_vals,TAD_col]=0
    }

    IDs=unique(q[,ID_col])
    for(i in 1:length(IDs)){

      # get times of qualifying doses and observations
      if(CMT_vals[1]==FALSE){
        dos_times=q[(q[,ID_col]==IDs[i] & q$EVID%in%c(1,4)),TIME_col]
        obs_times=q[(q[,ID_col]==IDs[i] & !q$EVID%in%c(1,4)),TIME_col]
      } else {
        dos_times=q[(q[,ID_col]==IDs[i] & q$EVID%in%c(1,4) & q$CMT%in%CMT_vals),TIME_col]
        obs_times=q[((q[,ID_col]==IDs[i] & !q$EVID%in%c(1,4))) | ((q[,ID_col]==IDs[i] & q$EVID%in%c(1,4)) & !q$CMT%in%CMT_vals),TIME_col]   # dosing records with CMT not in CMT_vals are counted as obs records
      }

      # find dose time relevant to each obs time
      dose_time_vals=0  # dose_time_vals will be a vector of same length as obs_times that will contain the next qualifying dose time for each obs time
      TAD_id=0
      if(length(obs_times)>0){
        for(k in 1:length(obs_times)){
          if(any(obs_times[k]>=dos_times)==TRUE){
            dose_time_vals[k]=max(dos_times[obs_times[k]>=dos_times])
          } else {
            # the current obs time is less than any of the dose times
            # so we assume the observation is pre first dose and set associated dose time to time of first qualifying dose
            dose_time_vals[k]=dos_times[1]
          }
        } # end of for k

        # deal with alternative treatment of trough observations which have same TIME as a qualifying dosing record
        if(TROUGH_val!=0){
          # find any observation records (after first qualifying dosing record) that are at same TIME as a qualifying dosing record
          obs_num_to_edit=which(obs_times%in%dos_times[-1])  # find the index of the observation records, within obs_times, that need to be assigned to a different time within dose_time_vals
          corrected_dos_time=dos_times[match(obs_times[obs_num_to_edit],dos_times)-1]
          # now do the correction to dose_time_vals
          if(length(obs_num_to_edit)>0){
            dose_time_vals[obs_num_to_edit]=corrected_dos_time
          }
        }

        # calculate TAD for current ID
        TAD_id=obs_times-dose_time_vals

        # now add the calculated TAD back to q
        if(CMT_vals[1]==FALSE){
          q[(q[,ID_col]==IDs[i] & !q$EVID%in%c(1,4)),TAD_col]=TAD_id
        } else {
          q[((q[,ID_col]==IDs[i] & !q$EVID%in%c(1,4))) | ((q[,ID_col]==IDs[i] & q$EVID%in%c(1,4)) & !q$CMT%in%CMT_vals),TAD_col]=TAD_id
        }

      }
    } # end of for i
    # if any patients had no qualifying dosing records then their TAD will be NA. So we replace with 0
    q[is.na(q[,TAD_col])==TRUE,TAD_col]=0
    res=q
  } # end of else
  return(res)
}
