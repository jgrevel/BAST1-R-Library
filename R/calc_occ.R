#' Calculates OCC (occasion) for a NONMEM data file.
#'
#' Calculates dosing occasion for any NONMEM file that contains dosing records.
#'
#' @author Rupert Austin
#' @param data data frame containing data in NONMEM format.
#' @param ID_col name of column containing patient IDs.
#' @param TIME_col name of column containing TIME.
#' @param OCC_col name of the new column that will contain OCC.
#' @param CMT_vals_dose vector of value(s) of CMT for which doses qualify for contribution to the OCC calculation. If CMT_vals_dose is left
#'        as FALSE and a CMT column exists then all doses will contribute to the calculation of OCC, irrespective of the value of CMT. As an
#'        example, you might have oral doses with CMT=1 and IV doses with CMT=2 and you only want the IV doses to register as doses in the
#'        OCC calculation, then set: CMT_vals_dose=2.
#' @param CMT_vals_obs vector of value(s) of CMT for which observations qualify for contribution to the OCC calculation. This function
#'        argument will only influence the calculation if calc_method='by_obs'. If CMT_vals_obs is left as FALSE then all observations will
#'        influence the calculation of OCC.
#' @param calc_method 'by_dose' (default) or 'by_obs'. Sets the calculation method.
#'        \itemize{
#'          \item 'by_obs' method defines that the value of OCC will increment by +1 whenever there is a new (qualifying) dosing record that is
#'                followed by at least one (qualifying) observation record before the next (qualifying) dosing record.
#'          \item 'by_dose' method defines that the value of OCC will increment by +1 whenever there is a new (qualifying) dosing record
#'                irrespective of observations.
#'        }
#' @return The input data frame with OCC added in new column with name OCC_col.
#' @details RULES FOR CALCULATION OF OCC:
#'
#'          calc_method = 'by_obs' \cr
#'          OCC=0 for all observations and doses before the first dose (with CMT within CMT_vals_dose) that is followed by at least one
#'          observation (with CMT within CMT_vals_obs). \cr
#'          OCC=1 for the first dose (with CMT within CMT_vals_dose) that is also followed by at least one observation (with CMT within
#'          CMT_vals_obs), and OCC=1 for each of those observations. \cr
#'          OCC will become 2 on the next dose (with CMT within CMT_vals_dose) which is also followed by at least one observation (with CMT
#'          within CMT_vals_obs), and OCC=2 for each of those observations the pattern then continues.
#'
#'          calc_method = 'by_dose' \cr
#'          OCC=0 for all observations and doses before the first dose (with CMT within CMT_vals_dose). \cr
#'          OCC=1 for the first dose (with CMT within CMT_vals_dose), and OCC=1 for each subsequent observation until the next dose (with CMT
#'          within CMT_vals_dose). \cr
#'          OCC will become 2 on the next dose (with CMT within CMT_vals_dose), and OCC=2 for each subsequent observation until the next dose
#'          (with CMT within CMT_vals_dose) the pattern then continues.
#'
#'          calc_method = 'by_dose' is most suitable for building models where each and every administration of a dose can plausibly lead to a
#'          change in a model parameter. This is particularly suitable for modelling IOV of absorption-related parameters in a PK model where
#'          each absorption occasion can be randomly influenced by many things such as intake of food, gastric pH, exercise etc.
#'
#'          calc_method = 'by_obs' is most suitable for building models where parameters might be changing more slowly and not necessarily after
#'          every dose. For example, this is more suitable for modelling IOV of parameters in a PK model where IV infusions are given only once
#'          a week or once a month and the IOV in the parameter(s) is not really associated with the dosing event but more to do with changes in
#'          something physiological over time. The IOV is then only updated upon the collection of new observations which supply evidence with
#'          which to update the parameter estimate(s).
#'
#' @note calc_method = 'by_obs' is also the most suitable for visualizing data by occasion using the NONMEM_plot function (BAST NONMEM data file
#'        visualisation tool).
#' @export


#######################
#### calc_occ ##############
#######################
# calculates OCC (occasion) for a NONMEM data file.
#
# Function arguments:
#
# data               Data frame containing data in NONMEM format
#
# ID_col             (optional, default=ID) Name of column containing patient IDs
#
# TIME_col           (optional, default=TIME) Name of column containing TIME
#
# OCC_col            (optional, default=TAD) Name of the new column that will contain OCC
#
# CMT_vals_dose      (optional, default=FALSE) Vector of value(s) of CMT for which doses qualify for contribution to the OCC
#                    calculation. If CMT_vals_dose is left as FALSE and a CMT column exists then all
#                    doses will contribute to the calculation of OCC, irrespective of the value of CMT.
#                    As an example, you might have oral doses with CMT=1 and IV doses with CMT=2 and you only want the
#                    IV doses to register as doses in the OCC calculation, then set: CMT_vals_dose=2.
#
# CMT_vals_obs       (optional, default=FALSE) Vector of value(s) of CMT for which observations qualify for contribution to the OCC
#                    calculation. This function argument will only influence the calculation if calc_method='by_obs'. If CMT_vals_obs is
#                    left as FALSE then all observations will influence the calculation of OCC.
#
# calc_method        (optional, default='by_dose', alternative value='by_obs') Sets the calculation method. 'by_obs' method defines
#                    that the value of OCC will increment by +1 whenever there is a new (qualifying) dosing record that is followed by at least one
#                    (qualifying) observation record before the next (qualifying) dosing record. 'by_dose' method defines that the value of
#                    OCC will increment by +1 whenever there is a new (qualifying) dosing record irrespective of observations.
#
# function returns the input data frame with OCC added in new column with name OCC_col
#
# RULES FOR CALCULATION OF OCC ###
# calc_method='by_obs'
# OCC=0 for all observations and doses before the first dose (with CMT within CMT_vals_dose) that is followed by at least one observation (with CMT within CMT_vals_obs)
# OCC=1 for the first dose (with CMT within CMT_vals_dose) that is also followed by at least one observation (with CMT within CMT_vals_obs), and OCC=1 for each of those observations
# OCC will become 2 on the next dose (with CMT within CMT_vals_dose) which is also followed by at least one observation (with CMT within CMT_vals_obs), and OCC=2 for each of those observations
# the pattern then continues
#
# calc_method='by_dose'
# OCC=0 for all observations and doses before the first dose (with CMT within CMT_vals_dose)
# OCC=1 for the first dose (with CMT within CMT_vals_dose), and OCC=1 for each subsequent observation until the next dose (with CMT within CMT_vals_dose)
# OCC will become 2 on the next dose (with CMT within CMT_vals_dose), and OCC=2 for each subsequent observation until the next dose (with CMT within CMT_vals_dose)
# the pattern then continues
#
# calc_method='by_dose' is most suitable for building models where each and every administration of a dose can plausibly lead to a change in a model parameter.
# This is particularly suitable for modelling IOV of absorption-related parameters in a PK model where each absorption occasion can be randomly influenced
# by many things such as intake of food, gastric pH, exercise etc.
#
# calc_method='by_obs' is most suitable for building models where parameters might be changing more slowly and not necessarily after every dose.
# For example, this is more suitable for modelling IOV of parameters in a PK model where IV infusions are given only once a week or once a month
# and the IOV in the parameter(s) is not really associated with the dosing event but more to do with changes in something physiological over time.
# The IOV is then only updated upon the collection of new observations which supply evidence with which to update the parameter estimate(s).
# calc_method='by_obs' is also the most suitable for visualizing data by occasion using the NONMEM_plot function (BAST NONMEM data file visualisation tool)
#
############################################################################################
calc_occ = function(data,ID_col='ID',TIME_col='TIME',OCC_col='OCC',CMT_vals_dose=FALSE,CMT_vals_obs=FALSE,calc_method='by_dose'){

  ### First check for required columns
  nms=names(data)
  required_names=c('EVID',TIME_col)
  if(!all(required_names%in%nms)){
    stop("Cannot calculate OCC because EVID and/or TIME_col columns are missing")
  }

  if((CMT_vals_obs[1]!=FALSE | CMT_vals_dose[1]!=FALSE) & !'CMT'%in%nms){
    stop("Cannot calculate OCC because CMT column is missing")
  }

  ### NOW WE MUST SORT THE DATA BY ASCENDING ID THEN ASCENDING TIME THEN DESCENDING EVID
  ### THIS WILL ENSURE THAT WHEN DOSES AND OBSERVATIONS HAVE THE SAME TIME, THE DOSE WILL APPEAR FIRST
  data$EVID_NEWqq=data$EVID
  data$EVID_NEWqq[data$EVID_NEWqq==1]=9
  data$EVID_NEWqq[data$EVID_NEWqq==4]=10
  data=data[order(data$ID,data[,TIME_col],-data$EVID_NEWqq),]
  data$EVID_NEWqq=NULL

  if(calc_method=='by_obs'){

    data[,OCC_col]=-9  # set OCC to -9. OCC will only change from this if dosing and observation records are found
    subject=unique(data[,ID_col])

    for(i in 1:length(subject)){
      s=data[data[,ID_col]==subject[i],]
      # find times of first qualifying dose and first qualifying observation
      if(CMT_vals_dose[1]==FALSE){
        time_first_dose=s[s$EVID%in%c(1,4),TIME_col][1]
        qual_dose_rows=which(s[,"EVID"]%in%c(1,4))
        all_dose_rows=qual_dose_rows
      }else{
        time_first_dose=s[s$EVID%in%c(1,4) & s$CMT%in%CMT_vals_dose,TIME_col][1]
        qual_dose_rows=which(s[,"EVID"]%in%c(1,4) & s$CMT%in%CMT_vals_dose)
        all_dose_rows=which(s[,"EVID"]%in%c(1,4))
      }

      if(CMT_vals_obs[1]==FALSE){
        time_first_obs=s[s$EVID==0,][,'TIME'][1]
        qual_obs_rows=which(s[,"EVID"]==0)
        all_obs_rows=which(!s[,"EVID"]%in%c(1,4))
      }else{
        time_first_obs=s[s$EVID==0 & s$CMT%in%CMT_vals_obs,TIME_col][1]
        qual_obs_rows=which(s[,"EVID"]==0 & s$CMT%in%CMT_vals_obs)
        all_obs_rows=which(!s[,"EVID"]%in%c(1,4))
      }

      obs_OCC=rep(-9,length(all_obs_rows))
      dose_OCC=rep(-9,length(all_dose_rows))
      if(is.na(time_first_obs)==FALSE & is.na(time_first_dose)==FALSE){
        # deal with observations before first qualifying dose
        predose_obs=which(all_obs_rows<qual_dose_rows[1])
        if(length(predose_obs)>0){
          obs_OCC[predose_obs]=0
        }

        # deal with doses before first qualifying dose
        predose_dose=which(all_dose_rows<qual_dose_rows[1])
        if(length(predose_dose)>0){
          dose_OCC[predose_dose]=0
        }

        occ_count=0

        num_q_ds=length(qual_dose_rows)
        if(num_q_ds==1){
          ds=1
        }else{
          ds=1:(length(qual_dose_rows)-1)
        }
        for(k in ds){

          current_dose_row=qual_dose_rows[k]
          next_dose_row=qual_dose_rows[k+1]
          # find observations with row numbers >current_dose and <next_dose
          vals=which(all_obs_rows>current_dose_row & all_obs_rows<next_dose_row)  # vals contains the indices of all_obs_rows that lie between current_dose and next_dose
          # i.e. all_obs_rows[vals] are the row numbers of all observations that lie between current_dose and next_dose
          all_obs_rows_between_doses=all_obs_rows[vals]
          qual_obs_rows_between_doses=all_obs_rows_between_doses[which(all_obs_rows_between_doses%in%qual_obs_rows)]
          non_qual_obs_rows_between_doses=all_obs_rows_between_doses[which(!all_obs_rows_between_doses%in%qual_obs_rows)]
          non_qual_dose_rows_between_doses=all_dose_rows[which(all_dose_rows>current_dose_row & all_dose_rows<next_dose_row)]

          if(num_q_ds==1){
            vals=which(qual_obs_rows>current_dose_row)

            qual_obs_rows_between_doses=qual_obs_rows[vals]
            non_qual_obs_rows_between_doses=all_obs_rows[which(all_obs_rows>current_dose_row)]
            non_qual_dose_rows_between_doses=all_dose_rows[which(all_dose_rows>current_dose_row)]
          }

          if(length(qual_obs_rows_between_doses)>0){
            # at least one qualifying observation lies between the 2 qualifying doses
            # first increment occ_count by +1
            occ_count=occ_count+1
            # set the qualifying observation records to occ_count
            obs_OCC[match(qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            # set current dose to occ_count
            dose_OCC[match(current_dose_row,all_dose_rows)]=occ_count
            # set any non-qualifying observation records to occ_count
            if(length(non_qual_obs_rows_between_doses)>0){
              obs_OCC[match(non_qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            }
            # set any non-qualifying dose records to occ_count
            if(length(non_qual_dose_rows_between_doses)>0){
              dose_OCC[match(non_qual_dose_rows_between_doses,all_dose_rows)]=occ_count
            }
          }else{
            # set current dose to occ_count
            dose_OCC[match(current_dose_row,all_dose_rows)]=occ_count
            # set any non-qualifying observation records to occ_count
            if(length(non_qual_obs_rows_between_doses)>0){
              obs_OCC[match(non_qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            }
            # set any non-qualifying dose records to occ_count
            if(length(non_qual_dose_rows_between_doses)>0){
              dose_OCC[match(non_qual_dose_rows_between_doses,all_dose_rows)]=occ_count
            }
          }
        }
        # now deal with the final qualifying dose
        if(num_q_ds>1){
          current_dose_row=qual_dose_rows[k+1]
          # are there any qualifying observations after this final qualifying dose?
          vals=which(qual_obs_rows>current_dose_row)  # qual_obs_rows[vals]

          qual_obs_rows_between_doses=qual_obs_rows[vals]
          non_qual_obs_rows_between_doses=all_obs_rows[which(all_obs_rows>current_dose_row)]
          non_qual_dose_rows_between_doses=all_dose_rows[which(all_dose_rows>current_dose_row)]

          if(length(qual_obs_rows_between_doses)>0){
            # first increment occ_count by +1
            occ_count=occ_count+1
            # set the qualifying observation records to occ_count
            obs_OCC[match(qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            # set current dose to occ_count
            dose_OCC[match(current_dose_row,all_dose_rows)]=occ_count
            # set any non-qualifying observation records to occ_count
            if(length(non_qual_obs_rows_between_doses)>0){
              obs_OCC[match(non_qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            }
            # set any non-qualifying dose records to occ_count
            if(length(non_qual_dose_rows_between_doses)>0){
              dose_OCC[match(non_qual_dose_rows_between_doses,all_dose_rows)]=occ_count
            }
          }else{
            # set current dose to occ_count
            dose_OCC[match(current_dose_row,all_dose_rows)]=occ_count
            # set any non-qualifying observation records to occ_count
            if(length(non_qual_obs_rows_between_doses)>0){
              obs_OCC[match(non_qual_obs_rows_between_doses,all_obs_rows)]=occ_count
            }
            # set any non-qualifying dose records to occ_count
            if(length(non_qual_dose_rows_between_doses)>0){
              dose_OCC[match(non_qual_dose_rows_between_doses,all_dose_rows)]=occ_count
            }
          }
        }

        # enter OCC values into s
        s[all_obs_rows,OCC_col]=obs_OCC
        s[all_dose_rows,OCC_col]=dose_OCC

        # now ensure that values of OCC from the same value of TIME are equivalent
        for(b in 1:(nrow(s)-1)){
          if(s[b,TIME_col]==s[b+1,TIME_col]){
            s[b,OCC_col]=s[b+1,OCC_col]
          }
        }

      } else {
        # if there are no qualifying doses or observations then set OCC=0 for all records
        s[,OCC_col]=0
      }

      # now move the OCC values from s back to data
      data[data[,ID_col]==subject[i],OCC_col]=s[,OCC_col]
    } # end of i loop
    # end of calc_method=='by_obs'
  }else{
    # calc_method=='by_dose'
    data[,OCC_col]=-9  # set OCC to -9. OCC will only change from this if dosing and observation records are found
    subject=unique(data[,ID_col])

    for(i in 1:length(subject)){
      s=data[data[,ID_col]==subject[i],]
      occ_count=0
      for(k in 1:nrow(s)){
        if(CMT_vals_dose[1]==FALSE){
          s[k,OCC_col]=occ_count
          if(s[k,'EVID']%in%c(1,4)){
            occ_count=occ_count+1
            s[k,OCC_col]=occ_count
          }
        }else{
          s[k,OCC_col]=occ_count
          if(s[k,'EVID']%in%c(1,4) & s[k,'CMT']%in%CMT_vals_dose){
            occ_count=occ_count+1
            s[k,OCC_col]=occ_count
          }
        }
      }
      # now move the OCC values from s back to data
      data[data[,ID_col]==subject[i],OCC_col]=s[,OCC_col]
    } # end of i loop
  }# end of calc_method=='by_dose'
  return(data)
}
##############################################################
#################### END OF calc_occ function #####################
##############################################################
