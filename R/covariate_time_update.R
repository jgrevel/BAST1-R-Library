#' Adds column to a NONMEM data file containing values of a time-varying covariate.
#'
#' @author Katarzyna Nurzynska
#' @param data_cov data frame containing information about time-varying covariate.
#' @param data_dose data frame containing information about when the dose was administered.
#' @param data_final data frame to which column with time-varying covariate should be added.
#' @param cov_time column name in data_cov containing information about the time when the covariate was measured.
#' @param dose_time column name in data_dose containing information about the time when the dose was administered.
#' @param ID_col name of the ID column in data_final.
#' @param cov_name name of the time-varying covariate.
#' @param cov_value name that describes the value (e.g. "Concentration") of the time-varying covariate.
#' @param cov_column column name in data_cov containing information about the name of the covariate to be selected.
#'        e.g. weight, albumin, bilirubin etc.
#' @param date_format_dose logical. change date format from '\%Y-\%m-\%dT\%H:\%M' to '\%Y-\%m-\%d \%H:\%M'(if TRUE).
#' @param date_format_cov logical. change date format from '\%Y-\%m-\%dT\%H:\%M' to '\%Y-\%m-\%d \%H:\%M'(if TRUE).
#' @param time_zone name of required timezone. Daylight saving change is applied when time difference is calculated.
#' @return The input data frame with new column added containing values of time-varying covariate.
#' @note The data file containing information about the dose records should be filtered to include only records of interest e.g. related
#'        to a specific drug (e.g. BAY and not co-medication).
#' @note Time difference between times when the dose was administered and the covariate was measured is calculated in hours.
#' @note The ID columns in the original data files containing information about covariate and dose should be called SUBJIDN.
#' @note The time column in data_final (containing dosing and concentration times) should be called TIME.
#' @note The format of the dates in the cov_time and dose_time columns should be '\%Y-\%m-\%dT\%H:\%M' or
#'        '\%Y-\%m-\%d \%H:\%M' (it is assumed that clock time is present).
#' @export



########################################################################################################
#Date: 29/02/16
#Author: Katarzyna Nurzynska
#Project: Copanlicib, Pi3K-front BAY80-6946, popPK
#Purpose: Add a column to a data file containing values of time-varying covariate
#Lab journal: KN-04 p.67
#Arguments: data_cov - is the data file containing information about time-varying covariate
#           data_dose - is the data file containing information when the dose was administarted
#           data_final - is the final data file to which column with time-varying covariate should be added
#           cov_time - column in the data_cov containing information about the time when the covariate was measured
#           dose_time - column in the data_dose containing information about the time when the dose was administarted
#           ID_col - the name of the ID column in the data_final
#           cov_name - the name of the time-varying covariate
#           cov_value - value (e.g.concentration) of time-varying covariate
#           cov_column - column in the data_cov containing information about the name of the covariate to be selected e.g. weight, albumin, bilirubin etc.
#           date_format_dose / data_format_cov - change date format from '%Y-%m-%dT%H:%M' to '%Y-%m-%d %H:%M'(if TRUE)
#           time_zone - required timezone; daylight saving change is applied when time difference is calculated
#Notes: The data file (ex) containing information about the dose records should be filtered to include only records of interest e.g. related to a specific drug (e.g. BAY and not co-medication)
#       Time difference between times when the dose was administrated and the covariate was measured is calculated in hours
#       The ID columns in the original data files containing information about covariate and dose should be called SUBJIDN
#       The time column in the data_final (containing dosing and concentration times) should be called TIME
#       The format of the dates in cov_time and dose_time columns should be '%Y-%m-%dT%H:%M' or '%Y-%m-%d %H:%M' (it is assumed that clock time is present)
##Defalt arguments: add column containing information about albumin
########################################################################################################

covariate_time_update = function(data_cov, data_dose, data_final, cov_time="LBDTC", dose_time='EXSTDTC', ID_col='SID', cov_column='LBTESTCD', cov_name='ALB', cov_value="LBSTRESN", date_format_cov=FALSE, date_format_dose=TRUE, time_zone='US/Eastern'){

  #Add an empty column containing information about the covariate to the final data file (data_final)
  data_final [,cov_name]=9999
  data_cov<-data_cov[which(data_cov[,cov_column]==cov_name),] #From data_cov select only rows which contain info about the covariate of interest e.g. LBTESTCD -> ALBU

  #Change the format of dates in the data_cov
  if(date_format_cov!=FALSE){
    data_cov[,cov_time]=as.character(as.POSIXct(data_cov[,cov_time], format = '%Y-%m-%dT%H:%M', tz=time_zone), format = '%Y-%m-%d %H:%M', tz=time_zone)
  }

  #Create a data file containing dates when the first dose was administarted for each subject (to take into account multiple dose studies)
  data_dose=data_dose[!duplicated(data_dose$SUBJIDN),]

  #Change the format of dates in the data_dose
  if(date_format_dose!=FALSE){
    data_dose[,dose_time]=as.character(as.POSIXct(data_dose[,dose_time], format = '%Y-%m-%dT%H:%M', tz=time_zone), format = '%Y-%m-%d %H:%M', tz=time_zone)
  }

  #ACalculate time after forst dose for the covariate records
  data_cov[,"TIME"]=9999
  subject_cov=unique(data_cov$SUBJIDN)
  for (i in 1:length(subject_cov)){
    subjectunique_cov=data_cov[data_cov$SUBJIDN==subject_cov[i],]
    for (k in 1:nrow(subjectunique_cov)){
      TAFD<- difftime(subjectunique_cov[k,cov_time], data_dose[i,dose_time], units="hours")
      subjectunique_cov [k,"TIME"] <- TAFD
    }
    data_cov[data_cov$SUBJIDN==subject_cov[i],]=subjectunique_cov
  }

  #Update the covariate records with time
  subject=unique(data_final[,ID_col])
  for(i in 1:length(subject)){
    subjectunique=data_final[data_final[,ID_col]==subject[i],]
    subjectunique_cov=data_cov[data_cov[,"SUBJIDN"]==subject[i],]
    baseline=subjectunique_cov[subjectunique_cov[,"TIME"]<=0,] #select all the records when TIME is negative (baseline)
    subjectunique[,cov_name]=mean(as.numeric(as.character(baseline[,cov_value]))) #calculate avearge of all the values of the covariate before the administartion of the first dose (baseline value)
    for (k in 1:nrow(subjectunique_cov)) {
      for (n in 1:nrow (subjectunique)){
        if (subjectunique [n,"TIME"]>=subjectunique_cov [k, "TIME"]){
          subjectunique [n:nrow (subjectunique),cov_name]= as.numeric(as.character(subjectunique_cov [k, cov_value]))
        }
      }
    }
    data_final[data_final[,ID_col]==subject[i],]=subjectunique
  }

  return(data_final)
}
