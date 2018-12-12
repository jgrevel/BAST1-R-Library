#' Plots multiple GOF involving DV and CWRES
#'
#' Generates GOF plots involving DV and CWRES for all run folders within a modelling folder.
#'
#' @author Will Haese-Hill
#' @param mod_folder folder path containing the model run folders.
#' @param ind_var name of the independent variable column, usually TIME or TIME_AFTER_DOSE.
#' @param log_ind_var logical. If TRUE, plot x-axis on a log scale.
#' @param all logical. If TRUE, produce GOF plots for each run folder in mod_folder (and not just those where no GOF is present).
#' @return A panel of 8 different diagnostic plots for each run folder in mod_folder.
#' @details Uses BAST function plot_GOF.
#' @note Assumes all model folders are in format: "run---".
#' @export



##################################################################################################
####                                  plot_GOF_multi                                               ####
##################################################################################################
#
# Function that produces GOF plots for all model folders within a given modelling directory.
# Checks for existence of .tab file, and whether this contains CWRES.
# Uses BAST R function: plot_GOF
#
#####################
# Input parameters: #
#####################
#
# mod_folder  = "...\\Modelling\\"      : Folder containing the models of a given study
#
# ind_var     = "TIME"                  : Independent variable to plot against.
#
# log_x_axis  = T/F                     : Would user like to apply log scale to the ind_var axis?
#
# all         = T/F                     : Would user like to plot all GOF plots again (T) or only
#                                         those that are not currently present (F)?
#
#############
# Warnings: #
#############
#             * Assumes model folder is in format: "run---"
#
##################################################################################################


plot_GOF_multi =function(mod_folder,ind_var='TIME',log_ind_var=FALSE,all=FALSE){

  mod_folder = path_correct(path=mod_folder, back=T)

  files=list.files(mod_folder,pattern="run")

  counter=NULL
  for (i in 1:length(files)){
    path=paste0(mod_folder,files[i])
    if (all==FALSE){
      if (length(list.files(path,pattern="_GOF"))>0|(length(list.files(path,pattern=".tab"))==0|length(strsplit(files[i],split="")[[1]])!=6)){
        counter=c(counter,i)
      }
    } else {
      if (length(list.files(path,pattern=".tab"))==0|length(strsplit(files[i],split="")[[1]])!=6) counter=c(counter,i)
    }
  }
  files=files%w/o%files[counter]

  if (length(files)!=0){
    for (i in 1:length(files)){
      path=paste0(mod_folder,files[i])
      data_file_path=paste0(mod_folder,files[i],"\\",files[i],".tab")
      if (length(list.files(path,pattern=".tab"))==1) data_file_path=paste0(path,"\\",list.files(path,pattern=".tab")) # In case .tab file
      data=read.table(data_file_path,header=TRUE,sep="",stringsAsFactors=FALSE,skip=1)                                 #  has erronious name
      if (length(which(is.na(data)))!=0){
        data=na.omit(data)
        print(paste0("NA data in ",files[i]," has been omitted."))
      }
      if (is.null(data$CWRES)==FALSE){
        filepath=paste0(mod_folder,files[i],'\\',files[i])
        plot_GOF(filepath,data,ind_var,log_ind_var)
      } else {
        print(paste0(files[i]," has no CWRES data for GOF! Rerun with CWRES command in $TABLE."))
      }
    }
  } else print("All eligible GOF plots already produced!")
}

##############################################################
################ END OF plot_GOF_multi function ###################
##############################################################
