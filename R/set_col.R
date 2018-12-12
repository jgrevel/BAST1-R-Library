#' For setting colours and associated densities.
#'
#' Specify a colour by name, and alter the density without having to convert to RGB.
#'
#' @author Will Haese-Hill
#' @param col character. Name of colour.
#' @param den numeric. Density range from 0 (transparent) to 255 (opaque).
#' @return A colour hex code, for use in plotting etc.
#' @note col can have length greater than 1, and the function will output a character of the same length.
#' @export
#' @references For a list of named colours recognised by R, see: \url{http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf}.



######################################################################################################
####                                    set_col                                               ####
######################################################################################################
# Function to specify a colour by name, and alter the density without having to convert to RGB.
#
# density =  0 (transparent) - 255 (opaque)
#
# Example of use:
#           col = col_density(col = "lightblue",density = 100)
#
# Tips:
#         * col can have length greater than 1, and the function will output a character of the same length
######################################################################################################

set_col <- function(col,den=255){

  data<-col2rgb(col)
  data<-as.data.frame(data)

  for (i in 1:length(col)){
    rec<-rgb(r=data[1,i],g=data[2,i],b=data[3,i],alpha=den,maxColorValue=255)
    if (i==1){
      output=rec
    } else {
      output=cbind(output,rec)
    }
  }
  output<-as.character(output)
}

##############################################################
################ END OF set_col function #################
##############################################################
