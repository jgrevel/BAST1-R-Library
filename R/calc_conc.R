#' Calculates the molar concentration for a given compound's SI concentration.
#'
#' @author Will Haese-Hill
#' @param conc numeric. Observed SI concentration of compound.
#' @param MW numeric. Molecular weight of compound ( D (g/mol)).
#' @param SIu character. SI Units of observed concentration: 'm/V'. Options restricted to:
#' \itemize{
#'  \item m = g , mg, ug, ng, pg
#'  \item V = L , mL, uL, nL, pL
#' }
#' @param Mu character. Desired molar units of concentration: 'M', 'mM', 'uM', 'nM', 'pM'
#' @details Utilises the formula \deqn{C = ( m / V )( 1 / MW ),}where \eqn{C} is concentration in desired molar units.
#'
#' Tips:
#' \itemize{
#'  \item If SI units given for \eqn{Mu}, and \eqn{MW = 1}, function will perform a simple SI unit conversion.
#'  \item Set \eqn{conc = 1} (default) if you simply want to return the unit conversion factor.
#' }
#' @return numeric. The molar concentration of the compound, with unit \eqn{Mu}.
# @seealso \code{\link{nchar}} which this function wraps
#' @export
#' @examples
#' \donttest{
#' Observed SI concentration of compound 1000 ug/L.
#' Compound has molecular weight 494.56 D (g/mol).
#' Desired units for molar concentration: nM.
#'
#' new_conc <- calc_conc(conc = 1000, MW = 494.56, SIu = 'ug/L', Mu = 'nM')
#'
#' new_conc = 2022, so 1000 ug/L of this particular compound is equal to ~2022 nM.
#' }



######################################################################################################
####                                      calc_conc                                              ####
######################################################################################################
# Function which calculates the molar concentration for a given compound's SI concentration.
#
#
# Inputs:     conc  - Observed SI concentration of compound.
#             MW    - Molecular weight of compound ( D (g/mol))
#             SIu   - SI Units of observed concentration: 'm/V'
#                                                  where: m = g , mg, ug, ng, pg
#                                                         V = L , mL, uL, nL, pL
#             Mu    - Desired molar units of concentration: 'M', 'mM', 'uM', 'nM', 'pM'
#
# Formula:    C = ( m / v ) * ( 1 / MW )
#             where C is concentration in desired molar units.
################################################################

calc_conc <- function(conc=1,MW,SIu,Mu){

  rec<-strsplit(SIu,split='/',fixed=T)

  mass<-data.frame(units=c("g","mg","ug","ng","pg"),scalar=c(10**0,10**-3,10**-6,10**-9,10**-12))
  volume<-data.frame(units=c("L","mL","uL","nL","pL"),scalar=c(10**0,10**-3,10**-6,10**-9,10**-12))
  molar<-data.frame(units=c("M","mM","uM","nM","pM"),scalar=c(10**0,10**-3,10**-6,10**-9,10**-12))


  Mconc<-molar$scalar[match(Mu,molar$units)]
  if (is.na(Mconc)==T){
    rec1<-strsplit(Mu,split='/',fixed=T)
    Mconc<-mass$scalar[match(rec1[[1]][1],mass$units)]/volume$scalar[match(rec1[[1]][2],volume$units)]
    if (is.na(Mconc)==T){
      stop("Unrecognised output units")
    } else {
      print("Converted to SI units. Ensure MW = 1")
    }
  }

  SIconc<-mass$scalar[match(rec[[1]][1],mass$units)]/volume$scalar[match(rec[[1]][2],volume$units)]

  conc<-conc*(1/MW)*(SIconc/Mconc)

  return(conc)

}

##############################################################
################ END OF Molar_Calc function ##################
##############################################################
