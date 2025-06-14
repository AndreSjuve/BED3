#_______________________________________________________________________________
#' BED3
#' André Wattø Sjuve, andre.sjuve@nhh.no
#' Norwegian School of Economics
#' Created: "05 juni, 2025"

#' Description: This script converts the formula sheet into a pdf-print 
#' ready version and then renders that pdf
#_______________________________________________________________________________

source("R/utils.R")

convert_formula_qmd_for_pdf()
render_formelark_pdf()


#_______________________________________________________________________________
# Copyright Andre W. Sjuve 2025 ----