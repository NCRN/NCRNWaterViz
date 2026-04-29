#' Retrieve threshold description text for a characteristic
#'
#' @description
#' Retrieves the lower and upper threshold description text for a given
#' characteristic at a specified park and site. This function is a lightweight
#' wrapper around `getCharInfo()` that extracts the
#' `"LowerDescription"` and `"UpperDescription"` fields and returns only the
#' non-missing values.
#'
#' Threshold descriptions typically provide natural-language explanations of
#' management or assessment thresholds (e.g., narrative criteria, warning
#' text, or context for interpreting results).
#'
#' @param object An NCRNWater network or site object that can be passed to
#'   `getCharInfo()`.
#' @param parkcode Character scalar. Park code used by NCRNWater (e.g., `"ANTI"`,
#'   `"MONO"`).
#' @param sitecode Character scalar. Site code for the monitoring location
#'   (e.g., `"NCRN_ANTI_SHCK"`).
#' @param charname Character scalar. The characteristic name (e.g.,
#'   `"Specific conductance"`, `"pH"`) whose threshold descriptions you want.
#'
#' @details
#' The function calls:
#'
#' - `getCharInfo(..., info = "LowerDescription")`
#' - `getCharInfo(..., info = "UpperDescription")`
#'
#' It combines the results into a character vector and removes any `NA` values
#' so that only the available threshold descriptions are returned.
#'
#' If both descriptions are missing, the function returns an empty character
#' vector (`character(0)`).
#'
#' @return
#' A character vector of length 1 or 2 containing the available threshold
#' description text for the characteristic. Missing values are excluded.
#'
#' @examples
#' \dontrun{
#' # Retrieve threshold descriptions for a characteristic:
#' getThresholdText(
#'   object = ncrn_obj,
#'   parkcode = "ANTI",
#'   sitecode = "NCRN_ANTI_SHCK",
#'   charname = "Specific conductance"
#' )
#' }
#'
#' @seealso
#' \code{\link{getCharInfo}} for retrieving characteristic metadata.
#'
#' @author

getThresholdText<-function(object, parkcode,sitecode,charname){    
  x<-c(getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="LowerDescription"),
       getCharInfo(object, parkcode=parkcode, sitecode=sitecode, charname=charname, info="UpperDescription"))
  return(x[!is.na(x)])
}