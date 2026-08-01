#' Pilot reliability study for device life
#'
#' Simulated dataset containing observations from a fictional 90-day reliability pilot study
#' One hundred customers elect to participate in this study.  Time observations for
#' devices that have not failed by the end of the 90-day pilot are censored.
#' 
#'
#' @format A data frame with 100 rows and 7 variables:
#' \describe{
#'   \item{device_id}{Unique ID for each device}
#'   \item{mfg_location}{Manufacturing location (OR, WA, or CA)}
#'   \item{device_type}{Device SKU (A or B)}
#'   \item{pilot_start}{Start date of pilot study}
#'   \item{status}{Censoring indicator.  1 = no censoring.  0 = right censoring}
#'   \item{end_date}{Date of last observation (either censoring date or failure date)}
#'   \item{ttf}{Time till failure}
#' }
"mfg"


#' Repairability dataset
#'
#' Simulated dataset containing event data for 60 devices.
#'
#' @format A data frame with 239 rows and 5 variables:
#' \describe{
#'   \item{device_id}{Unique ID for each device}
#'   \item{repair_time}{Time at which event occurs (unit=days)}
#'   \item{status}{Censoring indicator.  1 = no censoring.  0 = right censoring.  The time value corresponding to status=0 is the out-of-service time for the device}
#'   \item{device_type}{Device SKU (A or B)}
#'   \item{mfg_location}{Manufacturing location (OR, WA, or CA)}
#' }
"device_repair"