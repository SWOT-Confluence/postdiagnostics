library(ncdf4)
library(plyr)
library(rjson)

#' Gets reference to SoS file
#' 
#' The reach identifier is determined from the index number which is used to 
#' select an element from the JSON list. 
#'
#' @param reaches_json string path to JSON file with reach data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach id and associated SoS file
get_sos_file_flpe <- function(reaches_json, input_dir, index) {
  json_data <- fromJSON(file=paste(input_dir, reaches_json, sep='/'))[[index]]
  return(list(reach_id=json_data$reach_id,
              sos=paste(input_dir, "sos", json_data$sos, sep='/')
  ))
}

#' Get files associated with a reach identifier - not complete
#' 
#' The reach identifier is determined from the index number which is used to 
#' select an element from the JSON list.  
#'
#' @param reaches_json string path to JSON file with reach data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach files associate with reach identifier
get_reach_files_integrator <- function(reaches_json, input_dir, index) {
  ## TODO implement: Not sure of integrator output.
}

#' Get FLPE discharge data (priors and posteriors)
#'
#' Retrieve and return SoS priors and FLPE discharge data.
#'
#' @param sos_file string path to SoS file
#' @param reach_id float reach identifier
#' @param input_dir string path to input data (FLPE, SOS, JSON)
#'
#' @return named list with FLPE and SoS data
get_data_flpe <- function(sos_file, reach_id, input_dir) {
  flpe_df <- get_flpe_q(reach_id, input_dir)
  nt <- dim(flpe_df)[1] - 1
  sos_df <- get_sos_q(sos_file, reach_id, nt)
  return(cbind(flpe_df, sos_df))
}

#' Get FLPE discharge output
#'
#' @param reach_id float reach identifier
#' @param input_dir string path to input directory (FLPE, SOS, JSON)
#'
#' @return dataframe of FLPE discharge data
get_flpe_q <- function(reach_id, input_dir) {
  # geobam
  file <- paste0(reach_id, "_geobam.nc")
  geobam <- nc_open(paste(input_dir, "flpe", "geobam", file, sep='/'))
  qmean_chain1 <- ncvar_get(geobam, "logQ/mean_chain1")
  qmean_chain2 <- ncvar_get(geobam, "logQ/mean_chain2")
  qmean_chain3 <- ncvar_get(geobam, "logQ/mean_chain3")
  qsd_chain1 <- ncvar_get(geobam, "logQ/sd_chain1")
  qsd_chain2 <- ncvar_get(geobam, "logQ/sd_chain2")
  qsd_chain3 <- ncvar_get(geobam, "logQ/sd_chain3")
  nc_close(geobam)
  
  # hivdi
  file <- paste0(reach_id, "_hivdi.nc")
  hivdi <- nc_open(paste(input_dir, "flpe", "hivdi", file, sep='/'))
  hivdi_q <- ncvar_get(hivdi, "reach/Q")
  nc_close(hivdi)
  
  # momma
  file <- paste0(reach_id, "_momma.nc")
  momma <- nc_open(paste(input_dir, "flpe", "momma", file, sep='/'))
  momma_q <- ncvar_get(momma, "Q")
  nc_close(momma)
  
  # sad
  file <- paste0(reach_id, "_sad.nc")
  sad <- nc_open(paste(input_dir, "flpe", "sad", file, sep='/'))
  sad_q <- ncvar_get(sad, "Qa")
  nc_close(sad)
  
  # metroman
  file <- list.files(path=paste(input_dir, "flpe", "metroman", sep='/'), 
                         pattern=paste0(".*", reach_id, ".*", "_metroman\\.nc"), 
                         recursive=TRUE, 
                         full.names=TRUE)
  metroman <- nc_open(file)
  reach_ids <- ncvar_get(metroman, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  metroman_q <- ncvar_get(metroman, "x1hat")[index]
  nc_close(metroman)
  
  return(data.frame(geobam_qmean_chain1 = qmean_chain1,
                    geobam_qmean_chain2 = qmean_chain2,
                    geobam_qmean_chain3 = qmean_chain3,
                    geobam_qsd_chain1 = qsd_chain1,
                    geobam_qsd_chain2 = qsd_chain2,
                    geobam_qsd_chain3 = qsd_chain3,
                    hivdi = hivdi_q,
                    momma = momma_q,
                    sad = sad_q,
                    metroman = c(metroman_q, rep(NA, length(sad_q) - 1))
  ))
}

#' Get SOS discharge priors
#'
#' @param sos_file string path to SoS file
#' @param reach_id float reach identifier
#' @param nt integer number of time steps
#'
#' @return named list of discharge priors
get_sos_q <- function(sos_file, reach_id, nt) {
  sos <- nc_open(sos_file)
  reach_ids <- ncvar_get(sos, "reaches/reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  qmean <- ncvar_get(sos, "reaches/q_mean")[index]
  qsd <- ncvar_get(sos, "reaches/q_sd")[index]
  qmin <- ncvar_get(sos, "reaches/q_min")[index]
  qmax <- ncvar_get(sos, "reaches/q_max")[index]
  return(data.frame(sos_qmean = c(qmean, rep(NA, nt)),
                    sos_qsd = c(qsd, rep(NA, nt)),
                    sos_qmin = c(qmin, rep(NA, nt)),
                    sos_qmax = c(qmax, rep(NA, nt))
  ))
}

#' Get Integrator discharge data (priors and posteriors) - not complete
#' 
#' @return ?
get_data_integrator <- function() {
  ## TODO implement
}