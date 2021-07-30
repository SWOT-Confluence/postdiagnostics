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
  json_data <- fromJSON(file=file.path(input_dir, reaches_json, fsep=.Platform$file.sep))[[index]]
  return(list(reach_id=json_data$reach_id,
              sos=file.path(input_dir, "sos", json_data$sos, fsep=.Platform$file.sep)
  ))
}

#' Get integrator file for basin
#' 
#' The reach identifier is determined from the index number which is used to 
#' select an element from the JSON list.  
#'
#' @param basin_json string path to JSON file with basin data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach files associate with reach identifier
get_integrator_file <- function(basin_json, input_dir, index) {
  json_data <- fromJSON(file=file.path(input_dir, basin_json, fsep=.Platform$file.sep))[[index]]
  return(file.path(input_dir, "integrator", paste0(json_data$basin_id, "_integrator.nc"), fsep=.Platform$file.sep))
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
  geobam <- nc_open(file.path(input_dir, "flpe", "geobam", file, fsep=.Platform$file.sep))
  gb_list <- get_gb_q(geobam)
  nc_close(geobam)
  
  # hivdi
  file <- paste0(reach_id, "_hivdi.nc")
  hivdi <- nc_open(file.path(input_dir, "flpe", "hivdi", file, fsep=.Platform$file.sep))
  hivdi_q <- temp_hivdi_q(gb_list$qmean, ncvar_get(hivdi, "reach/Q"))
  nc_close(hivdi)
  
  # momma
  file <- paste0(reach_id, "_momma.nc")
  momma <- nc_open(file.path(input_dir, "flpe", "momma", file, fsep=.Platform$file.sep))
  momma_q <- ncvar_get(momma, "Q")
  nc_close(momma)
  
  # sad
  file <- paste0(reach_id, "_sad.nc")
  sad <- nc_open(file.path(input_dir, "flpe", "sad", file, fsep=.Platform$file.sep))
  sad_q <- ncvar_get(sad, "Qa")
  sad_u <- ncvar_get(sad, "Q_u")
  nc_close(sad)
  
  # metroman
  file <- list.files(path=file.path(input_dir, "flpe", "metroman", fsep=.Platform$file.sep), 
                         pattern=paste0(".*", reach_id, ".*", "_metroman\\.nc"), 
                         recursive=TRUE, 
                         full.names=TRUE)
  metroman <- nc_open(file)
  reach_ids <- ncvar_get(metroman, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  metroman_q <- ncvar_get(metroman, "allq")[,index]
  metroman_u <- ncvar_get(metroman, "q_u")[1,index]
  nc_close(metroman)
  
  return(data.frame(geobam_q = gb_list$qmean,
                    geobam_u = gb_list$qsd,
                    hivdi_q = hivdi_q,
                    momma_q = momma_q,
                    sad_q = sad_q,
                    sad_u = sad_u,
                    metroman_q = metroman_q,
                    metroman_u = metroman_u
  ))
}

#' Get geoBAM discharge posteriors
#'
#' @param geobam ncdf4 dataset of geobam posteriors
#'
#' @return list of lists (mean and standard deviation discharge)
get_gb_q <- function(geobam) {
  qmean_chains <- cbind(ncvar_get(geobam, "logQ/mean_chain1"), 
                        ncvar_get(geobam, "logQ/mean_chain2"), 
                        ncvar_get(geobam, "logQ/mean_chain3"))
  
  qsd_chains <- cbind(ncvar_get(geobam, "logQ/sd_chain1"), 
                      ncvar_get(geobam, "logQ/sd_chain2"), 
                      ncvar_get(geobam, "logQ/sd_chain3"))
  
  qmean <- exp(rowMeans(qmean_chains, na.rm=TRUE))
  qsd <- exp(rowMeans(qsd_chains, na.rm=TRUE))
  
  qmean[is.nan(qmean)] <- NA
  qsd[is.nan(qsd)] <- NA
  
  return(list(qmean=qmean, qsd=qsd))
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
  qmean <- ncvar_get(sos, "reaches/mean_q")[index]
  qsd <- exp(ncvar_get(sos, "reaches/logQ_sd")[index])
  qmin <- ncvar_get(sos, "reaches/min_q")[index]
  qmax <- ncvar_get(sos, "reaches/max_q")[index]
  nc_close(sos)
  return(data.frame(sos_qmean = qmean,
                    sos_qsd = qsd,
                    sos_qmin = qmin,
                    sos_qmax = qmax
  ))
}

#' Get Integrator discharge data (priors and posteriors)
#' 
#' @param integrator_file path to integrator data file
#' 
#' @return ?
get_data_integrator <- function(integrator_file) {
  integrator <- nc_open(integrator_file)
  reach_ids <- ncvar_get(integrator, "reach_id")
  qmean <- ncvar_get(integrator, "Qmean")
  geobam <- ncvar_get(integrator, "geobam")
  hivdi <- ncvar_get(integrator, "hivdi")
  metroman <- ncvar_get(integrator, "metroman")
  momma <- ncvar_get(integrator, "momma")
  sad <- ncvar_get(integrator, "sad")
  nc_close(integrator)
  return(data.frame(reach_ids = reach_ids,
                    qmean = qmean,
                    geobam = geobam,
                    hivdi = hivdi,
                    metroman = metroman,
                    momma = momma,
                    sad = sad
  ))
}

#' Insert invalid time steps into HiVDI discharge data
#'
#' @param gb_q vector of geoBAM discharge with NA
#' @param hv_q vector of HiVDI discharge with no NA
#'
#' @return
temp_hivdi_q <- function(gb_q, hv_q) {
  
  indexes <- which(is.na(gb_q))
  for (index in indexes) {
    hv_q <- append(hv_q, NA, after=index-1)
  }
  return(hv_q)
}