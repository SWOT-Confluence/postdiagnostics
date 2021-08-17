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
  
  curr_df <- get_flpe_current(reach_id, input_dir)
  prev_df <- get_flpe_prev(reach_id, sos_file)
  sos_df <- get_sos_q(sos_file, reach_id)
  
  return(list(curr=cbind(curr_df, sos_df), prev=cbind(prev_df, sos_df)))
}

#' Get FLPE discharge output from current run
#'
#' @param reach_id float reach identifier
#' @param input_dir string path to input directory (FLPE, SOS, JSON)
#'
#' @return dataframe of current FLPE discharge data
get_flpe_current <- function(reach_id, input_dir) {
  
  # time
  file <- paste0(reach_id, "_SWOT.nc")
  swot <- open.nc(file.path(input_dir, "swot", file, fsep=.Platform$file.sep))
  nt = var.get.nc(swot, "nt")
  close.nc(swot)
  
  # geobam
  file <- paste0(reach_id, "_geobam.nc")
  geobam <- open.nc(file.path(input_dir, "flpe", "geobam", file, fsep=.Platform$file.sep))
  gb_list <- get_gb_q_cur(geobam, "logQ")
  close.nc(geobam)
  
  # # hivdi TODO
  # file <- paste0(reach_id, "_hivdi.nc")
  # hivdi <- open.nc(file.path(input_dir, "flpe", "hivdi", file, fsep=.Platform$file.sep))
  # hv_grp = grp.inq.nc(hivdi, "reach")$self
  # hivdi_q <- temp_hivdi_q(gb_list$qmean, var.get.nc(hv_grp, "Q"))
  # close.nc(hivdi)
  
  # momma
  file <- paste0(reach_id, "_momma.nc")
  momma <- open.nc(file.path(input_dir, "flpe", "momma", file, fsep=.Platform$file.sep))
  momma_q <- var.get.nc(momma, "Q")
  close.nc(momma)
  
  # # sad TODO
  # file <- paste0(reach_id, "_sad.nc")
  # sad <- open.nc(file.path(input_dir, "flpe", "sad", file, fsep=.Platform$file.sep))
  # sad_q <- var.get.nc(sad, "Qa")
  # sad_u <- var.get.nc(sad, "Q_u")
  # close.nc(sad)
  
  # metroman
  file <- list.files(path=file.path(input_dir, "flpe", "metroman", fsep=.Platform$file.sep), 
                     pattern=paste0(".*", reach_id, ".*", "_metroman\\.nc"), 
                     recursive=TRUE, 
                     full.names=TRUE)
  metroman <- open.nc(file)
  reach_ids <- var.get.nc(metroman, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  metroman_q <- var.get.nc(metroman, "allq")[,index]
  metroman_u <- var.get.nc(metroman, "q_u")[,index]
  close.nc(metroman)
  
  # TODO
  # return(data.frame(nt = nt,
  #                   geobam_q = gb_list$qmean,
  #                   geobam_u = gb_list$qsd,
  #                   hivdi_q = hivdi_q,
  #                   momma_q = momma_q,
  #                   sad_q = sad_q,
  #                   sad_u = sad_u,
  #                   metroman_q = metroman_q,
  #                   metroman_u = metroman_u
  # ))
  
  return(data.frame(nt = nt,
                    geobam_q = gb_list$qmean,
                    geobam_u = gb_list$qsd,
                    momma_q = momma_q,
                    metroman_q = metroman_q,
                    metroman_u = metroman_u
  ))
}

#' Get geoBAM discharge posteriors for current run
#'
#' @param ds NetCDF dataset
#' @param name str name of group
#'
#' @return list of lists (mean and standard deviation discharge)
get_gb_q_cur <- function(ds, name) {
  logq_grp = grp.inq.nc(ds, name)$self
  qmean_chains <- cbind(var.get.nc(logq_grp, "mean_chain1"), 
                        var.get.nc(logq_grp, "mean_chain2"), 
                        var.get.nc(logq_grp, "mean_chain3"))
  
  qsd_chains <- cbind(var.get.nc(logq_grp, "sd_chain1"), 
                      var.get.nc(logq_grp, "sd_chain2"), 
                      var.get.nc(logq_grp, "sd_chain3"))
  
  qmean <- exp(rowMeans(qmean_chains, na.rm=TRUE))
  qsd <- exp(rowMeans(qsd_chains, na.rm=TRUE))
  
  qmean[is.nan(qmean)] <- NA
  qsd[is.nan(qsd)] <- NA
  
  return(list(qmean=qmean, qsd=qsd))
}

#' Get FLPE algorithm discharge from previous run
#'
#' @param reach_id integer reach identifier
#' @param sos_file str path to sos file
#'
#' @return dataframe of previous FLPE discharge data
get_flpe_prev <- function(reach_id, sos_file) {
  
  # index
  sos = open.nc(sos_file)
  reach_ids = var.get.nc(sos, "num_reaches")
  index = which(reach_ids==reach_id, arr.ind=TRUE)
  
  # time
  nt = var.get.nc(sos, "time_steps")
  
  # geobam
  gb_list <- get_gb_q_prev(sos, "geobam/logQ", index)
  
  # hivdi TODO
  
  # mommma
  mo_grp <- grp.inq.nc(sos, "momma")$self
  mo_q <- var.get.nc(mo_grp, "Q")[,index]
  
  # sad TODO
  
  # metroman
  mm_grp <- grp.inq.nc(sos, "metroman")$self
  mm_q <- var.get.nc(mm_grp, "allq")[,index]
  mm_u <- var.get.nc(mm_grp, "q_u")[,index]
  
  close.nc(sos)
  # return(data.frame(nt = nt,
  #                   geobam_q = gb_list$qmean,
  #                   geobam_u = gb_list$qsd,
  #                   hivdi_q = hivdi_q,
  #                   momma_q = mo_q,
  #                   sad_q = sad_q,
  #                   sad_u = sad_u,
  #                   metroman_q = mm_q,
  #                   metroman_u = mm_u
  # ))
  return(data.frame(nt = nt,
                    geobam_q = gb_list$qmean,
                    geobam_u = gb_list$qsd,
                    momma_q = mo_q,
                    metroman_q = mm_q,
                    metroman_u = mm_u
  ))
}

#' Get geoBAM discharge posteriors for previous run
#'
#' @param ds NetCDF dataset
#' @param name str name of group
#' @param index integer to index data on
#'
#' @return list of lists (mean and standard deviation discharge)
get_gb_q_prev <- function(ds, name, index) {
  logq_grp = grp.inq.nc(ds, name)$self
  qmean_chains <- cbind(var.get.nc(logq_grp, "mean_chain1")[,index], 
                        var.get.nc(logq_grp, "mean_chain2")[,index], 
                        var.get.nc(logq_grp, "mean_chain3")[,index])
  
  qsd_chains <- cbind(var.get.nc(logq_grp, "sd_chain1")[,index], 
                      var.get.nc(logq_grp, "sd_chain2")[,index], 
                      var.get.nc(logq_grp, "sd_chain3")[,index])
  
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
#'
#' @return named list of discharge priors
get_sos_q <- function(sos_file, reach_id) {
  sos <- open.nc(sos_file)
  r_grp = grp.inq.nc(sos, "reaches")$self
  reach_ids <- var.get.nc(r_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  qmean <- var.get.nc(r_grp, "mean_q")[index]
  qsd <- exp(var.get.nc(r_grp, "logQ_sd")[index])
  qmin <- var.get.nc(r_grp, "min_q")[index]
  qmax <- var.get.nc(r_grp, "max_q")[index]
  close.nc(sos)
  return(data.frame(sos_qmean = qmean,
                    sos_qsd = qsd,
                    sos_qmin = qmin,
                    sos_qmax = qmax
  ))
}

#' Get Integrator discharge data (priors and posteriors)
#' 
#' ## TODO use RNetCDF library
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