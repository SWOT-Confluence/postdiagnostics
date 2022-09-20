#' Gets reference to reach identifier and SoS file
#' 
#' The reach identifier is determined from the index number which is used to 
#' select an element from the JSON list. 
#'
#' @param reaches_json string path to JSON file with reach data
#' @param input_dir string path to input directory
#' @param index integer index for JSON file
#'
#' @return named list of reach id and associated SoS file
get_input_data <- function(reaches_json, input_dir, index) {
  json_data <- fromJSON(file=file.path(input_dir, reaches_json, fsep=.Platform$file.sep))[[index]]
  return(list(reach_id=json_data$reach_id,
              sos=file.path(input_dir, "sos", json_data$sos, fsep=.Platform$file.sep)
  ))
}

#' Get FLPE discharge data (priors and posteriors)
#'
#' Retrieve and return SoS priors and FLPE discharge data.
#'
#' @param sos_file string path to SoS file
#' @param reach_id float reach identifier
#' @param input_dir string path to input data (FLPE, SOS, JSON)
#' @param flpe_dir string path to directory that contains reach-level flpe data
#'
#' @return named list of current Q dataframe and previous Q dataframe
get_data_flpe <- function(sos_file, reach_id, input_dir, flpe_dir) {
  
  curr_df <- get_flpe_current(reach_id, input_dir, flpe_dir)
  prev_df <- get_flpe_prev(reach_id, sos_file)
  sos_df <- get_sos_q(sos_file, reach_id)
  
  return(list(curr=cbind(curr_df, sos_df), prev=cbind(prev_df, sos_df)))
}

#' Get FLPE discharge output from current run
#'
#' @param reach_id float reach identifier
#' @param input_dir string path to input directory (FLPE, SOS, JSON)
#' @param flpe_dir string path to directory that contains reach-level flpe data
#'
#' @return dataframe of current FLPE discharge data
get_flpe_current <- function(reach_id, input_dir, flpe_dir) {
  
  # time
  file <- paste0(reach_id, "_SWOT.nc")
  swot <- open.nc(file.path(input_dir, "swot", file, fsep=.Platform$file.sep))
  nt = var.get.nc(swot, "nt")
  close.nc(swot)
  
  # geobam
  file <- paste0(reach_id, "_geobam.nc")
  geobam <- open.nc(file.path(flpe_dir, "geobam", file, fsep=.Platform$file.sep))
  gb_list <- get_gb_q_cur(geobam, "logQ")
  close.nc(geobam)
  
  # hivdi
  file <- paste0(reach_id, "_hivdi.nc")
  hivdi <- open.nc(file.path(flpe_dir, "hivdi", file, fsep=.Platform$file.sep))
  hv_grp <- grp.inq.nc(hivdi, "reach")$self
  hivdi_q <- var.get.nc(hv_grp, "Q")
  close.nc(hivdi)
  
  # momma
  file <- paste0(reach_id, "_momma.nc")
  momma <- open.nc(file.path(flpe_dir, "momma", file, fsep=.Platform$file.sep))
  momma_q <- var.get.nc(momma, "Q")
  close.nc(momma)
  
  # sad 
  file <- paste0(reach_id, "_sad.nc")
  sad <- open.nc(file.path(flpe_dir, "sad", file, fsep=.Platform$file.sep))
  sad_q <- var.get.nc(sad, "Qa")
  sad_u <- var.get.nc(sad, "Q_u")
  close.nc(sad)
  
  # sic4dvar
  file <- paste0(reach_id, "_sic4dvar.nc")
  sv <- open.nc(file.path(flpe_dir, "sic4dvar", file, fsep=.Platform$file.sep))
  sv_q5 <- var.get.nc(sv, "Qalgo5")
  sv_q31 <- var.get.nc(sv, "Qalgo31")
  close.nc(sv)
  
  # metroman
  file <- list.files(path=file.path(flpe_dir, "metroman", fsep=.Platform$file.sep), 
                     pattern=paste0(".*", reach_id, ".*", "_metroman\\.nc"), 
                     recursive=TRUE, 
                     full.names=TRUE)
  metroman <- open.nc(file)
  reach_ids <- var.get.nc(metroman, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  metroman_q <- var.get.nc(metroman, "allq")[,index]
  metroman_u <- var.get.nc(metroman, "q_u")[,index]
  close.nc(metroman)
  
  return(data.frame(date = nt,
                    geobam_q = gb_list$qmean,
                    geobam_u = gb_list$qsd,
                    hivdi_q = hivdi_q,
                    momma_q = momma_q,
                    sad_q = sad_q,
                    sad_u = sad_u,
                    sic4dvar5_q = sv_q5,
                    sic4dvar31_q = sv_q31,
                    metroman_q = metroman_q,
                    metroman_u = metroman_u
  ))
  
  # return(data.frame(date = nt,
  #                   geobam_q = gb_list$qmean,
  #                   geobam_u = gb_list$qsd,
  #                   hivdi_q = hivdi_q,
  #                   momma_q = momma_q,
  #                   metroman_q = metroman_q,
  #                   metroman_u = metroman_u
  # ))
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
  r_grp = grp.inq.nc(sos, "reaches")$self
  reach_ids = var.get.nc(r_grp, "reach_id")
  index = which(reach_ids==reach_id, arr.ind=TRUE)
  
  # time
  nt = var.get.nc(sos, "time")
  
  # geobam
  gb_list <- get_gb_q_prev(sos, "geobam/logQ", index)
  
  # hivdi
  hv_grp <- grp.inq.nc(sos, "hivdi")$self
  hv_q <- var.get.nc(hv_grp, "Q")[,index]
  
  # mommma
  mo_grp <- grp.inq.nc(sos, "momma")$self
  mo_q <- var.get.nc(mo_grp, "Q")[,index]
  
  # sad
  sd_grp <- grp.inq.nc(sos, "sad")$self
  sd_q <- var.get.nc(sd_grp, "Qa")[,index]
  sd_u <- var.get.nc(sd_grp, "Q_u")[,index]
  
  # sic4dvar
  sv_grp <- grp.inq.nc(sos, "sic4dvar")$self
  sv5_q <- var.get.nc(sv_grp, "Qalgo5")[,index]
  sv31_q <- var.get.nc(sv_grp, "Qalgo31")[,index]
  
  # metroman
  mm_grp <- grp.inq.nc(sos, "metroman")$self
  mm_q <- var.get.nc(mm_grp, "allq")[,index]
  mm_u <- var.get.nc(mm_grp, "q_u")[,index]
  
  close.nc(sos)
  return(data.frame(date = nt,
                    geobam_q = gb_list$qmean,
                    geobam_u = gb_list$qsd,
                    hivdi_q = hv_q,
                    momma_q = mo_q,
                    sad_q = sd_q,
                    sad_u = sd_u,
                    sic4dvar5_q = sv5_q,
                    sic4dvar31_q = sv31_q,
                    metroman_q = mm_q,
                    metroman_u = mm_u
  ))
  # return(data.frame(date = nt,
  #                   geobam_q = gb_list$qmean,
  #                   geobam_u = gb_list$qsd,
  #                   hivdi_q = hv_q,
  #                   momma_q = mo_q,
  #                   metroman_q = mm_q,
  #                   metroman_u = mm_u
  # ))
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
  r_grp <- grp.inq.nc(sos, "reaches")$self
  reach_ids <- var.get.nc(r_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  model_grp = grp.inq.nc(sos, "model")$self
  qmean <- var.get.nc(model_grp, "mean_q")[index]
  qmin <- var.get.nc(model_grp, "min_q")[index]
  qmax <- var.get.nc(model_grp, "max_q")[index]
  gb_grp <- grp.inq.nc(sos, "gbpriors")$self
  qsd <- exp(var.get.nc(gb_grp, "logQ_sd")[index])
  close.nc(sos)
  return(data.frame(sos_qmean = qmean,
                    sos_qsd = qsd,
                    sos_qmin = qmin,
                    sos_qmax = qmax
  ))
}

#' Get MOI discharge data (priors and posteriors)
#' 
#' @param sos_file string path to SoS file
#' @param reach_id float reach identifier
#' @param input_dir string path to input data (FLPE, SOS, JSON)
#' @param moi_dir string path to directory that contains basin-level moi data
#' 
#' @return named list of current moi dataframe and previous moi dataframe
get_data_moi <- function(sos_file, reach_id, input_dir, moi_dir) {
  
  curr_df <- get_moi_current(reach_id, input_dir, moi_dir)
  prev_df <- get_moi_prev(reach_id, sos_file)
  sos_df <- get_sos_q(sos_file, reach_id)
  sos_df <- subset(sos_df, select=-c(sos_qmean, sos_qsd))
  
  return(list(curr=cbind(curr_df, sos_df, row.names=NULL), prev=cbind(prev_df, sos_df, row.names=NULL)))
}

#' Get current MOI results
#'
#' @param reach_id float reach identifier
#' @param input_dir string path to input directory (FLPE, SOS, JSON)
#' @param moi_dir string path to directory that contains basin-level moi data
#'
#' @return dataframe of current MOI data
get_moi_current <- function(reach_id, input_dir, moi_dir) {
  
  # time
  file <- paste0(reach_id, "_SWOT.nc")
  swot <- open.nc(file.path(input_dir, "swot", file, fsep=.Platform$file.sep))
  nt = var.get.nc(swot, "nt")
  close.nc(swot)
  
  # integrator file
  file <- paste0(reach_id, "_integrator.nc")
  moi <- open.nc(file.path(moi_dir, file, fsep=.Platform$file.sep))
  
  # geobam
  gb_grp <- grp.inq.nc(moi, "geobam")$self
  gb_q <- var.get.nc(gb_grp, "q")
  gb_qmean_b <- var.get.nc(gb_grp, "qbar_reachScale")
  gb_qmean_a <- var.get.nc(gb_grp, "qbar_basinScale")
  
  # hivdi
  hv_grp <- grp.inq.nc(moi, "hivdi")$self
  hv_q <- var.get.nc(hv_grp, "q")
  hv_qmean_b <- var.get.nc(hv_grp, "qbar_reachScale")
  hv_qmean_a <- var.get.nc(hv_grp, "qbar_basinScale")
  
  # mommma
  mo_grp <- grp.inq.nc(moi, "momma")$self
  mo_q <- var.get.nc(mo_grp, "q")
  mo_qmean_b <- var.get.nc(mo_grp, "qbar_reachScale")
  mo_qmean_a <- var.get.nc(mo_grp, "qbar_basinScale")
  
  # sad
  sd_grp <- grp.inq.nc(moi, "sad")$self
  sd_q <- var.get.nc(sd_grp, "q")
  sd_qmean_b <- var.get.nc(sd_grp, "qbar_reachScale")
  sd_qmean_a <- var.get.nc(sd_grp, "qbar_basinScale")

  # sic4dvar
  sv_grp <- grp.inq.nc(moi, "sic4dvar")$self
  sv_q <- var.get.nc(sv_grp, "q")
  sv_qmean_b <- var.get.nc(sv_grp, "qbar_reachScale")
  sv_qmean_a <- var.get.nc(sv_grp, "qbar_basinScale")
  
  # metroman
  mm_grp <- grp.inq.nc(moi, "metroman")$self
  mm_q <- var.get.nc(mm_grp, "q")
  mm_qmean_b <- var.get.nc(mm_grp, "qbar_reachScale")
  mm_qmean_a <- var.get.nc(mm_grp, "qbar_basinScale")
  
  close.nc(moi)
  return(data.frame(date = nt,
                    geobam_q = gb_q,
                    gb_qmean_b = gb_qmean_b,
                    gb_qmean_a = gb_qmean_a,
                    hivdi_q = hv_q,
                    hv_qmean_b = hv_qmean_b,
                    hv_qmean_a = hv_qmean_a,
                    momma_q = mo_q,
                    mo_qmean_b = mo_qmean_b,
                    mo_qmean_a = mo_qmean_a,
                    metroman_q = mm_q,
                    mm_qmean_b = mm_qmean_b,
                    mm_qmean_a = mm_qmean_a,
                    sad_q = sd_q,
                    sd_qmean_b = sd_qmean_b,
                    sd_qmean_a = sd_qmean_a,
                    sic4dvar_q = sd_q,
                    sv_qmean_b = sv_qmean_b,
                    sv_qmean_a = sv_qmean_a
  ))
  # return(data.frame(date = nt,
  #                   geobam_q = gb_q,
  #                   gb_qmean_b = gb_qmean_b,
  #                   gb_qmean_a = gb_qmean_a,
  #                   hivdi_q = hv_q,
  #                   hv_qmean_b = hv_qmean_b,
  #                   hv_qmean_a = hv_qmean_a,
  #                   momma_q = mo_q,
  #                   mo_qmean_b = mo_qmean_b,
  #                   mo_qmean_a = mo_qmean_a,
  #                   metroman_q = mm_q,
  #                   mm_qmean_b = mm_qmean_b,
  #                   mm_qmean_a = mm_qmean_a
  # ))
  
}

#' Return previous MOI results
#'
#' @param reach_id integer reach identifier
#' @param sos_file str path to sos file
#'
#' @return dataframe of previous MOI data
get_moi_prev <- function(reach_id, sos_file) {
  # index
  sos = open.nc(sos_file)
  reach_grp = grp.inq.nc(sos, "reaches")$self
  reach_ids = var.get.nc(reach_grp, "reach_id")
  index = which(reach_ids==reach_id, arr.ind=TRUE)
  
  # time
  nt = var.get.nc(sos, "time")
  
  # geobam
  gb_grp <- grp.inq.nc(sos, "moi/geobam")$self
  gb_q <- var.get.nc(gb_grp, "q")[,index]
  gb_qmean_b <- var.get.nc(gb_grp, "qbar_reachScale")[index]
  gb_qmean_a <- var.get.nc(gb_grp, "qbar_basinScale")[index]
  
  # hivdi
  hv_grp <- grp.inq.nc(sos, "moi/hivdi")$self
  hv_q <- var.get.nc(hv_grp, "q")[,index]
  hv_qmean_b <- var.get.nc(hv_grp, "qbar_reachScale")[index]
  hv_qmean_a <- var.get.nc(hv_grp, "qbar_basinScale")[index]
  
  # mommma
  mo_grp <- grp.inq.nc(sos, "moi/momma")$self
  mo_q <- var.get.nc(mo_grp, "q")[,index]
  mo_qmean_b <- var.get.nc(mo_grp, "qbar_reachScale")[index]
  mo_qmean_a <- var.get.nc(mo_grp, "qbar_basinScale")[index]
  
  # sad 
  sd_grp <- grp.inq.nc(sos, "moi/sad")$self
  sd_q <- var.get.nc(sd_grp, "q")[,index]
  sd_qmean_b <- var.get.nc(sd_grp, "qbar_reachScale")[index]
  sd_qmean_a <- var.get.nc(sd_grp, "qbar_basinScale")[index]
  
  # sic4dvar 
  sv_grp <- grp.inq.nc(sos, "moi/sic4dvar")$self
  sv_q <- var.get.nc(sv_grp, "q")[,index]
  sv_qmean_b <- var.get.nc(sv_grp, "qbar_reachScale")[index]
  sv_qmean_a <- var.get.nc(sv_grp, "qbar_basinScale")[index]
  
  # metroman
  mm_grp <- grp.inq.nc(sos, "moi/metroman")$self
  mm_q <- var.get.nc(mm_grp, "q")[,index]
  mm_qmean_b <- var.get.nc(mm_grp, "qbar_reachScale")[index]
  mm_qmean_a <- var.get.nc(mm_grp, "qbar_basinScale")[index]
  
  close.nc(sos)
  return(data.frame(date = nt,
                    geobam_q = gb_q,
                    gb_qmean_b = gb_qmean_b,
                    gb_qmean_a = gb_qmean_a,
                    hivdi_q = hv_q,
                    hv_qmean_b = hv_qmean_b,
                    hv_qmean_a = hv_qmean_a,
                    momma_q = mo_q,
                    mo_qmean_b = mo_qmean_b,
                    mo_qmean_a = mo_qmean_a,
                    metroman_q = mm_q,
                    mm_qmean_b = mm_qmean_b,
                    mm_qmean_a = mm_qmean_a,
                    sad_q = sd_q,
                    sd_qmean_b = sd_qmean_b,
                    sd_qmean_a = sd_qmean_a,
                    sic4dvar_q = sv_q,
                    sv_qmean_b = sv_qmean_b,
                    sv_qmean_a = sv_qmean_a
  ))
  # return(data.frame(date = nt,
  #                   geobam_q = gb_q,
  #                   gb_qmean_b = gb_qmean_b,
  #                   gb_qmean_a = gb_qmean_a,
  #                   hivdi_q = hv_q,
  #                   hv_qmean_b = hv_qmean_b,
  #                   hv_qmean_a = hv_qmean_a,
  #                   momma_q = mo_q,
  #                   mo_qmean_b = mo_qmean_b,
  #                   mo_qmean_a = mo_qmean_a,
  #                   metroman_q = mm_q,
  #                   mm_qmean_b = mm_qmean_b,
  #                   mm_qmean_a = mm_qmean_a
  # ))
}