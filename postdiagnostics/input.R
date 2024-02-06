# Constants
VERS_LENGTH = 4    # length of SoS version identifier
CONT_LOOKUP = list("af", "eu", "as", "as", "oc", "sa", "na", "na", "na")    # Numeric continent identifier
S3_BUCKET = "confluence-sos"   # S3 SoS Bucket identifier
S3_TEMP = "/app/postdiagnostics"    # Path to store temporary SOS file
RESULT_SUFFIX = "_sword_v15_SOS_results.nc"    # Result file suffix, updated to sword 15
FLOAT_FILL = -999999999999    # NetCDF fill value for float variables
VENV_PATH = "/app/env"    # Path to Python interpreter
PYTHON_FILE = "/app/postdiagnostics/get_result_data.py"

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
get_input_data <- function(reaches_json, input_dir, index, local) {
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
get_data_flpe <- function(sos_file, reach_id, input_dir, flpe_dir, local_bool) {
  print('getting current')
  outlist <- get_flpe_current(reach_id, input_dir, flpe_dir)
  curr_df = outlist$df
  success_list = outlist$success_list
  print('getting previous')
  prev_df <- get_flpe_prev(reach_id, sos_file, success_list, local_bool)
  sos_df <- get_sos_q(sos_file, reach_id)
  print('combining current and sos')
  curr=cbind(curr_df, sos_df)
  print('combining previous and sos')
  prev=cbind(prev_df, sos_df)
  return(list(curr= curr, prev = prev))
}

#' Get FLPE discharge output from current run
#'
#' @param reach_id float reach identifier
#' @param input_dir string path to input directory (FLPE, SOS, JSON)
#' @param flpe_dir string path to directory that contains reach-level flpe data
#'
#' @return dataframe of current FLPE discharge data

get_flpe_current <- function(reach_id, input_dir, flpe_dir) {

  # example output dataframe
  # return(data.frame(date = nt,
  #                   geobam_q = geobam_q,
  #                   hivdi_q = hivdi_q,
  #                   momma_q = momma_q,
  #                   sad_q = sad_q,
  #                   sad_u = sad_u,
  #                   sic4dvar5_q = sv_q5,
  #                   sic4dvar31_q = sv_q31,
  #                   metroman_q = metroman_q,
  #                   metroman_u = metroman_u
  # ))

  # will keep track of what algos were ran in previous modules
  # this list will be passed between functions so we know what algos to run on
  success_list = list()

  # Data list in all functions will be a dynamic named list 
  # to build a dataframe out of
  data_list = list()

  # changed file reserved keyword to filepath throughout
  # 
  # time
  filename <- paste0(reach_id, "_SWOT.nc")
  filepath <- file.path(input_dir, "swot", filename, fsep=.Platform$file.sep)

  # ensures that we can find the file to run on
  if (file.exists(filepath)){
    swot <- open.nc(filepath)
    r_grp = grp.inq.nc(swot, "reach")$self
    nt = var.get.nc(r_grp, "time")
    close.nc(swot)
    # data_list$date = nt
    data_list$date = nt

  } else{
    stop("Could not find input file, be sure the reaches.json is pointing to processed data.")
  }

  # we check and see if there is an output file for each algo
  # if there is then we read the data into the named list
  # and we add the algo to the successful list
  # so that we know to run the rest of the script on it

  # geobam
  filename <- paste0(reach_id, "_geobam.nc")
  filepath <- file.path(flpe_dir, "geobam", filename, fsep=.Platform$file.sep)
  print(filepath)

  if (file.exists(filepath)){
    geobam <- open.nc(filepath)
    geobam_q <- get_gb_q_cur(geobam, "q")
    close.nc(geobam)
    data_list$geobam_q = geobam_q
    success_list = append(success_list, 'geobam')
  } else{
    print('Could not find geobam')
  }
  
  # hivdi
  filename <- paste0(reach_id, "_hivdi.nc")
  filepath <- file.path(flpe_dir, "hivdi", filename, fsep=.Platform$file.sep)
  print(filepath)

  if (file.exists(filepath)){
    hivdi <- open.nc(filepath)
    hv_grp <- grp.inq.nc(hivdi, "reach")$self
    hivdi_q <- var.get.nc(hv_grp, "Q")
    close.nc(hivdi)
    data_list$hivdi_q = hivdi_q
    success_list = c(success_list, 'hivdi')
  } else{
    print('Could not find')
  }

  
  # momma
  filename <- paste0(reach_id, "_momma.nc")
  filepath <- file.path(flpe_dir, "momma", filename, fsep=.Platform$file.sep)
  print(filepath)

  if (file.exists(filepath)){
    momma <- open.nc(filepath)
    momma_q <- var.get.nc(momma, "Q")
    close.nc(momma)
    data_list$momma_q = momma_q
    success_list = append(success_list, 'momma')
  } else{
    print('Could not find momma')
  }

  
  # sad 
  filename <- paste0(reach_id, "_sad.nc")
  filepath <- file.path(flpe_dir, "sad", filename, fsep=.Platform$file.sep)
  print(filepath)

  if (file.exists(filepath)){
    sad <- open.nc(filepath)
    sad_q <- var.get.nc(sad, "Qa")
    sad_u <- var.get.nc(sad, "Q_u")
    close.nc(sad)
    data_list$sad_q = sad_q
    data_list$sad_u = sad_u
    success_list = append(success_list, 'sad')
  } else{
    print('Could not find sad')
  }

  # sic4dvar
  filename <- paste0(reach_id, "_sic4dvar.nc")
  filepath <- file.path(flpe_dir, "sic4dvar", filename, fsep=.Platform$file.sep)
  print(filepath)

  if (file.exists(filepath)){
    sv <- open.nc(filepath)
    # sv_q5 <- var.get.nc(sv, "Qalgo5")
    sv_q_mm <- var.get.nc(sv, "Q_mm")
    sv_q_da <- var.get.nc(sv, "Q_da")
    close.nc(sv)
    # data_list$sic4dvar5_q = sv_q5
    data_list$sic4dvar_q_mm = sv_q_mm
    data_list$sic4dvar_q_da = sv_q_da
    success_list = append(success_list, 'sic4dvar')
  } else{
    print('Could not find sic')
  }

  
  # metroman
  filename <- list.files(path=file.path(flpe_dir, "metroman", fsep=.Platform$file.sep), 
                     pattern=paste0(".*", reach_id, ".*", "_metroman\\.nc"), 
                     recursive=TRUE, 
                     full.names=TRUE)
  #list.files returns a path so we do not need to convert, but if it could not find the file it is nan
  filepath <- filename

  if (length(filepath)>0){
    metroman <- open.nc(filename)
    reach_ids <- var.get.nc(metroman, "reach_id")
    index <- which(reach_ids==reach_id, arr.ind=TRUE)
    if (length(reach_ids)==1){
        metroman_q <- var.get.nc(metroman, "allq")[1]
        metroman_q[is.nan(metroman_q)] = NA
        metroman_u <- var.get.nc(metroman, "q_u")[1]
        metroman_u[is.nan(metroman_u)] = NA
    }else{
        metroman_q <- var.get.nc(metroman, "allq")[,index]
        metroman_q[is.nan(metroman_q)] = NA
        metroman_u <- var.get.nc(metroman, "q_u")[,index]
        metroman_u[is.nan(metroman_u)] = NA
    }
    close.nc(metroman)
    data_list$metroman_q = metroman_q
    data_list$metroman_u = metroman_u
    success_list = append(success_list, 'metroman')
  } else{
    print('Could not find metro')
  }
  print('making dataframe')
  print(names(data_list))
  for (x in 1:length(data_list)){
    print('index')
    print(x)
    print('name')
    print(names(data_list)[x])
    print('values')
    print(data_list[x])
    print('length')
    print(length(data_list[x]))
  }
  df = data.frame(data_list)

  print('dataframe to list')
  outlist <- list("df" = df, "success_list" = success_list)
  return(outlist)
}

#' Get geoBAM discharge posteriors for current run
#'
#' @param ds NetCDF dataset
#' @param name str name of group
#'
#' @return vector of discharge values
get_gb_q_cur <- function(ds, name) {
  q_grp = grp.inq.nc(ds, name)$self
  q_chains <- cbind(var.get.nc(q_grp, "q1"), 
                        var.get.nc(q_grp, "q2"), 
                        var.get.nc(q_grp, "q3"))
  q <- rowMeans(q_chains, na.rm=TRUE)
  q[is.nan(q)] = NA
  return(q)
}

#' Get FLPE algorithm discharge from previous run
#'
#' @param reach_id integer reach identifier
#' @param sos_file str path to sos file
#' @param success_list a list of algo names to run on
#'
#' @return dataframe of previous FLPE discharge data
get_flpe_prev <- function(reach_id, sos_file, success_list, local_bool) {
  print('in get flpe prev')
  
  # Result file
  key = get_result_file_name(reach_id, sos_file)
  
  # S3 access to result file
  file_name = paste(S3_TEMP, tail(strsplit(key, "/")[[1]], n=1), sep="/")
  # use_virtualenv(VENV_PATH)
  use_python("/usr/bin/python3")
  source_python(PYTHON_FILE)

  if (!local_bool){
    download_previous_result(S3_BUCKET, key, file_name)
  }else{
    file_name = paste("/mnt/data/results",basename(key), sep="/") 
  }
  print(file_name)
  
  
  # index
  sos = open.nc(file_name)
  r_grp = grp.inq.nc(sos, "reaches")$self
  reach_ids = var.get.nc(r_grp, "reach_id")
  index = which(reach_ids==reach_id, arr.ind=TRUE)

# example output data frame
#     return(data.frame(date = nt,
#                     geobam_q = gb_q,
#                     hivdi_q = hv_q,
#                     momma_q = mo_q,
#                     sad_q = sd_q,
#                     sad_u = sd_u,
#                     sic4dvar5_q = sv5_q,
#                     sic4dvar31_q = sv31_q,
#                     metroman_q = mm_q,
#                     metroman_u = mm_u
#   ))
# }

  data_list = list()
  nt = var.get.nc(r_grp, "time")[index][[1]]
  data_list$date = nt
  
  # only run on the algo if it is in the success list
  # geobam
  if ('geobam'%in%success_list){
    print('geobam')
    gb_grp <- grp.inq.nc(sos, "neobam")$self
    gb_q <- get_gb_q_prev(gb_grp, "q", index)
    data_list$geobam_q = gb_q

  }else{
    print('geobam not found')
  }
  
  # hivdi
  if ('hivdi'%in%success_list){
    print('hivdi')
    hv_grp <- grp.inq.nc(sos, "hivdi")$self
    hv_q <- var.get.nc(hv_grp, "Q")[index][[1]]
    hv_q[hv_q == FLOAT_FILL] = NA
    data_list$hivdi_q = hv_q
  }else{
    print('hivdi not found')
  }
  
  # mommma
  if ('momma'%in%success_list){
    print('momma')
    mo_grp <- grp.inq.nc(sos, "momma")$self
    mo_q <- var.get.nc(mo_grp, "Q")[index][[1]]
    mo_q[mo_q == FLOAT_FILL] = NA
    data_list$momma_q = mo_q
  }else{
    print('Momma not found')
  }
  # sad
  if ('sad'%in%success_list){
    print('sad')
    sd_grp <- grp.inq.nc(sos, "sad")$self
    sd_q <- var.get.nc(sd_grp, "Qa")[index][[1]]
    sd_q[sd_q == FLOAT_FILL] = NA
    sd_u <- var.get.nc(sd_grp, "Q_u")[index][[1]]
    sd_u[sd_u == FLOAT_FILL] = NA
    data_list$sad_q = sd_q
  }else{
    print('Sad not found')
  }
  
  # sic4dvar
  if ('sic4dvar'%in%success_list){
    print('sic')
    sv_grp <- grp.inq.nc(sos, "sic4dvar")$self
    # sv_q5 <- var.get.nc(sv_grp, "Qalgo5")[index][[1]]
    # sv_q5[sv_q5 == FLOAT_FILL] = NA
    # data_list$sic4dvar_q_mm = sv_qmm
    # data_list$sic4dvar_q_da = sv_q_da
    sv_q_mm <- var.get.nc(sv_grp, "Q_mm")[index][[1]]
    sv_q_mm[sv_q_mm == FLOAT_FILL] = NA
    sv_q_da <- var.get.nc(sv_grp, "Q_da")[index][[1]]
    sv_q_da[sv_q_da == FLOAT_FILL] = NA
    # data_list$sic4dvar5_q = sv_q5
    data_list$sic4dvar_q_mm = sv_q_mm
    data_list$sic4dvar_q_da = sv_q_da
  }else{
    print('Sic not found')
  }
  
  # metroman
  if ('metroman'%in%success_list){
    print('metro')
    mm_grp <- grp.inq.nc(sos, "metroman")$self
    mm_q <- var.get.nc(mm_grp, "allq")[index][[1]]
    mm_q[mm_q == FLOAT_FILL] = NA
    mm_u <- var.get.nc(mm_grp, "q_u")[index][[1]]
    mm_u[mm_u == FLOAT_FILL] = NA
    data_list$metroman_q = mm_q
    data_list$metroman_u = mm_u
  }else{
    print('metro not found')
  }
  
  close.nc(sos)
  if(!local_bool){
      file.remove(file_name)
  }

  df = data.frame(data_list)
  return(df)
}



#' Return previous version SOS result file name
#'
#' @param reach_id integer reach identifier
#' @param sos_file string path to current sos_file
#'
#' @return string name of previous sos result file
get_result_file_name <- function(reach_id, sos_file) {
  # Current SoS data
  sos = open.nc(sos_file)
  run_type = att.get.nc(sos, "NC_GLOBAL", "run_type")
  version = att.get.nc(sos, "NC_GLOBAL", "product_version")
  close.nc(sos)
  
  # Previous result version
  prev_version = as.character(strtoi(version) - 1)
  padding = paste(rep(0, (VERS_LENGTH - length(prev_version))), sep="", collapse="")
  prev_version = paste(padding, prev_version, sep="")
  
  # Continent
  continent = CONT_LOOKUP[[floor(reach_id / 10000000000)]]
  
  # SOS result file name
  return(paste(run_type, prev_version, paste(continent, RESULT_SUFFIX, sep=""), sep="/"))
}

#' Get geoBAM discharge posteriors for previous run
#'
#' @param ds NetCDF dataset
#' @param name str name of group
#' @param index integer to index data on
#'
#' @return list of lists (mean and standard deviation discharge)
get_gb_q_prev <- function(ds, name, index) {
  q_grp = grp.inq.nc(ds, name)$self
  q_chains <- cbind(var.get.nc(q_grp, "q1")[index][[1]], 
                        var.get.nc(q_grp, "q2")[index][[1]], 
                        var.get.nc(q_grp, "q3")[index][[1]])
  q_chains[q_chains == FLOAT_FILL] = NA
  q <- rowMeans(q_chains, na.rm=TRUE)
  q[is.nan(q)] = NA
  return(q)
}

#' Get SOS discharge priors
#'
#' @param sos_file string path to SoS file
#' @param reach_id float reach identifier
#'
#' @return named list of discharge priors
get_sos_q <- function(sos_file, reach_id) {
  print('in get sos q')
  sos <- open.nc(sos_file)
  r_grp <- grp.inq.nc(sos, "reaches")$self
  reach_ids <- var.get.nc(r_grp, "reach_id")
  index <- which(reach_ids==reach_id, arr.ind=TRUE)
  model_grp = grp.inq.nc(sos, "model")$self
  qmean <- var.get.nc(model_grp, "mean_q")[index]
  qmin <- var.get.nc(model_grp, "min_q")[index]
  qmax <- var.get.nc(model_grp, "max_q")[index]
  gb_grp <- grp.inq.nc(sos, "gbpriors/reach")$self
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
get_data_moi <- function(sos_file, reach_id, input_dir, moi_dir, local_bool) {
  
  curr_df <- get_moi_current(reach_id, input_dir, moi_dir)
  prev_df <- get_moi_prev(reach_id, sos_file, local_bool)
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
  
  file <- paste0(reach_id, "_SWOT.nc")
  swot <- open.nc(file.path(input_dir, "swot", file, fsep=.Platform$file.sep))
  r_grp = grp.inq.nc(swot, "reach")$self
  nt = var.get.nc(r_grp, "time")
  close.nc(swot)
  
  # integrator file
  file <- paste0(reach_id, "_integrator.nc")
  print(file.path(moi_dir, file, fsep=.Platform$file.sep))
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
}

#' Return previous MOI results
#'
#' @param reach_id integer reach identifier
#' @param sos_file str path to sos file
#'
#' @return dataframe of previous MOI data
get_moi_prev <- function(reach_id, sos_file, local_bool) {
  
  # result file
  key = get_result_file_name(reach_id, sos_file)
  print(key)

  # S3 access to result file
  file_name = paste(S3_TEMP, tail(strsplit(key, "/")[[1]], n=1), sep="/")
  # use_virtualenv(VENV_PATH)
  use_python("/usr/bin/python3")
  source_python(PYTHON_FILE)
  if (!local_bool){
    download_previous_result(S3_BUCKET, key, file_name)
  }else{
    file_name = paste("/mnt/data/results",basename(key), sep = "/") 
  }
  print(file_name)

  
  # index
  sos = open.nc(file_name)
  reach_grp = grp.inq.nc(sos, "reaches")$self
  reach_ids = var.get.nc(reach_grp, "reach_id")
  index = which(reach_ids==reach_id, arr.ind=TRUE)
  
  # time
  nt = var.get.nc(reach_grp, "time")[index][[1]]
  
  # geobam
  gb_grp <- grp.inq.nc(sos, "moi/geobam")$self
  gb_q <- var.get.nc(gb_grp, "q")[index][[1]]
  gb_q[gb_q == FLOAT_FILL] = NA
  gb_qmean_b <- var.get.nc(gb_grp, "qbar_reachScale")[index]
  gb_qmean_a <- var.get.nc(gb_grp, "qbar_basinScale")[index]
  
  # hivdi
  hv_grp <- grp.inq.nc(sos, "moi/hivdi")$self
  hv_q <- var.get.nc(hv_grp, "q")[index][[1]]
  hv_q[hv_q == FLOAT_FILL] = NA
  hv_qmean_b <- var.get.nc(hv_grp, "qbar_reachScale")[index]
  hv_qmean_a <- var.get.nc(hv_grp, "qbar_basinScale")[index]
  
  # mommma
  mo_grp <- grp.inq.nc(sos, "moi/momma")$self
  mo_q <- var.get.nc(mo_grp, "q")[index][[1]]
  mo_q[mo_q == FLOAT_FILL] = NA
  mo_qmean_b <- var.get.nc(mo_grp, "qbar_reachScale")[index]
  mo_qmean_a <- var.get.nc(mo_grp, "qbar_basinScale")[index]
  
  # sad 
  sd_grp <- grp.inq.nc(sos, "moi/sad")$self
  sd_q <- var.get.nc(sd_grp, "q")[index][[1]]
  sd_q[sd_q == FLOAT_FILL] = NA
  sd_qmean_b <- var.get.nc(sd_grp, "qbar_reachScale")[index]
  sd_qmean_a <- var.get.nc(sd_grp, "qbar_basinScale")[index]
  
  # sic4dvar 
  sv_grp <- grp.inq.nc(sos, "moi/sic4dvar")$self
  sv_q <- var.get.nc(sv_grp, "q")[index][[1]]
  sv_q[sv_q == FLOAT_FILL] = NA
  sv_qmean_b <- var.get.nc(sv_grp, "qbar_reachScale")[index]
  sv_qmean_a <- var.get.nc(sv_grp, "qbar_basinScale")[index]
  
  # metroman
  mm_grp <- grp.inq.nc(sos, "moi/metroman")$self
  mm_q <- var.get.nc(mm_grp, "q")[index][[1]]
  mm_q[mm_q == FLOAT_FILL] = NA
  mm_qmean_b <- var.get.nc(mm_grp, "qbar_reachScale")[index]
  mm_qmean_a <- var.get.nc(mm_grp, "qbar_basinScale")[index]
  
  close.nc(sos)
  if(!local_bool){
    file.remove(file_name)
  }
  
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
}
