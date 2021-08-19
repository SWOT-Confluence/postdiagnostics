source("input.R")
source("output.R")

#' Run FLPE diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#' @param output_dir string path to directory to write output to
#'
#' @return dataframe of FLPE and SoS data
run_flpe_diagnostics <- function(input_dir, output_dir) {
  # INPUT  
  # args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 14
  reach_files <- get_input_data(reaches_json, input_dir, index)
  data <- get_data_flpe(reach_files$sos, reach_files$reach_id, input_dir)
  
  # PROCESSING
  diag_data_flpe <- flpe_diagnostics(data$curr, data$prev, 2)    ## TODO decide what is appropriate tolerance
  
  # OUTPUT
  write_data_flpe(diag_data_flpe, reach_files$reach_id, output_dir)
}

#' Run Integrator diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#'
#' @return ?
run_moi_diagnostics <- function(input_dir) {
  # INPUT  
  # args <- commandArgs(trailingOnly=TRUE)
  # reaches_json <- ifelse(is.null(args), "reaches.json", args[1])
  reaches_json <- "reaches.json"
  # index <- strtoi(Sys.getenv("AWS_BATCH_JOB_ARRAY_INDEX")) + 1 ## TODO for container
  index <- 14
  reach_files <- get_input_data(reaches_json, input_dir, index)
  flpe_data <- get_flpe_current(reach_files$reach_id, input_dir)
  moi_data <- get_data_moi(reach_files$sos, reach_files$reach_id, input_dir)
  
  # PROCESSING
  diag_data_moi <- moi_diagnostics(flpe_data, moi_data$curr, moi_data$prev)
  
  # OUTPUT
  # write_data_integrator(diag_data)
}

#' Diagnostics on FLPE discharge data
#' 
#' TODO: ‘-’ only defined for equally-sized data frames (stability check)
#'
#' @param current_discharge dataframe of current discharge
#' @param previous_discharge dataframe of previous discharge
#' @param tolerance ??
#'
#' @return ??
flpe_diagnostics <- function(current_discharge, previous_discharge, tolerance) {
  #in all these checks, I want to be lazy and compare by column position, but that 
  #isn't a good idea in case they move. Therefore, I need to search on name, which
  #expands the code substantially unless I get overly fancy. I therefore decide to 
  #go needlessly fancy with an explicit name check and a lapply statment
  
  # algo_names=c("geobam", "hivdi", "momma", "sad", "metroman")
  algo_names=c("geobam", "hivdi", "momma", "metroman")
  
  #realism check
  realism_check= function(current_discharge,algo_name){
    realism_flag=0
    this_algo_q= select(current_discharge,paste0(algo_name,'_q'))
    if (paste0(algo_name,'_u') %in% names(data)){ #does explicit uncertainty exist?
      this_algo_u= select(current_discharge,paste0(algo_name,'_u'))} else{
        
        this_algo_u=0
      }
    
    if ( any ( (this_algo_q + this_algo_u) > current_discharge$sos_qmax,na.rm=T ) | #logical OR. Masks whether or not we're high or low
         any ( (this_algo_q - this_algo_u) < current_discharge$sos_qmin,na.rm=T )
    ){
      realism_flag=1
    }  
    
    return(realism_flag)
  }# end realism check 
  
  #stability check  ## TODO ‘-’ only defined for equally-sized data frames 
  stability_check=function(current_discharge, previous_discharge, algo_name, tolerance){
    #pass a tolerance in percent change to determine how much change is too much
    this_algo_q_now = select(current_discharge,paste0(algo_name,'_q')) %>%
      dplyr::filter(current_discharge$date %in% previous_discharge$date)
    this_algo_q_then = select(previous_discharge,paste0(algo_name,'_q'))
    
    stability_flag=0
    if (any(  abs(((this_algo_q_now- this_algo_q_then)/this_algo_q_now)*100) > tolerance,
              na.rm=T  )  ){
      stability_flag=1}
    
    return(stability_flag)
  }
  
  #run the checks
  realism_flags=sapply(algo_names,realism_check,current_discharge=current_discharge, simplify=T)
  # stability_flags= sapply(algo_names, stability_check, current_discharge=data, 
  #                         previous_discharge=data+100,tolerance=difference_tolerance)
  stability_flags= sapply(algo_names, stability_check, current_discharge=current_discharge, 
                          previous_discharge=previous_discharge, tolerance=tolerance)
  
  return(list(realism_flags=realism_flags, stability_flags=stability_flags))
}

#' Diagnostics on Integrator discharge data
#'
#' @param flpe_discharge reach-level FLPE discharge dataframe
#' @param current_integrator current integrator discharge dataframe
#' @param previous_integrator previous integrator discharge dataframe
#' @return ??
moi_diagnostics <- function(flpe_discharge, current_integrator, previous_integrator) {
  # Helpful info: 
  # current_integrator and previous_integrator columns name meanings:
  # "algoname_q" means integrated discharge
  # "algoname_qmean_b" means discharge before integration
  # "algoname_qmean_a" means discharge after integration
  # "sos_qmin" and "sos_qmax" are SoS priors
  
  ## COLIN WRITE MOI DIAGNOSTICS HERE
}
