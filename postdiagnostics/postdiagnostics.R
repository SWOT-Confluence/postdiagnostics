#' Run FLPE diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#' @param flpe_dir string path to directory that contains reach-level flpe data
#' @param reaches_json str name of JSON file with reach data
#' @param output_dir string path to directory to write output to
#' @param index integer index to locate reach data for
#' @param tolerance ??
run_flpe_diagnostics <- function(input_dir, flpe_dir, output_dir, reaches_json, 
                                 index, tolerance) {
  # INPUT
  print('getting input files')
  reach_files <- get_input_data(reaches_json, input_dir, index)
  print('getting flpe data')
  data <- get_data_flpe(reach_files$sos, reach_files$reach_id, input_dir, flpe_dir)
  
  # PROCESSING
  print('processing')
  diag_data_flpe <- flpe_diagnostics(data$curr, data$prev, tolerance)
  
  # OUTPUT
  print('outputting')
  write_data_flpe(diag_data_flpe, reach_files$reach_id, output_dir)
}

#' Diagnostics on FLPE discharge data
#'
#' @param current_discharge dataframe of current discharge
#' @param previous_discharge dataframe of previous discharge
#' @param tolerance ??
#'
#' @return named list of flags for reach-level flpe algorithms
flpe_diagnostics <- function(current_discharge, previous_discharge, tolerance) {
  #in all these checks, I want to be lazy and compare by column position, but that 
  #isn't a good idea in case they move. Therefore, I need to search on name, which
  #expands the code substantially unless I get overly fancy. I therefore decide to 
  #go needlessly fancy with an explicit name check and a lapply statment. 
  #This now needs to be dynamic. I will parse the current discharge headers to decide what algos to run on.
  
  # possible_algo_names=c("geobam", "hivdi", "momma", "metroman", "sad", "sic4dvar5", "sic4dvar31")

# example current discharge df
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

  headers = colnames(current_discharge)

  algo_names = list()

  # created dynamic algo names list by referencing the headers of current discharge

  if ('sic4dvarmm_q'%in%headers){
    algo_names = append(algo_names, "sic4dvarmm")
  }
  if ('sad_q'%in%headers){
    algo_names = append(algo_names, 'sad')
  }
  if ('metroman_q'%in%headers){
    algo_names = append(algo_names, 'metroman')
  }
  if ('momma_q'%in%headers){
    print('found momma')
    algo_names = append(algo_names, 'momma')
  }
  if ('hivdi_q'%in%headers){
    algo_names = append(algo_names, 'hivdi')
  }
  if ('geobam_q'%in%headers){
    algo_names = append(algo_names, 'geobam')
  }

  #run the checks
  realism_flags=sapply(algo_names, flpe_realism_check, current_discharge=current_discharge, simplify=T)
  # stability_flags= sapply(algo_names, stability_check, current_discharge=data, 
  #                         previous_discharge=data+100,tolerance=difference_tolerance)
  stability_flags= sapply(algo_names, flpe_stability_check, current_discharge=current_discharge, 
                          previous_discharge=previous_discharge, tolerance=tolerance)
  
  # bugfix, later in the script we call names for these lists to add them
  # to the output file
  names(stability_flags) = algo_names
  names(realism_flags) = algo_names

  return(list(realism_flags=realism_flags, stability_flags=stability_flags))
}

#realism check
flpe_realism_check <- function(current_discharge, algo_name){
  realism_flag=0
  this_algo_q= select(current_discharge,paste0(algo_name,'_q'))
  # bugfix, change undefined data varaible to current discharge
  if (paste0(algo_name,'_u') %in% names(current_discharge)){ #does explicit uncertainty exist?
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

#stability check
flpe_stability_check <- function(current_discharge, previous_discharge, algo_name, tolerance){
  #pass a tolerance in percent change to determine how much change is too much

  # separated selection from filtering for readability and error catching
  this_algo_q_now = select(current_discharge,paste0(algo_name,'_q')) 
  this_algo_q_now = this_algo_q_now%>%
    dplyr::filter(current_discharge$date %in% previous_discharge$date)


# previous q had NA values instead of fill so we need to filter both
  this_algo_q_then = select(previous_discharge,paste0(algo_name,'_q'))%>%
    dplyr::filter(previous_discharge$date %in% current_discharge$date)
  
  stability_flag=0

  test1 = this_algo_q_now- this_algo_q_then
  test2 = test1/this_algo_q_now

  if (any(  abs(test2*100) > tolerance,
            na.rm=T  )  ){
    stability_flag=1}
  
  return(stability_flag)
}

#' Run Integrator diagnostics - not complete
#' 
#' Uses environment variable AWS_BATCH_JOB_ARRAY_INDEX as an index to reach
#' file.
#' 
#' @param input_dir string path to directory that contains input data
#' @param flpe_dir string path to directory that contains reach-level flpe data
#' @param moi_dir string path to directory that contains basin-level moi data
#' @param output_dir string path to directory to write output to
#' @param index integer index to locate reach data for
#' @param tolerance ??
run_moi_diagnostics <- function(input_dir, flpe_dir, moi_dir, output_dir, index, tolerance) {
  # INPUT  
  reach_files <- get_input_data(reaches_json, input_dir, index)
  outlist <- get_flpe_current(reach_files$reach_id, input_dir, flpe_dir)
  flpe_data = outlist$df
  success_list = outlist$success_list
  moi_data <- get_data_moi(reach_files$sos, reach_files$reach_id, input_dir, moi_dir)
  
  # FORMAT FLPE DATA FOR DIAGS - currently selects algo31 for diagnostics
  headers = colnames(flpe_data)

  if ('sic4dvarmm_q'%in%headers){
    # flpe_data <- subset(flpe_data, select=-c({old_algo_name}))
    flpe_data <- flpe_data %>% rename(sic4dvar_q = sic4dvarmm_q)
  }
  
  # PROCESSING
  diag_data_moi <- moi_diagnostics(flpe_data, moi_data$curr, moi_data$prev, tolerance, success_list)
  
  # OUTPUT
  write_data_moi(diag_data_moi, reach_files$reach_id, output_dir)
}

#' Diagnostics on Integrator discharge data
#'
#' @param flpe_discharge reach-level FLPE discharge dataframe
#' @param current_integrator current integrator discharge dataframe
#' @param previous_integrator previous integrator discharge dataframe
#' @param tolerance ??
#' 
#' @return list of flags for reach-level flpe algorithms
moi_diagnostics <- function(flpe_discharge, current_integrator, previous_integrator, tolerance, algo_names) {
  # Helpful info: 
  # current_integrator and previous_integrator columns name meanings:
  # "algoname_q" means integrated discharge
  # "algoname_qmean_b" means discharge before integration
  # "algoname_qmean_a" means discharge after integration
  # "sos_qmin" and "sos_qmax" are SoS priors
  
  # algo_names=c("geobam", "hivdi", "momma", "metroman", "sad", "sic4dvar")


  
  realism_flags=sapply(algo_names, moi_realism_check, current_integrator=current_integrator, simplify=T)
  
  stability_flags= sapply(algo_names, moi_stability_check, current_integrator=current_integrator, 
                          previous_integrator=previous_integrator, tolerance=tolerance)
  
  prepost_flags= sapply(algo_names, moi_prepost_check, current_integrator=current_integrator, 
                        flpe_discharge=flpe_discharge, tolerance=tolerance)
  names(stability_flags) = algo_names
  names(realism_flags) = algo_names
  return(list(realism_flags=realism_flags, stability_flags=stability_flags, prepost_flags=prepost_flags))
}

#realism 
moi_realism_check <- function(current_integrator, algo_name){
  realism_flag=0
  this_algo_q= select(current_integrator,paste0(algo_name,'_q'))
  if (paste0(algo_name,'_u') %in% names(data)){ #does explicit uncertainty exist?
    this_algo_u= select(current_integrator,paste0(algo_name,'_u'))} else{
      
      this_algo_u=0
    }
  
  if ( any ( (this_algo_q + this_algo_u) > current_integrator$sos_qmax,na.rm=T ) | #logical OR. Masks whether or not we're high or low
       any ( (this_algo_q - this_algo_u) < current_integrator$sos_qmin,na.rm=T )
  ){
    realism_flag=1
  }  
  
  return(realism_flag)
}# end realism check 

#stability
#simple ANY operation for diff > tolerance
moi_stability_check <- function(current_integrator, previous_integrator, algo_name, tolerance){
  #pass a tolerance in percent change to determine how much change is too much
  this_algo_q_now = select(current_integrator,paste0(algo_name,'_q'),date) %>%
    dplyr::filter(date %in% previous_integrator$date)%>%
    select(-date)
  
  # this_algo_q_then = select(previous_integrator,paste0(algo_name,'_q'))

  # # previous q had NA values instead of fill so we need to filter both
  this_algo_q_then = select(previous_integrator,paste0(algo_name,'_q'), date)%>%
    dplyr::filter(previous_integrator$date %in% current_integrator$date)%>%
    select(-date)
  
  stability_flag=0
  # if (any(  abs(((this_algo_q_now- this_algo_q_then)/this_algo_q_now)*100) > tolerance,
  #           na.rm=T  )  ){
  #   stability_flag=1}
  
  # return(stability_flag)
  test1 = this_algo_q_now- this_algo_q_then
  test2 = test1/this_algo_q_now

  if (any(  abs(test2*100) > tolerance,
            na.rm=T  )  ){
    stability_flag=1}
  
  return(stability_flag)
}

#pre post
#simple ANY operation for diff > tolerance
moi_prepost_check <- function(current_integrator, flpe_discharge, algo_name, tolerance){
  prepost_flag=0
  integrator_q= select(current_integrator,paste0(algo_name,'_q'))
  flpe_q= select(flpe_discharge,paste0(algo_name,'_q'))
  
  if (any(  abs(((integrator_q- flpe_q)/integrator_q)*100) > tolerance,
            na.rm=T  )  ){
    prepost_flag=1}
  
  return(prepost_flag)
}
