#' Write FLPE diagnostics data to file
#'
#' @param diag_data_flpe list of realism and stability flags for each algo
#' @param reach_id unique integer reach identifier
#' @param output_dir path to output directory
write_data_flpe <- function(diag_data_flpe, reach_id, output_dir) {
  nc_file <- paste(output_dir, paste0(reach_id, "_flpe_diag.nc"), sep=.Platform$file.sep)
  nc_out <- create.nc(nc_file, format="netcdf4")
  
  # Global attribute
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_INT64", reach_id)
  
  # Dim and coord var
  dim.def.nc(nc_out, "num_algos", length(diag_data_flpe$realism_flags))
  var.def.nc(nc_out, "num_algos", "NC_INT", "num_algos")
  att.put.nc(nc_out, "num_algos", "units", "NC_STRING", "number of algorithms")
  var.put.nc(nc_out, "num_algos", c(1:length(diag_data_flpe$realism_flags)))
  
  # Vars
  fill = -999
  var.def.nc(nc_out, "algo_names", "NC_STRING", "num_algos")
  var.put.nc(nc_out, "algo_names", names(diag_data_flpe$realism_flags))
  
  var.def.nc(nc_out, "realism_flags", "NC_INT", "num_algos")
  att.put.nc(nc_out, "realism_flags", "_FILLVALUE", "NC_INT", fill)
  var.put.nc(nc_out, "realism_flags", diag_data_flpe$realism_flags)
  
  var.def.nc(nc_out, "stability_flags", "NC_INT", "num_algos")
  att.put.nc(nc_out, "stability_flags", "_FILLVALUE", "NC_INT", fill)
  var.put.nc(nc_out, "stability_flags", diag_data_flpe$stability_flags)
  
  close.nc(nc_out)
}

#' Write Integrator diagnostics data to file
#'
#' @param diag_data_moi list of realism, stability, prepost flags for each algo
#' @param reach_id unique integer reach identifier
#' @param output_dir path to output directory
write_data_moi <- function(diag_data_moi, reach_id, output_dir) {
  nc_file <- paste(output_dir, paste0(reach_id, "_moi_diag.nc"), sep=.Platform$file.sep)
  nc_out <- create.nc(nc_file, format="netcdf4")
  
  # Global attribute
  att.put.nc(nc_out, "NC_GLOBAL", "reach_id", "NC_INT64", reach_id)
  
  # Dim and coord var
  dim.def.nc(nc_out, "num_algos", length(diag_data_moi$realism_flags))
  var.def.nc(nc_out, "num_algos", "NC_INT", "num_algos")
  att.put.nc(nc_out, "num_algos", "units", "NC_STRING", "number of algorithms")
  var.put.nc(nc_out, "num_algos", c(1:length(diag_data_moi$realism_flags)))
  
  # Vars
  fill = -999
  var.def.nc(nc_out, "algo_names", "NC_STRING", "num_algos")
  var.put.nc(nc_out, "algo_names", names(diag_data_moi$realism_flags))
  
  var.def.nc(nc_out, "realism_flags", "NC_INT", "num_algos")
  att.put.nc(nc_out, "realism_flags", "_FILLVALUE", "NC_INT", fill)
  var.put.nc(nc_out, "realism_flags", diag_data_moi$realism_flags)
  
  var.def.nc(nc_out, "stability_flags", "NC_INT", "num_algos")
  att.put.nc(nc_out, "stability_flags", "_FILLVALUE", "NC_INT", fill)
  var.put.nc(nc_out, "stability_flags", diag_data_moi$stability_flags)
  
  var.def.nc(nc_out, "prepost_flags", "NC_INT", "num_algos")
  att.put.nc(nc_out, "prepost_flags", "_FILLVALUE", "NC_INT", fill)
  var.put.nc(nc_out, "prepost_flags", diag_data_moi$prepost_flags)
  
  close.nc(nc_out)
}