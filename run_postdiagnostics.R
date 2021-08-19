source("input.R")
source("postdiagnostics.R")
source("output.R")

library(RNetCDF)
library(dplyr)
library(rjson)

# RUN FLPE DIAGNOSTICS
start <- Sys.time()
input_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                       "diagnostics", "post_data", "input", fsep=.Platform$file.sep)
output_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                        "diagnostics", "post_data", "output", "flpe", fsep=.Platform$file.sep)
run_flpe_diagnostics(input_dir, output_dir)
end <- Sys.time()
print(paste0("Execution time: ", end - start))

# RUN INTEGRATOR DIAGNOSTICS
input_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                       "diagnostics", "post_data", "input", fsep=.Platform$file.sep)
output_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                        "diagnostics", "post_data", "output", "moi", fsep=.Platform$file.sep)   
run_moi_diagnostics(input_dir, output_dir)
end <- Sys.time()
print(paste0("Execution time: ", end - start))