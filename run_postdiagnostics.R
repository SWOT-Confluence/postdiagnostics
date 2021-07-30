source("input.R")
source("postdiagnostics")
source("output.R")

# RUN FLPE DIAGNOSTICS
start <- Sys.time()
# CHANGE ME #
input_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                       "diagnostics", "post_data", fsep=.Platform$file.sep)    
run_flpe_diagnostics(input_dir)
end <- Sys.time()
print(paste0("Execution time: ", end - start))

# RUN INTEGRATOR DIAGNOSTICS
start <- Sys.time()
# CHANGE ME #
input_dir <- file.path("/home", "nikki", "Documents", "confluence", "workspace",
                       "diagnostics", "post_data", fsep=.Platform$file.sep)    
run_integrator_diagnostics(input_dir)
end <- Sys.time()
print(paste0("Execution time: ", end - start))