# postdiagnostics

postdiagnostics serves as a diagnostic module for Confluence. It parses FLPE and integrator data and compares priors and posteriors on discharge. This module outputs data ?.

TO DO:
- Missing/invalid data in HiVDI: Input.temp_hivdi_q
- Implement input ops for HiVDI and SAD
- Implement run_diagnostics methods (FLPE and integrator) in "postdiagnostics.R" file
- Implement write_data in "output.R" file
- Update Integrator input operations to use RNetCDF library
- Postdiagnostics.R error -> flpe_diagnostics: '-' only defined for equally-sized data frames (stability check)

# installation

# setup

# execution