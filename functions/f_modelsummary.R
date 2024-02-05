if (!require("pacman")) install.packages("pacman")
pacman::p_load(modelsummary, install = TRUE, update = FALSE)

# Define a custom function for generating a model summary
modelsummary_cstm <- function(model,                   # Model object
                              out = "default",         # Output format
                              expt = FALSE,            # Whether to exponentiate estimates
                              sig.dig = 4,             # Significant digits for estimates and standard errors
                              p.dig = 3,               # Decimal places for p-values
                              est = "{estimate}{stars} ({std.error})", # Format for estimates
                              stat = NULL,             # Statistic to report
                              bs.r = 250,
                              bs.type = "wild") {      # Bootstrap replications
  
  if(bs.type == "wild"){
    vcov.function <- function(x) vcovBS(x,
                                        cluster = ~vac_uid,
                                        R = bs.r,
                                        type = bs.type)
  } else if(bs.type == "none"){
    vcov.function <- function(x) vcovCL(x,
                                        cluster = ~vac_uid)
  } else {
    vcov.function <- function(x) vcovHC(x,
                                        type = "HC1")
  }
  
  # Call the modelsummary function with custom formatting options
  modelsummary(model,
               output = out,                          # Output format
               fmt = fmt_statistic(
                 "estimate" = sig.dig,
                 "std.error" = sig.dig,
                 "p.value" = p.dig
               ),                                     # Custom formatting for estimates, standard errors, and p-values
               vcov = vcov.function,                  # Variance-covariance matrix specification
               estimate = est,                        # Estimate format
               statistic = stat,                      # Statistic to report
               stars = stars,                         # Significance stars
               exponentiate = expt,                   # Whether to exponentiate estimates
               shape = term + statistic ~ model)      # Table shape
}