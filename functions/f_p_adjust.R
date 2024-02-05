if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr, stringr, tidyr, tibble, install = TRUE, update = FALSE)

# Define custom function for adjusting p-values
p_adjust_cstm <- function(model,                  # Model object
                          p.dig = 10,             # Number of decimal places for p-values
                          out = "data.frame",     # Output format
                          est = "{p.value}",      # Estimate to extract from the model
                          mtd = "holm",           # Adjustment method (default: Holm's method),
                          bs.type = "wild",
                          bs.r = 1000) {         
  
  # Extract p-values and variable names from the model, and create a tibble
  vartable <- modelsummary_cstm(model, out = out, est = est, p.dig = p.dig,
                                bs.type = bs.type, bs.r = bs.r) %>%
    as_tibble() %>%
    mutate(interaction = grepl("×", term),                                 # Identify interaction terms
           var = case_when(interaction == FALSE ~                          # Extract variable names
                             str_extract(term,
                                         paste0(names(model$model), collapse = "|")),
                           interaction == TRUE ~
                             str_extract(term,
                                         paste0(paste0(str_c("can_minorityYes × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityTurkish × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityHispanic × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityArab × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityWhite American × ",
                                                             names(model$model)), 
                                                       collapse = "|"),
                                                paste0(str_c("can_identityBlack American × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityCentral African × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityAsian × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_identityEastern European × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                paste0(str_c("can_sexYes × ",
                                                             names(model$model)),
                                                       collapse = "|"),
                                                sep = "|")))) %>%
    rename(p = `(1)`) %>%
    drop_na() %>%
    mutate(p = as.numeric(p))  # Convert p-values to numeric
  
  # Count the number of occurrences of each variable in the model
  varcount <- table(vartable$var) %>% as.data.frame() %>% as_tibble() %>%
    rename(var = Var1,
           freq = Freq)
  
  # Join the variable count data to the main table
  vartable <- vartable %>%
    left_join(varcount, by = "var")
  
  # Initialize an empty tibble for storing adjusted p-values
  adjusted.p <- tibble(
    term = character(),
    p_adjust = numeric()
  )
  
  # Loop through each term in the model
  for(trm in vartable$term){
    # Get the p-value and frequency of the current term
    p <- vartable %>%
      filter(term == trm) %>%
      select(p) %>%
      pull()
    
    freq <- vartable %>%
      filter(term == trm) %>%
      select(freq) %>%
      pull()
    
    # Adjust the p-value using the specified method
    p_adj <- p.adjust(p = p,
                      method = mtd,
                      n = freq)
    
    # Add the adjusted p-value to the table
    adjusted.p <- adjusted.p %>%
      add_row(
        term = trm,
        p_adjust = p_adj
      )
  }
  
  # Join the adjusted p-values to the main table
  vartable <- vartable %>%
    left_join(adjusted.p, by = "term") %>%
    select(term, p, p_adjust)  # Select desired columns
  
  # Return the final table
  return(vartable)
}