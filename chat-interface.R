### This is example code used in 2023 to retrieve responses from ChatGPT
### Note that at present, in 2025, there are far better options to achieve this
### One solution would be https://ellmer.tidyverse.org/, specifically the batch_chat function

# Load required libraries, install them if needed
if (!require("pacman")) install.packages("pacman")
pacman::p_load(here, readr, dplyr, stringr, janitor, openxlsx, progress,
               install = TRUE,
               update = FALSE)


# Define directories for data retrieval and storage
gen.dir <- "data"
prt.dir <- file.path(gen.dir, "prompts")
tmp.dir <- file.path(gen.dir, "temp")
fns.dir <- file.path(gen.dir, "functions")


# Load input data and API key for ChatGPT
load(file.path(here(), gen.dir, "input", "chatgpt_input.RData"))
apiKey <- read_lines(file.path(here(), gen.dir, "chatgpt_api_key.txt")) # Make sure to provide your own API key


# Load functions
source(file.path(here(), fns.dir, "f_chatgpt.R"))
source(file.path(here(), fns.dir, "f_save.R"))


# Prompts
## Base prompts
prompt_c <- read_file(file.path(here(), prt.dir, "prompt_c_none.txt"))


# Prepare messages for ChatGPT
prompt <- prompt_c

data <- data %>%
  mutate(message = str_c("Vacature:",
                         "\n",
                         vac_scraped_text,
                         "\n\n",
                         "Kandidaat:",
                         "\n",
                         can_name,
                         "\n\n",
                         "Curriculum Vitae:",
                         "\n",
                         can_cv),
         rownumber = c(1:nrow(data))) %>%
  relocate(rownumber)


# Cost calculation
ppt <- .002 # Price per 1K tokens in USD
ppw <- ppt / 750 # Price per word in USD
text <- str_c(prompt, data$message)
wordcount <- str_count(text, "\\S+")
markup <- 1.45 # Markup based on data acquisition with 34K observations
cost <- wordcount %>% sum() * ppw * markup


# Check if the cost exceeds the set limit
costlimit <- 75
if (cost > costlimit) {
  # If the cost is too high, print a message and stop the script
  message(paste0("Cost equals ",round(cost, 4), " USD, which is greater than ", costlimit, " USD."))
  stop("Stopping script execution")
}

# If the cost is within limits, continue executing the script
message(paste0("Estimated total cost = ", round(cost, 4), " USD. ",
               "Script execution continuing..."))


# Initialize an empty tibble for storing output
output <- tibble(
  uid = character(),
  prompt = character(),
  bias_instruction = character(),
  temperature = numeric(),
  gpt_id = character(),
  gpt_created = numeric(),
  gpt_model = character(),
  gpt_usage_prompt = numeric(),
  gpt_usage_completion = numeric(),
  gpt_usage_total = numeric(),
  output = character()
)
error_ids <- c()


# Randomisation
set.seed(8888)
n <- 1000
prompt.motivation.sample <- sample(data$uid,
                                   size = ifelse(nrow(data) < n,
                                                 nrow(data)/10, 
                                                 n),
                                   replace = FALSE)
set.seed(88888)
n <- 1000
prompt.bias.sample <- sample(data$uid,
                             size = ifelse(nrow(data) < n,
                                           nrow(data)/10, 
                                           n),
                             replace = FALSE)


# Resume in case of unknown error
#data <- data %>% filter(rownumber %in% c(1:34560)) # format: c(error_row+1:end_row)


# Load the progress bar
source(file.path(here(), fns.dir, "f_progressbar.R"))


set.seed(88888)

# Iterate through each uid in the data, sending messages to ChatGPT and storing the responses
for (id in data$uid) {
  msg <- data %>%
    filter(uid == id) %>%
    select(message) %>%
    pull()
  
  if(id %in% prompt.motivation.sample){
    prompt_gpt <- prompt_mot
    max_tokens <- 250
    
  } else {
    prompt_gpt <- prompt
    max_tokens <- 25
  }
  
  bias_instruction <- "Unspecified"
  
  msg <- str_c(prompt_gpt, msg, sep = "\n\n")
  
  temp <- sample(x = seq(0, 1.5, .25),
                 prob = c(0.60, rep((1-0.60-0.05)/4, 4), rep(0.05/2, 2)),
                 size = 1, 
                 replace = FALSE)
  
  repeat{
    tryCatch({
      # Generate a response
      gpt_response <-
        chatGPT(prompt = msg,
                role = "user",
                modelname = "gpt-3.5-turbo-0613", # see https://platform.openai.com/docs/models/gpt-4
                temperature = temp,
                max_tokens = max_tokens,
                apiKey = apiKey
        )
      
    }, error = function(e) {
      # First check if the error message contains "overloaded"
      if (grepl("overloaded", e$message)) {
        cat("Server overload error occurred for id:", id, ". Pausing for 20 seconds.\n")
        Sys.sleep(20)
        
        # Then check if the error is related to 502 Bad Gateway
      } else if (grepl("502", e$message)) { 
        cat("502 Bad Gateway error occurred for id:", id, ". Pausing for 10 seconds.\n")
        Sys.sleep(10)
        
        # Then check if the error message contains "Rate limit"
      } else if (grepl("Rate limit", e$message)) {
        cat("Rate limit error occurred for id:", id, ". Pausing for 30 seconds.\n")
        Sys.sleep(30)
        
        # Then check if the error message contains "processing your request"
      } else if (grepl("processing your request", e$message)) {
        cat("Request processing error occurred for id:", id, ". Pausing for 10 seconds.\n")
        Sys.sleep(10)
        
        # Finally handle other types of errors
      } else {
        rownbr <- data %>%
          filter(uid == id) %>%
          select(rownumber) %>%
          pull()
        
        cat("Other error occurred for id:", id, "(rownbr:", rownbr, ")", "\n")
        cat("Error message:", e$message, "\n")
        error_ids <<- append(error_ids, id)
        break  # Break the repeat loop
      }
    })
    
    if (!is.null(gpt_response)) {
      output <- output %>%
        add_row(
          uid = id,
          prompt = prompt_gpt,
          bias_instruction = bias_instruction,
          temperature = temp,
          gpt_id = gpt_response$id,
          gpt_created = gpt_response$created,
          gpt_model = gpt_response$model,
          gpt_usage_prompt = gpt_response$usage$prompt_tokens,
          gpt_usage_completion = gpt_response$usage$completion_tokens,
          gpt_usage_total = gpt_response$usage$total_tokens,
          output = gpt_response$choices[[1]]$message$content
        )
      
      break  # Break the repeat loop
    }
  }
  
  # Update the progress bar
  pb$tick()
  
  # Save the temporary output every 1000 iterations
  i <- data %>%
    filter(uid == id) %>%
    select(rownumber) %>%
    pull()
  
  if (i %% 50 == 0) {
    message(paste0("Saving temporary output at iteration ", i, "..."))
    save_output(output, paste0("chatgpt_output_temp_at_", i, ".RData"))
  }
}


# Join the output to the original data
data <- data %>%
  left_join(output, by = "uid") %>%
  mutate(score = as.numeric(str_extract(output, "\\d+")),
         motivation = str_replace(output, "Score: \\d+\\n\\n", ""),
         motivation = ifelse(motivation == as.character(score), NA_character_, motivation)) %>%
  relocate(prompt, .before = message)


# Save the final dataset as an RData file
save(data, file = file.path(here(), gen.dir, "chatgpt.RData"))
save(output, file = file.path(here(), gen.dir, "chatgpt_output.RData"))
save(error_ids, file = file.path(here(), gen.dir, "error_ids.RData"))

# Save to xlsx
write.xlsx(data,
           file.path(here(), gen.dir, "chatgpt.xlsx"),
           overwrite = TRUE)

write.xlsx(output,
           file.path(here(), gen.dir, "chatgpt_output.xlsx"),
           overwrite = TRUE)


# Print the final estimated cost
message(paste0("Estimated total cost = ", round(cost, 4), " USD."))
