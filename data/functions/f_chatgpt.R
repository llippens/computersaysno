# Based on https://gist.github.com/rasmusab/c25badf55f5dacee14ab13834798d3ef

if (!require("pacman")) install.packages("pacman")
pacman::p_load(httr, jsonlite,
               install = TRUE,
               update = FALSE)

chatGPT <- function(prompt,
                    role = "user",
                    modelname = "gpt-3.5-turbo-0613",
                    temperature = 0,
                    max_tokens = 10^3,
                    apiKey = Sys.getenv("chatGPT_API_KEY")) {
  
  if(nchar(apiKey)<1) {
    apiKey <- readline("Paste your API key here: ")
    Sys.setenv(chatGPT_API_KEY = apiKey)
  }
  
  response <- POST(
    url = "https://api.openai.com/v1/chat/completions", 
    add_headers(Authorization = paste("Bearer", apiKey)),
    content_type_json(),
    encode = "json",
    body = list(
      model = modelname,
      temperature = temperature,
      messages = list(list(
        role = role, 
        content = prompt
      )),
      max_tokens = max_tokens
    )
  )
  
  content(response)
}