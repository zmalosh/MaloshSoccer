source('requirements.R')
source('configuration/project_settings.R')

get_api_football_json_from_url <- function(url){
  settings <- get_project_settings()

  key <- settings$apiFootballKey
  headers <- c(key)
  names(headers) <- 'X-RapidAPI-Key'

  response <- httr::GET(url, add_headers(.headers = headers))
  rawJson <- httr::content(response, as = 'text')
  json <- jsonlite::fromJSON(rawJson)$api
  return (json)
}