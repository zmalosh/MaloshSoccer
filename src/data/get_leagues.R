source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_leagues <- function(){
  localPath <- 'data/raw/leagues.csv'
  if(file.exists(localPath)){
    leagues <- read_csv(localPath)
    return (leagues)
  }
  
  source('src/data/get_api_football_json_from_url.R')
  
  url <- 'https://api-football-v1.p.rapidapi.com/v2/leagues'
  
  json <- get_api_football_json_from_url(url)
  leagues <- json$leagues
  write_csv(leagues, localPath)
  return (leagues)
}
