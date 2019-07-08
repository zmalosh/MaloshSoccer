source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_teams_by_league <- function(leagueId){
  url <- paste0('https://api-football-v1.p.rapidapi.com/v2/teams/league/', leagueId)
  localPath <- paste0('data/raw/teams_', leagueId, '.Rda')
  
  if(file.exists(localPath)){
   teams <- readRDS(localPath)
   return (teams)
  }
  json <- get_api_football_json_from_url(url)
  teams <- json$teams
  teams$league_id <- leagueId
  saveRDS(teams, localPath)
  return (teams)
}

get_all_teams <- function(){
  source('src/data/get_leagues.R')
  leagues <- get_leagues()
  teams <- get_teams_by_league(1)
  for(i in seq(from = 2, to = nrow(leagues), by = 1)){
    league <- leagues[i,]
    leagueId <- league$league_id
    leagueTeams <- get_teams_by_league(leagueId)
    teams <- rbind(teams, leagueTeams)
  }
  return (teams)
}
