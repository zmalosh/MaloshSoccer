source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_teams_by_league <- function(leagueId, allowCache = TRUE){
  url <- paste0('https://api-football-v1.p.rapidapi.com/v2/teams/league/', leagueId)
  localPath <- paste0(getwd(), '/data/raw/teams/teams_', str_pad(leagueId, 4, pad = '0'), '.csv')

  if(allowCache && file.exists(localPath)){
   teams <- read_csv(localPath)
   return (teams)
  }
  json <- get_api_football_json_from_url(url)
  teams <- json$teams
  teams$league_id <- leagueId
  write_csv(teams, localPath)
  return (teams)
}

get_all_teams <- function(allowCache = TRUE){
  source('src/data/get_leagues.R')
  leagues <- get_leagues()
  teams <- get_teams_by_league(1, allowCache = allowCache)
  for(i in seq(from = 2, to = nrow(leagues), by = 1)){
    league <- leagues[i,]
    leagueId <- league$league_id
    leagueTeams <- get_teams_by_league(leagueId, allowCache = allowCache)
    teams <- rbind(teams, leagueTeams)
  }
  return (teams)
}
