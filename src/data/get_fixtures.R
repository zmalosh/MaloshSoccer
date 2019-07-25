source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_fixtures_by_league <- function(leagueId){
  url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/league/', leagueId)
  localPath <- paste0(getwd(), '/data/raw/fixtures_', str_pad(leagueId, 4, pad = '0'), '.csv')
  
  if(file.exists(localPath)){
    teams <- read_csv(localPath)
    return (teams)
  }
  json <- get_api_football_json_from_url(url)
  fixtures <- json$fixtures
  fixtures$homeTeamId <- fixtures$homeTeam$team_id
  fixtures$homeTeamName <- fixtures$homeTeam$team_name
  fixtures$awayTeamId <- fixtures$awayTeam$team_id
  fixtures$awayTeamName <- fixtures$awayTeam$team_name
  fixtures$homeTeam <- NULL
  fixtures$awayTeam <- NULL
  fixtures$scoreHalfTime <- fixtures$score$halftime
  fixtures$scoreFullTime <- fixtures$score$fulltime
  fixtures$scoreExtraTime <- fixtures$score$extratime
  fixtures$scorePenalty <- fixtures$score$penalty
  fixtures$score <- NULL
  write_csv(fixtures, path = localPath)
  return (fixtures)
}

get_all_fixtures <- function(){
  source('src/data/get_leagues.R')
  leagues <- get_leagues()
  fixtures <- get_fixtures_by_league(1)
  row.names(fixtures) <- NULL
  for(i in seq(from = 2, to = nrow(leagues), by = 1)){
    league <- leagues[i,]
    leagueId <- league$league_id
    leagueFixtures <- get_fixtures_by_league(leagueId)
    row.names(leagueFixtures) <- NULL
    fixtures <- rbind(fixtures, leagueFixtures)
  }
  return (fixtures)
}

get_detailed_fixture <- function(fixtureId){
  url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/id/', fixtureId)
  localPath <- paste0(getwd(), '/data/raw/fixtureDetails_', str_pad(fixtureId, 7, pad = '0'), '.Rda')
  
  if(file.exists(localPath)){
    print(localPath)
    fixture <- readRDS(localPath)
    return (fixture)
  }
  json <- get_api_football_json_from_url(url)
  fixture <- json$fixtures
  if(!is.null(fixture)){
    saveRDS(fixture, localPath)
  }
  return (fixture)
}

get_all_detailed_fixtures <- function(){
  allFixtureSummaries <- get_all_fixtures() 
  allFixtureSummaries <- allFixtureSummaries %>% arrange(desc(event_date), fixture_id)
  allFixtureSummaries <- allFixtureSummaries %>% filter(status %in% c('Match Finished', 'Match Abandoned', 'Match Suspended'))
  fixtureSummary <- allFixtureSummaries[1,]
  fixtureId <- fixtureSummary$fixture_id
  fixtures <- get_detailed_fixture(fixtureId)
  row.names(fixtures) <- NULL
  for(i in seq(from = 2, to = nrow(allFixtureSummaries), by = 1)){
    print(i)
    fixtureSummary <- allFixtureSummaries[i,]
    fixtureId <- fixtureSummary$fixture_id
    fixture <- get_detailed_fixture(fixtureId)
    row.names(fixture) <- NULL
    #fixtures <- rbind(fixtures, fixture)
  }
  return (fixtures)
}
