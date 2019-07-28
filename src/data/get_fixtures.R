source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_fixtures_by_league <- function(leagueId, allowCache = TRUE){
  url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/league/', leagueId)
  localPath <- paste0(getwd(), '/data/raw/leagueFixtures/fixtures_', str_pad(leagueId, 4, pad = '0'), '.csv')

  if(allowCache && file.exists(localPath)){
    cols <- cols(
      FixtureId = col_double(),
      LeagueId = col_double(),
      GameDate = col_character(),
      StatusShort = col_character(),
      HomeTeamId = col_double(),
      HomeTeamName = col_character(),
      AwayTeamId = col_double(),
      AwayTeamName = col_character(),
      HomeScore = col_double(),
      AwayScore = col_double(),
      ScoreHalfTime = col_character(),
      ScoreFullTime = col_character(),
      # ScoreExtraTime = col_character(),
      # ScorePenalty = col_character(),
      TimeElapsed = col_double(),
      Referee = col_character(),
      Venue = col_character(),
      Round = col_character(),
      Status = col_character(),
      EventTimestamp = col_double(),
      FirstHalfStart = col_double(),
      SecondHalfStart = col_double(),
      HomeTeamLogo = col_character(),
      AwayTeamLogo = col_character()
    )
    fixtures <- read_csv(localPath, col_types = cols)
    return (fixtures)
  }

  json <- get_api_football_json_from_url(url)
  fixtures <- json$fixtures
  fixtures$FixtureId <- fixtures$fixture_id
  fixtures$LeagueId <- fixtures$league_id
  fixtures$GameDate <- fixtures$event_date
  fixtures$StatusShort <- fixtures$statusShort
  fixtures$HomeTeamId <- fixtures$homeTeam$team_id
  fixtures$HomeTeamName <- fixtures$homeTeam$team_name
  fixtures$AwayTeamId <- fixtures$awayTeam$team_id
  fixtures$AwayTeamName <- fixtures$awayTeam$team_name
  fixtures$HomeScore <- fixtures$goalsHomeTeam
  fixtures$AwayScore <- fixtures$goalsAwayTeam
  fixtures$ScoreHalfTime <- as.character(fixtures$score$halftime)
  fixtures$ScoreFullTime <- as.character(fixtures$score$fulltime)
  # fixtures$ScoreExtraTime <- fixtures$score$extratime
  # fixtures$ScorePenalty <- fixtures$score$penalty
  fixtures$TimeElapsed <- fixtures$elapsed
  fixtures$Referee <- fixtures$referee
  fixtures$Venue <- fixtures$venue
  fixtures$Round <- fixtures$round
  fixtures$Status <- fixtures$status
  fixtures$EventTimestamp <- fixtures$event_timestamp
  fixtures$FirstHalfStart <- fixtures$firstHalfStart
  fixtures$SecondHalfStart <- fixtures$secondHalfStart
  fixtures$HomeTeamLogo <- fixtures$homeTeam$logo
  fixtures$AwayTeamLogo <- fixtures$awayTeam$logo
  fixtures$fixture_id <- NULL
  fixtures$league_id <- NULL
  fixtures$homeTeam <- NULL
  fixtures$awayTeam <- NULL
  fixtures$score <- NULL
  fixtures$referee <- NULL
  fixtures$venue <- NULL
  fixtures$event_timestamp <- NULL
  fixtures$firstHalfStart <- NULL
  fixtures$secondHalfStart <- NULL
  fixtures$event_date <- NULL
  fixtures$round <- NULL
  fixtures$status <- NULL
  fixtures$statusShort <- NULL
  fixtures$elapsed <- NULL
  fixtures$goalsAwayTeam <- NULL
  fixtures$goalsHomeTeam <- NULL
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
