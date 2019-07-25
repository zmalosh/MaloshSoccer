source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_leagues <- function(){
  localPath <- 'data/raw/leagues.csv'
  cols <- cols(
    league_id = col_double(),
    name = col_character(),
    country = col_character(),
    country_code = col_character(),
    season = col_double(),
    season_start = col_date(format = ""),
    season_end = col_date(format = ""),
    logo = col_character(),
    flag = col_character(),
    standings = col_double(),
    is_current = col_double()
  )
  
  if(file.exists(localPath)){
    leagues <- read_csv(localPath, col_types = cols)
    return (leagues)
  }
  
  source('src/data/get_api_football_json_from_url.R')
  
  url <- 'https://api-football-v1.p.rapidapi.com/v2/leagues'
  
  json <- get_api_football_json_from_url(url)
  leagues <- json$leagues
  leagues <- leagues %>%
    transform(
      LeagueId = league_id,
      LeagueName = name,
      Country = country,
      CountryCode = country_code,
      Season = season,
      SeasonStartDate = season_start,
      SeasonEndDate = season_end,
      LogoUrl = logo,
      FlagUrl = flag,
      HasStandings = standings == 1,
      IsCurrentSeason = is_current == 1
    ) %>%
    select(LeagueId, LeagueName, Country, CountryCode,
           Season, SeasonStartDate, SeasonEndDate,
           LogoUrl, FlagUrl, HasStandings, IsCurrentSeason)
  write_csv(leagues, localPath)
  return (leagues)
}
