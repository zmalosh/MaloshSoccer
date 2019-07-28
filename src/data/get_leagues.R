source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_leagues <- function(allowCache = TRUE){
	localPath <- 'data/raw/leagues.csv'

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath)){
			cols <- cols(
				LeagueId = col_double(),
				LeagueName = col_character(),
				Country = col_character(),
				CountryCode = col_character(),
				Season = col_double(),
				SeasonStartDate = col_date(format = ""),
				SeasonEndDate = col_date(format = ""),
				LogoUrl = col_character(),
				FlagUrl = col_character(),
				HasStandings = col_logical(),
				IsCurrentSeason = col_logical()
			)
			leagues <- read_csv(localPath, col_types = cols)
			return (leagues)
		}
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
	if(allowCache){
		write_csv(leagues, localPath)
	}
	return (leagues)
}
