source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_teams_by_league <- function(leagueId, allowCache = TRUE){
	localPath <- paste0(getwd(), '/data/raw/teams/teams_', str_pad(leagueId, 4, pad = '0'), '.csv')

	if(allowCache && file.exists(localPath)){
		cols <- cols(
			TeamId = col_double(),
			TeamName = col_character(),
			TeamCode = col_character(),
			Country = col_character(),
			LogoUrl = col_character(),
			FoundedYear = col_double(),
			VenueName = col_character(),
			VenueSurface = col_character(),
			VenueAddress = col_character(),
			VenueCity = col_character(),
			VenueCapacity = col_double(),
			LeagueId = col_double()
		)
		teams <- read_csv(localPath, col_types = cols)
		return (teams)
	}

	inputLeagueId <- leagueId
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/teams/league/', leagueId)
	json <- get_api_football_json_from_url(url)
	teams <- json$teams
	teams <- teams %>%
		mutate(TeamId = team_id,
			   TeamName = name,
			   TeamCode = code,
			   LogoUrl = logo,
			   Country = country,
			   FoundedYear = founded,
			   VenueName = venue_name,
			   VenueSurface = venue_surface,
			   VenueAddress = venue_address,
			   VenueCity = venue_city,
			   VenueCapacity = venue_capacity,
			   LeagueId = inputLeagueId
		) %>%
		select(TeamId, TeamName, TeamCode, Country,
			   LogoUrl, FoundedYear, VenueName,
			   VenueSurface, VenueAddress, VenueCity,
			   VenueCapacity, LeagueId)
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
