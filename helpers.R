getLeagueOptions <- function(season, leagues){
	if(season == -1){
		return(list('SELECT LEAGUE...' = -1))
	}

	seasonLeagues <- leagues %>% filter(Season == season) %>% arrange(Country, LeagueName)
	leagueOptions <- as.list(seasonLeagues$LeagueId)
	setNames(leagueOptions, paste0(seasonLeagues$Country, ' - ', seasonLeagues$LeagueName))
	return(leagueOptions)
}
