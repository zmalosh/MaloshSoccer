source('requirements.r')
source('src/data/get_api_football_json_from_url.R')

get_roster_by_team <- function(teamId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/players/team/', teamId)
	localPath <- paste0(getwd(), '/data/raw/rosters/roster_', str_pad(teamId, 6, pad = '0'), '.csv')

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath)){
			roster <- read_csv(localPath)
			if(nrow(roster) == 0){
				roster <- NULL
			}
			return(roster)
		}
	}
	json <- get_api_football_json_from_url(url)
	roster <- json$players
	if(length(roster) == 0){
		write_csv(data.frame(), localPath)
		return(NULL)
	}
	roster$shotsTotal <- roster$shots$total
	roster$shotsOnTarget <- roster$shots$on
	roster$goalsTotal <- roster$goals$total
	roster$goalsConceded <- roster$goals$conceded
	roster$goalAssists <- roster$goals$assists
	roster$passesTotal <- roster$passes$total
	roster$passAcc <- roster$passes$accuracy
	roster$blocks <- roster$tackles$blocks
	roster$interceptions <- roster$tackles$interceptions
	roster$tackles <- roster$tackles$total
	roster$duelsTotal <- roster$duels$total
	roster$duelsWon <- roster$duels$won
	roster$dribblesTotal <- roster$dribbles$attempts
	roster$dribblesSuccessful <- roster$dribbles$success
	roster$foulsCommitted <- roster$fouls$committed
	roster$foulsSuffered <- roster$fouls$drawn
	roster$yellowCards <- roster$cards$yellow
	roster$yellowRedCards <- roster$cards$yellowred
	roster$redCards <- roster$cards$red
	roster$penaltiesMade <- roster$penalty$success
	roster$penaltiesMissed <- roster$penalty$missed
	roster$penaltiesSaved <- roster$penalty$saved
	roster$gamesPlayed <- roster$games$appearences
	roster$minutesPlayed <- roster$games$minutes_played
	roster$gamesStarted <- roster$games$lineups
	roster$subbedIn <- roster$substitutes$`in`
	roster$subbedOut <- roster$substitutes$out
	roster$subOnBench <- roster$substitutes$bench
	roster$shots <- NULL
	roster$goals <- NULL
	roster$passes <- NULL
	roster$tackles <- NULL
	roster$duels <- NULL
	roster$dribbles <- NULL
	roster$fouls <- NULL
	roster$cards <- NULL
	roster$penalty <- NULL
	roster$games <- NULL
	roster$substitutes <- NULL
	if(allowCache){
		write_csv(roster, localPath)
	}
	return (roster)
}

get_all_rosters <- function(allowCache = TRUE){
  source('src/data/get_teams.R')
  teams <- get_all_teams()
  team <- teams[1,]
  teamId <- team$team_id
  # IF ROSTERS FOR FIRST TEAM IS NULL, THIS WILL NOT BE GOOD
  rosters <- get_roster_by_team(teamId, allowCache = allowCache)
  row.names(rosters) <- NULL
  for(i in seq(from = 2, to = nrow(teams), by = 1)){
    print(i)
    team <- teams[i,]
    teamId <- team$team_id
    roster <- get_roster_by_team(teamId, allowCache = allowCache)
    #if(!is.null(roster)){
      #row.names(roster) <- NULL
      #rosters <- rbind(rosters, roster)
    #}
  }
  return (rosters)
}

rosters <- get_all_rosters()
