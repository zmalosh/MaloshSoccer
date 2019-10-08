source('requirements.R')
source('src/data/get_api_football_json_from_url.R')

get_fixtures_by_league <- function(leagueId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/league/', leagueId)
	localPath <- paste0(getwd(), '/data/raw/leagueFixtures/fixtures_', str_pad(leagueId, 4, pad = '0'), '.csv')
	cacheExpirationMin <- 15

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath) && (file.info(localPath)$ctime + (cacheExpirationMin * 60)) > Sys.time()){
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
	if(allowCache){
		write_csv(fixtures, path = localPath)
	}
	return (fixtures)
}

get_all_fixtures <- function(allowCache = TRUE){
	source('src/data/get_leagues.R')
	leagues <- get_leagues()
	league <- leagues[1,]
	fixtures <- get_fixtures_by_league(league$LeagueId)
	row.names(fixtures) <- NULL
	for(i in seq(from = 2, to = nrow(leagues), by = 1)){
		league <- leagues[i,]
		leagueId <- league$LeagueId
		leagueFixtures <- get_fixtures_by_league(leagueId)
		row.names(leagueFixtures) <- NULL
		fixtures <- rbind(fixtures, leagueFixtures)
	}
	return (fixtures)
}

get_detailed_fixture <- function(fixtureId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/fixtures/id/', fixtureId)
	localPath <- paste0('data/raw/fixtureDetails/fixtureDetails_', str_pad(fixtureId, 7, pad = '0'), '.Rda')
	cacheExpirationMin <- 24 * 60

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath) && (file.info(localPath)$ctime + (cacheExpirationMin * 60)) > Sys.time()){
			fixture <- readRDS(localPath)
			return (fixture)
		}
	}

	json <- get_api_football_json_from_url(url)
	rawFixture <- json$fixtures

	lineups1 <- rawFixture$lineups[,1]
	lineups2 <- rawFixture$lineups[,2]

	formation1 <- as.character(lineups1$formation)
	formation2 <- as.character(lineups2$formation)
	coach1 <- ifelse(is.null(lineups1$coach), NA, as.character(lineups1$coach))
	coach2 <- ifelse(is.null(lineups2$coach), NA, as.character(lineups2$coach))

	events <- data.frame(rawFixture$events) %>%
		mutate(Minute = elapsed,
			   TeamId = team_id,
			   PlayerId = player_id,
			   PlayerName = trimws(str_replace(player, '\\(o.g.\\)', '')),
			   IsOwnGoal = str_detect(player, '\\(o.g.\\)'),
			   EventType = ifelse(type == 'subst', 'Sub', type),
			   EventDetail = str_replace(ifelse(IsOwnGoal, 'Own Goal', detail), ' ', '')) %>%
		select(Minute, TeamId, PlayerId, PlayerName, EventType, EventDetail)

	lineups <- rbind(
		data.frame(rawFixture$lineups[,1][,'startXI']) %>%
			mutate(Role = 'Starter',
				   TeamId = team_id,
				   PlayerId = player_id,
				   PlayerName = player,
				   JerseyNumber = number,
				   Position = pos) %>%
			select(TeamId, Role, PlayerId, PlayerName, JerseyNumber, Position),
		data.frame(rawFixture$lineups[,1][,'substitutes']) %>%
			mutate(Role = 'Starter',
				   TeamId = team_id,
				   PlayerId = player_id,
				   PlayerName = player,
				   JerseyNumber = number,
				   Position = pos) %>%
			select(TeamId, Role, PlayerId, PlayerName, JerseyNumber, Position),
		data.frame(rawFixture$lineups[,2][,'startXI']) %>%
			mutate(Role = 'Starter',
				   TeamId = team_id,
				   PlayerId = player_id,
				   PlayerName = player,
				   JerseyNumber = number,
				   Position = pos) %>%
			select(TeamId, Role, PlayerId, PlayerName, JerseyNumber, Position),
		data.frame(rawFixture$lineups[,2][,'substitutes']) %>%
			mutate(Role = 'Starter',
				   TeamId = team_id,
				   PlayerId = player_id,
				   PlayerName = player,
				   JerseyNumber = number,
				   Position = pos) %>%
			select(TeamId, Role, PlayerId, PlayerName, JerseyNumber, Position)
	)

	if(is.null(rawFixture$statistics) || is.na(rawFixture$statistics)){
		teamStats <- NULL
	} else {
		teamStats <- suppressWarnings({
			data.frame(StatName = names(rawFixture$statistics),
					   Home = as.numeric(lapply(rawFixture$statistics, function(df){df$home})),
					   Away = as.numeric(lapply(rawFixture$statistics, function(df){df$away})))
		})
	}

	summary <- rawFixture %>%
		mutate(FixtureId = fixture_id,
			   LeagueId = league_id,
			   GameDate = event_date,
			   EventTimestamp = event_timestamp,
			   FirstHalfStart = firstHalfStart,
			   SecondHalfStart = secondHalfStart,
			   Round = round,
			   Status = status,
			   StatusShort = statusShort,
			   TimeElapsed = elapsed,
			   Venue = venue,
			   Referee = as.character(referee),
			   HomeTeamId = homeTeam$team_id,
			   HomeTeamName = homeTeam$team_name,
			   HomeTeamLogo = homeTeam$logo,
			   HomeScore = goalsHomeTeam,
			   AwayTeamId = awayTeam$team_id,
			   AwayTeamName = awayTeam$team_name,
			   AwayTeamLogo = awayTeam$logo,
			   AwayScore = goalsAwayTeam,
			   ScoreHalfTime = as.character(score$halftime),
			   ScoreFullTime = as.character(score$fulltime),
			   HomeFormation = formation1,
			   AwayFormation = formation2,
			   HomeCoach = coach1,
			   AwayCoach = coach2,
			   GameEvents = events) %>%
		select(FixtureId, LeagueId, GameDate, StatusShort,
			   HomeTeamId, HomeTeamName, AwayTeamId, AwayTeamName,
			   HomeScore, AwayScore, ScoreHalfTime, ScoreFullTime,
			   TimeElapsed, Referee, Venue, Round, Status,
			   EventTimestamp, FirstHalfStart, SecondHalfStart,
			   HomeTeamLogo, AwayTeamLogo, HomeFormation, AwayFormation,
			   HomeCoach, AwayCoach)

	fixture <- list(
		Summary = summary,
		GameEvents = events,
		Lineups = lineups,
		TeamStats = teamStats
	)

	if(allowCache && !is.null(fixture)){
		saveRDS(fixture, localPath)
	}
	return (fixture)
}

get_detailed_fixtures_by_league <- function(leagueId, allowCache = TRUE){
	fixtureSummaries <- get_fixtures_by_league(leagueId, allowCache = allowCache)
	fixtureSummaries <- fixtureSummaries %>%
		filter(Status %in% c('Match Finished', 'Match Abandoned', 'Match Suspended')) %>%
		arrange(desc(GameDate), FixtureId)
	fixtureSummary <- fixtureSummaries[1,]
	fixtureId <- fixtureSummary$FixtureId
	detailedFixture <- get_detailed_fixture(fixtureId, allowCache = allowCache)

	fixtureInfos <- data.frame(detailedFixture$Summary)
	fixtureLineups <- data.frame(FixtureId = fixtureId, detailedFixture$Lineups)
	fixtureTeamStats <- data.frame(FixtureId = fixtureId, detailedFixture$TeamStats)
	fixtureGameEvents <- data.frame(FixtureId = fixtureId, detailedFixture$GameEvents)

	rownames(fixtureInfos) <- NULL
	rownames(fixtureLineups) <- NULL
	rownames(fixtureTeamStats) <- NULL
	rownames(fixtureGameEvents) <- NULL

	for(i in seq(from = 2, to = nrow(fixtureSummaries), by = 1)){
		print(i)
		fixtureSummary <- fixtureSummaries[i,]
		fixtureId <- fixtureSummary$FixtureId
		detailedFixture <- get_detailed_fixture(fixtureId, allowCache = allowCache)

		fixtureInfos <- rbind(fixtureInfos, data.frame(detailedFixture$Summary))
		if(!is.null(detailedFixture$Lineups)){
			fixtureLineups <- rbind(fixtureLineups, data.frame(FixtureId = fixtureId, detailedFixture$Lineups))
		}
		if(!is.null(detailedFixture$TeamStats)){
			fixtureTeamStats <- rbind(fixtureTeamStats, data.frame(FixtureId = fixtureId, detailedFixture$TeamStats))
		}
		if(!is.null(detailedFixture$GameEvents)){
			fixtureGameEvents <- rbind(fixtureGameEvents, data.frame(FixtureId = fixtureId, detailedFixture$GameEvents))
		}

		rownames(fixtureInfos) <- NULL
		rownames(fixtureLineups) <- NULL
		rownames(fixtureTeamStats) <- NULL
		rownames(fixtureGameEvents) <- NULL
	}

	result <- list(
		Summaries = fixtureInfos,
		Lineups = fixtureLineups,
		TeamStats = fixtureTeamStats,
		GameEvents = fixtureGameEvents
	)
	return(result)
}

get_all_detailed_fixtures <- function(allowCache = TRUE){
	allFixtureSummaries <- get_all_fixtures()
	allFixtureSummaries <- allFixtureSummaries %>%
		filter(Status %in% c('Match Finished', 'Match Abandoned', 'Match Suspended')) %>%
		arrange(desc(GameDate), fixture_id)
	fixtureSummary <- allFixtureSummaries[1,]
	fixtureId <- fixtureSummary$FixtureId
	fixtures <- get_detailed_fixture(fixtureId, allowCache = allowCache)
	row.names(fixtures) <- NULL
	for(i in seq(from = 2, to = nrow(allFixtureSummaries), by = 1)){
		print(i)
		fixtureSummary <- allFixtureSummaries[i,]
		fixtureId <- fixtureSummary$FixtureId
		fixture <- get_detailed_fixture(fixtureId, allowCache = allowCache)
		row.names(fixture) <- NULL
		#fixtures <- rbind(fixtures, fixture)
	}
	return (fixtures)
}
