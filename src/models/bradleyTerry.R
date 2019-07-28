source('requirements.R')

setup_games <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores){
	g <- data.frame(GameId = gameIds,
					HomeTeamId = homeTeamIds,
					AwayTeamId = awayTeamIds,
					HomeScore = homeScores,
					AwayScore = awayScores)

	# SCORES SHOULD EITHER BE BOTH NA OR NEITHER NA. GET RID OF BAD GAMES
	g <- g %>% filter(!((is.na(HomeScore) & !is.na(AwayScore)) | (!is.na(HomeScore) & is.na(AwayScore))))

	# MAKE DRAWS INTO A 1-GOAL WIN FOR HOME AND A 1-GOAL WIN FOR AWAY (2 REPLACEMENT GAMES)
	draws <- g %>% filter(!is.na(AwayScore) & !is.na(HomeScore) & AwayScore == HomeScore)
	drawHomeWinReplacement <- data.frame(GameId = draws$GameId,
										 HomeTeamId = draws$HomeTeamId,
										 AwayTeamId = draws$AwayTeamId,
										 HomeScore = draws$HomeScore + 1,
										 AwayScore = draws$AwayScore)
	drawAwayWinReplacement <- data.frame(GameId = draws$GameId,
										 HomeTeamId = draws$HomeTeamId,
										 AwayTeamId = draws$AwayTeamId,
										 HomeScore = draws$HomeScore,
										 AwayScore = draws$AwayScore + 1)
	g <- g %>% filter(is.na(AwayScore) | is.na(HomeScore) | AwayScore != HomeScore)
	g <- rbind(g, drawHomeWinReplacement, drawAwayWinReplacement)

	g <- g %>%
		mutate(HomeMarginOfVictory = HomeScore - AwayScore,
			   GameResult = ifelse(HomeMarginOfVictory > 0, 1, 0))
	return(g)
}

logisticFunction <- function(homeFieldAdvantage, homeTeamStrength, awayTeamStrength){
	return(1 / (1 + (exp(-(homeFieldAdvantage + homeTeamStrength - awayTeamStrength)))))
}

f <- function(p, teamIds, games){
	x <- p

	teamStrengths <- x[1:(length(x)-1)]
	names(teamStrengths) <- as.character(teamIds)
	homeFieldAdvantage <- x[length(x)]

	g <- games %>%
		mutate(HomeStrength = teamStrengths[as.character(HomeTeamId)],
			   AwayStrength = teamStrengths[as.character(AwayTeamId)],
			   LogisticVal = logisticFunction(homeFieldAdvantage, HomeStrength, AwayStrength),
			   Result = ifelse(GameResult == 1, LogisticVal, 1 - LogisticVal)
		)
	logLikelihood <- sum(log(g$Result))
	return(-1 * logLikelihood)
}

get_team_strengths <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores){
	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores)
	finalGames <- g %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
	teamIds <- as.character(sort(unique(c(finalGames$HomeTeamId, finalGames$AwayTeamId))))
	p <- rep(1, times = length(teamIds) + 1)
	strengthOptimization <- nlm(f, p, teamIds = teamIds, games = finalGames)
	strengths <- strengthOptimization$estimate
	names(strengths) <- c(teamIds, 'HomeFieldAdvantage')
	return(strengths)
}
