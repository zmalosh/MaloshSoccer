source('requirements.R')

get_bradley_terry_model <- function(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores){
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

	get_team_strengths <- function(games){
		finalGames <- games %>% filter(!is.na(HomeScore) & !is.na(AwayScore))
		teamIds <- as.character(sort(unique(c(finalGames$HomeTeamId, finalGames$AwayTeamId))))
		p <- rep(1, times = length(teamIds) + 1)
		strengthOptimization <- nlm(f, p, teamIds = teamIds, games = finalGames)
		strengths <- strengthOptimization$estimate
		names(strengths) <- c(teamIds, 'HomeFieldAdvantage')
		return(strengths)
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

	logisticFunction <- function(homeFieldAdvantage, homeTeamStrength, awayTeamStrength){
		return(1 / (1 + (exp(-(homeFieldAdvantage + homeTeamStrength - awayTeamStrength)))))
	}

	g <- setup_games(gameIds, homeTeamIds, awayTeamIds, homeScores, awayScores)
	teamStrengths <- get_team_strengths(g)
	homeFieldAdvantage <- teamStrengths['HomeFieldAdvantage']
	teamStrengths <- teamStrengths[names(teamStrengths) != 'HomeFieldAdvantage']
	g <- g %>%
		mutate(HomeStrength = teamStrengths[as.character(HomeTeamId)],
			   AwayStrength = teamStrengths[as.character(AwayTeamId)],
			   LogisticVal = logisticFunction(homeFieldAdvantage, HomeStrength, AwayStrength),
			   LogisticResult = ifelse(GameResult == 1, LogisticVal, 1 - LogisticVal))

	m <- lm(formula = HomeMarginOfVictory ~ LogisticResult, data = g)
	coefLogisticResult <- m$coefficients['LogisticResult']
	coefIntercept <- m$coefficients['(Intercept)']
	stdDev <- summary(m)$sigma

	g <- g %>%
		mutate(PredictedSpread = coefIntercept + (coefLogisticResult * LogisticResult),
			   HomeWinProb = 1 - pnorm(0, mean = PredictedSpread, sd = stdDev),
			   PredictedResult = ifelse(HomeWinProb > 0.5, 1, 0),
			   IsResultPredicted = ifelse(GameResult == PredictedResult, 1, 0),
			   PredictionError = PredictedSpread - HomeMarginOfVictory,
			   ProbErrorSq = (GameResult - HomeWinProb) ** 2,
			   LogError = (GameResult * log(HomeWinProb)) + ((1-GameResult) * log(1 - HomeWinProb)))

	benchmarks <- data.frame(RawAccuracy = mean(g$IsResultPredicted),
							 RSQ = summary(m)$r.squared,
							 RMSE = sqrt(mean(g$PredictionError ** 2)),
							 MAE = mean(abs(g$PredictionError)),
							 BrierScore = mean(g$ProbErrorSq),
							 LogLoss = -1 * mean(g$LogError))

	predict <- function(homeTeamId, awayTeamId, homeSpread = 0){
		homeGoalsFavored <- -1 * homeSpread
		awayGoalsFavored <- -1 * homeGoalsFavored
		homeStrength <- teamStrengths[as.character(homeTeamId)]
		awayStrength <- teamStrengths[as.character(awayTeamId)]
		logisticResult <- logisticFunction(homeFieldAdvantage, homeStrength, awayStrength)
		predictedHomeSpread <- as.numeric(coefIntercept + (coefLogisticResult * logisticResult))
		predictedAwaySpread <- -1 * predictedHomeSpread
		homeWinPct <- 1 - pnorm(homeGoalsFavored + 0.5, mean = predictedHomeSpread, sd = stdDev)
		awayWinPct <- 1 - pnorm(awayGoalsFavored + 0.5, mean = predictedAwaySpread, sd = stdDev)
		drawWinPct <- 1 - (homeWinPct + awayWinPct)
		result <- list(HomeTeamId = homeTeamId,
					   AwayTeamId = awayTeamId,
					   HomeSpread = homeSpread,
					   HomeWinPct = homeWinPct,
					   DrawWinPct = drawWinPct,
					   AwayWinPct = awayWinPct)
		return(result)
	}

	result <- list('teamStrengths' = teamStrengths,
				   'homeFieldStrength' = teamStrengths[length(teamStrengths)],
				   'coefLogisticResult' = coefLogisticResult,
				   'coefIntercept' = coefIntercept,
				   'logisticFunction' = logisticFunction,
				   'model' = m,
				   'predictGame' = predict,
				   'benchmarks' = benchmarks)

	return(result)
}
