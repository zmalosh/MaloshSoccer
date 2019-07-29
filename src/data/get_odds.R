source('requirements.R')
source('src/data/get_api_football_json_from_url.R')

get_odds_by_league <- function(leagueId, allowCache = TRUE){
	url <- paste0('https://api-football-v1.p.rapidapi.com/v2/odds/league/', leagueId)
	localPath <- paste0(getwd(), '/data/raw/odds/leagueOdds_', str_pad(leagueId, 4, pad = '0'), '.csv')
	cacheExpirationMin <- 5

	if(allowCache){
		if(!dir.exists(dirname(localPath))){
			dir.create(dirname(localPath))
		}
		if(file.exists(localPath) && (file.info(localPath)$ctime + (cacheExpirationMin * 60)) > Sys.time()){
			cols <- cols(
				LeagueId = col_double(),
				UpdateTime = col_double(),
				GameId = col_double(),
				BookmakerId = col_double(),
				BookmakerName = col_character(),
				BetTypeId = col_double(),
				BetTypeName = col_character(),
				MarketName = col_character(),
				MarketLine = col_double()
			)

			df <- read_csv(localPath, col_types = cols)
			return (df)
		}
	}
	json <- get_api_football_json_from_url(url)
	odds <- json$odds

	df <- data.frame(
		LeagueId = c(),
		UpdateTime = c(),
		GameId = c(),
		BookmakerId = c(),
		BookmakerName = c(),
		BetTypeId = c(),
		BetTypeName = c(),
		MarketName = c(),
		MarketLine = c()
	)
	for(idx_fixture in 1:length(odds$fixture)){
		fixture <- odds$fixture[idx_fixture,]
		fixtureId <- fixture$fixture_id
		leagueId <- fixture$league_id
		updateTime <- fixture$updateAt
		books <- data.frame(odds$bookmakers[idx_fixture])
		if(nrow(books) > 0){
			for(idx_book in 1:nrow(books)){
				book <- books[idx_book,]
				bookmakerId <- book$bookmaker_id
				bookmakerName <- book$bookmaker_name
				bets <- data.frame(book$bets)
				if(nrow(bets) > 0){
					for(idx_bet in 1:nrow(bets)){
						bet <- bets[idx_bet,]
						labelId <- bet$label_id
						labelName <- bet$label_name
						markets <- data.frame(bet$values)
						x <- data.frame(
							LeagueId = leagueId,
							UpdateTime = updateTime,
							GameId = fixtureId,
							BookmakerId = bookmakerId,
							BookmakerName = bookmakerName,
							BetTypeId = labelId,
							BetTypeName = labelName,
							MarketName = markets$value,
							MarketLine = markets$odd
						)
						df <- rbind(df, x)
					}
				}
			}
		}
	}
	if(allowCache){
		write_csv(df, path = localPath)
	}
	return (df)
}
odds <- get_odds_by_league(294)