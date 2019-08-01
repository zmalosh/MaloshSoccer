#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyjs)
library(shinyWidgets)

# Define UI for application that draws a histogram
ui <- fluidPage(
	shinyjs::useShinyjs(),

	# Application title
	titlePanel("Soccer Predictions"),

	# Sidebar with a slider input for number of bins
	sidebarLayout(
		sidebarPanel(
			uiOutput('Season'),
			uiOutput('LeagueId')
		),

		# Show a plot of the generated distribution
		mainPanel(
			tabsetPanel(
				tabPanel('Predict Game',
						 fluidRow(
						 	column(3,
						 		   uiOutput('PredictGameHomeTeamId')),
						 	column(3,
						 		   uiOutput('PredictGameAwayTeamId')),
						 	column(3,
						 		   uiOutput('PredictGameHomeSpread')),
						 	column(3,
						 		   uiOutput('PredictGameAllowAdjustments'))
						 ),
						 fluidRow(
						 	column(3,
						 		   uiOutput('PredictGameHomeStrength')),
						 	column(3,
						 		   uiOutput('PredictGameAwayStrength')),
						 	column(3,
						 		   uiOutput('PredictGameHomeFieldStrength'))
						 ),
						 DT::dataTableOutput('PredictGameResultsTable')
				),
				tabPanel('Upcoming Games', h3('THIS TAB IS NOT YET IMPLEMENTED. GO AWAY.')),
				tabPanel('Teams',
						 uiOutput('HomeFieldAdvantage'),
						 DT::dataTableOutput('TeamStrengths')),
				tabPanel('Completed Games', DT::dataTableOutput('FinalGamesTable'))
			)
		)
	)
)

	# Define server logic required to draw a histogram
server <- function(input, output, session) {
	options(shiny.reactlog=TRUE)
	useDataCache <- TRUE

	source('requirements.R')
	source('src/data/get_leagues.R')
	source('src/data/get_fixtures.R')
	source('src/data/get_teams.R')

	library(BradleyTerry)

	tableLogoHeight <- 20
	notSelectedVal <- -1
	currentSeasonVal <- -2

	leagues <- get_leagues(allowCache = useDataCache)
	seasonOptions <- as.list(c(notSelectedVal, currentSeasonVal, sort(unique(leagues$Season), decreasing = TRUE)))
	seasonNames <- c('NONE', 'CURRENT', sort(unique(as.character(leagues$Season)), decreasing = TRUE))
	names(seasonOptions) <- seasonNames
	output$Season <- renderUI({
		selectInput('Season',
					'Season',
					choices = seasonOptions,
					selected = notSelectedVal)
	})

	leagueOptions <- reactive({
		if(input$Season == notSelectedVal){
			leagueOptions <- list('SELECT LEAGUE...' = notSelectedVal)
		} else {
			if (input$Season == currentSeasonVal){
				seasonLeagues <- leagues %>% filter(IsCurrentSeason & SeasonStartDate < as.Date(Sys.time())) %>% arrange(Country, LeagueName)
			} else{
				seasonLeagues <- leagues %>% filter(Season == input$Season & SeasonStartDate < as.Date(Sys.time())) %>% arrange(Country, LeagueName)
			}
			leagueOptions <- c(notSelectedVal, as.list(seasonLeagues$LeagueId))
			leagueNames <- c('NONE', paste0(seasonLeagues$Country, ' - ', seasonLeagues$LeagueName))
			setNames(leagueOptions, leagueNames)
		}
	})
	output$LeagueId <- renderUI({
		if(is.null(input$Season) || input$Season == notSelectedVal){
			return(NULL)
		}
		selectInput('LeagueId',
					'Leagues',
					choices = leagueOptions())
	})

	leagueId <- reactive({
		if(is.null(input$LeagueId)){
			return (NULL)
		}
		leagueId <- input$LeagueId
	})

	games <- reactive({
		leagueId <- leagueId()
		if(is.null(input$Season) || input$Season == notSelectedVal || is.null(leagueId) || leagueId == notSelectedVal){
			games <- NULL
		} else {
			leagueId <- input$LeagueId
			games <- get_fixtures_by_league(leagueId, allowCache = useDataCache)
		}
	})

	leagueTeams <- reactive({
		games <- games()
		if(is.null(games) || nrow(games) == 0){
			leagueTeams <- NULL
		} else {
			leagueId <- input$LeagueId
			leagueTeams <- get_teams_by_league(leagueId, allowCache = useDataCache)
		}
	})

	btModel <- reactive({
		games <- games()
		if(is.null(games) || nrow(games) == 0){
			btModel <- NULL
		} else {
			btModel <- bradley_terry(gameIds = games$FixtureId,
									 homeTeamIds = as.character(games$HomeTeamId),
									 awayTeamIds = as.character(games$AwayTeamId),
									 homeScore = games$HomeScore,
									 awayScore = games$AwayScore)
		}
	})

	teamStrengths <- reactive({
		games <- games()
		if(is.null(games) || nrow(games) == 0){
			return(NULL)
		}
		teams <- leagueTeams()
		if(is.null(teams) || nrow(teams) == 0){
			return(NULL)
		}
		btModel <- btModel()
		if(is.null(btModel)){
			return(NULL)
		}

		rawTeamStrengths <- btModel[['teamStrengths']]
		teamStandings <- rbind(
			games %>%
				filter(!is.na(HomeScore) & !is.na(AwayScore)) %>%
				transform(TeamId = HomeTeamId, Score = HomeScore, OppScore = AwayScore, IsHome = TRUE) %>%
				select(TeamId, Score, OppScore, IsHome),
			games %>%
				filter(!is.na(HomeScore) & !is.na(AwayScore)) %>%
				transform(TeamId = AwayTeamId, Score = AwayScore, OppScore = HomeScore, IsHome = FALSE) %>%
				select(TeamId, Score, OppScore, IsHome)) %>%
			group_by(TeamId, IsHome) %>%
			summarise(GoalsFor = sum(Score),
					  GoalsAgainst = sum(OppScore),
					  Wins = sum(ifelse(Score > OppScore, 1, 0)),
					  Draws = sum(ifelse(Score == OppScore, 1, 0)),
					  Losses = sum(ifelse(Score < OppScore, 1, 0))) %>%
			group_by(TeamId) %>%
			summarise(Wins = sum(Wins),
					  Draws = sum(Draws),
					  Losses = sum(Losses),
					  GoalsFor = sum(GoalsFor),
					  GoalsAgainst = sum(GoalsAgainst),
					  HomeWins = sum(ifelse(IsHome, Wins, 0)),
					  HomeDraws = sum(ifelse(IsHome, Draws, 0)),
					  HomeLosses = sum(ifelse(IsHome, Losses, 0)),
					  HomeGoalsFor = sum(ifelse(IsHome, GoalsFor, 0)),
					  HomeGoalsAgainst = sum(ifelse(IsHome, GoalsAgainst, 0)),
					  AwayWins = sum(ifelse(!IsHome, Wins, 0)),
					  AwayDraws = sum(ifelse(!IsHome, Draws, 0)),
					  AwayLosses = sum(ifelse(!IsHome, Losses, 0)),
					  AwayGoalsFor = sum(ifelse(!IsHome, GoalsFor, 0)),
					  AwayGoalsAgainst = sum(ifelse(!IsHome, GoalsAgainst, 0)))

		teamStrengths <- teams %>%
			mutate(Strength = rawTeamStrengths[as.character(TeamId)]) %>%
			inner_join(teamStandings, by = 'TeamId') %>%
			select(TeamId, TeamName, Country, LogoUrl, Strength,
				   Wins, Draws, Losses, GoalsFor, GoalsAgainst,
				   HomeWins, HomeDraws, HomeLosses, HomeGoalsFor, HomeGoalsAgainst,
				   AwayWins, AwayDraws, AwayLosses, AwayGoalsFor, AwayGoalsAgainst)
	})
	teamStrengthsTable <- reactive({
		teamStrengths <- teamStrengths()
		if(is.null(teamStrengths) || nrow(teamStrengths) == 0){
			teamStrengthsTable <- NULL
		} else {
			teamStrengthsTable <- teamStrengths %>%
				mutate(Team = paste0('<img src="', LogoUrl, '" height="', tableLogoHeight, '"></img> ', TeamName),
					   GP = Wins + Draws + Losses,
					   P = (3 * Wins) + (1 * Draws) + (0 * Losses),
					   GD = GoalsFor - GoalsAgainst,
					   Strength = round(Strength, digits = 3)) %>%
				transform(W = Wins, D = Draws, L = Losses, GF = GoalsFor, GA = GoalsAgainst) %>%
				select(Team, Strength, GP, P, W, D, L, GF, GA, GD) %>%
				arrange(desc(Strength))
		}
	})
	output$TeamStrengths <- DT::renderDataTable({DT::datatable(teamStrengthsTable(), escape = FALSE)})
	output$HomeFieldAdvantage <- renderUI({
		h3(paste('Home Field Advantage:', btModel()$homeFieldStrength))
	})

	finalGames <- reactive({
		games <- games()
		btModel <- btModel()
		if(is.null(games) || nrow(games) == 0){
			finalGames <- NULL
		} else {
			gamePredictions <- mapply(btModel$predictGameByIds, homeTeamId = games$HomeTeamId, awayTeamId = games$AwayTeamId)
			games$GameDate <- lapply(X = str_split(games$GameDate, 'T'), FUN = function(x){x[1]})
			games$HomeProb <- str_pad(round(unlist(gamePredictions['HomeWinPct',]), digits = 4), width = 6, side = 'right', pad = '0')
			games$DrawProb <- str_pad(round(unlist(gamePredictions['DrawWinPct',]), digits = 4), width = 6, side = 'right', pad = '0')
			games$AwayProb <- str_pad(round(unlist(gamePredictions['AwayWinPct',]), digits = 4), width = 6, side = 'right', pad = '0')
			finalGames <- games %>% transform(Status = StatusShort) %>%
				mutate(GameDate <- str_split(GameDate, 'T')[1],
					   Score = paste0(HomeScore, '-', AwayScore),
					   HomeTeam = paste0('<img src="',HomeTeamLogo,'" height="',tableLogoHeight,'"></img> ',HomeTeamName),
					   AwayTeam = paste0('<img src="',AwayTeamLogo,'" height="',tableLogoHeight,'"></img> ',AwayTeamName)) %>%
				filter(StatusShort == 'FT') %>%
				select(GameDate, HomeTeam, Score, AwayTeam, HomeProb, DrawProb, AwayProb, Venue, Referee)
		}
	})
	output$FinalGamesTable <- DT::renderDataTable({DT::datatable(finalGames(), escape = FALSE)})

	# PREDICT GAME TAB
	teamOptions <- reactive({
		teams <- leagueTeams()
		if(is.null(teams) || nrow(teams) == 0){
			teamOptions <- NULL
		} else {
			allTeamOptions <- teams %>%
				transform(SelectDisplay = sprintf('<div><img src="%s" height="%s"></img>  <span>%s</span></div>',
												  LogoUrl,
												  tableLogoHeight,
												  TeamName)) %>%
				select(TeamId, TeamName, LogoUrl, SelectDisplay) %>%
				unique() %>%
				arrange(TeamName)
			teamOptions <- allTeamOptions
		}
	})

	output$PredictGameHomeTeamId <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return(NULL)
		}
		teamChoices <- as.list(tmOpts$TeamId)
		setNames(teamChoices, tmOpts$TeamName)
		pickerInput('PredictGameHomeTeamId',
					'Home Team',
					choices = teamChoices,
					choicesOpt = list(content = tmOpts$SelectDisplay))
	})

	output$PredictGameAwayTeamId <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return(NULL)
		}
		teamChoices <- as.list(tmOpts$TeamId)
		setNames(teamChoices, tmOpts$TeamName)
		pickerInput('PredictGameAwayTeamId',
					'Away Team',
					choices = teamChoices,
					choicesOpt = list(content = tmOpts$SelectDisplay))
	})

	output$PredictGameHomeSpread <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return(NULL)
		}
		sliderInput('PredictGameHomeSpread',
					'Home Spread',
					min = -5,
					max = 5,
					value = 0,
					step = 0.5,
					ticks = TRUE)
	})

	output$PredictGameAllowAdjustments <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return(NULL)
		}
		checkboxInput('PredictGameAllowAdjustments',
					  'Allow Adjustments',
					  value = FALSE)
	})

	output$PredictGameHomeStrength <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return (NULL)
		}
		btModel <- btModel()
		if(is.null(btModel)){
			return(NULL)
		}
		teamStrengths <- teamStrengths()
		if(is.null(teamStrengths) || length(teamStrengths) == 0){
			return(NULL)
		}

		sliderInput('PredictGameHomeStrength',
					'Home Strength',
					min = round(min(teamStrengths$Strength) - 1, digits = 2),
					max = round(max(teamStrengths$Strength) + 1, digits = 2),
					value = (teamStrengths %>% filter(TeamId == input$PredictGameHomeTeamId))$Strength,
					ticks = TRUE)
	})

	output$PredictGameAwayStrength <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return (NULL)
		}
		btModel <- btModel()
		if(is.null(btModel)){
			return(NULL)
		}
		teamStrengths <- teamStrengths()
		if(is.null(teamStrengths) || length(teamStrengths) == 0){
			return(NULL)
		}

		sliderInput('PredictGameAwayStrength',
					'Away Strength',
					min = round(min(teamStrengths$Strength) - 1, digits = 2),
					max = round(max(teamStrengths$Strength) + 1, digits = 2),
					value = (teamStrengths %>% filter(TeamId == input$PredictGameAwayTeamId))$Strength,
					ticks = TRUE)
	})

	output$PredictGameHomeFieldStrength <- renderUI({
		tmOpts <- teamOptions()
		if(is.null(tmOpts) || nrow(tmOpts) == 0){
			return (NULL)
		}
		btModel <- btModel()
		if(is.null(btModel)){
			return(NULL)
		}
		teamStrengths <- teamStrengths()
		if(is.null(teamStrengths) || length(teamStrengths) == 0){
			return(NULL)
		}

		sliderInput('PredictGameHomeFieldStrength',
					'Home Field Advantage',
					min = round(min(btModel$teamStrengths) - 1, digits = 2),
					max = round(max(btModel$teamStrengths) + 1, digits = 2),
					value = btModel$homeFieldStrength,
					ticks = TRUE)
	})

	# observeEvent(input$PredictGameAllowAdjustments, {
	# 	if(input$PredictGameAllowAdjustments == T){
	# 		shinyjs::showElement('PredictGameHomeStrength')
	# 		shinyjs::showElement('PredictGameAwayStrength')
	# 		shinyjs::showElement('PredictGameHomeFieldStrength')
	# 	} else {
	# 		shinyjs::hideElement('PredictGameHomeStrength')
	# 		shinyjs::hideElement('PredictGameAwayStrength')
	# 		shinyjs::hideElement('PredictGameHomeFieldStrength')
	# 	}
	# })

	predictGameSettings <- reactive({
		btModel <- btModel()
		teams <- leagueTeams()
		if(is.null(btModel) ||
		   is.null(input$PredictGameHomeStrength) ||
		   is.null(input$PredictGameAwayStrength) ||
		   is.null(input$PredictGameHomeSpread)||
		   is.null(input$PredictGameHomeFieldStrength) ||
		   is.null(input$PredictGameAllowAdjustments)){
			predictGameSettings <- NULL
		# } else if(!input$PredictGameAllowAdjustments){
		# 	homeTeamId <- input$PredictGameHomeTeamId
		# 	homeStrength <- btModel$teamStrengths[as.character(homeTeamId)]
		# 	awayTeamId <- input$PredictGameAwayTeamId
		# 	awayStrength <- btModel$teamStrengths[as.character(awayTeamId)]
		# 	homeFieldStrength <- btModel$homeFieldStrength
		# 	predictGameSettings <- list(
		# 		HomeStrength = as.numeric(homeStrength),
		# 		AwayStrength = as.numeric(awayStrength),
		# 		HomeFieldStrength = as.numeric(homeFieldStrength),
		# 		HomeSpread = as.numeric(input$PredictGameHomeSpread)
		# 	)
		} else {
			predictGameSettings <- list(
				HomeStrength = as.numeric(input$PredictGameHomeStrength),
				AwayStrength = as.numeric(input$PredictGameAwayStrength),
				HomeFieldStrength = as.numeric(input$PredictGameHomeFieldStrength),
				HomeSpread = as.numeric(input$PredictGameHomeSpread)
			)
		}
	})

	predictGameResults <- reactive({
		predictGameSettings <- predictGameSettings()
		teams <- leagueTeams()
		btModel <- btModel()
		if(is.null(predictGameSettings) || is.null(teams) || nrow(teams) == 0 || is.null(btModel) || is.null(predictGameSettings$HomeFieldStrength)){
			predictGameResults <- NULL
		} else {
			p <- data.frame(HomeSpread = as.numeric(predictGameSettings$HomeSpread),
							HomeStrength = as.numeric(predictGameSettings$HomeStrength),
							AwayStrength = as.numeric(predictGameSettings$AwayStrength),
							HomeFieldStrength = as.numeric(predictGameSettings$HomeFieldStrength))

			x <- mapply(
				FUN = btModel$predictGame,
				homeStrength = p$HomeStrength,
				awayStrength = p$AwayStrength,
				homeFieldAdvantage = p$HomeFieldStrength,
				homeSpread = p$HomeSpread)
			p$HomeProb <- round(unlist(x['HomeWinPct',]), digits = 4)
			p$DrawProb <- round(unlist(x['DrawWinPct',]), digits = 4)
			p$AwayProb <- round(unlist(x['AwayWinPct',]), digits = 4)

			predictGameResults <- p %>%
				select(HomeSpread, HomeProb, DrawProb, AwayProb)
		}
	})

	output$PredictGameResultsTable <- DT::renderDataTable({DT::datatable(predictGameResults(), escape = FALSE)})
}

# Run the application
shinyApp(ui = ui, server = server)

