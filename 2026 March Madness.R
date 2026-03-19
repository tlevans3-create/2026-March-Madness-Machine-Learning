library(tidyverse)
library(dplyr)
library(xgboost)

barttorvik.nothome <- read_csv("Barttorvik Away-Neutral.csv")
barttorvik.home <- read_csv("Barttorvik Home.csv")
kenpom <- read_csv("KenPom Barttorvik.csv")
shooting.splits <- read_csv("Shooting Splits.csv")
tournament.matchups <- read_csv("Tournament Matchups.csv")
evan.miya <- read_csv("EvanMiya.csv")

#barttorvik.nothome <- barttorvik.nothome %>% select(YEAR,TEAM,`BADJ EM`,`BADJ O`,`BADJ D`,FTR,FTRD,`TOV%`,`TOV%D`,`OREB%`,`DREB%`,`BLK%`,`BLKED%`)
#barttorvik.home <- barttorvik.home %>% select(YEAR,TEAM,`BADJ EM`,`BADJ O`,`BADJ D`,FTR,FTRD,`TOV%`,`TOV%D`,`OREB%`,`DREB%`,`BLK%`,`BLKED%`)
#kenpom <- kenpom %>% select(YEAR,TEAM,`KADJ T`,`KADJ O`,`KADJ D`,FTR,FTRD,`TOV%`,`TOV%D`,`OREB%`,`DREB%`,`BLK%`,`BLKED%`,`EFF HGT`,`FT%`,`ELITE SOS`)
#shooting.splits <- shooting.splits %>% select(YEAR,TEAM,`DUNKS FG%`,`DUNKS SHARE`,`DUNKS FG%D`,`DUNKS D SHARE`,`CLOSE TWOS FG%`,`CLOSE TWOS SHARE`,`CLOSE TWOS FG%D`,`CLOSE TWOS D SHARE`,`FARTHER TWOS FG%`,`FARTHER TWOS SHARE`,`FARTHER TWOS FG%D`,`FARTHER TWOS D SHARE`,`THREES FG%`,`THREES D SHARE`)
#evan.miya <- evan.miya %>% select(YEAR,TEAM,`OPPONENT ADJUST`)

kenpom <- kenpom %>% select(YEAR,TEAM,`KADJ O`,`KADJ D`,`KADJ T`,`EFG%`,`EFG%D`,`3PT%`,`3PT%D`,`2PT%`,`2PT%D`,`2PTR`,`2PTRD`,`ELITE SOS`,`AVG HGT`,`OREB%`,`DREB%`,`FT%`,FTR,FTRD)
shooting.splits <- shooting.splits %>% select(YEAR,TEAM,`FARTHER TWOS FG%`,`FARTHER TWOS FG%D`,`DUNKS SHARE`,`DUNKS D SHARE`,`FARTHER TWOS SHARE`,`FARTHER TWOS D SHARE`)
evan.miya <- evan.miya %>% select(YEAR,TEAM)

home.difference <- data.frame(YEAR = barttorvik.nothome$YEAR, TEAM = barttorvik.home$TEAM, `BADJ EM` = barttorvik.home$`BADJ EM`-barttorvik.nothome$`BADJ EM`, `BADJ O` = barttorvik.home$`BADJ O`-barttorvik.nothome$`BADJ O`,
                             `BADJ D` = barttorvik.home$`BADJ D`-barttorvik.nothome$`BADJ D`,`EFG%` = barttorvik.home$`EFG%`-barttorvik.nothome$`EFG%`,`EFG%D` = barttorvik.home$`EFG%D`-barttorvik.nothome$`EFG%D`,FTR = barttorvik.home$FTR-barttorvik.nothome$FTR,
                             FTRD = barttorvik.home$FTRD-barttorvik.nothome$FTRD,`TOV%` = barttorvik.home$`TOV%` - barttorvik.nothome$`TOV%`,`TOV%D` = barttorvik.home$`TOV%D` - barttorvik.nothome$`TOV%D`,
                             `OREB%` = barttorvik.home$`OREB%` - barttorvik.nothome$`OREB%`,`DREB%` = barttorvik.home$`DREB%` - barttorvik.nothome$`DREB%`,`2PT%` = barttorvik.home$`2PT%` - barttorvik.nothome$`2PT%`,
                             `2PT%D` = barttorvik.home$`2PT%D` - barttorvik.nothome$`2PT%D`,`3PT%` = barttorvik.home$`3PT%` - barttorvik.nothome$`3PT%`,`3PT%D` = barttorvik.home$`3PT%D` - barttorvik.nothome$`3PT%D`,
                             `BLK%` = barttorvik.home$`BLK%` - barttorvik.nothome$`BLK%`,`BLKED%` = barttorvik.home$`BLKED%` - barttorvik.nothome$`BLKED%`,`AST%` = barttorvik.home$`AST%` - barttorvik.nothome$`AST%`,
                             `OP AST%` = barttorvik.home$`OP AST%` - barttorvik.nothome$`OP AST%`,`3PTR` = barttorvik.home$`3PTR` - barttorvik.nothome$`3PTR`,`3PTRD` = barttorvik.home$`3PTRD` - barttorvik.nothome$`3PTRD`,
                             `BADJ T` = barttorvik.home$`BADJ T` - barttorvik.nothome$`BADJ T`,`FT%` = barttorvik.home$`FT%` - barttorvik.nothome$`FT%`)

home.difference <- home.difference %>% select(YEAR,TEAM,X3PT.,X3PT.D,X2PT.,X2PT.D,FT.)
barttorvik.home <- barttorvik.home %>% select(YEAR,TEAM)
barttorvik.nothome <- barttorvik.nothome %>% select(YEAR,TEAM)

stats <- kenpom %>% left_join(shooting.splits, by = join_by(YEAR,TEAM)) %>% left_join(evan.miya, by = join_by(YEAR,TEAM)) %>% left_join(barttorvik.home, by = join_by(YEAR,TEAM)) %>% left_join(barttorvik.nothome, by = join_by(YEAR,TEAM)) %>% left_join(home.difference)

matchups.edited <- tournament.matchups %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% filter(YEAR > 2012) %>% group_by(matchup_id,YEAR) %>% summarize(Round = `CURRENT ROUND`[1], 
                                                                                                                                                                   highseedTeam = if_else(SEED[1] == SEED[2], TEAM[1],if_else(SEED[1] < SEED[2], TEAM[1], TEAM[2])),
                                                                                                                                                                   lowseedTeam = if_else(SEED[1] == SEED[2], TEAM[2],if_else(SEED[1] < SEED[2], TEAM[2], TEAM[1])),
                                                                                                                                                                   highseed = min(SEED),
                                                                                                                                                                   lowseed = max(SEED),
                                                                                                                                                                   highseedScore = if_else(SEED[1] == SEED[2], SCORE[1],if_else(SEED[1] < SEED[2], SCORE[1], SCORE[2])),
                                                                                                                                                                   lowseedScore = if_else(SEED[1] == SEED[2], SCORE[2],if_else(SEED[1] < SEED[2], SCORE[2],SCORE[1])),
                                                                                                                                                                   highseedWin = as.integer(if_else(SEED[1] == SEED[2], SCORE[1],if_else(SEED[1] < SEED[2], SCORE[1], SCORE[2])) > if_else(SEED[1] == SEED[2], SCORE[2],if_else(SEED[1] < SEED[2], SCORE[2],SCORE[1]))))
higher.stats <- left_join(matchups.edited, stats, by = join_by(YEAR, highseedTeam == TEAM))
lower.stats <- left_join(matchups.edited, stats, by = join_by(YEAR, lowseedTeam == TEAM))
final.stats <- higher.stats %>% left_join(lower.stats, by = join_by(matchup_id,YEAR,Round,highseedTeam,lowseedTeam,highseed,lowseed,highseedScore,lowseedScore,highseedWin))


traindata <- final.stats %>% filter(!YEAR %in% c(2023,2024,2025))
testdata <- final.stats %>% filter(YEAR %in% c(2023,2024,2025))

traindata.clean <- traindata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)
testdata.clean <- testdata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)

x_train <- traindata.clean %>% select(-highseedWin)
x_test <- testdata.clean %>% select(-highseedWin)

x_train <- as.matrix(x_train)
y_train <- traindata.clean$highseedWin

x_test <- as.matrix(x_test)
y_test <- testdata.clean$highseedWin

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

params <- list(
  objective = "binary:logistic",
  eta = 0.01,
  max_depth = 4,
  min_child_weight = 0.5
)

num_round <- 1250
set.seed(1234)

xgboost1 <- xgb.train(
  params = params,
  data = dtrain,
  nround = num_round,
  #early_stopping_rounds = 250,
  #watchlist = list(train = dtrain, val = dtest)
)

preds1 <- predict(xgboost1, dtest)
#testdatapreds <- cbind(testdata, preds)
#testdatapreds <- testdatapreds %>% rename(Preds = ...69) %>% select(Round,Preds,YEAR,highseedTeam,lowseedTeam,highseed,lowseed,highseedWin,matchup_id)



traindata <- final.stats %>% filter(!YEAR %in% c(2024,2025))
testdata <- final.stats %>% filter(YEAR %in% c(2024,2025))

traindata.clean <- traindata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)
testdata.clean <- testdata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)

x_train <- traindata.clean %>% select(-highseedWin)
x_test <- testdata.clean %>% select(-highseedWin)

x_train <- as.matrix(x_train)
y_train <- traindata.clean$highseedWin

x_test <- as.matrix(x_test)
y_test <- testdata.clean$highseedWin

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

xgboost2 <- xgb.train(
  params = params,
  data = dtrain,
  nround = num_round,
  #early_stopping_rounds = 250,
  #watchlist = list(train = dtrain, val = dtest)
)

preds2 <- predict(xgboost2, dtest)


traindata <- final.stats %>% filter(!YEAR %in% c(2025))
testdata <- final.stats %>% filter(YEAR %in% c(2025))

traindata.clean <- traindata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)
testdata.clean <- testdata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)

x_train <- traindata.clean %>% select(-highseedWin)
x_test <- testdata.clean %>% select(-highseedWin)

x_train <- as.matrix(x_train)
y_train <- traindata.clean$highseedWin

x_test <- as.matrix(x_test)
y_test <- testdata.clean$highseedWin

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

xgboost3 <- xgb.train(
  params = params,
  data = dtrain,
  nround = num_round,
  #early_stopping_rounds = 250,
  #watchlist = list(train = dtrain, val = dtest)
)

preds3 <- predict(xgboost3, dtest)


traindata <- final.stats# %>% filter(!YEAR %in% c(2025))
testdata <- final.stats %>% filter(YEAR %in% c(2025))

traindata.clean <- traindata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)
testdata.clean <- testdata %>% ungroup() %>% select(-YEAR,-lowseedTeam,-highseedTeam,-highseed,-lowseed,-highseedScore,-lowseedScore,-matchup_id,-Round)

x_train <- traindata.clean %>% select(-highseedWin)
x_test <- testdata.clean %>% select(-highseedWin)

x_train <- as.matrix(x_train)
y_train <- traindata.clean$highseedWin

x_test <- as.matrix(x_test)
y_test <- testdata.clean$highseedWin

dtrain <- xgb.DMatrix(data = x_train, label = y_train)
dtest <- xgb.DMatrix(data = x_test, label = y_test)

xgboost4 <- xgb.train(
  params = params,
  data = dtrain,
  nround = num_round,
  #early_stopping_rounds = 250,
  #watchlist = list(train = dtrain, val = dtest)
)

#preds4 <- predict(xgboost3, dtest)

#importance_matrix <- xgb.importance(feature_names = colnames(dtrain), model = xgboost)

#xgb.plot.importance(importance_matrix)

#matchups_correct <- testdatapreds %>% mutate(prediction = round(Preds), correct = abs(prediction - highseedWin))

#mean((matchups_correct$highseedWin - matchups_correct$Preds)^2)
#(length(matchups_correct$Round)-sum(matchups_correct$correct))/length(matchups_correct$Round)
#sum(matchups_correct$highseedWin)/length(matchups_correct$highseedWin)


kenpom.2026 <- read_csv("Kenpom Bart 2026.csv")
shooting.splits.2026 <- read_csv("Shooting Splits 2026.csv")
bart.home.2026 <- read_csv("Bart Home 2026.csv")
bart.nothome.2026 <- read_csv("Bart Away Neutral 2026.csv")

higher.stats <- left_join(matchups.edited, stats, by = join_by(YEAR, highseedTeam == TEAM))
lower.stats <- left_join(matchups.edited, stats, by = join_by(YEAR, lowseedTeam == TEAM))
final.stats <- higher.stats %>% left_join(lower.stats, by = join_by(matchup_id,YEAR,Round,highseedTeam,lowseedTeam,highseed,lowseed,highseedScore,lowseedScore,highseedWin))

possible.matchups <- as.data.frame(t(combn(stats.2026$TEAM,2))) %>% rename(`Higher Seed` = V1, `Lower Seed` = V2)

kenpom.2026 <- kenpom.2026 %>% mutate(`2PTR` = (100-`3P Rate`),`2PTRD` = (100-`3P Rate D`),`DREB%` = (100-`Op OReb%`))
kenpom.2026 <- kenpom.2026 %>% select(Team,`KADJ O`,`KADJ D`,`KADJ T`,`EFG%` = eFG, `EFG%D` = `eFG D.`,`3PT%` = `3P %`,`3PT%D` = `3P % D.`,`2PT%` = `2P %`,`2PT%D` = `2P % D.`,`2PTR`,`2PTRD`,`ELITE SOS` = `Elite SOS`,`AVG HGT` = `Avg Hgt.`,`OREB%` = `O Reb%`,`DREB%`,`FT%`,FTR = `FT Rate`,FTRD = `FT Rate D`)
shooting.splits.2026 <- shooting.splits.2026 %>% select(Team,`FARTHER TWOS FG%`,`FARTHER TWOS FG%D`,`DUNKS SHARE`,`DUNKS D SHARE`,`FARTHER TWOS SHARE`,`FARTHER TWOS D SHARE`)
home.difference.2026 <- data.frame(Team = bart.home.2026$Team,X3PT. = bart.home.2026$`3P %` - bart.nothome.2026$`3P %`, X3PT.D = bart.home.2026$`3P % D.` - bart.nothome.2026$`3P % D.`, X2PT. = bart.home.2026$`2P %` - bart.nothome.2026$`2P %`,X2PT.D = bart.home.2026$`2P % D.` - bart.nothome.2026$`2P % D.`,FT. = bart.home.2026$`FT%` - bart.nothome.2026$`FT%`)

stats.2026 <- kenpom.2026 %>% left_join(shooting.splits.2026, by = join_by(Team)) %>% left_join(home.difference.2026, by = join_by(Team)) %>% rename(TEAM = Team)
higher.stats.2026 <- left_join(possible.matchups, stats.2026, by = join_by(`Higher Seed` == TEAM))
lower.stats.2026 <- left_join(possible.matchups, stats.2026, by = join_by(`Lower Seed` == TEAM))
final.stats.2026 <- higher.stats.2026 %>% left_join(lower.stats.2026, by = join_by(`Higher Seed`,`Lower Seed`))

final.stats.2026.clean <- final.stats.2026 %>% select(-`Higher Seed`,-`Lower Seed`)
final.stats.2026.clean <- as.matrix(final.stats.2026.clean)
final.stats.2026.clean <- xgb.DMatrix(data = final.stats.2026.clean)
clean.2026 <- final.stats.2026 %>% select(-`Higher Seed`,-`Lower Seed`)

preds1 <- predict(xgboost1, final.stats.2026.clean)
preds2 <- predict(xgboost2, final.stats.2026.clean)
preds3 <- predict(xgboost3, final.stats.2026.clean)
preds4 <- predict(xgboost4, final.stats.2026.clean)
preds_dataframe <- data.frame(model1 = preds1, model2 = preds2, model3 = preds3, model4 = preds4)
preds.2026 <- rowMeans(preds_dataframe)


predictions.2026 <- data.frame(`High Seed` = possible.matchups$`Higher Seed`, `Low Seed`= possible.matchups$`Lower Seed`, Pred = preds.2026)


simulate_game <- function(high_seed, low_seed, matchup_df) {
  prob <- matchup_df %>%
    filter(`High.Seed` == high_seed, `Low.Seed` == low_seed)
  print(high_seed)
  prob = prob$Pred
  if (runif(1,0,1) < prob) {
    return(high_seed)
  } else {
    return(low_seed)
  }
}

first_four_simulation <- function(first_round_df,matchup_df) {
  prairie_view_vs_lehigh <- simulate_game("Lehigh","Prairie View",matchup_df)
  texas_vs_nc_state <- simulate_game("North Carolina St.","Texas",matchup_df)
  miami_oh_vs_smu <- simulate_game("SMU","Miami OH",matchup_df)
  umbc_vs_howard <- simulate_game("Howard","UMBC",matchup_df)
  first_round_df$lowSeed[9] = "Prairie View"
  first_round_df$lowSeed[21] = "Texas"
  first_round_df$lowSeed[29] = "Miami OH"
  first_round_df$lowSeed[25] = "Howard"
  return(first_round_df)
}

simulate_tournament_with_path <- function(first_round_df, matchup_df,seed_list) {
  first_round_df <- first_four_simulation(first_round_df,matchup_df)
  first_round_winners <- c()
  for (matchup_id in first_round_df$matchup_id) {
    winner <- simulate_game(first_round_df$highSeed[matchup_id],first_round_df$lowSeed[matchup_id],matchup_df)
    first_round_winners <- c(first_round_winners,winner) 
  }
  second_round_df <- tibble(
    Team = first_round_winners,
    Seed = sapply(first_round_winners, function(team) {
      seed_list$Seed[seed_list$Team == team]
    })
  )
  second_round_df <- second_round_df %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = ifelse(Seed[1] < Seed[2],Team[1],Team[2]),lowSeed = ifelse(Seed[1] > Seed[2],Team[1],Team[2]))
  second_round_winners <- c()
  for (matchup_id in second_round_df$matchup_id) {
    winner <- simulate_game(second_round_df$highSeed[matchup_id],second_round_df$lowSeed[matchup_id],matchup_df)
    second_round_winners <- c(second_round_winners,winner)
  }
  sweet_sixteen_df <- tibble(
    Team = second_round_winners,
    Seed = sapply(second_round_winners, function(team) {
      seed_list$Seed[seed_list$Team == team]
    })
  )
  sweet_sixteen_df <- sweet_sixteen_df %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = ifelse(Seed[1] < Seed[2],Team[1],Team[2]),lowSeed = ifelse(Seed[1] > Seed[2],Team[1],Team[2]))
  sweet_sixteen_winners <- c()
  for (matchup_id in sweet_sixteen_df$matchup_id) {
    winner <- simulate_game(sweet_sixteen_df$highSeed[matchup_id],sweet_sixteen_df$lowSeed[matchup_id],matchup_df)
    sweet_sixteen_winners <- c(sweet_sixteen_winners,winner)
  }
  elite_eight_df <- tibble(
    Team = sweet_sixteen_winners,
    Seed = sapply(sweet_sixteen_winners, function(team) {
      seed_list$Seed[seed_list$Team == team]
    })
  )
  elite_eight_df <- elite_eight_df %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = ifelse(Seed[1] < Seed[2],Team[1],Team[2]),lowSeed = ifelse(Seed[1] > Seed[2],Team[1],Team[2]))
  elite_eight_winners <- c()
  for(matchup_id in elite_eight_df$matchup_id) {
    winner <- simulate_game(elite_eight_df$highSeed[matchup_id],elite_eight_df$lowSeed[matchup_id],matchup_df)
    elite_eight_winners <- c(elite_eight_winners,winner)
  }
  final_four_df <- tibble(
    Team = elite_eight_winners,
    Seed = sapply(elite_eight_winners, function(team) {
      seed_list$Seed[seed_list$Team == team]
    })
  )
  final_four_df <- final_four_df %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = ifelse(Seed[1] < Seed[2],Team[1],Team[2]),lowSeed = ifelse(Seed[1] > Seed[2],Team[1],Team[2]))
  final_four_winners <- c()
  for (matchup_id in final_four_df$matchup_id) {
    winner <- simulate_game(final_four_df$highSeed[matchup_id],final_four_df$lowSeed[matchup_id],matchup_df)
    final_four_winners <- c(final_four_winners,winner)
  }
  championship <- tibble(
    Team = final_four_winners,
    Seed = sapply(final_four_winners, function(team) {
      seed_list$Seed[seed_list$Team == team]
    })
  )
  championship <- championship %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = ifelse(Seed[1] < Seed[2],Team[1],Team[2]),lowSeed = ifelse(Seed[1] > Seed[2],Team[1],Team[2]))
  champion <- simulate_game(championship$highSeed,championship$lowSeed,matchup_df)
  champion <- tibble(Champion = champion)
  path <- list()
  path[["First Round Winners"]] = first_round_winners
  path[["Second Round Winners"]] = second_round_winners
  path[["Sweet Sixteen Winners"]] = sweet_sixteen_winners
  path[["Elite Eight Winners"]] = elite_eight_winners
  path[["Final Four Winners"]] = final_four_winners
  path[["Championship Winner"]] = champion
  return(path)
}

run_simulations <- function(n, first_round_df, matchup_df, seed_list) {
  # Initialize a matrix to store how often each team reaches each round
  round_results <- matrix(0, nrow = nrow(seed_list), ncol = 6)
  rownames(round_results) <- seed_list$Team
  colnames(round_results) <- c("Round 1", "Round 2", "Sweet 16", "Elite 8", "Final 4", "Championship")
  
  for (i in 1:n) {
    path <- simulate_tournament_with_path(first_round_df, matchup_df, seed_list)
    
    # Track each team's progress through the rounds
    for (team in path[["First Round Winners"]]) {
      round_results[team, "Round 1"] <- round_results[team, "Round 1"] + 1
    }
    for (team in path[["Second Round Winners"]]) {
      round_results[team, "Round 2"] <- round_results[team, "Round 2"] + 1
    }
    for (team in path[["Sweet Sixteen Winners"]]) {
      round_results[team, "Sweet 16"] <- round_results[team, "Sweet 16"] + 1
    }
    for (team in path[["Elite Eight Winners"]]) {
      round_results[team, "Elite 8"] <- round_results[team, "Elite 8"] + 1
    }
    for (team in path[["Final Four Winners"]]) {
      round_results[team, "Final 4"] <- round_results[team, "Final 4"] + 1
    }
    winner <- path[["Championship Winner"]]$Champion
    round_results[winner, "Championship"] <- round_results[winner, "Championship"] + 1
  }
  
  # Convert frequency to percentage (out of 100) and round to one decimal place
  round_probs <- round(round_results / n * 100, 1)
  
  # Convert to data frame and add the 'Team' column
  round_probs <- as.data.frame(round_probs)
  round_probs$Team <- rownames(round_probs)
  
  # Reorder so the 'Team' column is first
  round_probs <- round_probs[, c("Team", colnames(round_probs)[1:(ncol(round_probs)-1)])]
  
  # Return the data frame
  return(round_probs)
}

convert_to_tibble <- function(df) {
  tibble(Team = df$Team, Round32 = df$`Round 1`,SweetSixteen = df$`Round 2`,EliteEight = df$`Sweet 16`, FinalFour = df$`Elite 8`, Championship = df$`Final 4`, Champion = df$Championship)
}

first_round_matchups <- read_csv("Bracket Order.csv",show_col_types = FALSE)
true_seeds <- data.frame(Team = stats.2026$TEAM, Seed = c(1:68))
first_round_matchups <- first_round_matchups %>% mutate(matchup_id = rep(1:(nrow(.) / 2), each = 2)) %>% group_by(matchup_id) %>% summarize(highSeed = Team[1],lowSeed = Team[2])

simulate_tournament_with_path(first_round_matchups,predictions.2026,true_seeds)

simulations <- convert_to_tibble(run_simulations(1000,first_round_matchups,predictions.2026,true_seeds))