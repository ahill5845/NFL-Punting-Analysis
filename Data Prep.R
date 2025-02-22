setwd("C:\\Users\\hilla\\OneDrive\\Documents\\418 Sports Analytics Project")
pacman::p_load(tidyverse, magrittr, lubridate, fastDummies, DataExplorer)
games <- read.csv("games.csv")
players <- read.csv("players.csv")
plays <- read.csv("plays.csv")
pff <- read.csv("PFFScoutingData.csv")

# Merge: games --> plays --------------------------------------------------
games_plays <- full_join(plays, games, by = "gameId")
games_plays %<>% unite("ID", gameId:playId, remove = F) 
games_plays$ID <- gsub("_", "", games_plays$ID)

games_plays %<>% select(., -playDescription, -penaltyJerseyNumbers, -passResult) 

punters <- players %>% filter(Position == "P") %>% rename(kickerId = nflId)
punters$kickerId <- as.character(punters$kickerId)
games_plays$kickerId <- as.character(games_plays$kickerId)

gms_plys_plyrs <- left_join(games_plays, punters, by = "kickerId")
gms_plys_plyrs %<>% select(., -Position, -gameId, -playId) 

pff %<>% unite("ID", gameId:playId, remove = F) 
pff$ID <- gsub("_", "", pff$ID)
pff %<>% select(., -playId, -gameId, -missedTackler, -assistTackler, -tackler, -gunners, -puntRushers, -specialTeamsSafeties, -vises)
df <- inner_join(gms_plys_plyrs, pff, by = "ID")

df %<>% select(., -ID, -kickerId, -returnerId, -kickBlockerId, -down, -specialTeamsPlayType, -yardlineNumber, -kickoffReturnFormation, 
               -penaltyCodes, -penaltyYards, -returnDirectionActual, -returnDirectionIntended,-kickReturnYardage) %>% filter(specialTeamsPlayType == "Punt")

df %<>% mutate(gameDate = mdy(df$gameDate),
               gameTimeEastern = hms(df$gameTimeEastern),
               gameHour = hour(gameTimeEastern),
               dayWeek = wday(gameDate, label = T, abbr = T),
               month = month(gameDate, label = T, abbr = T))
df %<>% mutate(gameTOD = case_when(gameHour == 9 ~ "Morning",
                                   gameHour %in% c(12,13,15,16,17) ~ "Afternoon",
                                   gameHour %in% c(19,20,21,22) ~ "Night"),
               primetime = ifelse(gameTOD == "Night", 1, 0),
               international = ifelse(gameTOD == "Morning", 1, 0))
df %<>% select(., -gameDate, -gameTimeEastern)

df %<>% mutate(puntPointDiff = ifelse(possessionTeam == visitorTeamAbbr, preSnapVisitorScore - preSnapHomeScore, 
                                      preSnapHomeScore - preSnapVisitorScore))

df %<>% select(., -preSnapHomeScore, -preSnapVisitorScore, -yardlineSide, -homeTeamAbbr, -visitorTeamAbbr, -gameClock)

rm("games", "players", "plays","punters", "games_plays", "gms_plys_plyrs", "pff")
write.csv(df, "STAcleanedData.csv")
