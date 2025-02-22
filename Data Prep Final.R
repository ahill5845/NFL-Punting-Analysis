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

# Remove columns: games_plays ---------------------------------------------
games_plays %<>% select(., -playDescription, -penaltyJerseyNumbers, -passResult) 

# Clean and format punters ------------------------------------------------
punters <- players %>% filter(Position == "P") %>% rename(kickerId = nflId)

punters %<>% 
  tidyr::separate(height, into = c("feet", "inches")) %>% 
  mutate(feet = as.numeric(feet),
         inches = as.numeric(inches),
         inches_2 = ifelse(feet > 10, feet, NA_real_),
         inches_3 = ifelse(feet < 10, feet*12 + inches, NA_real_),
         birth_date = as.Date(birthDate),
         punterAge = as.numeric((Sys.Date() - birth_date)/365),
         punterHt = coalesce(inches_2, inches_3)) %>% 
  select(.,-feet, -inches, -inches_2, -inches_3, -birthDate, -birth_date) %>% 
  rename(punterWt = weight)

punters$kickerId <- as.character(punters$kickerId)
games_plays$kickerId <- as.character(games_plays$kickerId)
gms_plys_plyrs <- left_join(games_plays, punters, by = "kickerId")

# Clean gms_plys_plyrs ----------------------------------------------------
gms_plys_plyrs %<>% select(., -Position, -displayName, -gameId, -playId) 

# Remove unnecessary data frames ------------------------------------------
rm("games", "players", "plays", "punters", "games_plays")

# Explore and clean pff ---------------------------------------------------
pff %<>% unite("ID", gameId:playId, remove = F) 
pff$ID <- gsub("_", "", pff$ID)
pff %<>% select(., -playId, -gameId, -missedTackler, -assistTackler, -tackler, -gunners, -puntRushers, -specialTeamsSafeties, -vises)

# Merge: pff --> gms_plys_plyrs -------------------------------------------
df <- inner_join(gms_plys_plyrs, pff, by = "ID")
rm("gms_plys_plyrs", "pff")

# Filter and remove columns -----------------------------------------------
df %<>% select(., -ID, -kickerId, -returnerId, -kickBlockerId, -down) %>% filter(specialTeamsPlayType == "Punt")
df %<>% select(., -specialTeamsPlayType, -yardlineNumber)
# Remove columns with more than 10% missing -------------------------------
df %<>% select(., -kickoffReturnFormation, -penaltyCodes, -penaltyYards, -returnDirectionActual, -returnDirectionIntended, 
               -kickReturnYardage) 

# Label the target variable and drop NAs -----------------------------------
# df$specialTeamsResult <- as.factor(df$specialTeamsResult)
df %<>% rename(target = specialTeamsResult)

# Format the date and time ------------------------------------------------
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

# Point Differential for Punting Team -------------------------------------
df %<>% mutate(puntPointDiff = ifelse(possessionTeam == visitorTeamAbbr, preSnapVisitorScore - preSnapHomeScore, 
                                      preSnapHomeScore - preSnapVisitorScore))

# Remove rows for blocked punts -------------------------------------------
df %<>% filter(target != "Blocked Punt") 
df %<>% filter(target != "Non-Special Teams Result")

# Convert target into a binary variable -----------------------------------
df$target <- gsub("Touchback","Bad",df$target)
df$target <- gsub("Return","Bad",df$target)
df$target <- gsub("Out of Bounds","Bad",df$target)
df$target <- gsub("Fair Catch","Good",df$target)
df$target <- gsub("Muffed","Good",df$target)

# Split downed punts into good and bad ------------------------------------
df$target <-  ifelse((df$target == "Downed" & df$kickContactType == "BB"), "Bad",
                               ifelse((df$target == "Downed" & df$kickContactType == "BF"), "Good",
                                      ifelse((df$target == "Downed" & df$kickContactType == "ICC"), "Bad",
                                             ifelse((df$target == "Downed" & df$kickContactType == "KTB"), "Good",
                                                    ifelse((df$target == "Downed" & df$kickContactType == "KTC"), "Good",
                                                           ifelse((df$target == "Downed" & df$kickContactType == "KTF"), "Bad",
                                                                  ifelse((df$target == "Downed" & df$kickContactType == "MBDR"), "Good",
                                                                         ifelse((df$target == "Downed" & df$kickContactType == "CC"), "Bad", df$target))))))))

# Convert target to a factor ----------------------------------------------
df$target <- as.factor(df$target)

# Remove unnecessary columns ----------------------------------------------
df %<>% select(., -possessionTeam, -preSnapHomeScore, -preSnapVisitorScore, -yardlineSide, -homeTeamAbbr, -visitorTeamAbbr, -gameClock, -kickContactType)

write.csv(df, "vizData.csv")

########## Analysis Cleaning #############


# Create dummies for categorical columns ----------------------------------
df <- dummy_columns(df, select_columns = c("snapDetail", "kickType", "kickDirectionIntended", "kickDirectionActual",
                                           "dayWeek", "month", "gameTOD", "collegeName"),
                    remove_first_dummy = T, remove_selected_columns = T)

# Remove/combine dummies with too many zeros ------------------------------
df %<>% mutate(dayWeek_WkDay = (dayWeek_Mon+dayWeek_Tue+dayWeek_Wed+dayWeek_Thu)) 
df %<>% select(.,-dayWeek_Mon,-dayWeek_Tue,-dayWeek_Wed,-dayWeek_Thu, -dayWeek_Fri)
df %<>% select(.,-`snapDetail_>`, -kickDirectionIntended_NA, -kickDirectionActual_NA) 

# Impute/filter the missing numeric variables -----------------------------
df$punterAge[is.na(df$punterAge)] <- median(df$punterAge, na.rm = T)
df$operationTime[is.na(df$operationTime)] <- median(df$operationTime, na.rm = T)
df$snapTime[is.na(df$snapTime)] <- median(df$snapTime, na.rm = T)
df$hangTime[is.na(df$hangTime)] <- median(df$hangTime, na.rm = T)
df$punterHt[is.na(df$punterHt)] <- median(df$punterHt, na.rm = T)
df$punterWt[is.na(df$punterWt)] <- median(df$punterWt, na.rm = T)
df %<>% filter(!is.na(kickDirectionActual_L))
df %<>% filter(!is.na(kickDirectionActual_R))
df %<>% filter(!is.na(kickDirectionIntended_L))
df %<>% filter(!is.na(kickDirectionIntended_R))

# Removing more columns ---------------------------------------------------
df %<>% select(.,-month_Mar, -month_Apr, -month_May, -month_Jul, -month_Jun, -month_Aug, -month_Feb)

# combining colleges into conferences -------------------------------------
df %<>% mutate(powerFive = (collegeName_Pittsburgh+`collegeName_North Carolina State`+`collegeName_Miami (Fla.)`+collegeName_Syracuse+collegeName_Clemson+
                              collegeName_Michigan+`collegeName_Ohio State`+collegeName_Nebraska+collegeName_Texas+collegeName_Oklahoma+collegeName_California+
                              collegeName_Stanford+collegeName_Utah+`collegeName_Arizona State`+`collegeName_Oregon State`+`collegeName_Texas A&amp;M`+
                            `collegeName_Texas A&M`+collegeName_Florida+`collegeName_Mississippi State`+collegeName_LSU+collegeName_Tennessee+`collegeName_South Carolina`),
               FCS = (`collegeName_Arkansas-Pine Bluff`+`collegeName_Eastern Kentucky`+`collegeName_Sam Houston State`+collegeName_Wofford+`collegeName_California-Davis`),
               nonPowerFive = (`collegeName_Alabama-Birmingham`+`collegeName_Appalachian State`+collegeName_Cincinnati+collegeName_Hawaii+`collegeName_Louisiana Tech`+
                                 collegeName_Miami+`collegeName_New Mexico`+collegeName_Rice+`collegeName_Southern Methodist`+collegeName_Toledo),
               D2 = (`collegeName_California-Davis`+`collegeName_Carson-Newman`+`collegeName_Fort Valley State`))

df %<>% select(.,-collegeName_Pittsburgh,-`collegeName_North Carolina State`,-`collegeName_Miami (Fla.)`,-collegeName_Syracuse,-collegeName_Clemson,-collegeName_Michigan,
               -`collegeName_Ohio State`,-collegeName_Nebraska,-collegeName_Texas,-collegeName_Oklahoma,-collegeName_California,-collegeName_Stanford,-collegeName_Utah,
               -`collegeName_Arizona State`,-`collegeName_Oregon State`,-`collegeName_Texas A&amp;M`,-`collegeName_Texas A&M`,-collegeName_Florida,
               -`collegeName_Mississippi State`,-collegeName_LSU,-collegeName_Tennessee,-`collegeName_South Carolina`,-`collegeName_Arkansas-Pine Bluff`,
               -`collegeName_Eastern Kentucky`,-`collegeName_Sam Houston State`,-collegeName_Wofford,-collegeName_NA, -`collegeName_California-Davis`,
               -`collegeName_Alabama-Birmingham`,-`collegeName_Appalachian State`,-collegeName_Cincinnati,-collegeName_Hawaii,-`collegeName_Louisiana Tech`,
               -collegeName_Miami,-`collegeName_New Mexico`,-collegeName_Rice,-`collegeName_Southern Methodist`,-collegeName_Toledo,-`collegeName_California-Davis`,
               -`collegeName_Carson-Newman`,-`collegeName_Fort Valley State`)

# Remove missing from college columns -------------------------------------
df %<>% filter(!is.na(D2))

saveRDS(df, "cleanedData.RDS")














