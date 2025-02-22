setwd("C:\\Users\\hilla\\OneDrive\\Documents\\418 Sports Analytics Project")
pacman::p_load(tidyverse, magrittr, lubridate, fastDummies, DataExplorer)
df <- read.csv("cleanedDataPart1.csv")

# Convert target to a factor ----------------------------------------------
df$target <- as.factor(df$target)
# Remove unnecessary columns ----------------------------------------------
df %<>% select(., -X, -possessionTeam, -preSnapHomeScore, -preSnapVisitorScore, -yardlineSide, -homeTeamAbbr, -visitorTeamAbbr, -gameClock)

# Create dummies for categorical columns ----------------------------------
df <- dummy_columns(df, select_columns = c("snapDetail", "kickType", "kickDirectionIntended", "kickDirectionActual", "kickContactType",
                                           "dayWeek", "month", "gameTOD"),
                    remove_first_dummy = T, remove_selected_columns = T)

# Remove/combine dummies with too many zeros ------------------------------
df %<>% mutate(dayWeek_WkDay = (dayWeek_Mon+dayWeek_Tue+dayWeek_Wed+dayWeek_Thu),
               dayWeek_WkEnd = (dayWeek_Sat+dayWeek_Sun)) 
df %<>% select(.,-dayWeek_Mon,-dayWeek_Tue,-dayWeek_Wed,-dayWeek_Thu,-dayWeek_Sat,-dayWeek_Sun)
df %<>% select(.,-`snapDetail_>`, -kickDirectionIntended_NA, -kickDirectionActual_NA, kickContactType_NA) 

# Impute/filter the missing numeric variables -----------------------------
df$punterAge[is.na(df$punterAge)] <- median(df$punterAge, na.rm = T)
df$operationTime[is.na(df$operationTime)] <- median(df$operationTime, na.rm = T)
df$snapTime[is.na(df$snapTime)] <- median(df$snapTime, na.rm = T)
df$hangTime[is.na(df$hangTime)] <- median(df$hangTime, na.rm = T)
df$punterHt[is.na(df$punterHt)] <- median(df$punterHt, na.rm = T)
df$punterWt[is.na(df$punterWt)] <- median(df$punterWt, na.rm = T)
df %<>% filter(!is.na(kickContactType_BC))
df %<>% filter(!is.na(kickContactType_BF))
df %<>% filter(!is.na(kickContactType_BOG))
df %<>% filter(!is.na(kickContactType_CC))
df %<>% filter(!is.na(kickContactType_CFFG))
df %<>% filter(!is.na(kickContactType_DEZ))
df %<>% filter(!is.na(kickContactType_ICC))
df %<>% filter(!is.na(kickContactType_KTB))
df %<>% filter(!is.na(kickContactType_KTC))
df %<>% filter(!is.na(kickContactType_KTF))
df %<>% filter(!is.na(kickContactType_MBC))
df %<>% filter(!is.na(kickContactType_OOB))
df %<>% filter(!is.na(kickDirectionActual_L))
df %<>% filter(!is.na(kickDirectionActual_R))
df %<>% filter(!is.na(kickDirectionIntended_L))
df %<>% filter(!is.na(kickDirectionIntended_R))

# Combining more dummies with mostly zeros --------------------------------














