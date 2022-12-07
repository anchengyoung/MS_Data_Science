library(tidyverse)
library(readxl)
library(lubridate)

## Import data sets

for (year in 12:22) {
  
  df_1 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 1) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  df_2 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 2) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  df_3 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 3) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  df_4 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 4) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  df_5 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 5) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  df_6 <- read_excel(paste0("data/data_",year,".xlsx"), sheet = 6) %>%
    mutate(year = as.integer(paste0("20",year)), Player = gsub("  ", " ", Player))
  
  assign(paste0("qb_pass_", year), df_1, envir = .GlobalEnv)
  assign(paste0("qb_rush_", year), df_2, envir = .GlobalEnv)
  assign(paste0("rb_rush_", year), df_3, envir = .GlobalEnv)
  assign(paste0("rb_pass_", year), df_4, envir = .GlobalEnv)
  assign(paste0("wr_pass_", year), df_5, envir = .GlobalEnv)
  assign(paste0("wr_rush_", year), df_6, envir = .GlobalEnv)
  
}

remove(df_1, df_2, df_3, df_4, df_5, df_6)

# Get player birthday to calculate age later
players <- nflreadr::load_players() %>%
  filter(!is.na(birth_date), birth_date != "",
         position_group %in% c("QB","WR","RB")) %>%
  mutate(birth_date = gsub("/","//",birth_date),
         birth_date = ifelse(nchar(birth_date) == 12,
                             paste0(substr(birth_date, 9, 12),"-",
                                    substr(birth_date, 1, 2),"-",
                                    substr(birth_date, 5, 6)), birth_date),
         birth_date = as.Date(birth_date),
         last_name = ifelse(last_name == "Harty", "Harris", last_name)) %>%
  filter(!display_name %in% c("Drew Anderson","Ryan Griffin","Alex Wesley",
                              "Chris Brown","Dontez Byrd","Damaris Johnson",
                              "Geremy Davis","Greg Jennings","Janarion Grant",
                              "Jamel Johnson","Keeon Johnson","Ka'Raun White",
                              "Marcus Harris","Marlon Brown","Reggie White Jr.",
                              "Titus Davis","Malachi Jones","Bronson Hill",
                              "Chase Reynolds","De'Angelo Henderson Sr.","Justice Hill",
                              "Jamal Robinson","Josh Robinson","Joe Williams",
                              "Mack Brown","Taiwan Jones","Trey Williams")) %>%
  rename(firstname = football_name, lastname = last_name) %>%
  select(firstname, lastname, birth_date, position_group)

## QB Stats ---------------------------------------------------------------------------------------

qb_pass <- rbind(qb_pass_12, qb_pass_13, qb_pass_14, qb_pass_15, qb_pass_16,
                 qb_pass_17, qb_pass_18, qb_pass_19, qb_pass_20, qb_pass_21)
qb_rush <- rbind(qb_rush_12, qb_rush_13, qb_rush_14, qb_rush_15, qb_rush_16,
                 qb_rush_17, qb_rush_18, qb_rush_19, qb_rush_20, qb_rush_21)

qb_id <- rbind(qb_pass %>% select(Player), qb_rush %>% select(Player)) %>%
  unique() %>% arrange(Player) %>% mutate(player_id = row_number())

qb_pass <- qb_pass %>% left_join(qb_id, by = "Player")
qb_rush <- qb_rush %>% left_join(qb_id, by = "Player")

qb_total <- full_join(qb_pass %>% rename(DYAR_pass = DYAR) %>%
                        select(player_id, year, DYAR_pass),
                      qb_rush %>% rename(DYAR_rush = DYAR) %>%
                        select(player_id, year, DYAR_rush),
                      by = c("player_id","year")) %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR = DYAR_pass + DYAR_rush)

# Calculate QB Age
qb_age <- players %>%
  filter(position_group == "QB") %>% select(-position_group) %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 1), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(qb_id, by = "Player") %>%
  select(-firstname, -lastname, -Player) %>%
  right_join(qb_id, by = "player_id")

#write.csv(qb_age, "age_qb.csv", row.names = FALSE, na = "")

qb_age_2 <- read.csv("data/age_qb_2.csv") %>%
  mutate(birth_date = as.Date(birth_date, "%m/%d/%Y"),
         age_22 = trunc((birth_date %--% Sys.Date()) / years(1))) %>%
  select(player_id, age_22)

qb_total <- qb_total %>%
  left_join(qb_age_2, by = "player_id") %>%
  mutate(age = age_22 - (2022 - year))

## QB year-to-year --------------------------------------------------------------------------------

# QB one-year model 
qb_yty <- qb_total %>%
  filter(year < 2021) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>% drop_na()

qb_model_1 <- lm(DYAR_yr2 ~ DYAR_yr1 + age, data = qb_yty)
#summary(qb_model_2)
qb_yty <- cbind(qb_yty, prediction = predict(qb_model_1, newdata = qb_yty))
cor(qb_yty$DYAR_yr2, qb_yty$prediction)^2

ggplot(qb_yty, aes(x = DYAR_yr2, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Quarterbacks DYAR vs. Projected DYAR from One-Year Model",
       subtitle = "with 401 QB-Seasons from 2013 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(qb_yty$DYAR_yr2, qb_yty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# QB two-year model 
qb_yyty <- qb_total %>%
  filter(year < 2020) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>% drop_na()

qb_model_2 <- lm(DYAR_yr3 ~ DYAR_yr1 + DYAR_yr2 + age, data = qb_yyty)
#summary(qb_model_2)
qb_yyty <- cbind(qb_yyty, prediction = predict(qb_model_2, newdata = qb_yyty))
cor(qb_yyty$DYAR_yr3, qb_yyty$prediction)^2

ggplot(qb_yyty, aes(x = DYAR_yr3, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Quarterbacks DYAR vs. Projected DYAR from Two-Year Model",
       subtitle = "with 274 Projected QB-Seasons from 2014 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(qb_yyty$DYAR_yr3, qb_yyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# QB three-year model 
qb_yyyty <- qb_total %>%
  filter(year < 2019) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>%
  left_join(qb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr4 = DYAR) %>%
              mutate(year = year - 3),
            by = c("player_id","year")) %>% drop_na()

qb_model_3 <- lm(DYAR_yr4 ~ DYAR_yr1 + DYAR_yr2 + DYAR_yr3 + age, data = qb_yyyty)
#summary(qb_model_3)
qb_yyyty <- cbind(qb_yyyty, prediction = predict(qb_model_3, newdata = qb_yyyty))
cor(qb_yyyty$DYAR_yr4, qb_yyyty$prediction)^2

ggplot(qb_yyyty, aes(x = DYAR_yr4, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Quarterbacks DYAR vs. Projected DYAR from Three-Year Model",
       subtitle = "with 193 Projected QB-Seasons from 2015 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(qb_yyyty$DYAR_yr4, qb_yyyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# QB Projections
qb_yyty <- qb_yyty %>% mutate(proj_id = paste0(player_id,"_",year+2))

qb_project <- qb_yyty %>% mutate(year = year + 2) %>%
  select(player_id, year, prediction) %>%
  rbind(qb_yty %>% filter(year > 2012) %>%
          mutate(year = year + 1,
                 proj_id = paste0(player_id,"_",year)) %>%
          filter(!proj_id %in% qb_yyty$proj_id) %>%
          select(player_id, year, prediction)) %>%
  pivot_wider(names_from = year, names_prefix = "proj_",
              values_from = prediction) %>%
  mutate_all(~replace(., is.na(.), 0))

for (i in 22:30) {
  
  qb_project <- qb_project %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]],
           year = i + 2000) %>%
    left_join(qb_age_2, by = "player_id") %>%
    mutate(age = age_22 - (2022 - year)) %>%
    mutate(prediction = predict(qb_model_2, newdata = .)) %>%
    select(-DYAR_yr1, -DYAR_yr2, -age, -age_22, -year) %>%
    rename(!!paste0("proj_20", i) := prediction)
  
}
  
## WR Stats ---------------------------------------------------------------------------------------

wr_pass <- rbind(wr_pass_12, wr_pass_13, wr_pass_14, wr_pass_15, wr_pass_16,
                 wr_pass_17, wr_pass_18, wr_pass_19, wr_pass_20, wr_pass_21) %>%
  mutate(Player = case_when(Player == "A. Brown" & Team %in% c("TEN","PHI") ~ "A.J. Brown",
                            Player == "A. Brown" & Team %in% c("TB","PIT") ~ "An. Brown",
                            Player == "A. Robinson" & Team %in% c("CHI","JAX","LAR") ~ "All. Robinson",
                            Player == "A. Robinson" & Team %in% c("ATL","MIN") ~ "Ald. Robinson",
                            Player == "C. Johnson" & Team == "DET" ~ "Ca. Johnson",
                            Player == "C. Johnson" & Team == "MIN" ~ "Ch. Johnson",
                            Player == "D. Harris" & Team == "NO" ~ "Deo. Harris",
                            Player == "D. Harris" & Team == "IND" ~ "DeM. Harris",
                            Player == "D. Moore" & Team == "CAR" ~ "DJ. Moore",
                            Player == "D. Moore" & Team != "CAR" ~ "Da. Moore",
                            Player == "D. Thomas" & Team == "2TM" & year == 2018 ~ "Dem. Thomas",
                            Player == "D. Thomas" & Team == "2TM" & year == 2019 ~ "DeA. Thomas",
                            Player == "D. Thomas" & Team %in% c("DEN","NYJ") ~ "Dem. Thomas",
                            Player == "D. Thomas" & Team == "KC" ~ "DeA. Thomas",
                            Player == "D. Williams" & Team == "BUF" ~ "Du. Williams",
                            Player == "D. Williams" & Team == "TEN" ~ "Da. Williams",
                            Player == "J. Brown" & Team == "PIT" ~ "Ju. Brown",
                            Player == "J. Brown" & Team == "SEA" ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2013 ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2014 & TD == 5 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2014 & TD == 2 ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2015 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2017 & TD == 3 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2017 & TD == 4 ~ "Ja. Brown",
                            Player == "J. Brown" & Team %in% c("BAL","BUF","3TM") ~ "Jo. Brown",
                            Player == "J. Jones" & Team %in% c("ATL","TEN","TB") ~ "Ju. Jones",
                            Player == "J. Jones" & Team == "BAL" ~ "Jac. Jones",
                            Player == "J. Jones" & Team %in% c("GB","OAK") ~ "Jam. Jones",
                            Player == "J. Morgan" & Team == "NO" ~ "Joe. Morgan",
                            Player == "J. Morgan" & Team %in% c("WAS","CHI") ~ "Jos. Morgan",
                            Player == "J. Nelson" & Team == "ARI" ~ "J.J. Nelson",
                            Player == "J. Nelson" & Team != "ARI" ~ "Jo. Nelson",
                            Player == "J. Ross" & Team %in% c("DET","2TM") ~ "Je. Ross",
                            Player == "J. Ross" & Team %in% c("CIN","NYG") ~ "Jo. Ross",
                            Player == "J. Smith" & Team == "NE" ~ "Jo. Smith",
                            Player == "J. Smith" & Team == "NYJ" ~ "Je. Smith",
                            Player == "J. Wright" & Team == "CIN" ~ "Jam. Wright",
                            Player == "J. Wright" & Team %in% c("MIN","CAR") ~ "Jar. Wright",
                            Player == "M. Brown" & Team %in% c("ARI","BAL") ~ "Mar. Brown",
                            Player == "M. Brown" & Team %in% c("JAX","NE","NO") ~ "Mal. Brown",
                            Player == "M. Floyd" & Team == "SD" ~ "Ma. Floyd",
                            Player == "M. Floyd" & Team %in% c("ARI","MIN","2TM") ~ "Mi. Floyd",
                            Player == "M. Thomas" & Team == "NO" ~ "Mic. Thomas",
                            Player == "M. Thomas" & Team == "CIN" ~ "Mik. Thomas",
                            Player == "S. Smith" & Team %in% c("BAL","CAR") ~ "St. Smith",
                            Player == "S. Smith" & Team == "STL" ~ "Sh. Smith",
                            Player == "T. Hill" & Team %in% c("KC","MIA") ~ "Ty. Hill",
                            Player == "T. Hill" & Team == "NO" ~ "Ta. Hill",
                            Player == "T. Johnson" & Team %in% c("LAC","2TM") ~ "Tyr. Johnson",
                            Player == "T. Johnson" & Team %in% c("TB","HOU") ~ "Tyl. Johnson",
                            Player == "T. Jones" & Team == "DET" ~ "T.J. Jones",
                            Player == "T. Jones" & Team == "PIT" ~ "Te. Jones",
                            Player == "T. Smith" & Team == "NO" ~ "Tr. Smith",
                            Player == "T. Smith" & Team %in% c("BAL","CAR","SF","PHI") ~ "To. Smith",
                            Player == "T. Taylor" & Team %in% c("SF","CIN") ~ "Tr. Taylor",
                            Player == "T. Taylor" & Team %in% c("TEN","CLE") ~ "Ta. Taylor",
                            Player == "T. Williams" & Team == "DAL" ~ "Te. Williams",
                            Player == "T. Williams" & Team != "DAL" ~ "Ty. Williams",
                            TRUE ~ Player))

wr_rush <- rbind(wr_rush_12, wr_rush_13, wr_rush_14, wr_rush_15, wr_rush_16,
                 wr_rush_17, wr_rush_18, wr_rush_19, wr_rush_20, wr_rush_21) %>%
  mutate(Player = case_when(Player == "A. Brown" & Team %in% c("TEN","PHI") ~ "A.J. Brown",
                            Player == "A. Brown" & Team %in% c("TB","PIT") ~ "An. Brown",
                            Player == "A. Robinson" & Team %in% c("CHI","JAX","LAR") ~ "All. Robinson",
                            Player == "A. Robinson" & Team %in% c("ATL","MIN") ~ "Ald. Robinson",
                            Player == "C. Johnson" & Team == "DET" ~ "Ca. Johnson",
                            Player == "C. Johnson" & Team == "MIN" ~ "Ch. Johnson",
                            Player == "D. Harris" & Team == "NO" ~ "Deo. Harris",
                            Player == "D. Harris" & Team == "IND" ~ "DeM. Harris",
                            Player == "D. Moore" & Team == "CAR" ~ "DJ. Moore",
                            Player == "D. Moore" & Team != "CAR" ~ "Da. Moore",
                            Player == "D. Thomas" & Team == "2TM" & year == 2018 ~ "Dem. Thomas",
                            Player == "D. Thomas" & Team == "2TM" & year == 2019 ~ "DeA. Thomas",
                            Player == "D. Thomas" & Team %in% c("DEN","NYJ") ~ "Dem. Thomas",
                            Player == "D. Thomas" & Team == "KC" ~ "DeA. Thomas",
                            Player == "D. Williams" & Team == "BUF" ~ "Du. Williams",
                            Player == "D. Williams" & Team == "TEN" ~ "Da. Williams",
                            Player == "J. Brown" & Team == "PIT" ~ "Ju. Brown",
                            Player == "J. Brown" & Team == "SEA" ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2013 ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2014 & TD == 5 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2014 & TD == 2 ~ "Ja. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2015 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2017 & TD == 3 ~ "Jo. Brown",
                            Player == "J. Brown" & Team == "ARI" & year == 2017 & TD == 4 ~ "Ja. Brown",
                            Player == "J. Brown" & Team %in% c("BAL","BUF","3TM") ~ "Jo. Brown",
                            Player == "J. Jones" & Team %in% c("ATL","TEN","TB") ~ "Ju. Jones",
                            Player == "J. Jones" & Team == "BAL" ~ "Jac. Jones",
                            Player == "J. Jones" & Team %in% c("GB","OAK") ~ "Jam. Jones",
                            Player == "J. Morgan" & Team == "NO" ~ "Joe. Morgan",
                            Player == "J. Morgan" & Team %in% c("WAS","CHI") ~ "Jos. Morgan",
                            Player == "J. Nelson" & Team == "ARI" ~ "J.J. Nelson",
                            Player == "J. Nelson" & Team != "ARI" ~ "Jo. Nelson",
                            Player == "J. Ross" & Team %in% c("DET","2TM") ~ "Je. Ross",
                            Player == "J. Ross" & Team %in% c("CIN","NYG") ~ "Jo. Ross",
                            Player == "J. Smith" & Team == "NE" ~ "Jo. Smith",
                            Player == "J. Smith" & Team == "NYJ" ~ "Je. Smith",
                            Player == "J. Wright" & Team == "CIN" ~ "Jam. Wright",
                            Player == "J. Wright" & Team %in% c("MIN","CAR") ~ "Jar. Wright",
                            Player == "M. Brown" & Team %in% c("ARI","BAL") ~ "Mar. Brown",
                            Player == "M. Brown" & Team %in% c("JAX","NE","NO") ~ "Mal. Brown",
                            Player == "M. Floyd" & Team == "SD" ~ "Ma. Floyd",
                            Player == "M. Floyd" & Team %in% c("ARI","MIN","2TM") ~ "Mi. Floyd",
                            Player == "M. Thomas" & Team == "NO" ~ "Mic. Thomas",
                            Player == "M. Thomas" & Team == "CIN" ~ "Mik. Thomas",
                            Player == "S. Smith" & Team %in% c("BAL","CAR") ~ "St. Smith",
                            Player == "S. Smith" & Team == "STL" ~ "Sh. Smith",
                            Player == "T. Hill" & Team %in% c("KC","MIA") ~ "Ty. Hill",
                            Player == "T. Hill" & Team == "NO" ~ "Ta. Hill",
                            Player == "T. Johnson" & Team %in% c("LAC","2TM") ~ "Tyr. Johnson",
                            Player == "T. Johnson" & Team %in% c("TB","HOU") ~ "Tyl. Johnson",
                            Player == "T. Jones" & Team == "DET" ~ "T.J. Jones",
                            Player == "T. Jones" & Team == "PIT" ~ "Te. Jones",
                            Player == "T. Smith" & Team == "NO" ~ "Tr. Smith",
                            Player == "T. Smith" & Team %in% c("BAL","CAR","SF","PHI") ~ "To. Smith",
                            Player == "T. Taylor" & Team %in% c("SF","CIN") ~ "Tr. Taylor",
                            Player == "T. Taylor" & Team %in% c("TEN","CLE") ~ "Ta. Taylor",
                            Player == "T. Williams" & Team == "DAL" ~ "Te. Williams",
                            Player == "T. Williams" & Team != "DAL" ~ "Ty. Williams",
                            TRUE ~ Player))

wr_id <- rbind(wr_pass %>% select(Player), wr_rush %>% select(Player)) %>%
  unique() %>% arrange(Player) %>% mutate(player_id = row_number())

wr_rush <- wr_rush %>% left_join(wr_id, by = "Player")
wr_pass <- wr_pass %>% left_join(wr_id, by = "Player")

wr_total <- full_join(wr_pass %>% rename(DYAR_pass = DYAR) %>%
                        select(player_id, year, DYAR_pass),
                      wr_rush %>% rename(DYAR_rush = DYAR) %>%
                        select(player_id, year, DYAR_rush),
                      by = c("player_id","year")) %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR = DYAR_pass + DYAR_rush)

# Calculate WR Age
wr_age <- players %>%
  filter(position_group == "WR") %>% select(-position_group) %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 3), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>%
  select(-firstname, -lastname, -Player) %>%
  right_join(wr_id, by = "player_id")

#write.csv(wr_age, "age_wr.csv", row.names = FALSE, na = "")

wr_age_2 <- read.csv("data/age_wr_2.csv") %>%
  mutate(birth_date = as.Date(birth_date, "%m/%d/%Y"),
         age_22 = trunc((birth_date %--% Sys.Date()) / years(1))) %>%
  select(player_id, age_22)

wr_total <- wr_total %>%
  left_join(wr_age_2, by = "player_id") %>%
  mutate(age = age_22 - (2022 - year))

## WR year-to-year --------------------------------------------------------------------------------

# WR one-year model
wr_yty <- wr_total %>%
  filter(year < 2021) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>% drop_na()

wr_model_1 <- lm(DYAR_yr2 ~ DYAR_yr1 + age, data = wr_yty)
#summary(wr_model_2)
wr_yty <- cbind(wr_yty, prediction = predict(wr_model_1, newdata = wr_yty))
cor(wr_yty$DYAR_yr2, wr_yty$prediction)^2

ggplot(wr_yty, aes(x = DYAR_yr2, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Wide Receivers DYAR vs. Projected DYAR from One-Year Model",
       subtitle = "with 1001 WR-Seasons from 2013 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(wr_yty$DYAR_yr2, wr_yty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# WR two-year model
wr_yyty <- wr_total %>%
  filter(year < 2020) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>% drop_na()

wr_model_2 <- lm(DYAR_yr3 ~ DYAR_yr1 + DYAR_yr2 + age, data = wr_yyty)
#summary(wr_model_2)
wr_yyty <- cbind(wr_yyty, prediction = predict(wr_model_2, newdata = wr_yyty))
cor(wr_yyty$DYAR_yr3, wr_yyty$prediction)^2

ggplot(wr_yyty, aes(x = DYAR_yr3, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Wide Receivers DYAR vs. Projected DYAR from Two-Year Model",
       subtitle = "with 625 Projected WR-Seasons from 2014 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(wr_yyty$DYAR_yr3, wr_yyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# WR three-year model 
wr_yyyty <- wr_total %>%
  filter(year < 2019) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>%
  left_join(wr_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr4 = DYAR) %>%
              mutate(year = year - 3),
            by = c("player_id","year")) %>% drop_na()

wr_model_3 <- lm(DYAR_yr4 ~ DYAR_yr1 + DYAR_yr2 + DYAR_yr3 + age, data = wr_yyyty)
#summary(wr_model_3)
wr_yyyty <- cbind(wr_yyyty, prediction = predict(wr_model_3, newdata = wr_yyyty))
cor(wr_yyyty$DYAR_yr4, wr_yyyty$prediction)^2

ggplot(wr_yyyty, aes(x = DYAR_yr4, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Wide Receivers DYAR vs. Projected DYAR from Three-Year Model",
       subtitle = "with 408 Projected WR-Seasons from 2015 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(wr_yyyty$DYAR_yr4, wr_yyyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# WR Projections
wr_yyty <- wr_yyty %>% mutate(proj_id = paste0(player_id,"_",year+2))

wr_project <- wr_yyty %>% mutate(year = year + 2) %>%
  select(player_id, year, prediction) %>%
  rbind(wr_yty %>% filter(year > 2012) %>%
          mutate(year = year + 1,
                 proj_id = paste0(player_id,"_",year)) %>%
          filter(!proj_id %in% wr_yyty$proj_id) %>%
          select(player_id, year, prediction)) %>%
  pivot_wider(names_from = year, names_prefix = "proj_",
              values_from = prediction) %>%
  mutate_all(~replace(., is.na(.), 0))

for (i in 22:27) {
  
  wr_project <- wr_project %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]],
           year = i + 2000) %>%
    left_join(wr_age_2, by = "player_id") %>%
    mutate(age = age_22 - (2022 - year)) %>%
    mutate(prediction = predict(wr_model_2, newdata = .)) %>%
    select(-DYAR_yr1, -DYAR_yr2, -age, -age_22, -year) %>%
    rename(!!paste0("proj_20", i) := prediction)
  
}

## RB Stats ---------------------------------------------------------------------------------------

rb_rush <- rbind(rb_rush_12, rb_rush_13, rb_rush_14, rb_rush_15, rb_rush_16,
                 rb_rush_17, rb_rush_18, rb_rush_19, rb_rush_20, rb_rush_21) %>%
  mutate(Player = case_when(Player == "Da. Williams" ~ "Dam. Williams",
                            Player == "De. Williams" ~ "DeA. Williams",
                            Player == "D. Williams" & Team %in% c("PIT","CAR") ~ "DeA. Williams",
                            Player == "D. Williams" & Team == "KC" & year >= 2020 ~ "Dar. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2019 & TD %in% c(1, 3) ~ "Dar. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2018 ~ "Dam. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2019 & TD %in% c(2, 5) ~ "Dam. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2018 ~ "Dam. Williams",
                            Player == "D. Williams" & Team %in% c("MIA","CHI","ATL") ~ "Dam. Williams",
                            Player == "D. Johnson" & Team == "CLE" & year <= 2018 ~ "Du Johnson",
                            Player == "D. Johnson" & Team == "CLE" & year >= 2019 ~ "De Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2019 ~ "Du. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2021 ~ "Da. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2020 & TD == 1 ~ "Du. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2020 & TD != 1 ~ "Da. Johnson",
                            Player == "D. Johnson" & Team %in% c("MIA","BUF") ~ "Du. Johnson",
                            Player == "D. Johnson" & Team %in% c("ARI","NOR") ~ "Da. Johnson",
                            Player == "D. Harris" & Team == "NE" ~ "Da. Harris",
                            Player == "D. Harris" & Team != "NE" ~ "Du. Harris",
                            Player == "J. Hill" & Team == "CIN" ~ "Je. Hill",
                            Player == "J. Hill" & Team == "BAL" ~ "Ju. Hill",
                            Player == "J. Taylor" & Team == "NE" ~ "JJ. Taylor",
                            Player == "J. Taylor" & Team == "IND" ~ "Jo. Taylor",
                            Player == "J. Stewart" & Team %in% c("CAR","NYG") ~ "Jo. Stewart",
                            Player == "J. Stewart" & Team %in% c("OAK","DEN") ~ "Je. Stewart",
                            Player == "K. Williams" & Team == "BUF" ~ "Ka. Williams",
                            Player == "K. Williams" & Team %in% c("ARI","2TM") ~ "Ke. Williams",
                            Player == "D. Washington" & Team %in% c("OAK","2TM") ~ "De. Washington",
                            Player == "D. Washington" & Team %in% c("DET","NO") ~ "Dw. Washington",
                            Player == "R. Smith" & Team == "DAL" ~ "Rod. Smith",
                            Player == "R. Smith" & Team == "CAR" ~ "Rodn. Smith",
                            Player == "J. Williams" & Team == "DEN" ~ "Jav. Williams",
                            Player == "J. Williams" & Team == "GB" ~ "Jam. Williams",
                            Player == "J. Williams" & Team == "DET" & year >= 2021 ~ "Jam. Williams",
                            Player == "J. Williams" & Team == "DET" & year <= 2020 ~ "Jo. Williams",
                            Player == "J. Williams" & Team %in% c("IND","BUF","NO","WAS") ~ "Jo. Williams",
                            TRUE ~ Player))

rb_pass <- rbind(rb_pass_12, rb_pass_13, rb_pass_14, rb_pass_15, rb_pass_16,
                 rb_pass_17, rb_pass_18, rb_pass_19, rb_pass_20, rb_pass_21) %>%
  mutate(Player = case_when(Player == "Da. Williams" ~ "Dam. Williams",
                            Player == "De. Williams" ~ "DeA. Williams",
                            Player == "D. Williams" & Team %in% c("PIT","CAR") ~ "DeA. Williams",
                            Player == "D. Williams" & Team == "KC" & year >= 2020 ~ "Dar. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2019 & TD %in% c(1, 3) ~ "Dar. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2018 ~ "Dam. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2019 & TD %in% c(2, 5) ~ "Dam. Williams",
                            Player == "D. Williams" & Team == "KC" & year == 2018 ~ "Dam. Williams",
                            Player == "D. Williams" & Team %in% c("MIA","CHI","ATL") ~ "Dam. Williams",
                            Player == "D. Johnson" & Team == "CLE" & year <= 2018 ~ "Du Johnson",
                            Player == "D. Johnson" & Team == "CLE" & year >= 2019 ~ "De Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2019 ~ "Du. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2021 ~ "Da. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2020 & TD == 1 ~ "Du. Johnson",
                            Player == "D. Johnson" & Team == "HOU" & year == 2020 & TD != 1 ~ "Da. Johnson",
                            Player == "D. Johnson" & Team %in% c("MIA","BUF") ~ "Du. Johnson",
                            Player == "D. Johnson" & Team %in% c("ARI","NOR") ~ "Da. Johnson",
                            Player == "D. Harris" & Team == "NE" ~ "Da. Harris",
                            Player == "D. Harris" & Team != "NE" ~ "Du. Harris",
                            Player == "J. Hill" & Team == "CIN" ~ "Je. Hill",
                            Player == "J. Hill" & Team == "BAL" ~ "Ju. Hill",
                            Player == "J. Taylor" & Team == "NE" ~ "J.J. Taylor",
                            Player == "J. Taylor" & Team == "IND" ~ "Jo. Taylor",
                            Player == "J. Stewart" & Team %in% c("CAR","NYG") ~ "Jo. Stewart",
                            Player == "J. Stewart" & Team %in% c("OAK","DEN") ~ "Je. Stewart",
                            Player == "K. Williams" & Team == "BUF" ~ "Ka. Williams",
                            Player == "K. Williams" & Team %in% c("ARI","2TM") ~ "Ke. Williams",
                            Player == "D. Washington" & Team %in% c("OAK","2TM") ~ "De. Washington",
                            Player == "D. Washington" & Team %in% c("DET","NO") ~ "Dw. Washington",
                            Player == "R. Smith" & Team == "DAL" ~ "Rod. Smith",
                            Player == "R. Smith" & Team == "CAR" ~ "Rodn. Smith",
                            Player == "J. Williams" & Team == "DEN" ~ "Jav. Williams",
                            Player == "J. Williams" & Team == "GB" ~ "Jam. Williams",
                            Player == "J. Williams" & Team == "DET" & year >= 2021 ~ "Jam. Williams",
                            Player == "J. Williams" & Team == "DET" & year <= 2020 ~ "Jon. Williams",
                            Player == "J. Williams" & Team %in% c("IND","BUF","NO","WAS") ~ "Jon. Williams",
                            TRUE ~ Player))
  
rb_id <- rbind(rb_pass %>% select(Player), rb_rush %>% select(Player)) %>%
  unique() %>% arrange(Player) %>% mutate(player_id = row_number())

rb_rush <- rb_rush %>% left_join(rb_id, by = "Player")
rb_pass <- rb_pass %>% left_join(rb_id, by = "Player")

rb_total <- full_join(rb_pass %>% rename(DYAR_pass = DYAR) %>%
                        select(player_id, year, DYAR_pass),
                      rb_rush %>% rename(DYAR_rush = DYAR) %>%
                        select(player_id, year, DYAR_rush),
                      by = c("player_id","year")) %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR = DYAR_pass + DYAR_rush)

# Calculate RB Age
rb_age <- players %>%
  filter(position_group == "RB") %>% select(-position_group) %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 4), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 3), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  select(-firstname, -lastname, -Player) %>%
  right_join(rb_id, by = "player_id")

#write.csv(rb_age, "age_rb.csv", row.names = FALSE, na = "")

rb_age_2 <- read.csv("data/age_rb_2.csv") %>%
  select(-player_id) %>% left_join(rb_id, by = "Player") %>%
  mutate(birth_date = as.Date(birth_date, "%m/%d/%Y"),
         age_22 = trunc((birth_date %--% Sys.Date()) / years(1))) %>%
  select(player_id, age_22)

rb_total <- rb_total %>%
  left_join(rb_age_2, by = "player_id") %>%
  mutate(age = age_22 - (2022 - year))

## RB year-to-year --------------------------------------------------------------------------------

# RB one-year model
rb_yty <- rb_total %>%
  filter(year < 2021) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>% drop_na()

rb_model_1 <- lm(DYAR_yr2 ~ DYAR_yr1 + age, data = rb_yty)
#summary(rb_model_1)
rb_yty <- cbind(rb_yty, prediction = predict(rb_model_1, newdata = rb_yty))
cor(rb_yty$DYAR_yr2, rb_yty$prediction)^2

ggplot(rb_yty, aes(x = DYAR_yr2, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Running Backs DYAR vs. Projected DYAR from Two-Year Model",
       subtitle = "with 665 RB-Seasons from 2013 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(rb_yty$DYAR_yr2, rb_yty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# RB two-year model
rb_yyty <- rb_total %>%
  filter(year < 2020) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>% drop_na()

rb_model_2 <- lm(DYAR_yr3 ~ DYAR_yr1 + DYAR_yr2 + age, data = rb_yyty)
#summary(rb_model_2)
rb_yyty <- cbind(rb_yyty, prediction = predict(rb_model_2, newdata = rb_yyty))
cor(rb_yyty$DYAR_yr3, rb_yyty$prediction)^2

ggplot(rb_yyty, aes(x = DYAR_yr3, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Running Backs DYAR vs. Projected DYAR from Two-Year Model",
       subtitle = "with 394 Projected RB-Seasons from 2014 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(rb_yyty$DYAR_yr3, rb_yyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# RB three-year model 
rb_yyyty <- rb_total %>%
  filter(year < 2019) %>%
  select(player_id, year, age, DYAR) %>%
  rename(DYAR_yr1 = DYAR) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr2 = DYAR) %>%
              mutate(year = year - 1),
            by = c("player_id","year")) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr3 = DYAR) %>%
              mutate(year = year - 2),
            by = c("player_id","year")) %>%
  left_join(rb_total %>%
              select(player_id, year, DYAR) %>%
              rename(DYAR_yr4 = DYAR) %>%
              mutate(year = year - 3),
            by = c("player_id","year")) %>% drop_na()

rb_model_3 <- lm(DYAR_yr4 ~ DYAR_yr1 + DYAR_yr2 + DYAR_yr3 + age, data = rb_yyyty)
#summary(rb_model_3)
rb_yyyty <- cbind(rb_yyyty, prediction = predict(rb_model_3, newdata = rb_yyyty))
cor(rb_yyyty$DYAR_yr4, rb_yyyty$prediction)^2

ggplot(rb_yyyty, aes(x = DYAR_yr4, y = prediction)) +
  geom_point() + geom_smooth(method = 'lm') +
  labs(title = "NFL Running Backs DYAR vs. Projected DYAR from Three-Year Model",
       subtitle = "with 230 Projected RB-Seasons from 2015 to 2021",
       x = "Actual DYAR", y = "Projected DYAR",
       caption = paste0("R^2 = ", round(cor(rb_yyyty$DYAR_yr4, rb_yyyty$prediction)^2, 3))) +
  theme(plot.title = element_text(size = 20),
        plot.subtitle = element_text(size = 16),
        axis.title = element_text(size = 12),
        plot.caption = element_text(size = 15, face = "bold"))

# RB Projections
rb_yyty <- rb_yyty %>% mutate(proj_id = paste0(player_id,"_",year+2))

rb_project <- rb_yyty %>% mutate(year = year + 2) %>%
  select(player_id, year, prediction) %>%
  rbind(rb_yty %>% filter(year > 2012) %>%
          mutate(year = year + 1,
                 proj_id = paste0(player_id,"_",year)) %>%
          filter(!proj_id %in% rb_yyty$proj_id) %>%
          select(player_id, year, prediction)) %>%
  pivot_wider(names_from = year, names_prefix = "proj_",
              values_from = prediction) %>%
  mutate_all(~replace(., is.na(.), 0))

for (i in 22:26) {
  
  rb_project <- rb_project %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]],
           year = i + 2000) %>%
    left_join(rb_age_2, by = "player_id") %>%
    mutate(age = age_22 - (2022 - year)) %>%
    mutate(prediction = predict(rb_model_2, newdata = .)) %>%
    select(-DYAR_yr1, -DYAR_yr2, -age, -age_22, -year) %>%
    rename(!!paste0("proj_20", i) := prediction)
  
}

## Historical Contracts ---------------------------------------------------------------------------

historical <- readxl::read_excel("data/historical_contracts.xlsm")

# QB Dollar Valuation
qb_contract <- historical %>% filter(position == "QB", year_signed >= 2014,
                                     year_signed < 2022, guaranteed != 0) %>%
  select(player, position, year_signed, years, guaranteed, inflated_guaranteed) %>%
  separate(player, c("firstname","lastname"), sep = " ") %>%
  mutate(firstname = paste0(substr(firstname, 1, 1), ". "),
         Player = paste(firstname, lastname, sep = ""),
         year = year_signed) %>%
  select(-firstname, -lastname, -position) %>%
  left_join(qb_id, by = "Player") %>% filter(!is.na(player_id))

qb_project <- qb_project %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in seq(10)) {
  
  qb_contract <- qb_contract %>%
    left_join(qb_project, by = c("player_id","year")) %>%
    mutate(prediction = ifelse(years <= 0, 0, prediction),
           year = year + 1, years = years - 1) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

qb_contract <- qb_contract %>%
  mutate(proj_val = rowSums(across(year_1:year_10)))

qb_market <- qb_contract %>%
  group_by(year_signed) %>%
  summarize(dollars = sum(inflated_guaranteed, na.rm = TRUE) / 1000000,
            values = sum(proj_val, na.rm = TRUE),
            valuation = dollars / (values / 100))

ggplot(qb_market) +
  geom_point(aes(x = year_signed, y = dollars), size = 3, color = 'green3') +
  geom_line(aes(x = year_signed, y = dollars), size = 1.5, color = 'green3') +
  geom_point(aes(x = year_signed, y = values * 0.04), size = 3, color = 'red2') +
  geom_line(aes(x = year_signed, y = values * 0.04), size = 1.5, color = 'red2') +
  labs(title = "NFL Quarterbacks Total Market Value and Projected DYAR by Year") +
  scale_x_continuous(name = "Year Signed", breaks = c(2014:2021)) +
  scale_y_continuous(name = "Total Contract Values ($M)",
                     sec.axis = sec_axis(~. / 0.04, name = "Total Projected DYAR",
                                         breaks = c(10000, 12500, 15000,
                                                    17500, 20000, 22500)))

cor(qb_market$dollars, qb_market$values)
mean(qb_market$valuation)


# WR Dollar Valuation
wr_contract <- historical %>% filter(position == "WR", year_signed >= 2014,
                                     year_signed < 2022, guaranteed != 0) %>%
  select(player, position, year_signed, years, guaranteed, inflated_guaranteed) %>%
  separate(player, c("firstname","lastname"), sep = " ") %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 3), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>% mutate(year = year_signed) %>%
  select(-firstname, -lastname, -position) %>% filter(!is.na(player_id))

wr_project <- wr_project %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in seq(5)) {
  
  wr_contract <- wr_contract %>%
    left_join(wr_project, by = c("player_id","year")) %>%
    mutate(prediction = ifelse(years <= 0, 0, prediction),
           year = year + 1, years = years - 1) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

wr_contract <- wr_contract %>%
  mutate(proj_val = rowSums(across(year_1:year_5)))

wr_market <- wr_contract %>%
  group_by(year_signed) %>%
  summarize(dollars = sum(inflated_guaranteed, na.rm = TRUE) / 1000000,
            values = sum(proj_val, na.rm = TRUE),
            valuation = dollars / (values / 100))

ggplot(wr_market) +
  geom_point(aes(x = year_signed, y = dollars), size = 3, color = 'green3') +
  geom_line(aes(x = year_signed, y = dollars), size = 1.5, color = 'green3') +
  geom_point(aes(x = year_signed, y = values * 0.04), size = 3, color = 'red2') +
  geom_line(aes(x = year_signed, y = values * 0.04), size = 1.5, color = 'red2')+
  labs(title = "NFL Wide Receivers Total Market Value and Projected DYAR by Year") +
  scale_x_continuous(name = "Year Signed", breaks = c(2014:2021)) +
  scale_y_continuous(name = "Total Contract Values ($M)",
                     sec.axis = sec_axis(~. / 0.04, name = "Total Projected DYAR",
                                         breaks = c(5000, 10000, 15000, 20000)))

cor(wr_market$dollars, wr_market$values)
mean(wr_market$valuation)


# RB Dollar Valuation
rb_contract <- historical %>% filter(position == "RB", year_signed >= 2014,
                                     year_signed < 2022, guaranteed != 0) %>%
  select(player, position, year_signed, years, guaranteed, inflated_guaranteed) %>%
  separate(player, c("firstname","lastname"), sep = " ") %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 4), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 3), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>% mutate(year = year_signed) %>%
  select(-firstname, -lastname, -position) %>% filter(!is.na(player_id))

rb_project <- rb_project %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in seq(6)) {
  
  rb_contract <- rb_contract %>%
    left_join(rb_project, by = c("player_id","year")) %>%
    mutate(prediction = ifelse(years <= 0, 0, prediction),
           year = year + 1, years = years - 1) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

rb_contract <- rb_contract %>%
  mutate(proj_val = rowSums(across(year_1:year_6)))

rb_market <- rb_contract %>%
  group_by(year_signed) %>%
  summarize(dollars = sum(inflated_guaranteed, na.rm = TRUE) / 1000000,
            values = sum(proj_val, na.rm = TRUE),
            valuation = dollars / (values / 100))

ggplot(rb_market) +
  geom_point(aes(x = year_signed, y = dollars), size = 3, color = 'green3') +
  geom_line(aes(x = year_signed, y = dollars), size = 1.5, color = 'green3') +
  geom_point(aes(x = year_signed, y = values * 0.04), size = 3, color = 'red2') +
  geom_line(aes(x = year_signed, y = values * 0.04), size = 1.5, color = 'red2')+
  labs(title = "NFL Running Backs Total Market Value and Projected DYAR by Year") +
  scale_x_continuous(name = "Year Signed", breaks = c(2014:2021)) +
  scale_y_continuous(name = "Total Contract Values ($M)",
                     sec.axis = sec_axis(~. / 0.04, name = "Total Projected DYAR",
                                         breaks = c(2500, 3750, 5000, 6250)))

cor(rb_market$dollars, rb_market$values)
mean(rb_market$valuation)


## Current Contracts ------------------------------------------------------------------------------

# QB Assets
qb_current <- read_excel("data/current_contracts.xlsx", sheet = 1) %>%
  separate(Name, into = c("firstname", "lastname"), sep = " ") %>%
  mutate(firstname = paste0(substr(firstname, 1, 1), ". "),
         Player = paste0(firstname, lastname), year = 2023) %>%
  left_join(qb_id, by = "Player") %>% filter(!is.na(player_id)) %>%
  select(-firstname, -lastname, -Age, -`Year Signed`)

qb_two_year <- qb_total %>%
  filter(year == 2020, player_id %in% qb_current$player_id) %>%
  select(player_id, year, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  inner_join(qb_total %>%
               filter(year == 2021, player_id %in% qb_current$player_id) %>%
               mutate(year = year - 1) %>%
               select(player_id, year, DYAR) %>%
               rename(DYAR_yr2 = DYAR),
             by = c("player_id","year")) %>%
  mutate(prediction = predict(qb_model_2, newdata = .),
         age = age + 1) %>% select(-year, -DYAR_yr1)

qb_one_year <- qb_total %>%
  filter(year == 2021, player_id %in% qb_current$player_id &
         !player_id %in% qb_two_year$player_id) %>%
  select(player_id, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  mutate(prediction = predict(qb_model_1, newdata = .)) %>%
  rename(DYAR_yr2 = DYAR_yr1)
  
qb_projected <- rbind(qb_two_year, qb_one_year) %>%
  left_join(qb_pass_22 %>% 
              left_join(qb_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_pass = DYAR), by = "player_id") %>%
  left_join(qb_rush_22 %>% 
              left_join(qb_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_rush = DYAR), by = "player_id") %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR_22 = DYAR_pass + DYAR_rush) %>%
  mutate(prediction = (prediction + DYAR_22) / 2) %>%
  select(-DYAR_pass, -DYAR_rush, -DYAR_22)

for (i in 21:26) {
  
  qb_projected <- qb_projected %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]]) %>%
    mutate(prediction = predict(qb_model_2, newdata = .),
           age = age + 1) %>%
    relocate(DYAR_yr2, .after = last_col()) %>%
    relocate(prediction, .after = last_col()) %>%
    rename(!!paste0("proj_20", i) := DYAR_yr1)
  
}

qb_projected <- qb_projected %>%
  rename(proj_2027 = DYAR_yr2, proj_2028 = prediction) %>%
  select(-age) %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in 1:max(qb_current$Length)) {
  
  qb_current <- qb_current %>%
    left_join(qb_projected, by = c("player_id","year")) %>%
    mutate(year = year + 1, Length = Length - 1,
           prediction = ifelse(Length < 0, 0, prediction)) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

qb_current <- qb_current %>%
  mutate(proj_val = rowSums(across(year_1:year_6)),
         proj_dol = proj_val / 100 * mean(qb_market$valuation),
         surplus = proj_dol - (`Guaranteed Money` / 1000000)) %>%
  arrange(desc(surplus))

# WR Assets
wr_current <- read_excel("data/current_contracts.xlsx", sheet = 3) %>%
  separate(Name, c("firstname","lastname"), sep = " ", extra = "merge") %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 3), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = ""), year = 2023) %>% select(-player_id) %>%
  left_join(wr_id, by = "Player") %>% filter(!is.na(player_id)) %>%
  select(-firstname, -lastname, -Age, -`Year Signed`)

wr_two_year <- wr_total %>%
  filter(year == 2020, player_id %in% wr_current$player_id) %>%
  select(player_id, year, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  inner_join(wr_total %>%
               filter(year == 2021, player_id %in% wr_current$player_id) %>%
               mutate(year = year - 1) %>%
               select(player_id, year, DYAR) %>%
               rename(DYAR_yr2 = DYAR),
             by = c("player_id","year")) %>%
  mutate(prediction = predict(wr_model_2, newdata = .),
         age = age + 1) %>% select(-year, -DYAR_yr1)

wr_one_year <- wr_total %>%
  filter(year == 2021, player_id %in% wr_current$player_id &
           !player_id %in% wr_two_year$player_id) %>%
  select(player_id, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  mutate(prediction = predict(wr_model_1, newdata = .)) %>%
  rename(DYAR_yr2 = DYAR_yr1)

wr_projected <- rbind(wr_two_year, wr_one_year) %>%
  left_join(wr_pass_22 %>% 
              left_join(wr_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_pass = DYAR), by = "player_id") %>%
  left_join(wr_rush_22 %>% 
              left_join(wr_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_rush = DYAR), by = "player_id") %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR_22 = DYAR_pass + DYAR_rush) %>%
  mutate(prediction = (prediction + DYAR_22) / 2) %>%
  select(-DYAR_pass, -DYAR_rush, -DYAR_22)

for (i in 21:26) {
  
  wr_projected <- wr_projected %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]]) %>%
    mutate(prediction = predict(wr_model_2, newdata = .),
           age = age + 1) %>%
    relocate(DYAR_yr2, .after = last_col()) %>%
    relocate(prediction, .after = last_col()) %>%
    rename(!!paste0("proj_20", i) := DYAR_yr1)
  
}

wr_projected <- wr_projected %>%
  rename(proj_2027 = DYAR_yr2, proj_2028 = prediction) %>%
  select(-age) %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in 1:max(wr_current$Length)) {
  
  wr_current <- wr_current %>%
    left_join(wr_projected, by = c("player_id","year")) %>%
    mutate(year = year + 1, Length = Length - 1,
           prediction = ifelse(Length < 0, 0, prediction)) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

wr_current <- wr_current %>%
  mutate(proj_val = rowSums(across(year_1:year_5)),
         proj_dol = proj_val / 100 * mean(wr_market$valuation),
         surplus = proj_dol - (`Guaranteed Money` / 1000000)) %>%
  arrange(desc(surplus))

# RB Assets
rb_current <- read_excel("data/current_contracts.xlsx", sheet = 2) %>%
  separate(Name, c("firstname","lastname"), sep = " ") %>%
  mutate(firstname = gsub("'", "", firstname),
         firstname = paste0(substr(firstname, 1, 4), ". "),
         Player = paste(firstname, lastname, sep = "")) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 3), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 2), ". "), firstname),
         Player = paste(firstname, lastname, sep = "")) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>%
  mutate(firstname = ifelse(is.na(player_id), paste0(substr(firstname, 1, 1), ". "), firstname),
         Player = paste(firstname, lastname, sep = ""), year = 2023) %>% select(-player_id) %>%
  left_join(rb_id, by = "Player") %>% filter(!is.na(player_id)) %>%
  select(-firstname, -lastname, -Age, -`Year Signed`)

rb_two_year <- rb_total %>%
  filter(year == 2020, player_id %in% rb_current$player_id) %>%
  select(player_id, year, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  inner_join(rb_total %>%
               filter(year == 2021, player_id %in% rb_current$player_id) %>%
               mutate(year = year - 1) %>%
               select(player_id, year, DYAR) %>%
               rename(DYAR_yr2 = DYAR),
             by = c("player_id","year")) %>%
  mutate(prediction = predict(rb_model_2, newdata = .),
         age = age + 1) %>% select(-year, -DYAR_yr1)

rb_one_year <- rb_total %>%
  filter(year == 2021, player_id %in% rb_current$player_id &
           !player_id %in% rb_two_year$player_id) %>%
  select(player_id, age, DYAR) %>% rename(DYAR_yr1 = DYAR) %>%
  mutate(prediction = predict(rb_model_1, newdata = .)) %>%
  rename(DYAR_yr2 = DYAR_yr1)

rb_projected <- rbind(rb_two_year, rb_one_year) %>%
  left_join(rb_pass_22 %>% 
              left_join(rb_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_pass = DYAR), by = "player_id") %>%
  left_join(rb_rush_22 %>% 
              left_join(rb_id, by = "Player") %>%
              select(player_id, DYAR) %>%
              rename(DYAR_rush = DYAR), by = "player_id") %>%
  mutate(DYAR_pass = ifelse(is.na(DYAR_pass), 0, DYAR_pass),
         DYAR_rush = ifelse(is.na(DYAR_rush), 0, DYAR_rush),
         DYAR_22 = DYAR_pass + DYAR_rush) %>%
  mutate(prediction = (prediction + DYAR_22) / 2) %>%
  select(-DYAR_pass, -DYAR_rush, -DYAR_22)

for (i in 21:25) {
  
  rb_projected <- rb_projected %>%
    mutate(DYAR_yr1 = .[[ncol(.)-1]],
           DYAR_yr2 = .[[ncol(.)]]) %>%
    mutate(prediction = predict(rb_model_2, newdata = .),
           age = age + 1) %>%
    relocate(DYAR_yr2, .after = last_col()) %>%
    relocate(prediction, .after = last_col()) %>%
    rename(!!paste0("proj_20", i) := DYAR_yr1)
  
}

rb_projected <- rb_projected %>%
  rename(proj_2026 = DYAR_yr2, proj_2027 = prediction) %>%
  select(-age) %>%
  pivot_longer(cols = starts_with("proj_"), names_to = "year",
               names_prefix = "proj_", values_to = "prediction") %>%
  mutate(year = as.integer(year))

for (i in 1:max(rb_current$Length)) {
  
  rb_current <- rb_current %>%
    left_join(rb_projected, by = c("player_id","year")) %>%
    mutate(year = year + 1, Length = Length - 1,
           prediction = ifelse(Length < 0, 0, prediction)) %>%
    rename(!!paste0("year_", i) := prediction)
  
}

rb_current <- rb_current %>%
  mutate(proj_val = rowSums(across(year_1:year_4)),
         proj_dol = proj_val / 100 * mean(rb_market$valuation),
         surplus = proj_dol - (`Guaranteed Money` / 1000000)) %>%
  arrange(desc(surplus))
