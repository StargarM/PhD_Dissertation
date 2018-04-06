
# Clear variable list
rm(list=ls())

library(jsonlite); library(magrittr); library(dplyr); library(stringr); library(ggplot2)

url <- 'https://statsapi.web.nhl.com/api/v1/teams/'
All_teams <- fromJSON(url)
All_teams.df <- data.frame(All_teams)
Team.ids <- All_teams.df[,'teams.id']
Team.Names <- All_teams.df[,'teams.name']
Suffix <- '?expand=team.stats&season=20172018'
Team_List <- vector("list", length(Team.ids))

for (i in 1:length(Team.ids)){
  url_team <- paste(url, Team.ids[i], Suffix, sep="")
  Team_List[[i]] <- fromJSON(url_team)
}

Rank <- Summary <- matrix(data = NA, nrow = length(Team.ids), ncol = length(Team_List[[1]]$teams$teamStats[[1]]$splits[[1]]$stat))
Statistic <- colnames(Team_List[[1]]$teams$teamStats[[1]]$splits[[1]]$stat)

for (i in 1:length(Team.ids)){
  for (j in 1:length(Team_List[[1]]$teams$teamStats[[1]]$splits[[1]]$stat)){
    Summary[i,j] = Team_List[[i]]$teams$teamStats[[1]]$splits[[1]]$stat[1,j]
    Rank[i,j] = Team_List[[i]]$teams$teamStats[[1]]$splits[[1]]$stat[2,j]
  }
}

Rank <- data.frame(Rank); Summary <- data.frame(Summary)

rownames(Summary) <- Team.Names; colnames(Summary) <- Statistic
rownames(Rank) <- Team.Names; colnames(Rank) <- Statistic

# https://stackoverflow.com/questions/22772279/converting-multiple-columns-from-character-to-numeric-format-in-r
Summary2 <- Summary
Summary2[,1:ncol(Summary)] <- sapply(Summary2[,1:ncol(Summary)],as.character)
Summary2[,1:ncol(Summary)] <- sapply(Summary2[,1:ncol(Summary)],as.numeric)
Class_Summary <- sapply(Summary2[,1:ncol(Summary)],class)

# https://stackoverflow.com/questions/47445040/using-gsub-on-a-dataframe?rq=1
Rank2 <- Rank
Rank2[] <- sapply(Rank2, function(x) gsub("st","",x))
Rank2[] <- sapply(Rank2, function(x) gsub("nd","",x))
Rank2[] <- sapply(Rank2, function(x) gsub("rd","",x))
Rank2[] <- sapply(Rank2, function(x) gsub("th","",x))
Rank2[,1:ncol(Rank)] <- sapply(Rank2[,1:ncol(Rank)],as.character)
Rank2[,1:ncol(Rank)] <- sapply(Rank2[,1:ncol(Rank)],as.numeric)
Class_Rank <- sapply(Rank2[,1:ncol(Rank)],class)

ggplot(Summary2, aes(x = reorder(rownames(Summary2),Summary2$wins),y=Summary2$wins)) + xlab("") + 
  ylab("Season Wins") + geom_bar(stat = "identity") + coord_flip()
ggplot(Summary2, aes(x = reorder(rownames(Summary2),Summary2$evGGARatio),y=Summary2$evGGARatio)) + xlab("") + 
  ylab("Goals/Goals Against Ratio") + geom_bar(stat = "identity") + coord_flip()

