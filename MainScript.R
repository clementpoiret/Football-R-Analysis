# LICENSE ----
#
# Copyright 2018 Clement POIRET.
#
# Licensed under the Apache License, Version 2.0 (the "License");
# you may not use this file except in compliance with the License.
# You may obtain a copy of the License at
#
# http://www.apache.org/licenses/LICENSE-2.0
# 
# Unless required by applicable law or agreed to in writing, software
# distributed under the License is distributed on an "AS IS" BASIS,
# WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
# See the License for the specific language governing permissions and
# limitations under the License.
#
# INITIAL SETUP ----
rm(list=ls())

library(tidyverse)
library(fmsb)
library(matrixStats)
library(reshape2)
library(plotly)

setwd("~/Documents/Data/Université/Football-R-Analysis")

# VARIABLES ----
path <- "./Data/matchs/"
infos <- as.data.frame(read_csv("Data/2014-2015_20150526_Premier League_Player_Statistics.csv"))
dirList <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)
dirList <- dirList[dirList != path]
csvList <- NULL
tmpMat <- NULL

# FUNCTIONS ----
convertToKmh <- function(speedInMs){
  return(speedInMs*3.6)
}

loadData <- function(mList, mMatch, wrong_names) {
  
  playerDataList = NULL
  for (i in 1:length(mList)) {
    
    # READING EACH CSV IN THE LIST
    df <- read_csv(mList[i])
    MatchPlayed <- mList[i]
    MatchPlayed <- gsub('./Data/matchs//', '', MatchPlayed)
    MatchPlayed <- strsplit(MatchPlayed, "[/]")[[1]][1]
    MatchPlayed <- substr(MatchPlayed,1,nchar(MatchPlayed)-9)
    
    # FOR EACH CSV, LOOOOOOPING ;D
    half <- unique(df$Half)
    mDpzv <- NULL
    for (i in 1:2) {
      # JUST IN CASE ʕᵔᴥᵔʔ
      if (i == 1 & !is.null(mDpzv)) {
        mDpzv <- NULL
      }
      
      # HELL YEAH, JUST GETTIN' SPEED ¯\_(ツ)_/¯ WHILE SAVING IT IN 'ds'
      #df[df$Half==half[i],]
      xDiff <- diff(df[df$Half==half[i],]$XPos)
      yDiff <- diff(df[df$Half==half[i],]$Ypos)
      tDiff <- diff(df[df$Half==half[i],]$Time)
      
      speed <- convertToKmh(sqrt((yDiff^2) + (xDiff^2))/tDiff)
      
      d = sqrt((yDiff^2) + (xDiff^2))
      
      ds = cbind(d, speed)
      
      # SETTING DISTANCES PER SPEED ZONE 
      Dpzv1 = sum(subset(ds, speed < 6)[,1])
      Dpzv2 = sum(subset(ds, speed >= 6 & speed < 10)[,1])
      Dpzv3 = sum(subset(ds, speed >= 10 & speed < 14)[,1])
      Dpzv4 = sum(subset(ds, speed >= 14 & speed < 20)[,1])
      Dpzv5 = sum(subset(ds, speed >= 20)[,1])
      Total = sum(Dpzv1, Dpzv2, Dpzv3, Dpzv4, Dpzv5)
      TimePlayed = length(df$Time) / 600 / 2
      
      mDpzv <- t(colSums(as.matrix(rbind(mDpzv, cbind(Dpzv1, 
                                                      Dpzv2, 
                                                      Dpzv3, 
                                                      Dpzv4, 
                                                      Dpzv5,
                                                      Total,
                                                      TimePlayed)))))
      if (i == 2) {
        # END OF LOOP, MERGING MATCH POSITION AND PLAYER DATA
        mDpzv <- as.data.frame(mDpzv)
        
        # CHECK IF NAME IS IN LIST, IF YES: USE FIXED NAME
        nameToSearch <- ''
        
        if (gsub('.*\\ ', '', df$Player1Name[1]) %in% names(wrong_names)) {
          nameToSearch <- wrong_names[[gsub('.*\\ ', '', df$Player1Name[1])]]
        } else {
          nameToSearch <- gsub('.*\\ ', '', df$Player1Name[1])
        }
        
        Pos <- infos[grepl(nameToSearch, toupper(infos$Player)) & 
                       grepl(MatchPlayed, infos$Match),]$Position[1]
        
        if (is.null(Pos)) {
          # SETTING NA IF POS==NULL
          Pos <- 'NA'
        }
        
        Team <- infos[grepl(nameToSearch, toupper(infos$Player)) & 
                       grepl(MatchPlayed, infos$Match),]$Team[1]
        
        if (is.null(Team)) {
          # SETTING NA IF POS==NULL
          Team <- 'NA'
        }
        
        mDpzv <- cbind(MatchPlayed, Pos, Team, mDpzv)
      }
    }
    
    # MAGIC PIECE OF ART (∩｀-´)⊃━☆ﾟ.*･｡ﾟ
    playerDataList <- rbind(playerDataList, cbind(df$Player1Name[1], mDpzv))
    
  }
  
  return(playerDataList)
}

# MAIN SCRIPT ----
# CREATE LIST WITH ERRORS AND FIXES
wrong_names <- vector(mode='list', length=3)
names(wrong_names) <- c('SANCHEZ', 'OEZIL', 'AGUERO')
wrong_names[[1]] <- 'ALEXIS'
wrong_names[[2]] <- 'ÖZIL'
wrong_names[[3]] <- toupper('Agüero')

for (i in 1:length(dirList)) {
  # GETTING ALL CSVs, ENDING BY '*Trajectory.csv'
  csvList <- c(csvList, list.files(dirList[i], 
                                   pattern="*Trajectory.csv",
                                   full.names=TRUE))
  tmpMat <- rbind(tmpMat, gsub(path,'', dirList[i]))
  
  if (i == length(dirList)) {
    # LOADING DATA
    playerDataList <- loadData(csvList, i, wrong_names) 
  }
}

# NORMALIZATION y = x * 90 / z
i <- 5
while (i <= 10) {
  col_playerDataList <- ncol(playerDataList)
  playerDataList[, col_playerDataList+1] <- playerDataList[,i] *
    90 / playerDataList$TimePlayed
  colnames(playerDataList)[col_playerDataList+1] <- paste(colnames(playerDataList)[i], 
                                                          'Norm', sep='')
  i = i + 1
}

# SUMMARISE W/ DPLYR (GLOBAL STATS AND STATS PER MATCH)
postData <- playerDataList %>%
  group_by(Pos) %>%
  summarise(Dpzv1.Mean = mean(Dpzv1),
            Dpzv2.Mean = mean(Dpzv2),
            Dpzv3.Mean = mean(Dpzv3),
            Dpzv4.Mean = mean(Dpzv4),
            Dpzv5.Mean = mean(Dpzv5),
            Dpzv1Norm.Mean = mean(Dpzv1Norm),
            Dpzv2Norm.Mean = mean(Dpzv2Norm),
            Dpzv3Norm.Mean = mean(Dpzv3Norm),
            Dpzv4Norm.Mean = mean(Dpzv4Norm),
            Dpzv5Norm.Mean = mean(Dpzv5Norm),
            D1N.SD = sd(Dpzv1Norm),
            D2N.SD = sd(Dpzv2Norm),
            D3N.SD = sd(Dpzv3Norm),
            D4N.SD = sd(Dpzv4Norm),
            D5N.SD = sd(Dpzv5Norm),
            TimePlayed.Mean = mean(TimePlayed),
            Time.SD = sd(TimePlayed),
            TotalDistanceNorm.Mean = mean(TotalNorm),
            TotalDistanceNorm.SD = sd(TotalNorm),
            TotalDistance.Mean = mean(Total),
            TotalDistance.SD = sd(Total))

postDataExtended <- playerDataList %>%
  group_by(.dots=c('Pos','MatchPlayed', 'Team')) %>%
  summarise(Dpzv1.Mean = mean(Dpzv1),
            Dpzv2.Mean = mean(Dpzv2),
            Dpzv3.Mean = mean(Dpzv3),
            Dpzv4.Mean = mean(Dpzv4),
            Dpzv5.Mean = mean(Dpzv5),
            Dpzv1Norm.Mean = mean(Dpzv1Norm),
            Dpzv2Norm.Mean = mean(Dpzv2Norm),
            Dpzv3Norm.Mean = mean(Dpzv3Norm),
            Dpzv4Norm.Mean = mean(Dpzv4Norm),
            Dpzv5Norm.Mean = mean(Dpzv5Norm),
            TimePlayed.Mean = mean(TimePlayed),
            TotalDistanceNorm.Mean = mean(TotalNorm),
            TotalDistance.Mean = mean(Total))

postDataTeam <- playerDataList %>%
  group_by(.dots=c('Pos', 'Team')) %>%
  summarise(Dpzv1.Mean = mean(Dpzv1),
            Dpzv2.Mean = mean(Dpzv2),
            Dpzv3.Mean = mean(Dpzv3),
            Dpzv4.Mean = mean(Dpzv4),
            Dpzv5.Mean = mean(Dpzv5),
            Dpzv1Norm.Mean = mean(Dpzv1Norm),
            Dpzv2Norm.Mean = mean(Dpzv2Norm),
            Dpzv3Norm.Mean = mean(Dpzv3Norm),
            Dpzv4Norm.Mean = mean(Dpzv4Norm),
            Dpzv5Norm.Mean = mean(Dpzv5Norm),
            TimePlayed.Mean = mean(TimePlayed),
            TotalDistanceNorm.Mean = mean(TotalNorm),
            TotalDistance.Mean = mean(Total))

# LITTLE BIT OF CLEANING
dat <- c('infos', 
         'playerDataList',
         'postDataExtended',
         'postData',
         'postDataTeam')
rm(list=setdiff(ls(), dat))

# SAVE DATA TO CSV ----
write.csv(playerDataList, file = "fiche_par_joueur.csv")
write.csv(postData, file = "fiche_par_poste.csv")
write.csv(postDataExtended, file = "fiche_par_poste_etendue.csv")

# CHARTING ----

# RADARCHART
# DEF
data <- t(postDataTeam[grepl('defender', postDataTeam$Pos),][,-1] %>%
            group_by(Team) %>%
            summarise(Dpzv1 = mean(Dpzv1Norm.Mean),
                      Dpzv2 = mean(Dpzv2Norm.Mean),
                      Dpzv3 = mean(Dpzv3Norm.Mean),
                      Dpzv4 = mean(Dpzv4Norm.Mean),
                      Dpzv5 = mean(Dpzv5Norm.Mean)) %>%
            arrange(Team))
colnames(data) <- data[1,]
data <- t(data[-1,])
class(data) <- 'numeric'

max <- colMaxs(data)
min <- colMins(data)
data <- as.data.frame(rbind(max, min, data))

colors = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))
radarchart(data, axistype=1, 
           pcol=colors, pfcol=colors, plwd=0.5, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.4,
           title = "Défenseurs"
)

# MIDFIELD
data <- t(postDataTeam[grepl('midfielder', postDataTeam$Pos),][,-1] %>%
            group_by(Team) %>%
            summarise(Dpzv1 = mean(Dpzv1Norm.Mean),
                      Dpzv2 = mean(Dpzv2Norm.Mean),
                      Dpzv3 = mean(Dpzv3Norm.Mean),
                      Dpzv4 = mean(Dpzv4Norm.Mean),
                      Dpzv5 = mean(Dpzv5Norm.Mean)) %>%
            arrange(Team))
colnames(data) <- data[1,]
data <- t(data[-1,])
class(data) <- 'numeric'

max <- colMaxs(data)
min <- colMins(data)
data <- as.data.frame(rbind(max, min, data))

colors = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))
radarchart(data, axistype=1,
           pcol=colors, pfcol=colors, plwd=0.5, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.4,
           title = "Milieux"
)


# FORWARD
data <- t(postDataTeam[grepl('forward', postDataTeam$Pos),][,-1] %>%
            group_by(Team) %>%
            summarise(Dpzv1 = mean(Dpzv1Norm.Mean),
                      Dpzv2 = mean(Dpzv2Norm.Mean),
                      Dpzv3 = mean(Dpzv3Norm.Mean),
                      Dpzv4 = mean(Dpzv4Norm.Mean),
                      Dpzv5 = mean(Dpzv5Norm.Mean)) %>%
            arrange(Team))
colnames(data) <- data[1,]
data <- t(data[-1,])
class(data) <- 'numeric'

max <- colMaxs(data)
min <- colMins(data)
data <- as.data.frame(rbind(max, min, data))

colors = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))
radarchart(data, axistype=1,
           pcol=colors, pfcol=colors, plwd=0.5, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.4,
           title = "Avants"
)

# GOAL
data <- t(postDataTeam[grepl('Goal', postDataTeam$Pos),][,-1] %>%
            group_by(Team) %>%
            summarise(Dpzv1 = mean(Dpzv1Norm.Mean),
                      Dpzv2 = mean(Dpzv2Norm.Mean),
                      Dpzv3 = mean(Dpzv3Norm.Mean),
                      Dpzv4 = mean(Dpzv4Norm.Mean),
                      Dpzv5 = mean(Dpzv5Norm.Mean)) %>%
            arrange(Team))
colnames(data) <- data[1,]
data <- t(data[-1,])
class(data) <- 'numeric'

max <- colMaxs(data)
min <- colMins(data)
data <- as.data.frame(rbind(max, min, data))

colors = c(rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4), rgb(0.7,0.5,0.1,0.4))
radarchart(data, axistype=1, 
           pcol=colors, pfcol=colors, plwd=0.5, plty=1,
           cglcol="grey", cglty=1, axislabcol="grey", cglwd=0.4,
           title = "Gardiens"
)

## Cleanup after charting
dat <- c('infos', 
         'playerDataList',
         'postDataExtended',
         'postData',
         'postDataTeam')
rm(list=setdiff(ls(), dat))

# WORK IN PROGRESS
p <- plot_ly(playerDataList, y = ~TotalNorm, color = ~Pos, type = "box")
p

ddf2 = melt(postDataExtended[,c(1,3,15)])
ggplot(ddf2, aes(x=Pos, y=value, fill=Team))+
  geom_bar(stat='identity', position='dodge') +
  coord_cartesian(ylim = c(4000, 12500)) + 
  theme(axis.text.x = element_text(angle=45)) +
  scale_x_discrete(labels=c(1:11))

ddf3 = melt(postData[,c(1,7,8,9,10,11)])
ggplot(ddf3, aes(x=Pos, y=value, fill=variable))+
  geom_bar(stat='identity', position='dodge') + 
  theme(axis.text.x = element_text(angle=45)) +
  scale_x_discrete(labels=c(1:11))

dat <- c('infos', 
         'playerDataList',
         'postDataExtended',
         'postData',
         'postDataTeam')
rm(list=setdiff(ls(), dat))
