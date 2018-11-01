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

setwd("~/Documents/Data/Université/Football-R-Analysis")

# VARIABLES ----
path <- "./Data/matchs/"
infos <- as.data.frame(read_csv("Data/2014-2015_20150526_Premier League_Player_Statistics.csv"))
dirList <- list.dirs(path = path, full.names = TRUE, recursive = TRUE)
dirList <- dirList[dirList != path]
csvList <- NULL
tmpMat <- NULL
tmpLength <- NULL

# FUNCTIONS ----
convertToKmh <- function(speedInMs){
  return(speedInMs*3.6)
}

loadData <- function(mList, mMatch) {
  
  playerDataList = NULL
  for (i in 1:length(mList)) {
    
    # READING EACH CSV IN THE LIST
    df <- read_csv(mList[i])
    MatchPlayed <- mList[i]
    #MatchPlayed <- './Data/matchs//Arsenal v Manchester City-20140913/20140913-Arsenal v Manchester City-Aaron RAMSEY-Trajectory.csv'
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
        Pos <- infos[grepl(gsub('.*\\ ', '', df$Player1Name[1]), toupper(infos$Player)) & 
                       grepl(MatchPlayed, infos$Match),]$Position[1]
        if (is.null(Pos)) {
          # SETTING NA IF POS==NULL
          Pos <- 'NA'
        }
        mDpzv <- cbind(MatchPlayed, Pos, mDpzv)
      }
    }
    
    # MAGIC PIECE OF ART (∩｀-´)⊃━☆ﾟ.*･｡ﾟ
    playerDataList <- rbind(playerDataList, cbind(df$Player1Name[1], mDpzv))
    
  }
  
  return(playerDataList)
}

# MAIN SCRIPT ----
for (i in 1:length(dirList)) {
  # GETTING ALL CSVs, ENDING BY '*Trajectory.csv'
  tmpLength <- length(csvList)
  csvList <- c(csvList, list.files(dirList[i], 
                                   pattern="*Trajectory.csv",
                                   full.names=TRUE))
  tmpMat <- rbind(tmpMat, cbind(length(csvList)-tmpLength,
                                gsub(path,'', dirList[i])))
  
  if (i == length(dirList)) {
    # LOADING DATA
    playerDataList <- loadData(csvList, i) 
  }
}

# CONVERTING BAD TYPES
playerDataList <- as.data.frame(playerDataList)
playerDataList$TimePlayed <- as.numeric(as.character(playerDataList$TimePlayed))
playerDataList[, 4] <- as.numeric(as.character(playerDataList[, 4]))
playerDataList[, 5] <- as.numeric(as.character(playerDataList[, 5]))
playerDataList[, 6] <- as.numeric(as.character(playerDataList[, 6]))
playerDataList[, 7] <- as.numeric(as.character(playerDataList[, 7]))
playerDataList[, 8] <- as.numeric(as.character(playerDataList[, 8]))
playerDataList[, 9] <- as.numeric(as.character(playerDataList[, 9]))

# TIME PLAYED NORMALIZATION y = x * 90 / z
playerDataList$Dpzv1Norm <- playerDataList$Dpzv1 * 
  90 / 
  playerDataList$TimePlayed
playerDataList$Dpzv2Norm <- playerDataList$Dpzv2 * 
  90 / 
  playerDataList$TimePlayed
playerDataList$Dpzv3Norm <- playerDataList$Dpzv3 * 
  90 / 
  playerDataList$TimePlayed
playerDataList$Dpzv4Norm <- playerDataList$Dpzv4 * 
  90 / 
  playerDataList$TimePlayed
playerDataList$Dpzv5Norm <- playerDataList$Dpzv5 * 
  90 / 
  playerDataList$TimePlayed

# SUMMARISE W/ DPLYR
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
            TimePlayed.Mean = mean(TimePlayed),
            TotalDistance.Mean = mean(Total))

# LITTLE BIT OF CLEANING
dat <- c("infos", 
         "playerDataList",
         "postData")
rm(list=setdiff(ls(), dat))

# SAVE DATA TO CSV ----
write.csv(playerDataList, file = "fiche_par_joueur.csv")
write.csv(postData, file = "fiche_par_poste.csv")

# CHARTING ----

#add 2 lines to the dataframe: the max and min of each topic to show on the plot!
data <- as.matrix(t(postData[-nrow(postData),] ))
colnames(data) <- as.character(unlist(data[1,]))
data <- data[-1,]
class(data) <- "numeric" 
data <- t(data)

max <- colMaxs(data)
min <- colMins(data)
data <- as.data.frame(rbind(max, min, data))



#==================
# Plot 1: Default radar chart proposed by the library:
radarchart(data)


#==================
# Plot 2: Same plot with custom features
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data, axistype=1, 
            #custom polygon
            pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)



#=================
# Plot3: If you remove the 2 first lines, the function compute the max and min of each variable with the available data:
colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
radarchart( data[-c(1,2),], axistype=0 , maxmin=F,
            #custom polygon
            pcol=colors_border , pfcol=colors_in , plwd=4 , plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="black", cglwd=0.8, 
            #custom labels
            vlcex=0.8 
)
legend(x=0.7, y=1, legend = rownames(data[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "grey", cex=1.2, pt.cex=3)

colors_border=c( rgb(0.2,0.5,0.5,0.9), rgb(0.8,0.2,0.5,0.9) , rgb(0.7,0.5,0.1,0.9) )
colors_in=c( rgb(0.2,0.5,0.5,0.4), rgb(0.8,0.2,0.5,0.4) , rgb(0.7,0.5,0.1,0.4) )
data.two <- rbind(data[c(1,2,6,9,11), -c(1:6)], colMeans2(as.matrix(data[-c(1:6)])))
radarchart( data.two, axistype=1, 
            #custom polygon
            pcol=colors_border, pfcol=colors_in, plwd=4, plty=1,
            #custom the grid
            cglcol="grey", cglty=1, axislabcol="grey", caxislabels=seq(0,20,5), cglwd=0.8,
            #custom labels
            vlcex=0.8 ,
            title = "Comparaison des postes par diagramme de Kiviat"
)
legend(x=1, y=1, legend = rownames(data.two[-c(1,2),]), bty = "n", pch=20 , col=colors_in , text.col = "black", cex=1.2, pt.cex=3)



rm(infos, playerDataList)

# Create dataset
data.two <- rbind(postData[1,], postData[2,], postData[3,], postData[4,])

data.two <- data.two %>%
  gather(Dpzv, Value, 
         Dpzv1Norm.Mean:Dpzv5Norm.Mean, 
         factor_key=TRUE) %>%
  arrange(Pos)
data.two <- data.two[c('Dpzv', 'Pos', 'Value')]
rm(postData)
write.csv(data.two, file = "fiche.csv")
data.two <- as.data.frame(read_csv("fiche.csv")[,2:4])
data.two$Pos <- as.factor(data.two$Pos)
data.two$Dpzv <- as.factor(data.two$Dpzv)
data.two$Value <- data.two$Value / 50


data=data.frame(
  individual=paste( "Mister ", seq(1,60), sep=""),
  group=c( rep('A', 10), rep('B', 30), rep('C', 14), rep('D', 6)) ,
  value=sample( seq(10,100), 60, replace=T)
)

# Set a number of 'empty bar' to add at the end of each group
empty_bar = 3
to_add = as.data.frame(matrix(NA, empty_bar*nlevels(data.two$Pos), ncol(data.two)) )
colnames(to_add) = colnames(data.two)
to_add$Pos=rep(levels(data.two$Pos), each=empty_bar)
data.two=rbind(data.two, to_add)
data.two=data.two %>% arrange(Pos)
data.two$id=seq(1, nrow(data.two))

# Get the name and the y position of each label
label_data = data.two
number_of_bar = nrow(label_data)
angle = 90 - 360 * (label_data$id-0.5) / number_of_bar     # I substract 0.5 because the letter must have the angle of the center of the bars. Not extreme right(1) or extreme left (0)
label_data$hjust <- ifelse(angle < -90, 1, 0)
label_data$angle <- ifelse(angle < -90, angle+180, angle)

# prepare a data frame for base lines
base_data=data.two %>% 
  group_by(Pos) %>% 
  summarize(start=min(id), end=max(id) - empty_bar) %>% 
  rowwise() %>% 
  mutate(title=mean(c(start, end)))

# prepare a data frame for grid (scales)
grid_data = base_data
grid_data$end = grid_data$end[ c( nrow(grid_data), 1:nrow(grid_data)-1)] + 1
grid_data$start = grid_data$start - 1
grid_data=grid_data[-1,]

# Make the plot
p = ggplot(data.two, aes(x=as.factor(id), y=Value, fill=Pos)) +       # Note that id is a factor. If x is numeric, there is some space between the first bar
  
  geom_bar(aes(x=as.factor(id), y=Value, fill=Pos), stat="identity", alpha=0.5) +
  
  # Add a val=100/75/50/25 lines. I do it at the beginning to make sur barplots are OVER it.
  geom_segment(data=grid_data, aes(x = end, y = 80, xend = start, yend = 80), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 60, xend = start, yend = 60), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 40, xend = start, yend = 40), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  geom_segment(data=grid_data, aes(x = end, y = 20, xend = start, yend = 20), colour = "grey", alpha=1, size=0.3 , inherit.aes = FALSE ) +
  
  # Add text showing the value of each 100/75/50/25 lines
  annotate("text", x = rep(max(data.two$id),4), y = c(20, 40, 60, 80), label = c("20", "40", "60", "80") , color="grey", size=3 , angle=0, fontface="bold", hjust=1) +
  
  geom_bar(aes(x=as.factor(id), y=Value, fill=Pos), stat="identity", alpha=0.5) +
  ylim(-100,120) +
  theme_minimal() +
  theme(
    legend.position = "none",
    axis.text = element_blank(),
    axis.title = element_blank(),
    panel.grid = element_blank(),
    plot.margin = unit(rep(-1,4), "cm") 
  ) +
  coord_polar() + 
  geom_text(data=label_data, aes(x=id, y=Value+10, label=Dpzv, hjust=hjust), color="black", fontface="bold",alpha=0.6, size=2.5, angle= label_data$angle, inherit.aes = FALSE ) +
  
  # Add base line information
  geom_segment(data=base_data, aes(x = start, y = -5, xend = end, yend = -5), colour = "black", alpha=0.8, size=0.6 , inherit.aes = FALSE )  +
  geom_text(data=base_data, aes(x = title, y = -18, label=c('A', 'B', 'C', 'D')), hjust=c(1,1,0,0), colour = "black", alpha=0.8, size=4, fontface="bold", inherit.aes = FALSE)

p
