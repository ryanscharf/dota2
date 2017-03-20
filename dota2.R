#keep relevant information from matches
matches_i <- match[, c(1,10)]

#let's get rid of some missing data, 35 games
badheroids <- players[players$hero_id == 0, ]
playershero <- players[!players$match_id %in% badheroids$match_id,]

#let's get rid of some bad data, 7,745 games
badleavers <- players[players$leaver_status > 0, ]
playershero <- playershero[!playershero$match_id %in% badleavers$match_id,]
                      
#only keep hero information from players
playershero <- playershero[, c(1,3,4)]

#copy match outcome to players_hero
playershero$radiant_win <- matches_i[match(playershero$match_id, matches_i$match_id), 2]

#reshape the data from long to wide format with tidyr
library("tidyr")
playershero <- spread(playershero, player_slot, hero_id)

#create results matrix for radiant and dire
radiantwinrate <- matrix(data = NA, nrow = 112, ncol = 112)
colnames(radiantwinrate) <- c(hero_names$hero_id)
rownames(radiantwinrate) <- c(hero_names$hero_id)
direwinrate <- matrix(data = NA, nrow= 112, ncol = 112)
colnames(direwinrate) <- c(hero_names$hero_id)
rownames(direwinrate) <- c(hero_names$hero_id)

#create subsets for radiant wins/losses, and dire wins/losses
library("dplyr")
radiantwin <- filter(playershero, radiant_win == "True")
radiantloss <- filter(playershero, radiant_win == "False")
direwin <- filter(playershero, radiant_win == "False")
direloss <- filter(playershero, radiant_win == "True")

#get rid of irrelevant team for each category
radiantwin[,8:12]<- NULL
radiantloss[,8:12] <- NULL
direwin[,3:7] <- NULL
direloss[,3:7] <- NULL

######
#create hero pair permutations for radiant and dire of each match
#ptm <- proc.time()
test<- apply(direloss[,3:7], 1, function(x) combn(x, 2, simplify = F))
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))

#add back the match_id and radiant_win and clean up columns
colnames(df) <- c("Hero1", "Hero2")
df$match_id <- NA
df$radiant_win <- NA
df <- df[,c(3,4,1,2)]

#add back in match_ids
for (i in 1:length(direloss$match_id)){
  x = i * 10 -9
df[x:(x+9),1] <- paste(direloss[i,1]);
}

#add back in radiant_win
for (i in 1:length(direloss$radiant_win)){
  x = i * 10 -9
  df[x:(x+9),2] <- paste(direloss[i,2]);
}

direlossvals <- df


#i need one big list of all the wins and losses without counting up anything
winformation <- radiantwinvals
colnames(winformation) <- c("match_id", "Win", "Hero1", "Hero2")
df <- direlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "False"
winformation <- rbind(winformation, df)
df <- direwinvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "True"
winformation <- rbind(winformation, df)
df <- radiantlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "False"
winformation <- rbind(winformation, df)
winformation <- arrange(winformation, match_id)

aptm <- proc.time()
write.csv(winformation, "winformation.csv")
proc.time() - ptm


#get combination frequencies through column flipping and comparison
#####DOESN'T WORK#########

#comb1 <- df
#comb2 <- df[,c(1,2,4,3)]
#colnames(comb2) <- c("match_id", "radiant_win", "Hero1", "Hero2")
#combcount <- rbind(comb1, comb2)
#combcount <- plyr::count(combcount, vars = c("Hero1", "Hero2"))
#add identifiers to each column for using the %in% function  later
#comb1$colid <- paste(comb1$Hero1, comb1$Hero2, sep=",")
#combcount$colid <- paste(combcount$Hero1, combcount$Hero2, sep=",")
#combcount <- combcount[combcount$colid %in% comb1$colid,]
#combcount$colid <- NULL

########
#apply version of combination frequencies
#nonworkign method
#rwcounts <- radiantwinvals
#rlcounts <- radiantlossvals
#dwcounts <- direwinvals
#dlcounts <- direlossvals

#countperms <- function(xz){
#xz$Hero1 <- as.numeric(as.character(xz$Hero1))
#xz$Hero2 <- as.numeric(as.character(xz$Hero2))
#xz$min <- apply(xz[,3:4],1,min)
#xz$max <- apply(xz[,3:4],1,max)
#xz$Hero1 <- NULL
#xz$Hero2 <- NULL
#colnames(xz) <- c("match_id", "radiant_win", "Hero1", "Hero2")
#xz <- plyr::count(xz, vars = c("Hero1", "Hero2"))
#xz <<- xz
#assign(paste("count", deparse(substitute(xz)) , sep="_"), xz, envir=.GlobalEnv)
#}

#countperms(rwcounts)
#countperms(rlcounts)
#countperms(dwcounts)
#countperms(dlcounts)


#example function of dynamic renaming that DOESN'T WORK
#wtf <- function(xz){
#xz <- plyr::count(xz$x1)
#assign(paste("count", deparse(substitute(xz)) , sep=""), xz, envir=.GlobalEnv)
#}

######long dumb version of permutation frequencies
###############################################################
rwcounts <- radiantwinvals
  rwcounts$Hero1 <- as.numeric(as.character(rwcounts$Hero1))
  rwcounts$Hero2 <- as.numeric(as.character(rwcounts$Hero2))
  rwcounts$min <- apply(rwcounts[,3:4],1,min)
  rwcounts$max <- apply(rwcounts[,3:4],1,max)
  rwcounts$Hero1 <- NULL
  rwcounts$Hero2 <- NULL
  colnames(rwcounts) <- c("match_id", "radiant_win", "Hero1", "Hero2")
  rwcounts <- plyr::count(rwcounts, vars = c("Hero1", "Hero2"))
  rwcounts <- arrange(rwcounts, desc(freq))
  
rlcounts <- radiantlossvals
  rlcounts$Hero1 <- as.numeric(as.character(rlcounts$Hero1))
  rlcounts$Hero2 <- as.numeric(as.character(rlcounts$Hero2))
  rlcounts$min <- apply(rlcounts[,3:4],1,min)
  rlcounts$max <- apply(rlcounts[,3:4],1,max)
  rlcounts$Hero1 <- NULL
  rlcounts$Hero2 <- NULL
  colnames(rlcounts) <- c("match_id", "radiant_win", "Hero1", "Hero2")
  rlcounts <- plyr::count(rlcounts, vars = c("Hero1", "Hero2"))
  rlcounts <- arrange(rlcounts, desc(freq))
  
dwcounts <- direwinvals
  dwcounts$Hero1 <- as.numeric(as.character(dwcounts$Hero1))
  dwcounts$Hero2 <- as.numeric(as.character(dwcounts$Hero2))
  dwcounts$min <- apply(dwcounts[,3:4],1,min)
  dwcounts$max <- apply(dwcounts[,3:4],1,max)
  dwcounts$Hero1 <- NULL
  dwcounts$Hero2 <- NULL
  colnames(dwcounts) <- c("match_id", "radiant_win", "Hero1", "Hero2")
  dwcounts <- plyr::count(dwcounts, vars = c("Hero1", "Hero2"))
  dwcounts <- arrange(dwcounts, desc(freq))
  
dlcounts <- direlossvals
  dlcounts$Hero1 <- as.numeric(as.character(dlcounts$Hero1))
  dlcounts$Hero2 <- as.numeric(as.character(dlcounts$Hero2))
  dlcounts$min <- apply(dlcounts[,3:4],1,min)
  dlcounts$max <- apply(dlcounts[,3:4],1,max)
  dlcounts$Hero1 <- NULL
  dlcounts$Hero2 <- NULL
  colnames(dlcounts) <- c("match_id", "radiant_win", "Hero1", "Hero2")
  dlcounts <- plyr::count(dlcounts, vars = c("Hero1", "Hero2"))
  dlcounts <- arrange(dlcounts, desc(freq))  
  
  
overallwins <- merge(rwcounts, dwcounts, by=c("Hero1", "Hero2"))
overallwins$freq <- rowSums(overallwins[,3:4])
overallwins[,3:4] <- NULL
  
overallosses <- merge(rlcounts, dlcounts, by=c("Hero1", "Hero2"))
overallosses$freq <- rowSums(overallosses[,3:4])
overallosses[,3:4] <- NULL

overallwr <- merge(overallwins, overallosses, by=c("Hero1", "Hero2"))
colnames(overallwr) <- c("Hero1", "Hero2", "wins", "losses")
overallwr$num_of_picks <- rowSums(overallwr[,3:4])
overallwr <- mutate(overallwr, win_percentage = wins / num_of_picks)
  
  



library("ggplot2")
library("reshape2")
library("viridis")
library("scales")
library("ggthemes")
   radiant_pairing_winm <- reshape2::acast(rwcounts, Hero1~Hero2, value.var = "freq", drop = FALSE)

   
   
   
   ##plottin
gg <- ggplot(overallwr, aes(x=Hero1, y=Hero2, fill=win_percentage)) + geom_tile(color="white", size=0.4) + 
   scale_fill_viridis(name="win rate (%)", label=comma) + coord_equal() + theme_tufte(base_family="Helvetica") +
   scale_x_continuous(breaks = unique(rwcounts$Hero1), position = "top", sec.axis = dup_axis())  + 
   scale_y_continuous(trans = "reverse", breaks = unique(rwcounts$Hero2), position = "top", sec.axis = dup_axis()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1), plot.background = element_rect(fill = 'grey'), panel.grid.major = element_line(color= "black"))


gg

#ptm <- proc.time()
#proc.time() - ptm

#radiant win%
  21922/(20298+21922)
  
#lookup table for hero names
