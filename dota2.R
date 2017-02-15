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
direwinrate <- matrix(data = NA, nrow= 112, ncol = 11)2
colnames(direwinrate) <- c(hero_names$hero_id)
rownames(direwinrate) <- c(hero_names$hero_id)

#create subsets for radiant wins/losses, and dire wins/losses
radiantwin <- filter(playershero, radiant_win == "True")
radiantloss <- filter(playershero, radiant_win == "False")
direwin <- filter(playershero, radiant_win == "False")
direloss <- filter(playershero, radiant_win == "True")

#create hero pair permutations for radiant and dire of each match
test<- apply(Ttest[,3:7], 1, function(x) combn(x, 2, simplify = F))
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))

#add back the match_id and radiant_win and clean up columns
colnames(df) <- c("Hero1", "Hero2")
df$match_id <- NA
df$radiant_win <- NA
df <- df[,c(3,4,1,2)]

#add back in match_ids
for (i in 1:length(Ttest$match_id)){
  x = i * 10 -9
df[x:(x+9),1] <- paste(Ttest[i,1]);
}

#add back in radiant_win
for (i in 1:length(Ttest$radiant_win)){
  x = i * 10 -9
  df[x:(x+9),2] <- paste(Ttest[i,2]);
}

#get combination frequencies
ptm <- proc.time()

comb1 <- df
comb2 <- df[,c(1,2,4,3)]
colnames(comb2) <- c("match_id", "radiant_win", "Hero1", "Hero2")
combcount <- rbind(comb1, comb2)
combcount <- plyr::count(combcount, vars = c("Hero1", "Hero2"))
#add identifiers to each column for using the %in% function  later
comb1$colid <- paste(comb1$Hero1, comb1$Hero2, sep=",")
combcount$colid <- paste(combcount$Hero1, combcount$Hero2, sep=",")
combcount <- combcount[combcount$colid %in% comb1$colid,]
combcount$colid <- NULL

proc.time() - ptm


#apply version of combination frequencies
#ptm <- proc.time()

#df$Hero1 <- as.numeric(as.character(df$Hero1))
#df$Hero2 <- as.numeric(as.character(df$Hero2))
#df$min <- apply(df[,3:4],1,min)
#df$max <- apply(df[,3:4],1,max)
#df$Hero1 <- NULL
#df$Hero2 <- NULL
#colnames(df) <- c("match_id", "radiant_win", "Hero1", "Hero2")
#combcount <- plyr::count(df, vars = c("Hero1", "Hero2"))
##add identifiers to each column for using the %in% function  later
##comb1$colid <- paste(comb1$Hero1, comb1$Hero2, sep=",")
##combcount$colid <- paste(combcount$Hero1, combcount$Hero2, sep=",")
##combcount <- combcount[combcount$colid %in% comb1$colid,]
##combcount$colid <- NULL

#proc.time() - ptm

#lookup table for hero names
