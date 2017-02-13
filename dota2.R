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
test<- combn(Ttest1, 2, simplify=F)
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))

combdf <- unlist(test, recursive = FALSE)
df<- do.call("rbind", combdf)
















#lookup table for hero names
