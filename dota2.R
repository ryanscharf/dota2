#keep relevant information from matches
matches_i <- match[, c(1,10)]

#copy match outcome to players_hero
playershero$radiant_win <- matches_i[match(playershero$match_id, matches_i$match_id), 2]

#let's get rid of some missing data
badheroids <- players[players$hero_id == 0, ]
playershero <- players[!players$match_id %in% badheroids$match_id,]

#only keep hero information from players
playershero <- playershero[, c(1,3,4)]

#