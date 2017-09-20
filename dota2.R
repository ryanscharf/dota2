library("tidyr")
library("rgexf")
library("dplyr")
library("ggplot2")
library("reshape2")
library("viridis")
library("scales")
library("ggthemes")
library("circlize")


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

playershero <- spread(playershero, player_slot, hero_id)

#create results matrix for radiant and dire
radiantwinrate <- matrix(data = NA, nrow = 112, ncol = 112)
colnames(radiantwinrate) <- c(hero_names$hero_id)
rownames(radiantwinrate) <- c(hero_names$hero_id)
direwinrate <- matrix(data = NA, nrow= 112, ncol = 112)
colnames(direwinrate) <- c(hero_names$hero_id)
rownames(direwinrate) <- c(hero_names$hero_id)

#create subsets for radiant wins/losses, and dire wins/losses
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
df$Win <- TRUE
winformation <- rbind(winformation, df)
df <- radiantlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- FALSE
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

##here's a function to sort heroes, such that the hero with the lower id is always Hero1 and the higher id
##is Hero2.  This also allows us to make sure that hero pairs/permutations IE (AB and BA) are only counted
##as a single pair rathere than two different ones. 

sortheroes <- function(df){
  fundf<- df
  fundf$min <- apply(fundf[,3:4],1,min)
  fundf$max <- apply(fundf[,3:4],1,max)
  fundf$Hero1 <- NULL
  fundf$Hero2 <- NULL
  colnames(fundf) <- c("match_id", "radiant_win", "Hero1", "Hero2")
  fundf <- plyr::count(fundf, vars = c("Hero1", "Hero2"))
  fundf <<- arrange(fundf, desc(freq))  
}


######long dumb version of permutation frequencies
###############################################################
##Now slightly less dumb
rwcounts <- radiantwinvals
  rwcounts$Hero1 <- as.numeric(as.character(rwcounts$Hero1))
  rwcounts$Hero2 <- as.numeric(as.character(rwcounts$Hero2))
  sortheroes(rwcounts)
  rwcounts <- fundf
  
rlcounts <- radiantlossvals
  rlcounts$Hero1 <- as.numeric(as.character(rlcounts$Hero1))
  rlcounts$Hero2 <- as.numeric(as.character(rlcounts$Hero2))
  sortheroes(rlcounts)
  rlcounts <- fundf
  
dwcounts <- direwinvals
  dwcounts$Hero1 <- as.numeric(as.character(dwcounts$Hero1))
  dwcounts$Hero2 <- as.numeric(as.character(dwcounts$Hero2))
  sortheroes(dwcounts)
  dwcounts <- fundf
  
dlcounts <- direlossvals
  dlcounts$Hero1 <- as.numeric(as.character(dlcounts$Hero1))
  dlcounts$Hero2 <- as.numeric(as.character(dlcounts$Hero2))
  sortheroes(dlcounts)
  dlcounts <- fundf
  
  
overallwins <- merge(rwcounts, dwcounts, by=c("Hero1", "Hero2"), all = TRUE)
overallwins[is.na(overallwins)]<- 0
overallwins$freq <- rowSums(overallwins[,3:4])
overallwins[,3:4] <- NULL
  
overallosses <- merge(rlcounts, dlcounts, by=c("Hero1", "Hero2"), all=TRUE)
overallosses[is.na(overallosses)] <- 0
overallosses$freq <- rowSums(overallosses[,3:4])
overallosses[,3:4] <- NULL

overallwr <- merge(overallwins, overallosses, by=c("Hero1", "Hero2"), all = TRUE)
overallwr[is.na(overallwr)] <- 0
colnames(overallwr) <- c("Hero1", "Hero2", "wins", "losses")
overallwr$num_of_picks <- rowSums(overallwr[,3:4])
overallwr <- mutate(overallwr, win_percentage = wins / num_of_picks)
overallwr <- mutate(overallwr, pickpermedian = num_of_picks / median(overallwr$num_of_picks))
overallwr$H1name <-  hero_names$localized_name[match(overallwr$Hero1, hero_names$hero_id)]
overallwr$H2name <-  hero_names$localized_name[match(overallwr$Hero2, hero_names$hero_id)]
overallwr <- overallwr[,c(1,8,2,9,3,4,5,6,7)]
  


extrafont::loadfonts(device="win")

   radiant_pairing_winm <- reshape2::acast(rwcounts, Hero1~Hero2, value.var = "freq", drop = FALSE)

   
   ##plottin
gg <- ggplot(overallwr, aes(x=Hero1, y=Hero2, fill=pickpermedian)) + geom_tile(color="black", size=0.4) + 
   scale_fill_viridis(name="Number of Picks / Median)", label=comma) + coord_equal() + theme_few() +
   scale_x_continuous(expand = c(0,00), breaks = unique(overallwr$Hero1), labels = unique(overallwr$H1name), position = "top", sec.axis = dup_axis())  + 
   scale_y_continuous(expand = c(0,00), trans = "reverse", breaks = unique(overallwr$Hero2), labels = unique(overallwr$H2name), position = "top", sec.axis = dup_axis()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust=.5, size = 8), plot.background = element_rect(fill = 'white'), 
         panel.grid.major = element_line(color= "black"), axis.text.y = element_text(size = 8)) + ggtitle("Occurance of Hero Pairings Compared to Median")


gg

s#ptm <- proc.time()
#proc.time() - ptm

#radiant win%
  21922/(20298+21922)
  
  
  
#let's create a test set of data from test_player.csv and test_labels.csv
  library("tidyr")
test_set <- test_labels
#remove account_ids
test_player <- test_player[,-2]
#remove matches with bad hero_ids (15 games)
badheroids <- test_player[test_player$hero_id == 0, ]
test_player <- test_player[!test_player$match_id %in% badheroids$match_id,]
test_set <- test_set[!test_set$match_id %in% badheroids$match_id,]
#spread data to wide format
test_player <- spread(test_player, player_slot, hero_id)
#add match outcome from test_set
test_set <- merge(test_set, test_player, by=match_id)
test_set <- test_set[,-1]
test_set <- arrange(test_set, match_id)

#create subsets for radiant wins/losses, and dire wins/losses
radiantwin <- filter(test_set, radiant_win == "1")
radiantloss <- filter(test_set, radiant_win == "0")
direwin <- filter(test_set, radiant_win == "0")
direloss <- filter(test_set, radiant_win == "1")

#get rid of irrelevant team for each category
radiantwin[,8:12]<- NULL
radiantloss[,8:12] <- NULL
direwin[,3:7] <- NULL
direloss[,3:7] <- NULL

#create hero pair permutations for radiant and dire of each match
##radiantwin
test<- apply(radiantwin[,3:7], 1, function(x) combn(x, 2, simplify = F))
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))
#add back the match_id and radiant_win and clean up columns
colnames(df) <- c("Hero1", "Hero2")
df$match_id <- NA
df$radiant_win <- NA
df <- df[,c(3,4,1,2)]
#add back in match_ids
for (i in 1:length(radiantwin$match_id)){
  x = i * 10 -9
  df[x:(x+9),1] <- paste(radiantwin[i,1]);
}
#add back in radiant_win
for (i in 1:length(radiantwin$radiant_win)){
  x = i * 10 -9
  df[x:(x+9),2] <- paste(radiantwin[i,2]);
}
radiantwinvals <- df
##radiantloss
test<- apply(radiantloss[,3:7], 1, function(x) combn(x, 2, simplify = F))
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))
#add back the match_id and radiant_win and clean up columns
colnames(df) <- c("Hero1", "Hero2")
df$match_id <- NA
df$radiant_win <- NA
df <- df[,c(3,4,1,2)]
#add back in match_ids
for (i in 1:length(radiantloss$match_id)){
  x = i * 10 -9
  df[x:(x+9),1] <- paste(radiantloss[i,1]);
}
#add back in radiant_win
for (i in 1:length(radiantloss$radiant_win)){
  x = i * 10 -9
  df[x:(x+9),2] <- paste(radiantloss[i,2]);
}
radiantlossvals <- df
##direwin
test<- apply(direwin[,3:7], 1, function(x) combn(x, 2, simplify = F))
df <- data.frame(matrix(unlist(test),ncol = 2, byrow= T))
#add back the match_id and radiant_win and clean up columns
colnames(df) <- c("Hero1", "Hero2")
df$match_id <- NA
df$radiant_win <- NA
df <- df[,c(3,4,1,2)]
#add back in match_ids
for (i in 1:length(direwin$match_id)){
  x = i * 10 -9
  df[x:(x+9),1] <- paste(direwin[i,1]);
}
#add back in radiant_win
for (i in 1:length(direwin$radiant_win)){
  x = i * 10 -9
  df[x:(x+9),2] <- paste(direwin[i,2]);
}
direwinvals <- df
##direloss
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
test_set <- radiantwinvals
colnames(test_set) <- c("match_id", "Win", "Hero1", "Hero2")
test_set$Win <- TRUE
df <- direlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- FALSE
test_set <- rbind(test_set, df)
df <- direwinvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- TRUE
test_set <- rbind(test_set, df)
df <- radiantlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- FALSE
test_set <- rbind(test_set, df)
test_set$match_id <- as.numeric(as.character(test_set$match_id))
test_set <- arrange(test_set, match_id)

ptm <- proc.time()
write.csv(test_set, "test_set.csv")
proc.time() - ptm


##running knn
w_knn<-winformation
w_knn <- w_knn[,-1]
w_knn[,2] <- as.factor(w_knn[,2])
w_knn[,3] <- as.factor(w_knn[,3])
t_knn <- test_set[,-1]
t_knn[,2] <- as.factor(t_knn[,2])
t_knn[,3] <- as.factor(t_knn[,3])

w_knn.

#shuffling up the rowws
set.seed(34958)
gp <- runif(nrow(w_knn))
w_knn <- w_knn[order(gp),]
gp <- runif(nrow(t_knn))
t_knn <- t_knn[order(gp),]
train_target <- w_knn[,1]
test_target <- t_knn[,1]
#have to exclude class labels or else you get error
m1 <- knn(train = w_knn[,-1], test = t_knn[,-1], cl=w_knn$Win, k = 501, l = 15)



##restructure for association rules
#w1 <- as.data.frame(matrix(0, ncol = 111, nrow = length(winformation$match_id)))
#colnames(w1)[1] <- "Win"
#a1 <- unique(winformation$Hero1)
#a1 <- a1[order(a1)]

colnames(w1)[2:111] <- a1

w2 <- rbind(radiantwin, radiantloss)

#w2 %>%
colnames(w2) <- c("match_id", "win", "hero1", "hero2", "hero3", "hero4", "hero5")
 w3 <- gather(w2,Hero,Heroid,starts_with("Hero")) #%>%
  w3 <- mutate(w3, present = 1) #%>%
  w3 <- select(w3, -Hero) #%>%
  w3<- spread(w3, Heroid,present,fill = 0)
  w3$match_id <- as.numeric(as.character(w3$match_id))
arulesset<-w3
rownames(arulesset) <- arulesset[,1]
arulesset$match_id <- NULL
arulesset <- discretize(arulesset, categories = 2)

rules <- apriori(arulesset, parameter=list(support=0.01, confidence=0.5))


#check comparison of some stats for different sizes of the dataset

z1 <- winformation[1:281460,]
z2 <- winformation[281461:562941,]
z3 <- winformation[562942:844400,]
sortheroes(z1)
z1 <- fundf
sortheroes(z2)
z2 <- fundf
sortheroes(z3)
z3<- fundf


par(mfrow=c(3,1))
hist(z1$freq, xlab = "Number of times hero pair was picked", main = "Histogram of 1st 1/3rd")
hist(z2$freq, xlab = "Number of times hero pair was picked", main = "Histogram of 2nd 1/3rd")
hist(z3$freq, xlab = "Number of times hero pair was picked", main = "Histogram of 3rd 1/3rd")

par(mfrow = c(1,3))
boxplot(z1$freq, xlab = "1st 1/3rd median: ", sub = median(z1$freq))
boxplot(z2$freq, xlab = "2nd 1/3rd median: ", sub = median(z2$freq))
boxplot(z3$freq, xlab = "3rd 1/3rd median: ", sub = median(z3$freq))

#they're practically identical
#let's see what a progression of the data looks like
z1 <- winformation[1:281460,]
z2 <- winformation[1:562941,]
z3 <- winformation[1:844400,]
sortheroes(z1)
z1 <- fundf
sortheroes(z2)
z2 <- fundf
sortheroes(z3)
z3<- fundf

par(mfrow=c(3,1))
hist(z1$freq, xlab = "Number of times hero pair was picked", main = "Histogram of 1/3rd of matches")
hist(z2$freq, xlab = "Number of times hero pair was picked", main = "Histogram of 2/3rds of matches")
hist(z3$freq, xlab = "Number of times hero pair was picked", main = "Histogram of all matches")

par(mfrow = c(1,3))
boxplot(z1$freq, xlab = "1/3rd of matches \n median: ", sub = median(z1$freq))
boxplot(z2$freq, xlab = "2/3rds of matches \n median: ", sub = median(z2$freq))
boxplot(z3$freq, xlab = "all matches \n median: ", sub = median(z3$freq))



#using overallwr to visualize pairs of top combinations.


#opairsm <- radiant_pairing_winm
#opairsm <- as.matrix(opairsm)
#rownames(opairsm) <- colnames(opairsm)
#groupNames <- hero_names[,2:3]
#groupNames <-groupNames[-107,]
#groupNames <- groupNames[-111,]
#groupNames <- groupNames[,-1]
#opairsm <- t(opairsm)
#colnames(opairsm)<-groupNames$localized_name
#chorddiag(opairsm, groupNames = rownames(opairsm), groupColors = palette(rainbow(6)))


#opairsm <- read_csv("~/dota2/opairsm.csv")
#opairsm <- as.data.frame(opairsm)
#rownames(opairsm)<-groupNames$localized_name
#colnames(opairsm)<-groupNames$localized_name
#opairsm <- as.matrix(opairsm)
#class(opairsm) <- "numeric"

m <- overallwr[,c("H1name", "H2name", "win_percentage", "num_of_picks")]
mp <- arrange(m, desc(num_of_picks))
mp <- subset(mp, mp[,4] > 696)
m <- subset(m, m[,4] > 37) #subset with first quartile  of pick rate as lower bound.  is the 27%th percentile of recorded pairs
m <- arrange(m, desc(win_percentage))
m <- m[,-4]
m60 <- m[1:370,]             #subset with 60% win rate as lower bound.  is the 94th percentile of recorded pairs
m65 <- m[1:95, ]             #subset with 65% win rate as lower bound.  is the 98th percentile of recorded pairs.



#60%wr
#CairoWin()
CairoPNG("60pct.png",8,8, res = 300, units = "in", bg = "white",pointsize = 8)
chordDiagram(m60, link.sort = TRUE, link.decreasing = FALSE, symmetric=TRUE, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)
dev.off()

#all heroes
CairoPNG("apllpct.png",8,8, res = 300, units = "in", bg = "white",pointsize = 8)
chordDiagram(m, link.sort = TRUE, link.decreasing = FALSE, symmetric=TRUE, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)
dev.off()

#65% wr
CairoPNG("65pct.png",8,8, res = 300, units = "in", bg = "white",pointsize = 8)
chordDiagram(m65, link.sort = TRUE, link.decreasing = FALSE, symmetric=TRUE, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)
dev.off()

#top 2% picks
CairoPNG("2pctpicks.png",8,8, res = 300, units = "in", bg = "white",pointsize = 8)
chordDiagram(mp, link.sort = TRUE, link.decreasing = FALSE, symmetric=TRUE, annotationTrack = "grid", preAllocateTracks = list(track.height = 0.1))
circos.trackPlotRegion(track.index = 1, panel.fun = function(x, y) {
  xlim = get.cell.meta.data("xlim")
  xplot = get.cell.meta.data("xplot")
  ylim = get.cell.meta.data("ylim")
  sector.name = get.cell.meta.data("sector.index")
  if(abs(xplot[2] - xplot[1]) < 20) {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "clockwise",
                niceFacing = TRUE, adj = c(0, 0.5))
  } else {
    circos.text(mean(xlim), ylim[1], sector.name, facing = "inside",
                niceFacing = TRUE, adj = c(0.5, 0))
  }
}, bg.border = NA)
dev.off()







#creating netowrk
library(igraph)
networktest <- overallwr[,c(2,4,7, 8)]
networktest <- networktest[networktest$win_percentage > .65, ]
networktest <- networktest[networktest$num_of_picks > 37, ]
g <- graph.data.frame(networktest, directed = FALSE)
clp <- cluster_label_prop(g)
c_scale <- colorRamp(c('red','blue'))
networktest$scaledwr <- range01(networktest$win_percentage)
networktest$scaledpicks <- range01(networktest$num_of_picks)
E(g)$color = apply(c_scale(E(g)$scaledwr), 1, function(x) rgb(x[1]/255,x[2]/255,x[3]/255) )

plot.igraph( g, vertex.shape = "none", 
          vertex.label.font=2, vertex.label.color="black",
          vertex.label.cex=.7, layout = layout.fruchterman.reingold,
          edge.width=E(g)$scaledpicks*10, edge.curved = .1, rescale = T)


#creating miscellaneous visualizations
herocount <- players[!players$match_id %in% badheroids$match_id,]
herocount <- herocount[!herocount$match_id %in% badleavers$match_id,]
herocount <- herocount[,c(1,3)]
herocount <- plyr::count(herocount$hero_id)
herocount <- arrange(herocount, desc(freq))

herowins <- plyr::count(radiantwin$"0")
herowins <- cbind(herowins, plyr::count(radiantwin$"1"), plyr::count(radiantwin$"2"), 
                  plyr::count(radiantwin$"3"), plyr::count(radiantwin$"4"), plyr::count(direwin$"128"),
                  plyr::count(direwin$"129"), plyr::count(direwin$"130"), plyr::count(direwin$"131"),
                  plyr::count(direwin$"132"))
herowins <- herowins[,c(1,2,4,6,8,10,12,14,16,18,20)]
herowins <- mutate(herowins, sums = rowSums(herowins[, 2:11]))
herowins <- herowins[,c(1,12)]

herolosses <- plyr::count(radiantloss$"0")
herolosses <- cbind(herolosses, plyr::count(radiantloss$"1"), plyr::count(radiantloss$"2"), 
                    plyr::count(radiantloss$"3"), plyr::count(radiantloss$"4"), plyr::count(direloss$"128"),
                    plyr::count(direloss$"129"), plyr::count(direloss$"130"), plyr::count(direloss$"131"),
                    plyr::count(direloss$"132"))
herolosses <- herolosses[,c(1,2,4,6,8,10,12,14,16,18,20)]
herolosses <- mutate(herolosses, sums = rowSums(herolosses[, 2:11]))
herolosses <- herolosses[,c(1,12)]

herocount <- merge(herocount, herowins, by = "x")
herocount <- merge(herocount, herolosses, by = "x")
colnames(herocount) <- c("Hero", "number_of_picks", "wins", "losses")
herocount <- mutate(herocount, winrate = wins / number_of_picks)
herocount$winrate <- round(herocount$winrate * 10^3)/10^3

herocount$Hero <-  hero_names$localized_name[match(herocount$Hero, hero_names$hero_id)]

#r = max(herocount$number_of_picks)
r = 20000
herocount <- mutate(herocount, scaledpct = r * herocount$winrate)

#hero frequency plot
ggplot(herocount, aes(x = reorder(Hero, -number_of_picks), y = number_of_picks)) + geom_bar(stat = "identity") + 
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), plot.title = element_text(hjust=.5)) +
  labs(x = "Hero", y = "Number of Picks") + ggtitle("Frequency of Hero Picks") + 
  scale_y_continuous(breaks=seq(0, 20000, by=1000),expand = c(0,00)) +
  coord_cartesian(ylim=c(0,20000))

#hero frequency plot + winrate lines
ggplot(herocount, aes(x = reorder(Hero, -number_of_picks), y = number_of_picks)) + geom_bar(stat = "identity", alpha = .5) + 
  theme_few() + theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1, size = 8), plot.title = element_text(hjust=.5)) +
  labs(x = "Hero", y = "Number of Picks") + ggtitle("Frequency of Hero Picks") + 
  geom_point(aes(x = reorder(Hero, -number_of_picks), y = scaledpct), group = 1, color = "navy", size = 1) + 
  geom_line(aes(x = reorder(Hero, -number_of_picks), y = r/2), color = "red", group = 2, linetype = 2)+
  scale_y_continuous(breaks=seq(0, 20000, by=1000),expand = c(0,00),  sec.axis = sec_axis(~., labels =  c("0%", "25%", "50%", "75%", "100%"),name = "Win Rate")) +
  coord_cartesian(ylim=c(0,20000))

#pareto chart of tournament prize pools
library(scales)
ggplot(tournamentsyearly, aes(x = Year, y= Prize_Pool)) + geom_bar(stat = "identity") + 
  theme_few() + labs(x = "Year", y = "Prize Pool ($)") + 
  scale_y_continuous(expand = c(0,00), labels = comma, breaks=seq(0, 80000000, by=10000000), sec.axis = sec_axis(~. / 80000000 * 100, name = "Cumulative Total of Whole (%)")) + 
  scale_x_continuous(breaks = tournamentsyearly$Year,labels = tournamentsyearly$Year) +
  geom_point(aes(y = Cumulative, group = '1'), stat = "identity") + geom_line(aes(y = Cumulative, group = '1')) + coord_cartesian(ylim=c(0,85000000)) + ggtitle("Tournament Prize Pools")


#trying out gephi
overallwins <- winformation[winformation$Win == TRUE, ]
overallwins$Hero1 <- hero_names$localized_name[match(overallwins$Hero1, hero_names$hero_id)]
overallwins$Hero2 <- hero_names$localized_name[match(overallwins$Hero2, hero_names$hero_id)]

edges = data.frame(from=overallwins$Hero1,to=overallwins$Hero2,stringsAsFactors = F) %>% group_by(from,to) %>% dplyr::summarize(value = n())
nodes <- data.frame(id = unique(c(edges$from, edges$to)), label = unique(c(edges$from, edges$to)), stringsAsFactors = F) %>% tbl_df
rt_graph <- make_empty_graph() + vertices(nodes$id) + edges(as.vector(rbind(edges$from, edges$to)), weight = edges$value)
rg.gexf <- igraph.to.gexf(rt_graph)
f <- file("dota.gexf")
writeLines(rg.gexf$graph, con = f)
close(f)
