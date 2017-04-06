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
library("ggplot2")
library("reshape2")
library("viridis")
library("scales")
library("ggthemes")
   radiant_pairing_winm <- reshape2::acast(rwcounts, Hero1~Hero2, value.var = "freq", drop = FALSE)

   
   ##plottin
gg <- ggplot(overallwr, aes(x=Hero1, y=Hero2, fill=win_percentage)) + geom_tile(color="black", size=0.4) + 
   scale_fill_viridis(name="pick % / median # of picks", label=comma) + coord_equal() + theme_tufte(base_family="Helvetica") +
   scale_x_continuous(breaks = unique(overallwr$Hero1), position = "top", sec.axis = dup_axis())  + 
   scale_y_continuous(trans = "reverse", breaks = unique(overallwr$Hero2), position = "top", sec.axis = dup_axis()) +
   theme(axis.text.x = element_text(angle = 90, hjust = 1, size = 8), plot.background = element_rect(fill = 'white'), panel.grid.major = element_line(color= "black"),
         axis.text.y = element_text(size = 8))


gg

#ptm <- proc.time()
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
test_set$Win <- "True"
df <- direlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "False"
test_set <- rbind(test_set, df)
df <- direwinvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "True"
test_set <- rbind(test_set, df)
df <- radiantlossvals
colnames(df) <- c("match_id", "Win", "Hero1", "Hero2")
df$Win <- "False"
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



#lookup table for hero names
