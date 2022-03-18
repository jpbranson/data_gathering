
#original credit to Dr. Jonathan Bennett

library(rjson)
library(htmlTable)

timestamp.file <- make.names(Sys.time())

# Nate Silver Data

download.file('https://projects.fivethirtyeight.com/march-madness-api/2022/madness.json', paste0("data/madness/nate-json/madness-", timestamp.file, ".json"))
x <- fromJSON(file=paste0("data/madness/nate-json/madness-", timestamp.file, ".json"))
silver <- x$forecasts$mens$current_run$teams
silver <- data.frame(do.call(rbind,silver))
for (i in 1:ncol(silver)) {silver[,i] <- unlist(silver[,i])}
silver$regionid <- match(silver$team_region, c("West","East","South","Midwest"))
silver <- silver[order(silver$regionid, silver$team_slot),]

silver$playin_flag <- as.logical(silver$playin_flag)
silver$rd1 <- NA
silver$rd1[1] <- 1
for (i in 2:nrow(silver)) {if (silver$playin_flag[i] & silver$playin_flag[(i-1)])
	{silver$rd1[i] <- silver$rd1[i-1]} else {silver$rd1[i] <- silver$rd1[i-1]+1}}
silver$rd2 <- (silver$rd1 + 1) %/% 2
silver$rd3 <- (silver$rd2 + 1) %/% 2
silver$rd4 <- (silver$rd3 + 1) %/% 2
silver$rd5 <- (silver$rd4 + 1) %/% 2
silver$rd6 <- (silver$rd5 + 1) %/% 2
silver$rd7 <- (silver$rd6 + 1) %/% 2

silver$jbteam <- paste(gsub("[a-b]","",silver$team_seed),"-",silver$team_name)
silver$jbteam <- gsub("16 - Texas A&M-Corpus Christi","16 - Texas A&M CC",silver$jbteam,fixed=TRUE)
silver$jbteam <- gsub("5 - Connecticut","5 - UConn",silver$jbteam,fixed=TRUE)
silver$jbteam <- gsub("15 - Cal State Fullerton","15 - CSU Fullerton",silver$jbteam,fixed=TRUE)
silver$jbteam <- gsub("5 - Saint Mary's (CA)","5 - Saint Mary's",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("15 - Saint Peter's","15 - St. Peter's",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("9 - Texas Christian","9 - TCU",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("12 - Alabama-Birmingham","12 - UAB",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("10 - Loyola (IL)","10 - Loyola Chicago",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("6 - Louisiana State","6 - LSU",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("7 - Southern California","7 - USC",silver$jbteam,fixed=TRUE) # Check the punctuation mark
silver$jbteam <- gsub("10 - Miami (FL)","10 - Miami",silver$jbteam,fixed=TRUE) # Check the punctuation mark


Simulate_old <- function(Y) {
	# Simulate a likely reality
	silver$win_rd1 <- is.element(silver$jbteam, sapply(unique(silver$rd1), function(z) {
		sample(silver$jbteam[silver$rd1==z], prob=silver$rd1_win[silver$rd1==z], size=1)
	}))
	silver$prob_rd2 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd1[z],0,sum(silver$rd2_win[silver$rd1==silver$rd1[z]]))})

	silver$win_rd2 <- is.element(silver$jbteam, sapply(unique(silver$rd2), function(z) {
		sample(silver$jbteam[silver$rd2==z], prob=silver$prob_rd2[silver$rd2==z], size=1)
	}))
	silver$prob_rd3 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd2[z],0,sum(silver$rd3_win[silver$rd2==silver$rd2[z]]))})

	silver$win_rd3 <- is.element(silver$jbteam, sapply(unique(silver$rd3), function(z) {
		sample(silver$jbteam[silver$rd3==z], prob=silver$prob_rd3[silver$rd3==z], size=1)
	}))
	silver$prob_rd4 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd3[z],0,sum(silver$rd4_win[silver$rd3==silver$rd3[z]]))})

	silver$win_rd4 <- is.element(silver$jbteam, sapply(unique(silver$rd4), function(z) {
		sample(silver$jbteam[silver$rd4==z], prob=silver$prob_rd4[silver$rd4==z], size=1)
	}))
	silver$prob_rd5 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd4[z],0,sum(silver$rd5_win[silver$rd4==silver$rd4[z]]))})

	silver$win_rd5 <- is.element(silver$jbteam, sapply(unique(silver$rd5), function(z) {
		sample(silver$jbteam[silver$rd5==z], prob=silver$prob_rd5[silver$rd5==z], size=1)
	}))
	silver$prob_rd6 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd5[z],0,sum(silver$rd6_win[silver$rd5==silver$rd5[z]]))})

	silver$win_rd6 <- is.element(silver$jbteam, sapply(unique(silver$rd6), function(z) {
		sample(silver$jbteam[silver$rd6==z], prob=silver$prob_rd6[silver$rd6==z], size=1)
	}))
	silver$prob_rd7 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd6[z],0,sum(silver$rd7_win[silver$rd6==silver$rd6[z]]))})

	silver$win_rd7 <- is.element(silver$jbteam, sapply(unique(silver$rd7), function(z) {
		sample(silver$jbteam[silver$rd7==z], prob=silver$prob_rd7[silver$rd7==z], size=1)
	}))

	# Use this outcome as a possibility - Calculate expected points of that reality

	pts <- sum(silver$win_rd7*(200+10*as.integer(substr(silver$team_seed,1,2)))*silver$rd7_win +
	silver$win_rd6*(140+7*as.integer(substr(silver$team_seed,1,2)))*silver$rd6_win +
	silver$win_rd5*(100+5*as.integer(substr(silver$team_seed,1,2)))*silver$rd5_win +
	silver$win_rd4*(75+3*as.integer(substr(silver$team_seed,1,2)))*silver$rd4_win +
	silver$win_rd3*(40+2*as.integer(substr(silver$team_seed,1,2)))*silver$rd3_win +
	silver$win_rd2*(20+1*as.integer(substr(silver$team_seed,1,2)))*silver$rd2_win +
	silver$win_rd1*(20+0*as.integer(substr(silver$team_seed,1,2)))*silver$rd1_win*(silver$playin_flag))

	data.frame(rbind(c(silver$jbteam[silver$win_rd1 & silver$playin_flag],
	  silver$jbteam[silver$win_rd2],
	  silver$jbteam[silver$win_rd3],
	  silver$jbteam[silver$win_rd4],
	  silver$jbteam[silver$win_rd5],
	  silver$jbteam[silver$win_rd6],
	  silver$jbteam[silver$win_rd7],pts)))

}

Simulate_new <- function(Y) {
# Simulate a likely reality
	silver$win_rd1 <- is.element(silver$jbteam, sapply(unique(silver$rd1), function(z) {
		sample(silver$jbteam[silver$rd1==z], prob=silver$rd1_win[silver$rd1==z], size=1)
	}))
	silver$prob_rd2 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd1[z],0,ifelse(!silver$playin_flag[z],silver$rd2_win[z],1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd2==silver$rd2[z] & silver$win_rd1),z)])*30.464/400)) ))})

	silver$win_rd2 <- is.element(silver$jbteam, sapply(unique(silver$rd2), function(z) {
		sample(silver$jbteam[silver$rd2==z], prob=silver$prob_rd2[silver$rd2==z], size=1)
	}))
	silver$prob_rd3 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd2[z],0,1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd3==silver$rd3[z] & silver$win_rd2),z)])*30.464/400)) )})

	silver$win_rd3 <- is.element(silver$jbteam, sapply(unique(silver$rd3), function(z) {
		sample(silver$jbteam[silver$rd3==z], prob=silver$prob_rd3[silver$rd3==z], size=1)
	}))
	silver$prob_rd4 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd3[z],0,1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd4==silver$rd4[z] & silver$win_rd3),z)])*30.464/400)) )})

	silver$win_rd4 <- is.element(silver$jbteam, sapply(unique(silver$rd4), function(z) {
		sample(silver$jbteam[silver$rd4==z], prob=silver$prob_rd4[silver$rd4==z], size=1)
	}))
	silver$prob_rd5 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd4[z],0,1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd5==silver$rd5[z] & silver$win_rd4),z)])*30.464/400)) )})


	silver$win_rd5 <- is.element(silver$jbteam, sapply(unique(silver$rd5), function(z) {
		sample(silver$jbteam[silver$rd5==z], prob=silver$prob_rd5[silver$rd5==z], size=1)
	}))
	silver$prob_rd6 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd5[z],0,1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd6==silver$rd6[z] & silver$win_rd5),z)])*30.464/400)) )})


	silver$win_rd6 <- is.element(silver$jbteam, sapply(unique(silver$rd6), function(z) {
		sample(silver$jbteam[silver$rd6==z], prob=silver$prob_rd6[silver$rd6==z], size=1)
	}))
	silver$prob_rd7 <- sapply(1:nrow(silver), function(z) {ifelse(!silver$win_rd6[z],0,1/(1+10^(-1*(silver$team_rating[z]-silver$team_rating[setdiff(which(silver$rd7==silver$rd7[z] & silver$win_rd6),z)])*30.464/400)) )})

	silver$win_rd7 <- is.element(silver$jbteam, sapply(unique(silver$rd7), function(z) {
		sample(silver$jbteam[silver$rd7==z], prob=silver$prob_rd7[silver$rd7==z], size=1)
	}))

	# Use this outcome as a possibility - Calculate expected points of that reality

	pts <- sum(silver$win_rd7*(200+10*as.integer(substr(silver$team_seed,1,2)))*silver$rd7_win +
	silver$win_rd6*(140+7*as.integer(substr(silver$team_seed,1,2)))*silver$rd6_win +
	silver$win_rd5*(100+5*as.integer(substr(silver$team_seed,1,2)))*silver$rd5_win +
	silver$win_rd4*(75+3*as.integer(substr(silver$team_seed,1,2)))*silver$rd4_win +
	silver$win_rd3*(40+2*as.integer(substr(silver$team_seed,1,2)))*silver$rd3_win +
	silver$win_rd2*(20+1*as.integer(substr(silver$team_seed,1,2)))*silver$rd2_win +
	silver$win_rd1*(20+0*as.integer(substr(silver$team_seed,1,2)))*silver$rd1_win*(silver$playin_flag))

	data.frame(rbind(c(silver$jbteam[silver$win_rd1 & silver$playin_flag],
	  silver$jbteam[silver$win_rd2],
	  silver$jbteam[silver$win_rd3],
	  silver$jbteam[silver$win_rd4],
	  silver$jbteam[silver$win_rd5],
	  silver$jbteam[silver$win_rd6],
	  silver$jbteam[silver$win_rd7],pts)))

}

tenthousand_old <- do.call(rbind,lapply(1:10000,Simulate_old))
tenthousand_new <- do.call(rbind,lapply(1:10000,Simulate_new))

teamprobs <- data.frame(team=silver$jbteam, silver=silver$rd7_win)
param<-4300
teamprobs$tenthousand_old <- as.numeric(prop.table(table(tenthousand_old$X67))[match(teamprobs$team,names(table(tenthousand_old$X67)))])
teamprobs$tenthousand_new <- as.numeric(prop.table(table(tenthousand_new$X67))[match(teamprobs$team,names(table(tenthousand_new$X67)))])
temp <- prop.table(table(c(tenthousand_old$X67[1:param],tenthousand_new$X67[(param+1):10000])))
teamprobs$tenthousand_c <- as.numeric(temp[match(teamprobs$team,names(temp))])

teamprobs$tenthousand_old[is.na(teamprobs$tenthousand_old)] <- 0
teamprobs$tenthousand_new[is.na(teamprobs$tenthousand_new)] <- 0
teamprobs$tenthousand_c[is.na(teamprobs$tenthousand_c)] <- 0

teamprobs$silver <- as.numeric(round(teamprobs$silver,4))
teamprobs <- teamprobs[order(teamprobs$silver),]
teamprobs

# plot(teamprobs$silver,teamprobs$tenthousand_old)
# abline(a=0,b=1)
# plot(teamprobs$silver,teamprobs$tenthousand_new)
# abline(a=0,b=1)
# plot(teamprobs$silver,teamprobs$tenthousand_c)
# abline(a=0,b=1)

tenthousand <- rbind(tenthousand_old[1:param,], tenthousand_new[(param+1):10000,])

# Now look at brackets

df <- read.csv("data/qualtrics.csv", stringsAsFactors = FALSE)
for (i in match("Q5",colnames(df)):ncol(df)) {
	for (j in 1:66) {
		x <- paste("${q://QID",j,"/ChoiceGroup/SelectedChoices}",sep='')
		df[df[,i]==x,i] <- df[df[,i]==x,paste("Q",j,sep='')]
	}
}
#df
df <- df[order(df$Q68),]

# Validate
table(is.element(unlist(df[,match('Q1',colnames(df)):match('Q67',colnames(df))]), unlist(tenthousand[,1:67])))

# For each reality, calculate points
df$Q68 <- gsub(" ","",df$Q68)
df$Q68 <- gsub("-","",df$Q68,fixed=T)
colnames(df) <- gsub("^Q","X",colnames(df))

for (k in 1:length(df$X68)) {
tenthousand$temp <-
   apply(sapply(1:4, function(j){20 * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(5:36, function(j){(20+as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(37:52, function(j){(40+2*as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(53:60, function(j){(75+3*as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(61:64, function(j){(100+5*as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(65:66, function(j){(140+7*as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum) +
   apply(sapply(67, function(j){(200+10*as.integer(gsub(" .*","",tenthousand[,paste('X',j,sep='')]))) * (tenthousand[,paste('X',j,sep='')]==df[k,paste('X',j,sep='')]) }) ,1,sum)
colnames(tenthousand)[ncol(tenthousand)] <- df$X68[k]
}
tenthousand$first <- df$X68[sapply(1:nrow(tenthousand), function(z) {order(tenthousand[z,df$X68],decreasing=T)[1]})]
tenthousand$second <- df$X68[sapply(1:nrow(tenthousand), function(z) {order(tenthousand[z,df$X68],decreasing=T)[2]})]
tenthousand$third <- df$X68[sapply(1:nrow(tenthousand), function(z) {order(tenthousand[z,df$X68],decreasing=T)[3]})]

# PROBABILITY OF WINNING 1st, 2nd, 3rd
round(prop.table(table(tenthousand$first))*100,1)
round(prop.table(table(tenthousand$second))*100,1)
round(prop.table(table(tenthousand$third))*100,1)

sort(prop.table(table(tenthousand$X67)))

# POINTS NOW

for (i in 1:4) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd1_win[match(df[,x], silver$jbteam)]==1, 20, 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 5:36) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd2_win[match(df[,x], silver$jbteam)]==1, 20 + as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 37:52) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd3_win[match(df[,x], silver$jbteam)]==1, 40 + 2*as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 53:60) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd4_win[match(df[,x], silver$jbteam)]==1, 75 + 3*as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 61:64) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd5_win[match(df[,x], silver$jbteam)]==1, 100 + 5*as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 65:66) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd6_win[match(df[,x], silver$jbteam)]==1, 140 + 7*as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

for (i in 67) {
   x <- paste('X',i,sep='')
   df$temp <- ifelse(silver$rd7_win[match(df[,x], silver$jbteam)]==1, 200 + 10*as.integer(gsub(" .*","",df[,x])), 0)
   colnames(df)[ncol(df)] <- paste('points_',x,sep='')
}

df$current_pts <- apply(df[,grepl("^points",colnames(df))],1,sum)
df[,c("X68","current_pts")]
df[order(df$current_pts,decreasing=T),c("X68","current_pts")]

ptsreport <- cbind(c(paste("First Four",1:4),paste("First Round",1:32),paste("Second Round",1:16),paste("Sweet Sixteen",1:8),paste("Elite Eight",1:4),paste("Final Four",1:2),paste("Championship",1)),
   do.call(cbind,lapply(1:nrow(df), function(z){cbind(t(df[z,paste('X',1:67,sep='')]),t(df[z,paste('points_X',1:67,sep='')]))})))
colnames(ptsreport) <- c("Game",rep(df$X68,each=2))
colnames(ptsreport)[seq(3,ncol(ptsreport),2)] <- paste("points_",colnames(ptsreport)[seq(3,ncol(ptsreport),2)],sep='')

out <- cbind(round(prop.table(table(tenthousand$first))*100,2),round(prop.table(table(tenthousand$second))*100,2),round(prop.table(table(tenthousand$third))*100,2))
out <- data.frame(out)
out$points <- df$current_pts[match(rownames(out), df$X68)]
colnames(out) <- c("prob_first","prob_second","prob_third","points")
out$rank <- match(out$points, sort(out$points,decreasing=T))

#library(htmlTable)
mytableout <- htmlTable(out, caption = paste("Last Updated ",Sys.time()))
sink(paste0("data/madness/madness-", timestamp.file, ".html"))
print(mytableout,type='html',useViewer=FALSE)
sink()

tenthousand$X68 <- NULL
write.csv(tenthousand, row.names=FALSE, file = paste0("data/madness/simulations/sim-", timestamp.file, ".csv"))
#write.csv(ptsreport, row.names = FALSE, file = '/Users/jbennett/Delta Health Alliance/Research - DHA Team Folder/Madness/ptsreport_firstround.csv')

#write.csv(cbind(round(prop.table(table(tenthousand$first))*100,1),round(prop.table(table(tenthousand$second))*100,1),round(prop.table(table(tenthousand$third))*100,1)),file='/Users/jbennett/Delta Health Alliance/Research - DHA Team Folder/Madness/probs_firstround.csv')
