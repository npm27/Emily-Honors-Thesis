###Write some code to make our output useful!
####Set up####
#read in dataset
master = read.csv("Master Data Sheet.csv")

#put recall on correct scale
master$Scored = master$Scored * 100

#load libraries
library(reshape)
library(stringr)

#drop unused columns
master = master[ , -c(3:5, 10:18)]

#make sure everything is numeric
summary(master)

master$JOL = as.numeric(master$JOL)

#remove out of range scores
master$JOL[master$JOL > 100]

#subset by instruction type
summary(master$ExperimentName)

master$instruction_type = str_sub(master$ExperimentName, 1, str_length(master$ExperimentName) - 2)

IS = subset(master,
            master$instruction_type == "IS_JOL")
RL = subset(master,
            master$instruction_type == "RL_JOL")
Read = subset(master,
            master$instruction_type == "READ_JOL")

#remove last column
IS = IS[ , -8]
RL = RL[ , -8]
Read = Read[ , -8]

#reshape the data
long.IS = melt(IS,
               measure.vars = c("JOL", "Scored"))
long.RL = melt(RL,
               measure.vars = c("JOL", "Scored"))
long.Read = melt(Read,
               measure.vars = c("JOL", "Scored"))

#add useful column names
colnames(long.IS)[6:7] = c("Task", "Score")
colnames(long.RL)[6:7] = c("Task", "Score")
colnames(long.Read)[6:7] = c("Task", "Score")

##Subset by Task type
#Start with JOLs
JOL.IS = subset(long.IS,
                long.IS$Task == "JOL")
JOL.RL = subset(long.RL,
                long.RL$Task == "JOL")
JOL.Read = subset(long.Read,
                long.Read$Task == "JOL")

#Now do recall
Recall.IS = subset(long.IS,
                long.IS$Task == "Scored")
Recall.RL = subset(long.RL,
                long.RL$Task == "Scored")
Recall.Read = subset(long.Read,
                  long.Read$Task == "Scored")

####Now we can get subject level output####
#JOLs first
cast.IS = cast(na.omit(JOL.IS), Subject ~ Direction, mean)
cast.RL = cast(na.omit(JOL.RL), Subject ~ Direction, mean)
cast.Read = cast(na.omit(JOL.Read), Subject ~ Direction, mean)

cast.IS2 = cast(na.omit(Recall.IS), Subject ~ Direction, mean)
cast.RL2 = cast((Recall.RL), Subject ~ Direction, mean)
cast.Read2 = cast(na.omit(Recall.Read), Subject ~ Direction, mean)

##useful colnames
colnames(cast.IS)[2:5] = c("B_JOL", "F_JOL", "S_JOL", "U_JOL")
colnames(cast.RL)[2:5] = c("B_JOL", "F_JOL", "S_JOL", "U_JOL")
colnames(cast.Read)[2:5] = c("B_JOL", "F_JOL", "S_JOL", "U_JOL")

colnames(cast.IS2)[2:5] = c("B_Recall", "F_Recall", "S_Recall", "U_Recall")
colnames(cast.RL2)[2:5] = c("B_Recall", "F_Recall", "S_Recall", "U_Recall")
colnames(cast.Read2)[2:5] = c("B_Recall", "F_Recall", "S_Recall", "U_Recall")

##now combine
IS.Final = cbind(cast.IS, cast.IS2)
RL.Final = cbind(cast.RL, cast.RL2)
Read.Final = cbind(cast.Read, cast.Read2)

IS.Final = IS.Final[ , -6]
RL.Final = RL.Final[ , -6]
Read.Final = Read.Final[ , -6]

####CSV TIME!####
#write.csv(IS.Final, file = "Item Specific.csv", row.names = F)
#write.csv(RL.Final, file = "Relational.csv", row.names = F)
#write.csv(Read.Final, file = "Read.csv", row.names = F)