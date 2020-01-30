####Let's do some analyses!####
##set up
master = read.csv("Master Data Sheet.csv") #Loads the data set and saves it as dataframe called 'master'

summary(master) #lets see what we're working with here

#drop unused columns
#all we really need is EX Version, subject ID, JOL rating, RT, block, pair type, and whether or not they recalled it correctly
dat = master[ , -c(4, 11:12, 14:19)]
dat = dat[, -c(4:5, 7, 10)]

#okay, lets take another summary
summary(dat)

#JOLs are being treated as factor, we need to change this to a numeric variable so we can calculate means and what not
dat$JOL = as.numeric(as.character(dat$JOL))

#We also need to get JOLs and Recall on the same scale
colnames(dat)[7] = "Recall" #make the column name more helpful

dat$Recall = dat$Recall * 100

#Next, make a column for Experiment type
#split by condition and add column for condition type
read = subset(dat,
              dat$ExperimentName == "READ_JOL A" | dat$ExperimentName == "READ_JOL B" | dat$ExperimentName == "READ_JOL C" | dat$ExperimentName == "READ_JOL D")
read$type = rep("READ")

RL = subset(dat,
            dat$ExperimentName == "RL_JOL A" | dat$ExperimentName == "RL_JOL B" | dat$ExperimentName == "RL_JOL C" | dat$ExperimentName == "RL_JOL D")
RL$type = rep("RL")

IS = subset(dat,
            dat$ExperimentName == "IS_JOL A" | dat$ExperimentName == "IS_JOL B" | dat$ExperimentName == "IS_JOL C" | dat$ExperimentName == "IS_JOL D")
IS$type = rep("IS")

#put the things back together
combined = rbind(read, RL, IS)

####Lets get descriptives####
summary(combined$JOL) #we have some out of range JOLs
combined$JOL[combined$JOL > 100] = NA #Get rid of them!

summary(combined$JOL)

#JOLs
tapply(combined$JOL,
       list(combined$type, combined$Direction), mean, na.rm = T)
tapply(combined$Recall,
       list(combined$type, combined$Direction), mean, na.rm = T)

####Let's check for outliers and see if this pattern holds####
#use the reshape package to get each subjects mean JOL and Recall score for each Direction

#First subset by EX Type
#We already have these from when we made the type column
library(reshape)

#remove missing
read = na.omit(read)
IS = na.omit(IS)
RL = na.omit(RL)

#READ INSTRUCTIONS
long.read = melt(read, id = c("Subject", 'ExperimentName',
                                 "Block", "RT", "Direction", "type"))
summary(long.read)
colnames(long.read)[7] = "Task"
colnames(long.read)[8] = "Score"

#ITEM SPECIFIC
long.IS = melt(IS, id = c("Subject", 'ExperimentName',
                              "Block", "RT", "Direction", "type"))
summary(long.IS)
colnames(long.IS)[7] = "Task"
colnames(long.IS)[8] = "Score"

#RELATIONAL ENCODING
long.RL = melt(RL, id = c("Subject", 'ExperimentName',
                          "Block", "RT", "Direction", "type"))
summary(long.RL)
colnames(long.RL)[7] = "Task"
colnames(long.RL)[8] = "Score"

#Now get subject level means
#read condition
recall = subset(long.read,
                long.read$Task == "Recall")
jol = subset(long.read,
             long.read$Task == "JOL")

recall.read = cast(recall[ , -6], Subject ~ Direction, mean)
jol.read = cast(jol[ , -6], Subject ~ Direction, mean)

colnames(recall.read)[3] = "f"
colnames(jol.read)[3] = "f"

#Item Specific
recall = subset(long.IS,
                long.IS$Task == "Recall")
jol = subset(long.IS,
             long.IS$Task == "JOL")

recall.IS = cast(recall[ , -6], Subject ~ Direction, mean)
jol.IS = cast(jol[ , -6], Subject ~ Direction, mean)

colnames(recall.IS)[3] = "f"
colnames(jol.IS)[3] = "f"

#Relational processing
recall = subset(long.RL,
                long.RL$Task == "Recall")
jol = subset(long.RL,
             long.RL$Task == "JOL")

recall.RL = cast(recall[ , -6], Subject ~ Direction, mean)
jol.RL = cast(jol[ , -6], Subject ~ Direction, mean)

colnames(recall.RL)[3] = "f"
colnames(jol.RL)[3] = "f"

####Now Check for Outliers####
#start with Read
#JOLs
jol.read$zu = scale(jol.read$U)
jol.read$zs = scale(jol.read$S)
jol.read$zb = scale(jol.read$B)
jol.read$zf = scale(jol.read$f) #No z-score outliers for any, but keep an eye on 75

#Recall
recall.read$zu = scale(recall.read$U)
recall.read$zs = scale(recall.read$S)
recall.read$zb = scale(recall.read$B)
recall.read$zf = scale(recall.read$f) #No outliers

#Now do IS
#JOLs
jol.IS$zu = scale(jol.IS$U)
jol.IS$zs = scale(jol.IS$S)
jol.IS$zb = scale(jol.IS$B)
jol.IS$zf = scale(jol.IS$f) #No z-score outliers for any, but keep an eye on 18

#Recall
recall.IS$zu = scale(recall.IS$U)
recall.IS$zs = scale(recall.IS$S)
recall.IS$zb = scale(recall.IS$B)
recall.IS$zf = scale(recall.IS$f) #No outliers, but 8 is pretty low across the board

#Finally do RL
#JOLs
jol.RL$zu = scale(jol.RL$U)
jol.RL$zs = scale(jol.RL$S)
jol.RL$zb = scale(jol.RL$B)
jol.RL$zf = scale(jol.RL$f)

#Recall
recall.RL$zu = scale(recall.RL$U)
recall.RL$zs = scale(recall.RL$S)
recall.RL$zb = scale(recall.RL$B)
recall.RL$zf = scale(recall.RL$f)

##Note: We'll use the individual means for follow up t-tests

##make a long data set now for doing ANOVAs
anova_data = rbind(long.IS, long.read, long.RL)

####Okay, Lets run some ANOVAs now!####
library(ez)

#turn off scientific notation
options(scipen = 999)

#Note: Direction is within subjects, task is within subjects, ex type is between groups
output1 = ezANOVA(data = anova_data,
                  wid = Subject,
                  between = type,
                  within = .(Direction, Task),
                  dv = Score,
                  type = 3,
                  detailed = T)
output1

length(unique(anova_data$Subject)) #84 subjects

#overall score
tapply(anova_data$Score,
       list(anova_data$type, anova_data$task), mean)

#recall
anova_recall = subset(anova_data,
                      anova_data$Task == "Recall")
tapply(anova_recall$Score,
       list(anova_recall$type, anova_recall$Direction), mean)

#jol
anova_jol = subset(anova_data,
                      anova_data$Task == "JOL")
tapply(anova_jol$Score,
       list(anova_jol$type, anova_jol$Direction), mean)

#how many in each group?
length(tapply(anova_data$Subject,
       anova_data$type, unique)[[1]]) #IS
length(tapply(anova_data$Subject,
       anova_data$type, unique)[[2]]) #READ
length(tapply(anova_data$Subject,
       anova_data$type, unique)[[3]]) #RL

####Remove RL D####
anova_data2 = subset(anova_data,
                     anova_data$ExperimentName != "RL_JOL D")

output2 = ezANOVA(data = anova_data2,
                  wid = Subject,
                  between = type,
                  within = .(Direction, Task),
                  dv = Score,
                  type = 3,
                  detailed = T)
output2
output1

tapply(anova_data2$Score,
       list(anova_data2$type, anova_data2$task), mean)

#recall
anova_recall = subset(anova_data2,
                      anova_data2$Task == "Recall")
tapply(anova_recall$Score,
       list(anova_recall$type, anova_recall$Direction), mean)

#jol
anova_jol = subset(anova_data2,
                   anova_data2$Task == "JOL")
tapply(anova_jol$Score,
       list(anova_jol$type, anova_jol$Direction), mean)

