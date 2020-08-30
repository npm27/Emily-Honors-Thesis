####Set up####
library(mice)
library(reshape)
library(ez)

warning = read.csv("warning.csv")
no_warning = read.csv("No Warning.csv")

##Fix condition labels
#No warning
no_warning$Condition.Description = substr(no_warning$Condition.Description, start = 1, stop = 4)

no_warning$Condition.Description[no_warning$Condition.Description == "RELA"] = "RELATIONAL"
no_warning$Condition.Description[no_warning$Condition.Description == "ITEM"] = "ITEM SPECIFIC"

#Warning
warning$Condition.Description = substr(warning$Condition.Description, start = 10, stop = 13)

warning$Condition.Description[warning$Condition.Description == "RL V"] = "RELATIONAL"
warning$Condition.Description[warning$Condition.Description == "IS V"] = "ITEM SPECIFIC"

#Fix column names
colnames(warning)[9] = "Direction"
colnames(no_warning)[9] = "Direction"

####Clean the Data####
##Note that this will change as we get more data
summary(warning)
summary(no_warning)

#Remove out of range scores
warning$JOL = as.numeric(warning$JOL)

warning$JOL[warning$JOL > 100] = NA
no_warning$JOL[no_warning$JOL > 100] = NA

##How many people do we have in each cell?
#Start with warning
is.w = subset(warning,
              warning$Condition.Description == "ITEM SPECIFIC")
length(unique(is.w$Username)) # 9 people

rl.w = subset(warning,
              warning$Condition.Description == "RELATIONAL")
length(unique(rl.w$Username)) # 8 people

read.w = subset(warning,
                warning$Condition.Description == "READ")
length(unique(read.w$Username)) #11 people

#Now check no warning
is.n = subset(no_warning,
              no_warning$Condition.Description == "ITEM SPECIFIC")
length(unique(is.n$Username)) #16 people

rl.n = subset(no_warning,
              no_warning$Condition.Description == "RELATIONAL")
length(unique(rl.n$Username)) #14 people

read.n = subset(no_warning,
                no_warning$Condition.Description == "READ")
length(unique(read.n$Username)) #12 people

##remove the idiot who put 0's for every JOL rating
no_warning = subset(no_warning,
                    no_warning$Username != "w10003113_hkb")

####Get Descriptives####
tapply(warning$JOL, list(warning$Condition.Description, warning$Direction), mean, na.rm = T)
tapply(warning$Recall_Score, list(warning$Condition.Description, warning$Direction), mean, na.rm = T)

tapply(no_warning$JOL, list(no_warning$Condition.Description, no_warning$Direction), mean, na.rm = T)
tapply(no_warning$Recall_Score, list(no_warning$Condition.Description, no_warning$Direction), mean, na.rm = T)

length(unique(no_warning$Username))

####Split it by block####
warning_block1 = subset(warning,
                        warning$Procedure.Shuffle == "Study Phase")
warning_block2 = subset(warning,
                        warning$Procedure.Shuffle == "Study Phase2")

##Block1
tapply(warning_block1$JOL, list(warning_block1$Condition.Description, warning_block1$Direction), mean, na.rm = T)
tapply(warning_block1$Recall_Score, list(warning_block1$Condition.Description, warning_block1$Direction), mean, na.rm = T)

##Block2
tapply(warning_block2$JOL, list(warning_block2$Condition.Description, warning_block2$Direction), mean, na.rm = T)
tapply(warning_block2$Recall_Score, list(warning_block2$Condition.Description, warning_block2$Direction), mean, na.rm = T)

length(unique(warning_block2$Username))

####Load in cleaned E-Prime Data here####
Eprime = read.csv("Eprime.csv")

##Need to fix labels and column headings and separate out warning vs no warning conditions
colnames(Eprime)[1] = "Condition.Description"
colnames(Eprime)[2] = "Username"
colnames(Eprime)[19] = "Recall_Score"

#Fix condition description
Eprime$Condition.Description = substr(Eprime$Condition.Description, start = 1, stop = 2)

Eprime$Condition.Description[Eprime$Condition.Description == "IS"] = "ITEM SPECIFIC"
Eprime$Condition.Description[Eprime$Condition.Description == "RL"] = "RELATIONAL"
Eprime$Condition.Description[Eprime$Condition.Description == "RE"] = "READ"

#Split Eprime based on old vs new instructions
Eprime_no_warning = subset(Eprime,
                           Eprime$Username < 13)
Eprime_warning = subset(Eprime,
                        Eprime$Username > 12)

##Now start dropping and reordering columns to get dataframes to line up correctly
#Do no warning first
Eprime_no_warning = Eprime_no_warning[ , -c(4:6, 8, 10:18)]
no_warning = no_warning[ , -c(2:3, 5:8, 12:14)]

colnames(no_warning)[4] = "Block"

Eprime_no_warning = Eprime_no_warning[ , c(2, 1, 4, 3, 5, 6)]

Eprime_no_warning$Version = rep("Eprime")

colnames(no_warning)[7] = "Version"

Eprime_no_warning$Username = as.character(Eprime_no_warning$Username)

#no_warning$Block = as.numeric(no_warning$Block)
no_warning$Block[no_warning$Block == "Study Phase"] = 1
no_warning$Block[no_warning$Block == "Study Phase2"] = 2

combined_no_warning = rbind(no_warning, Eprime_no_warning)

#Now do warning data
Eprime_warning = Eprime_warning[ , -c(4:6, 8, 10:18)]
warning = warning[ , -c(2:3, 5:8, 12:14)]

colnames(warning)[4] = "Block"

Eprime_warning = Eprime_warning[ , c(2, 1, 4, 3, 5, 6)]

Eprime_warning$Version = rep("Eprime")
colnames(warning)[7] = "Version"

Eprime_warning$Username = as.character(Eprime_warning$Username)

#warning$Block = as.numeric(warning$Block)
warning$Block[warning$Block == "Study Phase"] = 1
warning$Block[warning$Block == "Study Phase2"] = 2

combined_warning = rbind(warning, Eprime_warning)

combined_warning_block1 = subset(combined_warning,
                                 combined_warning$Block == 1)
combined_warning_block2 = subset(combined_warning,
                                 combined_warning$Block == 2)

####Clean the combined data####
##Make sure scores are in range
summary(combined_no_warning)
summary(combined_warning_block1)
summary(combined_warning_block2)

####Check for outliers####
##Start with no_warning data
cnw_IS = subset(combined_no_warning,
                combined_no_warning$Condition.Description == "ITEM SPECIFIC")
cnw_RL = subset(combined_no_warning,
                combined_no_warning$Condition.Description == "RELATIONAL")
cnw_READ = subset(combined_no_warning,
                  combined_no_warning$Condition.Description == "READ")

##Need to look for JOL and Recall outliers

##Item Specific
#JOL
IS1 = cast(cnw_IS[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

IS1$z_B = scale(IS1$B)
IS1$z_F = scale(IS1$F)
IS1$z_S = scale(IS1$S)
IS1$z_U = scale(IS1$U)

#Recall
IS2 = cast(cnw_IS[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

IS2$z_B = scale(IS2$B)
IS2$z_F = scale(IS2$F)
IS2$z_S = scale(IS2$S)
IS2$z_U = scale(IS2$U)

##Relational
RL1 = cast(cnw_RL[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RL1$z_B = scale(RL1$B)
RL1$z_F = scale(RL1$F)
RL1$z_S = scale(RL1$S)
RL1$z_U = scale(RL1$U)

#w984625_KW

#Recall
RL2 = cast(cnw_RL[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RL2$z_B = scale(RL2$B)
RL2$z_F = scale(RL2$F)
RL2$z_S = scale(RL2$S)
RL2$z_U = scale(RL2$U)

##Read
RE1 = cast(cnw_READ[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RE1$z_B = scale(RE1$B)
RE1$z_F = scale(RE1$F)
RE1$z_S = scale(RE1$S)
RE1$z_U = scale(RE1$U)

#Recall
RE2 = cast(cnw_READ[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RE2$z_B = scale(RE2$B)
RE2$z_F = scale(RE2$F)
RE2$z_S = scale(RE2$S)
RE2$z_U = scale(RE2$U)

#w10006452SW

##Remove no_warning outliers
#Note that these may change as we get more data
combined_no_warning = subset(combined_no_warning,
                             combined_no_warning$Username != "w10006452SW")

###Now look at warning block 1 data
cb1_IS = subset(combined_warning_block1,
                combined_warning_block1$Condition.Description == "ITEM SPECIFIC")
cb1_RL = subset(combined_warning_block1,
                combined_warning_block1$Condition.Description == "RELATIONAL")
cb1_READ = subset(combined_warning_block1,
                 combined_warning_block1$Condition.Description == "READ")

##Item Specific
#JOL
IS1 = cast(cb1_IS[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

IS1$z_B = scale(IS1$B)
IS1$z_F = scale(IS1$F)
IS1$z_S = scale(IS1$S)
IS1$z_U = scale(IS1$U)

#Recall
IS2 = cast(cb1_IS[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

IS2$z_B = scale(IS2$B)
IS2$z_F = scale(IS2$F)
IS2$z_S = scale(IS2$S)
IS2$z_U = scale(IS2$U)

##Relational
RL1 = cast(cb1_RL[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RL1$z_B = scale(RL1$B)
RL1$z_F = scale(RL1$F)
RL1$z_S = scale(RL1$S)
RL1$z_U = scale(RL1$U)

#Recall
RL2 = cast(cb1_RL[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RL2$z_B = scale(RL2$B)
RL2$z_F = scale(RL2$F)
RL2$z_S = scale(RL2$S)
RL2$z_U = scale(RL2$U)

##Read
RE1 = cast(cb1_READ[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RE1$z_B = scale(RE1$B)
RE1$z_F = scale(RE1$F)
RE1$z_S = scale(RE1$S)
RE1$z_U = scale(RE1$U)

#Recall
RE2 = cast(cb1_READ[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RE2$z_B = scale(RE2$B)
RE2$z_F = scale(RE2$F)
RE2$z_S = scale(RE2$S)
RE2$z_U = scale(RE2$U)

###Now look at warning block 2 data
cb2_IS = subset(combined_warning_block2,
                combined_warning_block2$Condition.Description == "ITEM SPECIFIC")
cb2_RL = subset(combined_warning_block2,
                combined_warning_block2$Condition.Description == "RELATIONAL")
cb2_READ = subset(combined_warning_block2,
                  combined_warning_block2$Condition.Description == "READ")

##Item Specific
#JOL
IS1 = cast(cb2_IS[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

IS1$z_B = scale(IS1$B)
IS1$z_F = scale(IS1$F)
IS1$z_S = scale(IS1$S)
IS1$z_U = scale(IS1$U)

#Recall
IS2 = cast(cb2_IS[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

IS2$z_B = scale(IS2$B)
IS2$z_F = scale(IS2$F)
IS2$z_S = scale(IS2$S)
IS2$z_U = scale(IS2$U)

##Relational
RL1 = cast(cb2_RL[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RL1$z_B = scale(RL1$B)
RL1$z_F = scale(RL1$F)
RL1$z_S = scale(RL1$S)
RL1$z_U = scale(RL1$U)

#Recall
RL2 = cast(cb2_RL[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RL2$z_B = scale(RL2$B)
RL2$z_F = scale(RL2$F)
RL2$z_S = scale(RL2$S)
RL2$z_U = scale(RL2$U)

##Read
RE1 = cast(cb2_READ[ , c(1, 3, 5)], Username ~ Direction, mean, na.rm = T)

RE1$z_B = scale(RE1$B)
RE1$z_F = scale(RE1$F)
RE1$z_S = scale(RE1$S)
RE1$z_U = scale(RE1$U)

#Recall
RE2 = cast(cb2_READ[ , c(1, 3, 6)], Username ~ Direction, mean, na.rm = T)

RE2$z_B = scale(RE2$B)
RE2$z_F = scale(RE2$F)
RE2$z_S = scale(RE2$S)
RE2$z_U = scale(RE2$U)

##Split no warning by block
combined_no_warning_block1 = subset(combined_no_warning,
                                 combined_no_warning$Block == 1)
combined_no_warning_block2 = subset(combined_no_warning,
                                 combined_no_warning$Block == 2)

####Warning Descriptives####
##Sample size
length(unique(combined_no_warning$Username)) #77
length(unique(combined_warning$Username))  #87

##Block1
tapply(combined_warning_block1$JOL,
       list(combined_warning_block1$Condition.Description, combined_warning_block1$Direction), mean, na.rm = T)

tapply(combined_warning_block1$Recall_Score,
       list(combined_warning_block1$Condition.Description, combined_warning_block1$Direction), mean, na.rm = T)

##Block2
tapply(combined_warning_block2$JOL,
       list(combined_warning_block2$Condition.Description, combined_warning_block2$Direction), mean, na.rm = T)

tapply(combined_warning_block2$Recall_Score,
       list(combined_warning_block2$Condition.Description, combined_warning_block2$Direction), mean, na.rm = T)

##Collapsed
tapply(combined_warning$JOL, list(combined_warning$Direction, combined_warning$Condition.Description), mean, na.rm = T)

tapply(combined_warning$Recall_Score, list(combined_warning$Direction, combined_warning$Condition.Description), mean, na.rm = T)

####Control Descriptives####
##Block 1
tapply(combined_no_warning_block1$JOL,
       list(combined_no_warning_block1$Condition.Description, combined_no_warning_block1$Direction), mean, na.rm = T)
tapply(combined_no_warning_block1$Recall_Score,
       list(combined_no_warning_block1$Condition.Description, combined_no_warning_block1$Direction), mean, na.rm = T)

##Block 2
tapply(combined_no_warning_block2$JOL,
       list(combined_no_warning_block2$Condition.Description, combined_no_warning_block2$Direction), mean, na.rm = T)
tapply(combined_no_warning_block2$Recall_Score,
       list(combined_no_warning_block2$Condition.Description, combined_no_warning_block2$Direction), mean, na.rm = T)

##Collapsed
tapply(combined_no_warning$JOL, list(combined_no_warning$Direction, combined_no_warning$Condition.Description), mean, na.rm = T)

tapply(combined_no_warning$Recall_Score, list(combined_no_warning$Direction, combined_no_warning$Condition.Description), mean, na.rm = T)

####After cleaning, how many people do we have in each cell?
##How many people do we have in each cell?
#Start with warning
is.w = subset(combined_warning,
              combined_warning$Condition.Description == "ITEM SPECIFIC")
length(unique(is.w$Username)) # 36 people

rl.w = subset(combined_warning,
              combined_warning$Condition.Description == "RELATIONAL")
length(unique(rl.w$Username)) # 30 people

read.w = subset(combined_warning,
                combined_warning$Condition.Description == "READ")
length(unique(read.w$Username)) #36 people

#Now check no warning
is.n = subset(combined_no_warning,
              combined_no_warning$Condition.Description == "ITEM SPECIFIC")
length(unique(is.n$Username)) #32 people

rl.n = subset(combined_no_warning,
              combined_no_warning$Condition.Description == "RELATIONAL")
length(unique(rl.n$Username)) #31 people

read.n = subset(combined_no_warning,
                combined_no_warning$Condition.Description == "READ")
length(unique(read.n$Username)) #40 people

####Save combined dataset as .csv####
#write.csv(combined_no_warning_block1, file = "no_warning_b1.csv", row.names = F)
#write.csv(combined_no_warning_block2, file = "no_warning_b2.csv", row.names = F)

#write.csv(combined_warning_block1, file = "warning_b1.csv", row.names = F)
#write.csv(combined_warning_block2, file = "warning_b2.csv", row.names = F)

##Will need to write new files with updated data

####SET UP FOR ANOVA####
##Get the data in the right format

#No warning block 1
colnames(combined_no_warning_block1)[6] = "Recall"
long.cnw_b1 = melt(combined_no_warning_block1, measure.vars = c("JOL", "Recall"))

colnames(long.cnw_b1)[6] = "Task"
colnames(long.cnw_b1)[7] = "Score"

#No warning block 2
colnames(combined_no_warning_block2)[6] = "Recall"
long.cnw_b2 = melt(combined_no_warning_block2, measure.vars = c("JOL", "Recall"))

colnames(long.cnw_b2)[6] = "Task"
colnames(long.cnw_b2)[7] = "Score"

#warning block 1
colnames(combined_warning_block1)[6] = "Recall"
long.cw_b1 = melt(combined_warning_block1, measure.vars = c("JOL", "Recall"))

colnames(long.cw_b1)[6] = "Task"
colnames(long.cw_b1)[7] = "Score"

#warning block 2
colnames(combined_warning_block2)[6] = "Recall"
long.cw_b2 = melt(combined_warning_block2, measure.vars = c("JOL", "Recall"))

colnames(long.cw_b2)[6] = "Task"
colnames(long.cw_b2)[7] = "Score"

##Combine warning/no_warnings
long.warning = rbind(long.cw_b1, long.cw_b2)
long.warning$Warning = rep("yes")

long.no_warning = rbind(long.cnw_b1, long.cnw_b2)
long.no_warning$Warning = rep("no")

##now make the final data
anova.data = rbind(long.warning, long.no_warning)

anova.data$Block = as.character(anova.data$Block)

#Remove missing
anova.data2 = na.omit(anova.data)

anova.data3 = subset(anova.data2,
                     anova.data2$Task == "JOL")
anova.data4 = subset(anova.data2,
                     anova.data2$Task == "Recall")

anova.data5 = subset(anova.data,
                     anova.data$Task == "JOL")

#tapply(anova.data3$Score, list(anova.data3$Condition.Description, anova.data3$Block, anova.data3$Warning, anova.data3$Direction), mean, na.rm = F)

##Could try imputing missing JOL scores
library(mice)
percentmiss = function(x){ sum(is.na(x)/length(x)) * 100}

##jol
tempnomiss = mice(as.data.frame(anova.data5[ , c(1, 7)]))
replaced = complete(tempnomiss, 1)

##by subject
##block 1
replacepeople1 = subset(combined.jol.1, missing1 <= 5)
nopeople1  = subset(combined.jol.1, missing1 > 5)

replaceall = replacepeople1[ , -c(1,82)]
nocolumn = replacepeople1[ , c(1,82)]

summary(is.na(replaceall))
table(is.na(replaceall))

tempnomiss = mice(replaceall)
replaced = complete(tempnomiss, 1)

allcolumns = cbind(nocolumn, replaced)
nomiss1 = allcolumns

anova.data5$Score = replaced$Score

anova.data6 = rbind(anova.data5, anova.data4)

####Run the ANOVA####
model1 = ezANOVA(data = anova.data6,
                 wid = Username,
                 between = .(Warning, Condition.Description),
                 within = .(Direction, Task),
                 type = 3,
                 dv = Score,
                 detailed = T)
model1
