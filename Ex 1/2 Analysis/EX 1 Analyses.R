####Let's do some analyses!####
##set up
master = read.csv("Master Data Sheet.csv") #Loads the data set and saves it as dataframe called 'master'

summary(master) #lets see what we're working with here

#drop unused columns
#all we really need is EX Version, subject ID, JOL rating, RT, block, pair type, and whether or not they recalled it correctly
dat = master[ , -c(4, 11:12, 14:18)]
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

unique(anova_data$Block)

length(unique(anova_data$Subject))

anova_data$Block = as.character(anova_data$Block)

#Note: Direction is within subjects, task is within subjects, ex type is between groups
output1 = ezANOVA(data = anova_data,
                  wid = Subject,
                  between = type,
                  within = .(Direction, Task, Block),
                  dv = Score,
                  type = 3,
                  detailed = T)
output1

anovaLength = length(output1$ANOVA)
output1$ANOVA$MSE = output1$ANOVA$SSd/output1$ANOVA$DFd
output1$ANOVA$MSE

length(unique(anova_data$Subject))


length(unique(anova_data$Subject)) #88 subjects

##supplemental stuff
tapply(anova_data$Score, anova_data$Block, mean, na.rm = T)

#overall score
tapply(anova_data$Score,
       list(anova_data$type), mean)

tapply(anova_data$Score,
       list(anova_data$Direction), mean)

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

####Get Means for Post-Hocs####
tapply(anova_data$Score, anova_data$Direction, mean) #main effect of direction
tapply(anova_data$Score, anova_data$Task, mean) #main effect of JOL vs Recall
tapply(anova_data$Score, anova_data$type, mean) #main effect of instruction type

tapply(anova_data$Score, list(anova_data$Task, anova_data$Direction), mean) #This is our two-way between task and direction
tapply(anova_data$Score, list(anova_data$Task, anova_data$type), mean) #Two way between JOL vs Recall and instruction type
tapply(anova_data$Score, list(anova_data$type, anova_data$Direction), mean) #Two way between task and direction #Note that these scores are combined across JOLs and Recall

####Get the three-way interaction####
#recall
anova_recall = subset(anova_data,
                      anova_data$Task == "Recall")

#jol
anova_jol = subset(anova_data,
                   anova_data$Task == "JOL")

#Three-way output
tapply(anova_recall$Score,
       list(anova_recall$type, anova_recall$Direction), mean)
tapply(anova_jol$Score,
       list(anova_jol$type, anova_jol$Direction), mean)

####Post-Hocs Start Here!####

#####MAIN EFFECT OF DIRECTION####
#Make the data
Direction.Combined = cast(anova_data, Subject ~ Direction, mean)

#Get SD
sd(Direction.Combined$F)
sd(Direction.Combined$B)
sd(Direction.Combined$S)
sd(Direction.Combined$U)

mean(Direction.Combined$F)
mean(Direction.Combined$B)
mean(Direction.Combined$S)
mean(Direction.Combined$U)

#t-tests
#F vs B
temp1 = t.test(Direction.Combined$F, Direction.Combined$B, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92

temp1;SEM1 #SIG

mean(Direction.Combined$F)
mean(Direction.Combined$S)

sd(Direction.Combined$F)
sd(Direction.Combined$S)

#F vs S
temp2 = t.test(Direction.Combined$F, Direction.Combined$S, paired = T, p.adjust.methods = "Bonferroni")
p2 = round(temp2$p.value, 3)
t2 = temp2$statistic
SEM2 = (temp2$conf.int[2] - temp2$conf.int[1]) / 3.92

temp2;SEM2 #SIG

#F vs U
temp3 = t.test(Direction.Combined$F, Direction.Combined$U, paired = T, p.adjust.methods = "Bonferroni")
p3 = round(temp3$p.value, 3)
t3 = temp3$statistic
SEM3 = (temp3$conf.int[2] - temp3$conf.int[1]) / 3.92

temp3;SEM3 #SIG

#B vs S
temp4 = t.test(Direction.Combined$B, Direction.Combined$S, paired = F, p.adjust.methods = "Bonferroni")
p4 = round(temp4$p.value, 3)
t4 = temp4$statistic
SEM4 = (temp4$conf.int[2] - temp4$conf.int[1]) / 3.92

temp4;SEM4 #SIG

#B vs U
temp5 = t.test(Direction.Combined$B, Direction.Combined$S, paired = F, p.adjust.methods = "Bonferroni")
p5 = round(temp5$p.value, 3)
t5 = temp5$statistic
SEM5 = (temp5$conf.int[2] - temp5$conf.int[1]) / 3.92

temp5;SEM5 #SIG

#S vs U
temp6 = t.test(Direction.Combined$S, Direction.Combined$U, paired = F, p.adjust.methods = "Bonferroni")
p6 = round(temp6$p.value, 3)
t6 = temp6$statistic
SEM6 = (temp6$conf.int[2] - temp6$conf.int[1]) / 3.92

temp6;SEM6 #SIG

####MAIN EFFECT OF TASK TYPE (JOL vs RECALL)####
#Make the data
Task.Combined = cast(anova_data, Subject ~ Task, mean)

#Get SD
sd(Task.Combined$JOL)
sd(Task.Combined$Recall)

mean(Task.Combined$JOL)
mean(Task.Combined$Recall)

#Now do the t-tests
#JOL vs Recall
temp7 = t.test(Task.Combined$JOL, Task.Combined$Recall, paired = T, p.adjust.methods = "Bonferroni")
p7 = round(temp7$p.value, 3)
t7 = temp7$statistic
SEM7 = (temp7$conf.int[2] - temp7$conf.int[1]) / 3.92

temp7;SEM7 #SIG

####MAIN EFFECT OF INSTRUCTION TYPE####
#make the data
Type.Combined = cast(anova_data, Subject ~ type, mean)

#get sd
sd(Type.Combined$IS, na.rm = T)
sd(Type.Combined$READ, na.rm = T)
sd(Type.Combined$RL, na.rm = T)

mean(Type.Combined$IS, na.rm = T)
mean(Type.Combined$READ, na.rm = T)
mean(Type.Combined$RL, na.rm = T)


#Now do t-tests
#IS vs RL
temp8 = t.test(Type.Combined$IS, Type.Combined$RL, paired = F, p.adjust.methods = "Bonferroni")
p8 = round(temp8$p.value, 3)
t8 = temp8$statistic
SEM8 = (temp8$conf.int[2] - temp8$conf.int[1]) / 3.92

temp8;SEM8 #NON-SIG

#IS vs READ
temp9 = t.test(Type.Combined$IS, Type.Combined$READ, paired = F, p.adjust.methods = "Bonferroni")
p9 = round(temp9$p.value, 3)
t9 = temp9$statistic
SEM9 = (temp9$conf.int[2] - temp9$conf.int[1]) / 3.92

temp9;SEM9 #SIG

#RL vs READ
temp10 = t.test(Type.Combined$RL, Type.Combined$READ, paired = F, p.adjust.methods = "Bonferroni")
p10 = round(temp10$p.value, 3)
t10 = temp10$statistic
SEM10 = (temp10$conf.int[2] - temp10$conf.int[1]) / 3.92

temp10;SEM10 #SIG

##Do pbic
relational1 = Type.Combined[ , c(1,4)]
is1 = Type.Combined[ , c(1,2)]

relational1$task = rep("rel")
is1$task = rep("is")

relational1 = na.omit(relational1)
is1 = na.omit(is1)

colnames(relational1)[2] = "Score"
colnames(is1)[2] = "Score"

pbic1 = rbind(is1, relational1)

####NOW DO THE TWO-WAY INTERACTIONS####
####INTERACTION BETWEEN TASK AND DIRECTION (ILLUSION OF COMPETENCE)
#First will need to separate out JOL and Recall data
IOC.JOL = subset(anova_data,
                anova_data$Task == "JOL")
IOC.Recall = subset(anova_data,
                    anova_data$Task == "Recall")

#Now get subject level means for each direction type
#make the data, one for JOLs and one for recall
IOC.JOL2 = cast(IOC.JOL, Subject ~ Direction, mean)
IOC.Recall2 = cast(IOC.Recall, Subject ~ Direction, mean)

#Get SDs
#Start with JOLs
sd(IOC.JOL2$B)
sd(IOC.JOL2$F)
sd(IOC.JOL2$S)
sd(IOC.JOL2$U)

#Now Recall
sd(IOC.Recall2$B)
sd(IOC.Recall2$F)
sd(IOC.Recall2$S)
sd(IOC.Recall2$U)

##Now do the t-tests
##Forward
temp11 = t.test(IOC.JOL2$F, IOC.Recall2$F, paired = T, p.adjust.methods = "Bonferroni")
p11 = round(temp11$p.value, 3)
t11 = temp11$statistic
SEM11 = (temp11$conf.int[2] - temp11$conf.int[1]) / 3.92

temp11;SEM11 #NON-SIG

##Backward
temp12 = t.test(IOC.JOL2$B, IOC.Recall2$B, paired = T, p.adjust.methods = "Bonferroni")
p12 = round(temp12$p.value, 3)
t12 = temp12$statistic
SEM12 = (temp12$conf.int[2] - temp12$conf.int[1]) / 3.92

temp12;SEM12 #Sig!

##Symmetrical
temp13 = t.test(IOC.JOL2$S, IOC.Recall2$S, paired = T, p.adjust.methods = "Bonferroni")
p13 = round(temp13$p.value, 3)
t13 = temp13$statistic
SEM13 = (temp13$conf.int[2] - temp13$conf.int[1]) / 3.92

temp13;SEM13 #NON-Sig

##Unrelated
temp14 = t.test(IOC.JOL2$U, IOC.Recall2$U, paired = T, p.adjust.methods = "Bonferroni")
p14 = round(temp14$p.value, 3)
t14 = temp14$statistic
SEM14 = (temp14$conf.int[2] - temp14$conf.int[1]) / 3.92

temp14;SEM14 #Sig!

##So collapsing across instruction type, the IOC replicates for backward and unrelated pairs, does not occur for symmetrical pairs!

####Three-way interaction####
##Need to subset by instruction type now

#Start with JOLs
Read.JOL = subset(IOC.JOL,
                  IOC.JOL$type == "READ")
IS.JOL = subset(IOC.JOL,
                IOC.JOL$type == "IS")
RL.JOL = subset(IOC.JOL,
                IOC.JOL$type == "RL")

#Now do recall
Read.RECALL = subset(IOC.Recall,
                     IOC.Recall$type == "READ")
IS.RECALL = subset(IOC.Recall,
                   IOC.Recall$type == "IS")
RL.RECALL = subset(IOC.Recall,
                   IOC.Recall$type == "RL")

#Now get subject level means for these datasets
#JOLs
Read.JOL2 = cast(Read.JOL, Subject ~ Direction, mean)
IS.JOL2 = cast(IS.JOL, Subject ~ Direction, mean)
RL.JOL2 = cast(RL.JOL, Subject ~ Direction, mean)

x1 = apply(Read.JOL2[ , -1], 2, mean)
x2 = apply(IS.JOL2[ , -1], 2, mean)
x3 = apply(RL.JOL2[ , -1], 2, mean)

y1 = apply(Read.JOL2[ , -1], 2, sd)
y2 = apply(IS.JOL2[ , -1], 2, sd)
y3 = apply(RL.JOL2[ , -1], 2, sd)

z1 = y1 / sqrt(nrow(Read.JOL2))
z2 = y2 / sqrt(nrow(IS.JOL2))
z3 = y3 / sqrt(nrow(RL.JOL2))

z1 = z1 * 1.96
z2 = z2 * 1.96
z3 = z3 * 1.96

x1 + z1
x1 - z1

x2 + z2
x2 - z2

x3 + z3
x3 - z3

#Recall
Read.RECALL2 = cast(Read.RECALL, Subject ~ Direction, mean)
IS.RECALL2 = cast(IS.RECALL, Subject ~ Direction, mean)
RL.RECALL2 = cast(RL.RECALL, Subject ~ Direction, mean)

A1 = apply(Read.RECALL2[ , -1], 2, mean)
A2 = apply(IS.RECALL2[ , -1], 2, mean)
A3 = apply(RL.RECALL2[ , -1], 2, mean)

B1 = apply(Read.RECALL2[ , -1], 2, sd)
B2 = apply(IS.RECALL2[ , -1], 2, sd)
B3 = apply(RL.RECALL2[ , -1], 2, sd)

C1 = B1 / sqrt(nrow(Read.RECALL2))
C2 = B2 / sqrt(nrow(IS.RECALL2))
C3 = B3 / sqrt(nrow(RL.RECALL2))

C1 = C1 * 1.96
C2 = C2 * 1.96
C3 = C3 * 1.96

A1 + z1
A1 - z1

A2 + z2
A2 - z2

A3 + z3
A3 - z3

#Now get SD
#Start with Recall
#Read
sd(Read.RECALL2$B)
sd(Read.RECALL2$F)
sd(Read.RECALL2$S)
sd(Read.RECALL2$U)

#IS
sd(IS.RECALL2$B)
sd(IS.RECALL2$F)
sd(IS.RECALL2$S)
sd(IS.RECALL2$U)

#RL
sd(RL.RECALL2$B)
sd(RL.RECALL2$F)
sd(RL.RECALL2$S)
sd(RL.RECALL2$U)

#Now JOLs
#Read
sd(Read.JOL2$B)
sd(Read.JOL2$F)
sd(Read.JOL2$S)
sd(Read.JOL2$U)

#IS
sd(IS.JOL2$B)
sd(IS.JOL2$F)
sd(IS.JOL2$S)
sd(IS.JOL2$U)

#RL
sd(RL.JOL2$B)
sd(RL.JOL2$F)
sd(RL.JOL2$S)
sd(RL.JOL2$U)

##Now do the t-tests
###Start with Unrelated
#IS
temp = t.test(IS.JOL2$U, IS.RECALL2$U, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

#Read
temp = t.test(Read.JOL2$U, Read.RECALL2$U, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

#RL
temp = t.test(RL.JOL2$U, RL.RECALL2$U, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

###Now do backward pairs
#IS
temp = t.test(IS.JOL2$B, IS.RECALL2$B, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

#Read
temp = t.test(Read.JOL2$B, Read.RECALL2$B, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

#RL
temp = t.test(RL.JOL2$B, RL.RECALL2$B, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

##Forward pairs
#IS
temp = t.test(IS.JOL2$F, IS.RECALL2$F, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(IS.JOL2$F)
mean(IS.RECALL2$F)

sd(IS.JOL2$F)
sd(IS.RECALL2$F)

#Read
temp = t.test(Read.JOL2$F, Read.RECALL2$F, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(Read.JOL2$F)
mean(Read.RECALL2$F)

Read.JOL2$task = rep("JOL")
Read.RECALL2$task = rep("RECALL")

pbic2 = rbind(Read.JOL2, Read.RECALL2)

length(unique(pbic2$Subject))

ezANOVA(pbic2,
        dv = F,
        within = task,
        wid = Subject,
        type = 3,
        detailed = T)

#RL
temp = t.test(RL.JOL2$F, RL.RECALL2$F, paired = T, p.adjust.methods = "Bonferroni")
p = round(temp$p.value, 3)
t = temp$statistic
SEM = (temp$conf.int[2] - temp$conf.int[1]) / 3.92

mean(RL.JOL2$F)
mean(RL.RECALL2$F)

RL.JOL2$task = rep("JOL")
RL.RECALL2$task = rep("RECALL")

pbic3 = rbind(RL.JOL2, RL.RECALL2)

length(unique(pbic3$Subject))

ezANOVA(pbic3,
        dv = F,
        within = task,
        wid = Subject,
        type = 3,
        detailed = T)

##symmetrical pairs


####Put together output for Bar charts/Tables####
##First, drop the Block, RT Column, and task columns
anova_jol2 = anova_jol[ , -c(3, 4, 7)]
anova_recall2 = anova_recall[ , -c(3, 4, 7)]

##Now subset based on encoding strategy
table(anova_jol2$type)

##jols
IS_JOL3 = subset(anova_jol2,
                 anova_jol2$type == "IS")
RL_JOL3 = subset(anova_jol2,
                 anova_jol2$type == "RL")
Read_JOL3 = subset(anova_jol2,
                   anova_jol2$type == "READ")

##Recall
IS.RECALL3 = subset(anova_recall2,
                    anova_recall2$type == "IS")
RL.RECALL3 = subset(anova_recall2,
                    anova_recall2$type == "RL")
Read.RECALL3 = subset(anova_recall2,
                      anova_recall2$type == "READ")

##Okay, now cast them into the right form
##jols
IS_JOL4 = cast(IS_JOL3, Subject ~ Direction, mean, na.rm = T)
RL_JOL4 = cast(RL_JOL3, Subject ~ Direction, mean, na.rm = T)
READ_JOL4 = cast(Read_JOL3, Subject ~ Direction, mean, na.rm = T)

##Recall
IS.RECALL4 = cast(IS.RECALL3, Subject ~ Direction, mean, na.rm = T)
RL.RECALL4 = cast(RL.RECALL3, Subject ~ Direction, mean, na.rm = T)
Read.RECALL4 = cast(Read.RECALL3, Subject ~ Direction, mean, na.rm = T)

##Write stuff to .csv
#write.csv(IS_JOL4, file = "IS JOLS.csv", row.names = F)
#write.csv(RL_JOL4, file = "RL JOLS.csv", row.names = F)
#write.csv(READ_JOL4, file = "Read JOLs.csv", row.names = F)
#write.csv(IS.RECALL4, file = "IS RECALL.csv", row.names = F)
#write.csv(RL.RECALL4, file = "RL RECALL.csv", row.names = F)
#write.csv(Read.RECALL4, file = "Read RECALL.csv", row.names = F)

##Do the supplemental table
block1 = subset(anova_data, anova_data$Block == 1)
block2 = subset(anova_data, anova_data$Block == 2)

#Now split each block by measure
block1.jol = subset(block1, block1$Task == "JOL")
block2.jol = subset(block2, block2$Task == "JOL")

block1.recall = subset(block1, block1$Task == "Recall")
block2.recall = subset(block2, block2$Task == "Recall")

tapply(block1.jol$Score, list(block1.jol$type, block1.jol$Direction), mean, na.rm = T)
tapply(block1.recall$Score, list(block1.recall$type, block1.recall$Direction), mean, na.rm = T)

tapply(block2.jol$Score, list(block2.jol$type, block2.jol$Direction), mean, na.rm = T)
tapply(block2.recall$Score, list(block2.recall$type, block2.recall$Direction), mean, na.rm = T)

##Do some t-tests


####Do that other analysis Mark wants because he's never satisfied####
JOLS = subset(anova_data,
              anova_data$Task == "JOL")
RECALL = subset(anova_data,
                anova_data$Task == "Recall")

##Get means
tapply(JOLS$Score, JOLS$type, mean, na.rm = T)

tapply(RECALL$Score, RECALL$type, mean, na.rm = T)

##JOLs
output1 = ezANOVA(JOLS,
        dv = Score,
        between = type,
        wid = Subject,
        type = 3,
        detailed = 3)

output1$ANOVA$MSE = output1$ANOVA$SSd/output1$ANOVA$DFd
output1$ANOVA$MSE

##Now do Recall
RECALL2 = cast(RECALL, Subject ~ type, mean)

IS = RECALL2$IS
IS = na.omit(IS)

RL = RECALL2$RL
RL = na.omit(RL)

READ = RECALL2$READ
READ = na.omit(READ)

t.test(IS, RL, paired = F, p.adjust.methods = "Bonferroni")
t.test(READ, RL, paired = F, p.adjust.methods = "Bonferroni")
t.test(IS, READ, paired = F, p.adjust.methods = "Bonferroni")

mean(READ)
mean(RL)

sd(READ)
sd(RL)

####Do the three-way interactions####
View(RECALL)

tapply(RECALL$Score, list(RECALL$Direction, RECALL$Block), mean, na.rm = T)
tapply(JOLS$Score, list(JOLS$Direction, JOLS$Block), mean, na.rm = T)

#Do t tests
B1 = subset(RECALL, RECALL$Block == "1")
B2 = subset(RECALL, RECALL$Block == "2")

B11 = subset(JOLS, JOLS$Block == "1")
B22 = subset(JOLS, JOLS$Block == "2")

RL1 = cast(B1, Subject ~ type, mean)
RL2 = cast(B2, Subject ~ type, mean)

JL1 = cast(B11, Subject ~ type, mean)
JL2 = cast(B22, Subject ~ type, mean)

#Test time!
#Read
temp1 = t.test(JL1$READ, RL1$READ, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

temp1 = t.test(JL2$READ, RL2$READ, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

mean(JL2$READ, na.rm = T)
mean(RL2$READ, na.rm = T)

sd(JL2$READ, na.rm = T)
sd(RL2$READ, na.rm = T)

temp1 = t.test(JL2$IS, RL2$IS, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

mean(JL2$RL, na.rm = T)
mean(RL2$RL, na.rm = T)

sd(JL2$RL, na.rm = T)
sd(RL2$RL, na.rm = T)

temp1 = t.test(JL2$RL, RL2$RL, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

#do pbic
pbic3 = RL1[ , c(1, 2)]
pbic4 = JL1[ , c(1, 2)]

pbic3$block = rep("1")
pbic4$block = rep("2")

pbic5 = rbind(pbic3, pbic4)
pbic5 = na.omit(pbic5)

ezANOVA(pbic5,
        wid = Subject,
        dv = RL,
        within = block,
        detailed = T)

#Now do relational
temp1 = t.test(JL1$RL, RL1$RL, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

##Now do the stuff collapsed across direction
RL1 = cast(B1, Subject ~ Direction, mean)
RL2 = cast(B2, Subject ~ Direction, mean)

JL1 = cast(B11, Subject ~ Direction, mean)
JL2 = cast(B22, Subject ~ Direction, mean)

#do more t-tests
temp1 = t.test(JL1$U, RL1$U, paired = T, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

mean(JL1$U, na.rm = T)
mean(RL1$U, na.rm = T)

sd(JL1$U, na.rm = T)
sd(RL1$U, na.rm = T)

#pbic
#do pbic
pbic3 = RL1[ , c(1, 4)]
pbic4 = JL1[ , c(1, 4)]

pbic3$block = rep("1")
pbic4$block = rep("2")

pbic5 = rbind(pbic3, pbic4)
pbic5 = na.omit(pbic5)

ezANOVA(pbic5,
        wid = Subject,
        dv = S,
        within = block,
        detailed = T)

##Okay, do that last analysis
e2 = read.csv("e2 recall.csv")

RECALL1 = subset(RECALL,
                 RECALL$Task != "READ")
RECALL2 = subset(e2,
                 e2$Task != "READ")

mean(RECALL1$Score)
mean(RECALL2$Score)

