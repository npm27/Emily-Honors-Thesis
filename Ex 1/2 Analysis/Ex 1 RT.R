##Set up
master = read.csv("Master Data Sheet.csv") #Loads the data set and saves it as dataframe called 'master'

options(scipen = 999)

summary(master) ##We have RT data!

##Okay, get the data
dat = master[ , -c(4, 11:12, 14:18)]
dat = dat[, -c(4:5, 7, 10)]

##Make ex name columns
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

table(combined$JOL)

combined$JOL[combined$JOL == "go"] = NA

combined$JOL = as.numeric(as.character(combined$JOL))
combined$JOL[combined$JOL > 100] = NA #Get rid of them!

##And now remove missing data
nomiss = na.omit(combined)

##Get means
tapply(combined$RT, combined$type, mean) #Okay, most time in item-specific, second most RL

##Okay, now need to get these values for Ex 2
EX2 = read.csv("emily e2 sona.csv")
Eprime = read.csv("eprime.csv")
prolific = read.csv("emily prolific e2.csv")

length(unique(EX2$Username))
length(unique(Eprime$Subject))
length(unique(prolific$Username))

EX2 = rbind(EX2, prolific)

#Okay, just get second block of collector data
block2 = subset(EX2,
                EX2$Procedure.Shuffle == "Study Phase2")

table(block2$Condition.Description)

##Fix warning labels
colnames(block2)[5] = "Warning"

table(block2$Warning)

block2$Warning[block2$Warning == "NEW"] = "Y"
block2$Warning[block2$Warning == "OLD"] = "N"

table(block2$Warning)

##subset by warning
warning = subset(block2, block2$Warning == "Y")
no_warning = subset(block2, block2$Warning == "N")

##Fix condition labels
#No warning
no_warning$Condition.Description = substr(no_warning$Condition.Description, start = 1, stop = 4)

no_warning$Condition.Description[no_warning$Condition.Description == "RELA"] = "RELATIONAL"
no_warning$Condition.Description[no_warning$Condition.Description == "ITEM"] = "ITEM SPECIFIC"

#Warning
warning$Condition.Description = substr(warning$Condition.Description, start = 10, stop = 13)

warning$Condition.Description[warning$Condition.Description == "RL V"] = "RELATIONAL"
warning$Condition.Description[warning$Condition.Description == "IS V"] = "ITEM SPECIFIC"

block2 = rbind(no_warning, warning)

##CLean up the eprime data
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
Eprime_no_warning = Eprime_no_warning[ , -c(4:6, 8, 11:18)]
Eprime_no_warning$Username = as.character(Eprime_no_warning$Username)

Eprime_warning = Eprime_warning[ , -c(4:6, 8, 11:18)]
Eprime_warning$Username = as.character(Eprime_warning$Username)

Eprime_warning$warning = rep("Y")
Eprime_no_warning$warning = rep("N")

Eprime_all = rbind(Eprime_warning, Eprime_no_warning)

Eprime_block2 = subset(Eprime_all,
                       Eprime_all$Block == 2)

##Okay, now get the collector data in the right format
#CONDITION, USERNAME, BLOCK, DIRECTION, JOL, RT, RECALL, WARNING
#Drop recall from eprime
Eprime_block2 = Eprime_block2[ , -7]

block2 = block2[ , -c(2, 6:8, 10:11, 13)]

block2 = block2[ , -2]

Eprime_block2 = Eprime_block2[ , -3]

##Now make sure all column names are the same and in the correct order
Eprime_block2 = Eprime_block2[ , c(2, 1, 6, 3, 5, 4)]

##Fix colnames
names = c("Subject", "Encoding_Group", "Warning", "Direction", "RT", "JOL")

colnames(Eprime_block2)[1:6] = names
colnames(block2)[1:6] = names

Eprime_block2$platform = rep("Eprime")
block2$platform = rep("Collector")

final = rbind(block2, Eprime_block2)

##Okay, drop the bad subjects
final = subset(final,
                  final$Subject != "5b5b9fdfe36fe200014d239a")

final = subset(final,
                  final$Subject != "5e17594efd004205da90b6de")

final = subset(final,
                  final$Subject != "5e605068ba28793a7dd55099")

final = subset(final,
                  final$Subject != "w10015978cjb")

final = subset(final,
                  final$Subject != "w843943JC")

final = subset(final,
                  final$Subject != "w993002_MR")

final = subset(final,
                  final$Subject != "w10084909JR")

final = subset(final,
                  final$Subject != "w917542_rm")

final = subset(final,
                  final$Subject != "w979990amf")

final = subset(final,
                  final$Subject != "w10016099CJ")

final = subset(final,
                  final$Subject != "w10021803mm")

length(unique(final$Subject)) #Why do I only have 101 subjects?

##Okay, now I think I can get means
tapply(combined$RT, combined$type, mean) ##Ex 1
tapply(final$RT, final$Encoding_Group, mean) ##Ex 2

tapply(Eprime_block2$RT, Eprime_block2$Encoding_Group, mean)
tapply(block2$RT, block2$Encoding_Group, mean)

combined = combined[ , -7]
combined = combined[ , -3]
combined = combined[ , -1]

final2 = final[ , -3]
final2 = final2[ , -6]

##get columns in right order
combined = combined[ , c(1, 5, 2, 4, 3)]

names2 = colnames(combined)

colnames(final2)[1:5] = names2

final2$ex = rep("two")
combined$ex = rep("one")

combined2 = rbind(combined, final2)

combined2$type[combined2$type == "ITEM SPECIFIC"] = "IS"
combined2$type[combined2$type == "RELATIONAL"] = "RL"

library(ez)
library(reshape)

model1 = ezANOVA(combined2,
                 between = .(ex, type),
                 dv = RT,
                 wid = Subject,
                 detailed = T,
                 type = 3)
model1 #Signficant main effects of experiment and encoding group

Ex1 = cast(combined[ , -c(5:6)], Subject ~ type, mean)
Ex2 = cast(final2[ , -c(5:6)], Subject ~ type, mean)

temp1 = t.test(Ex1$IS , Ex2$`ITEM SPECIFIC`, paired = F, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

temp1 = t.test(Ex1$RL , Ex2$RELATIONAL, paired = F, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

temp1 = t.test(Ex1$READ , Ex2$READ, paired = F, p.adjust.methods = "Bonferroni")
p1 = round(temp1$p.value, 3)
t1 = temp1$statistic
SEM1 = (temp1$conf.int[2] - temp1$conf.int[1]) / 3.92
temp1 #sig

##Do the pbic
ex1.read = Ex1[ , c(1, 3)]
ex1.read$ex = rep("one")

ex2.read = Ex2[ , c(1, 3)]
ex2.read$ex = rep("two")

pbic = rbind(ex1.read, ex2.read)
pbic = na.omit(pbic)

model2 = ezANOVA(pbic,
                 dv = READ,
                 wid = Subject,
                 between = ex,
                 detailed = T,
                 type = 3)
model2

##means and sds for d
mean(Ex1$IS, na.rm = T)
mean(Ex2$`ITEM SPECIFIC`, na.rm = T)

sd(Ex1$IS, na.rm = T)
sd(Ex2$`ITEM SPECIFIC`, na.rm = T)

mean(Ex1$RL, na.rm = T)
mean(Ex2$RELATIONAL, na.rm = T)

sd(Ex1$RL, na.rm = T)
sd(Ex2$RELATIONAL, na.rm = T)
