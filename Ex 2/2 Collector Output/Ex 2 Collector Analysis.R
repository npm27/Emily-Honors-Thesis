####Set up####
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

####Will check for outliers here when we get more data####
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

##make a combined dataset here

##check combined descriptives here

##Save combined dataset as .csv
write.csv(no_warning, file = "no_warning_graphs.csv", row.names = F)
write.csv(warning_block1, file = "warning_b1_graphs.csv", row.names = F)
write.csv(warning_block2, file = "warning_b2_graphs.csv", row.names = F)
