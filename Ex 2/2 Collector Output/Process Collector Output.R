####This script will be used to read in all the EX2 Collector data####
setwd("C:/Users/nickm.000/Documents/GitHub/Emily-Honors-Thesis/Ex 2/2 Collector Output/Raw EX 2 Output") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username)) #70 participants

#Now move back to the 2 collector output folder
#This is where I'll store the combined final output for scoring
setwd('..')

####Clean up the data####
##Drop Unused Columns
dat = dat[ , c(1, 5, 8, 11:15, 16, 19, 24, 26, 33)]

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")

#Remove practice trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "PRACTICE")

#Subset out instruction trials -- will need these to check whether participants completed the task correctly
manip.checks = subset(dat,
                      dat$Procedure.Trial.Type == "FreeRecall")

#Remove instructions
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")

#Remove free recall responses
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")

#Remove picture trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "StudyPic")

####Set the data up for scoring####
#Start by subsetting out the recall and JOL data for each dataset
dat.JOL = subset(dat,
                 dat$Procedure.Trial.Type == "JOL")
dat.Recall = subset(dat,
                    dat$Procedure.Trial.Type == "Test")

dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Cue), ]
dat.JOL = dat.JOL[order(dat.JOL$Condition.Number), ]
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Shuffle), ]

dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Cue), ]
dat.Recall = dat.Recall[order(dat.Recall$Condition.Number), ]
dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat.Recall = dat.Recall[ , c(9:12)]

combined = cbind(dat.JOL, dat.Recall)
combined = combined[ , -c(10, 12, 14)]

colnames(combined)[11] = "JOL"
colnames(combined)[14] = "Recall.Response"

####Score the recall data####
library(lrd)

#Set up for scoring
key = tolower(combined$Stimuli.Answer)
Response = tolower(combined$Recall.Response)
ID = combined$Username

#Run the scoring functions
match = percent_match.cr(Response, key = key, id = ID)
score_recall.cr(match, cutoff = 0.75)

scored = read.csv("output.csv")

combined$Recall_Score = scored$scored * 100

####Now split into warning and no warning####
warning = subset(combined,
                 combined$Condition.Notes == "NEW")
No_Warning = subset(combined,
                    combined$Condition.Notes == "OLD")

##Get participant numbers
length(unique(warning$Username)) #28
length(unique(No_Warning$Username)) #42

warning$platform = rep("sona")
No_Warning$platform = rep("sona")

####Now do the prolific data####
setwd("C:/Users/nickm.000/Documents/GitHub/Emily-Honors-Thesis/Ex 2/2 Collector Output/Raw Prolific EX 2") #get the correct directory

#Get the files names
files = list.files(pattern = "*.csv")

#Put them in one dataframe. First apply read.csv, then rbind
dat = do.call(rbind, lapply(files, function(x) read.csv(x, stringsAsFactors = FALSE)))

#get the number of participants
length(unique(dat$Username)) #20 participants

#Now move back to the 2 collector output folder
#This is where I'll store the combined final output for scoring
setwd('..')

####Clean up the data####
##Drop Unused Columns
dat = dat[ , c(1, 5, 8, 11:15, 16, 19, 24, 26, 33)]

#Next, remove buffer trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "Buffer")

#Remove practice trials
dat = subset(dat,
             dat$Stimuli.Stimuli.Notes != "PRACTICE")

#Subset out instruction trials -- will need these to check whether participants completed the task correctly
manip.checks = subset(dat,
                      dat$Procedure.Trial.Type == "FreeRecall")

#Remove instructions
dat = subset(dat,
             dat$Procedure.Trial.Type != "Instruct")

#Remove free recall responses
dat = subset(dat,
             dat$Procedure.Trial.Type != "FreeRecall")

#Remove picture trials
dat = subset(dat,
             dat$Procedure.Trial.Type != "StudyPic")

####Set the data up for scoring####
#Start by subsetting out the recall and JOL data for each dataset
dat.JOL = subset(dat,
                 dat$Procedure.Trial.Type == "JOL")
dat.Recall = subset(dat,
                    dat$Procedure.Trial.Type == "Test")

dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Cue), ]
dat.JOL = dat.JOL[order(dat.JOL$Condition.Number), ]
dat.JOL = dat.JOL[order(dat.JOL$Stimuli.Shuffle), ]

dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Cue), ]
dat.Recall = dat.Recall[order(dat.Recall$Condition.Number), ]
dat.Recall = dat.Recall[order(dat.Recall$Stimuli.Shuffle), ]

#Okay, put it back together now
dat.Recall = dat.Recall[ , c(9:12)]

combined = cbind(dat.JOL, dat.Recall)
combined = combined[ , -c(10, 12, 14)]

colnames(combined)[11] = "JOL"
colnames(combined)[14] = "Recall.Response"

####Score the recall data####
library(lrd)

#Set up for scoring
key = tolower(combined$Stimuli.Answer)
Response = tolower(combined$Recall.Response)
ID = combined$Username

#Run the scoring functions
match = percent_match.cr(Response, key = key, id = ID)
score_recall.cr(match, cutoff = 0.75)

scored = read.csv("output.csv")

combined$Recall_Score = scored$scored * 100

####Now split into warning and no warning####
warning2 = subset(combined,
                 combined$Condition.Notes == "NEW")
No_Warning2 = subset(combined,
                    combined$Condition.Notes == "OLD")

##Get participant numbers
length(unique(warning2$Username))
length(unique(No_Warning2$Username))

warning2$platform = rep("prolific")
No_Warning2$platform = rep("prolific")

####Write the output to .csv files####
warning3 = rbind(warning, warning2)
no_warning3 = rbind(No_Warning, No_Warning2)

#write.csv(warning3, file = "warning.csv", row.names = F)
#write.csv(no_warning3, file = "No Warning.csv", row.names = F)


