####set up####
##read in data
dat = read.csv("processed 10_31.csv")

##load libraries
library(stringr)
library(dplyr)
library(tidyr)
library(wrapr)
library(sqldf)

####Drop Unused columns####
dat = dat[c('ExperimentName', 'Subject', 'cue_target', 'Direction', 'JOL', 'RT', 'cue_target.1', 'Response')]

#colnames(dat)[7] = "cue_target.1"
colnames(dat)[8] = "Recall_Response"

#convert to character strings
dat$cue_target = as.character(dat$cue_target)
dat$cue_target.1 = as.character(dat$cue_target.1)

#split cue_target into JOL cue and target
dat = dat %>% separate(cue_target, 
                c("jol_cue", "jol_target"))

#split cue_target.1 int0 recall cue
dat = dat %>% separate(cue_target.1, 
                       c("recall_cue", "recall_target"))
dat = dat[ , -9]

##get rows in correct order
##first subset by EX Version
table(dat$ExperimentName)

##read
A1 = subset(dat,
           dat$ExperimentName == "READ_JOL A")
B1 = subset(dat,
           dat$ExperimentName == "READ_JOL B")
C1 = subset(dat,
           dat$ExperimentName == "READ_JOL C")
D1 = subset(dat,
           dat$ExperimentName == "READ_JOL D")

##Relational
A2 = subset(dat,
            dat$ExperimentName == "RL_JOL A")
B2 = subset(dat,
            dat$ExperimentName == "RL_JOL B")
C2 = subset(dat,
            dat$ExperimentName == "RL_JOL C")
D2 = subset(dat,
            dat$ExperimentName == "RL_JOL D")

##Item specific
A3 = subset(dat,
            dat$ExperimentName == "IS_JOL A")
B3 = subset(dat,
            dat$ExperimentName == "IS_JOL B")
C3 = subset(dat,
            dat$ExperimentName == "IS_JOL C")
D3 = subset(dat,
            dat$ExperimentName == "IS_JOL D")

#combine all the things
A = rbind(A1, A2, A3)
B = rbind(B1, B2, B3)
C = rbind(C1, C2, C3)
D = rbind(D1, D2, D3)

##sort on each experiment version
#sort version A
xa = match(A$recall_cue, A$jol_cue)
A[c("sorted_JOL_CUE", "sorted_JOL_TARGET")] = (A[xa, c(3:4)])

#sort version B
xb = match(B$recall_cue, B$jol_cue)
B[c("sorted_JOL_CUE", "sorted_JOL_TARGET")] = (B[xb, c(3:4)])

#sort version C
xc = match(C$recall_cue, C$jol_cue)
C[c("sorted_JOL_CUE", "sorted_JOL_TARGET")] = (C[xc, c(3:4)])

#sort version D
xd = match(D$recall_cue, D$jol_cue)
D[c("sorted_JOL_CUE", "sorted_JOL_TARGET")] = (D[xd, c(3:4)])

##put everything back together
combined = rbind(A, B, C, D)
#combined = rbind(C, D)

##drop unused columns
combined = combined[ , -c(3:4)]

####Score recall responses####
##What I want to do is check whether the contents in column 5 match the contents in column 7
#first, convert everything to lowercase
combined$sorted_JOL_TARGET = tolower(combined$sorted_JOL_TARGET)
combined$Recall_Response = tolower(combined$Recall_Response)

##make a column denoting the percentage that strings match each other
temp = sqldf("select *, 
  max(100.0 * (instr(sorted_JOL_TARGET, Recall_Response) > 0) * length(sorted_JOL_TARGET) / length(Recall_Response),
      100.0 * (instr(sorted_JOL_TARGET, Recall_Response) > 0) * length(Recall_Response) / length(sorted_JOL_TARGET))
      percent from combined")

combined$percent_match = temp$percent

#now make a scored column
combined$scored = as.numeric(combined$percent_match >= 100 & combined$percent_match < 200)

#turn NAs from scored column to zeros
combined$scored[is.na(combined$scored)] = 0

#Write output to a .csv
#write.csv(combined, file = "auto score 10_18.csv", row.names = FALSE)

(tapply(combined$scored,
       list(combined$ExperimentName, combined$Direction), mean)) * 100

tapply(combined$JOL,
       list(combined$ExperimentName, combined$Direction), mean, na.rm = T)
