#write a script to make set up for scoring easier
####set things up####
#load libraries
library(reshape)
library(dplyr)

####NOTES####
#Need to get pword 7 and pword 8 from e-prime file

#read in data
dat = read.csv("raw 10_31 2.csv") #switch this out for the most recent file

summary(dat)
table(dat$ExperimentName)
table(dat$Procedure.Trial.)

#remove practice trials
dat = subset(dat,
             dat$Procedure.Trial. != "Practice1")

#subset the data by experiment version
#start with A
A = subset(dat,
           dat$ExperimentName == "READ_JOL A")
A1 = subset(dat,
            dat$ExperimentName == "IS_JOL A")
A2 = subset(dat,
            dat$ExperimentName == "RL_JOL A")

A = rbind(A, A1, A2)

##drop study proc 8's
A = A[ , -c(16, 17)]

#Now do B
B = subset(dat,
           dat$ExperimentName == "READ_JOL B")
B1 = subset(dat,
            dat$ExperimentName == "IS_JOL B")
B2 = subset(dat,
            dat$ExperimentName == "RL_JOL B")

B = rbind(B, B1, B2)

##drop study proc 10's
B = B[ , -c(20, 21)]

#next c
C = subset(dat,
           dat$ExperimentName == "READ_JOL C")
C1 = subset(dat,
            dat$ExperimentName == "IS_JOL C")
C2 = subset(dat,
            dat$ExperimentName == "RL_JOL C")

C = rbind(C, C1, C2)

##drop study proc 10's
C = C[ , -c(20, 21)]

#Finally, D
D = subset(dat,
           dat$ExperimentName == "READ_JOL D")
D1 = subset(dat,
            dat$ExperimentName == "IS_JOL D")
D2 = subset(dat,
            dat$ExperimentName == "RL_JOL D")

D = rbind(D, D1, D2)

##drop study proc 10's
D = D[ , -c(20, 21)]

####Start with version A!####
##Now remove buffer trials
A = subset(A,
           A$ListNum > 5)
summary(A)

#add study or recall coding
A.recall = subset(A,
                  A$Procedure.Trial. == "recallproc1" | A$Procedure.Trial. == "recallproc2")
A.JOL1 = subset(A,
                A$Procedure.Trial. == "StudyProc3" | A$Procedure.Trial. == "StudyProc5")
A.JOL2 = subset(A,
                A$Procedure.Trial. == "StudyProc7" | A$Procedure.Trial. == "StudyProc8")

##Next, get things sorted
#jols
#block1
A.JOL1 = A.JOL1[order(A.JOL1$Subject, decreasing = FALSE), ]
A.JOL1 = A.JOL1[order(A.JOL1$ListNum, decreasing = FALSE), ]
A.JOL1 = A.JOL1[order(A.JOL1$Subject, decreasing = FALSE), ]
A.JOL1 = A.JOL1[order(A.JOL1$Block, decreasing = FALSE), ]

#block 2
A.JOL2 = A.JOL2[order(A.JOL2$Subject, decreasing = FALSE), ]
A.JOL2 = A.JOL2[order(A.JOL2$ListNum, decreasing = FALSE), ]
A.JOL2 = A.JOL2[order(A.JOL2$Subject, decreasing = FALSE), ]
A.JOL2 = A.JOL2[order(A.JOL2$Block, decreasing = FALSE), ]

#recall
A.recall = A.recall[order(A.recall$Subject, decreasing = FALSE), ]
A.recall = A.recall[order(A.recall$ListNum, decreasing = FALSE), ]
A.recall = A.recall[order(A.recall$Subject, decreasing = FALSE), ]
A.recall = A.recall[order(A.recall$Block, decreasing = FALSE), ]

#put back together
A.JOL = rbind(A.JOL2, A.JOL1)
A.JOL$Type = rep("JOL")

#sort the combined JOL
A.JOL = A.JOL[order(A.JOL$ListNum, decreasing = FALSE), ]
A.JOL = A.JOL[order(A.JOL$Subject, decreasing = FALSE), ]
A.JOL = A.JOL[order(A.JOL$Block, decreasing = FALSE), ]

#combined jols and recall
A.recall$Type = rep("recall")

A = rbind(A.recall, A.JOL)

##Now separate jol and recall trials
A.recall = subset(A,
                  A$Type == "recall")
A.JOL = subset(A,
               A$Type == "JOL")


##split JOL and recall by block
#JOLs

#drop unused columns (recall)
A.JOL = A.JOL[ , -c(13:15)]

#now subset
A1 = subset(A.JOL,
            A.JOL$Block == 1)
A2 = subset(A.JOL,
            A.JOL$Block == 2)

#Recall
#drop jol columns
A.recall = A.recall[ , -c(9:12, 16:19)]

#subset
A3 = subset(A.recall,
            A.recall$Block == 1)
A4 = subset(A.recall,
            A.recall$Block == 2)

#now get all jols and RTs/ recall responses into one column
#time for more subsetting!

##block 1
#study proc 3
proc3 = subset(A1,
               A1$Procedure.Trial. == "StudyProc3")
#study proc 7
proc7 = subset(A1,
               A1$Procedure.Trial. == "StudyProc7")

##block 2
#study proc 5
proc5 = subset(A2,
               A2$Procedure.Trial. == "StudyProc5")

#study proc 8
proc8 = subset(A2,
               A2$Procedure.Trial. == "StudyProc8")

##now start removing unused columns
#proc 3
proc3 = proc3[ , -c(11:16)]
colnames(proc3)[9:10] = c("JOL", "RT")

#proc 5
proc5 = proc5[ , -c(9:10, 13:16)]
colnames(proc5)[9:10] = c("JOL", "RT")

#proc 7
proc7 = proc7[ , -c(9:12, 15:16)]
colnames(proc7)[9:10] = c("JOL", "RT")

#proc 8
proc8 = proc8[ , -c(9:14)]
colnames(proc8)[9:10] = c("JOL", "RT")

##PUT THEM BACK TOGETHER!
JOLs = rbind(proc3, proc7, proc5, proc8)

#sort
JOLs = JOLs[order(JOLs$ListNum, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Subject, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Block, decreasing = FALSE), ]

#recall
recall2 = A4[ - c(7, 9, 10)]
colnames(recall2)[8] = "Response" 

recall1 = A3[ - c(7, 10, 11)]
colnames(recall1)[8] = "Response" 

##put back together
recall = rbind(recall1, recall2)

#now combine JOLs and recall
combinedA = cbind(JOLs, recall)
combinedA = combinedA[ , -c(13:16)]

####MAKE EVERYTHING BELOW MATCH A####

####Now do the same for version B####
##First remove buffer trials
B = subset(B,
           B$ListNum > 5)
summary(B)

#add study or recall coding
B.recall = subset(B,
                  B$Procedure.Trial. == "recallproc1" | B$Procedure.Trial. == "recallproc2")
B.JOL1 = subset(B,
                B$Procedure.Trial. == "StudyProc3" | B$Procedure.Trial. == "StudyProc5")
B.JOL2 = subset(B,
                B$Procedure.Trial. == "StudyProc7" | B$Procedure.Trial. == "StudyProc8")

##Next, get things sorted
#jols
#block1
B.JOL1 = B.JOL1[order(B.JOL1$Subject, decreasing = FALSE), ]
B.JOL1 = B.JOL1[order(B.JOL1$ListNum, decreasing = FALSE), ]
B.JOL1 = B.JOL1[order(B.JOL1$Subject, decreasing = FALSE), ]
B.JOL1 = B.JOL1[order(B.JOL1$Block, decreasing = FALSE), ]

#block 2
B.JOL2 = B.JOL2[order(B.JOL2$Subject, decreasing = FALSE), ]
B.JOL2 = B.JOL2[order(B.JOL2$ListNum, decreasing = FALSE), ]
B.JOL2 = B.JOL2[order(B.JOL2$Subject, decreasing = FALSE), ]
B.JOL2 = B.JOL2[order(B.JOL2$Block, decreasing = FALSE), ]

#recall
B.recall = B.recall[order(B.recall$Subject, decreasing = FALSE), ]
B.recall = B.recall[order(B.recall$ListNum, decreasing = FALSE), ]
B.recall = B.recall[order(B.recall$Subject, decreasing = FALSE), ]
B.recall = B.recall[order(B.recall$Block, decreasing = FALSE), ]

#put back together
B.JOL = rbind(B.JOL2, B.JOL1)
B.JOL$Type = rep("JOL")

#sort the combined JOL
B.JOL = B.JOL[order(B.JOL$ListNum, decreasing = FALSE), ]
B.JOL = B.JOL[order(B.JOL$Subject, decreasing = FALSE), ]
B.JOL = B.JOL[order(B.JOL$Block, decreasing = FALSE), ]

#combined jols and recall
B.recall$Type = rep("recall")

B = rbind(B.recall, B.JOL)

##Now separate jol and recall trials
B.recall = subset(B,
                  B$Type == "recall")
B.JOL = subset(B,
               B$Type == "JOL")

##split JOL and recall by block
#JOLs

#drop unused columns (recall)
B.JOL = B.JOL[ , -c(13:15)]

#now subset
B1 = subset(B.JOL,
            B.JOL$Block == 1)
B2 = subset(B.JOL,
            B.JOL$Block == 2)

#Recall
#drop jol columns
B.recall = B.recall[ , -c(9:12, 16:19)]

#subset
B3 = subset(B.recall,
            B.recall$Block == 1)
B4 = subset(B.recall,
            B.recall$Block == 2)

#now get all jols and RTs/ recall responses into one column
#time for more subsetting!

##block 1
#study proc 3
proc3 = subset(B1,
               B1$Procedure.Trial. == "StudyProc3")
#study proc 7
proc7 = subset(B1,
               B1$Procedure.Trial. == "StudyProc7")

##block 2
#study proc 5
proc5 = subset(B2,
               B2$Procedure.Trial. == "StudyProc5")

#study proc 8
proc8 = subset(B2,
               B2$Procedure.Trial. == "StudyProc8")

##now start removing unused columns
#proc 3
proc3 = proc3[ , -c(11:16)]
colnames(proc3)[9:10] = c("JOL", "RT")

#proc 5
proc5 = proc5[ , -c(9:10, 13:16)]
colnames(proc5)[9:10] = c("JOL", "RT")

#proc 7
proc7 = proc7[ , -c(9:12, 15:16)]
colnames(proc7)[9:10] = c("JOL", "RT")

#proc 8
proc8 = proc8[ , -c(9:14)]
colnames(proc8)[9:10] = c("JOL", "RT")

##PUT THEM BACK TOGETHER!
JOLs = rbind(proc3, proc7, proc5, proc8)

#sort
JOLs = JOLs[order(JOLs$ListNum, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Subject, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Block, decreasing = FALSE), ]

#recall
recall2 = B4[ - c(7, 9, 10)]
colnames(recall2)[8] = "Response" 

recall1 = B3[ - c(7, 10, 11)]
colnames(recall1)[8] = "Response" 

##put back together
recall = rbind(recall1, recall2)

#now combine JOLs and recall
combinedB = cbind(JOLs, recall)
combinedB = combinedB[ , -c(13:16)]

####Now do the same for version C####
##First remove buffer trials
C = subset(C,
           C$ListNum > 5)
summary(C)

#add study or recall coding
C.recall = subset(C,
                  C$Procedure.Trial. == "recallproc1" | C$Procedure.Trial. == "recallproc2")
C.JOL1 = subset(C,
                C$Procedure.Trial. == "StudyProc3" | C$Procedure.Trial. == "StudyProc5")
C.JOL2 = subset(C,
                C$Procedure.Trial. == "StudyProc7" | C$Procedure.Trial. == "StudyProc8")

##Next, get things sorted
#jols
#block1
C.JOL1 = C.JOL1[order(C.JOL1$Subject, decreasing = FALSE), ]
C.JOL1 = C.JOL1[order(C.JOL1$ListNum, decreasing = FALSE), ]
C.JOL1 = C.JOL1[order(C.JOL1$Subject, decreasing = FALSE), ]
C.JOL1 = C.JOL1[order(C.JOL1$Block, decreasing = FALSE), ]

#block 2
C.JOL2 = C.JOL2[order(C.JOL2$Subject, decreasing = FALSE), ]
C.JOL2 = C.JOL2[order(C.JOL2$ListNum, decreasing = FALSE), ]
C.JOL2 = C.JOL2[order(C.JOL2$Subject, decreasing = FALSE), ]
C.JOL2 = C.JOL2[order(C.JOL2$Block, decreasing = FALSE), ]

#recall
C.recall = C.recall[order(C.recall$Subject, decreasing = FALSE), ]
C.recall = C.recall[order(C.recall$ListNum, decreasing = FALSE), ]
C.recall = C.recall[order(C.recall$Subject, decreasing = FALSE), ]
C.recall = C.recall[order(C.recall$Block, decreasing = FALSE), ]

#put back together
C.JOL = rbind(C.JOL2, C.JOL1)
C.JOL$Type = rep("JOL")

#sort the combined JOL
C.JOL = C.JOL[order(C.JOL$ListNum, decreasing = FALSE), ]
C.JOL = C.JOL[order(C.JOL$Subject, decreasing = FALSE), ]
C.JOL = C.JOL[order(C.JOL$Block, decreasing = FALSE), ]

#combined jols and recall
C.recall$Type = rep("recall")

C = rbind(C.recall, C.JOL)

##Now separate jol and recall trials
C.recall = subset(C,
                  C$Type == "recall")
C.JOL = subset(C,
               C$Type == "JOL")

##split JOL and recall by block
#JOLs

#drop unused columns (recall)
C.JOL = C.JOL[ , -c(13:15)]

#now subset
C1 = subset(C.JOL,
            C.JOL$Block == 1)
C2 = subset(C.JOL,
            C.JOL$Block == 2)

#Recall
#drop jol columns
C.recall = C.recall[ , -c(9:12, 16:19)]

#subset
C3 = subset(C.recall,
            C.recall$Block == 1)
C4 = subset(C.recall,
            C.recall$Block == 2)

#now get all jols and RTs/ recall responses into one column
#time for more subsetting!

##block 1
#study proc 3
proc3 = subset(C1,
               C1$Procedure.Trial. == "StudyProc3")
#study proc 7
proc7 = subset(C1,
               C1$Procedure.Trial. == "StudyProc7")

##block 2
#study proc 5
proc5 = subset(C2,
               C2$Procedure.Trial. == "StudyProc5")

#study proc 8
proc8 = subset(C2,
               C2$Procedure.Trial. == "StudyProc8")

##now start removing unused columns
#proc 3
proc3 = proc3[ , -c(11:16)]
colnames(proc3)[9:10] = c("JOL", "RT")

#proc 5
proc5 = proc5[ , -c(9:10, 13:16)]
colnames(proc5)[9:10] = c("JOL", "RT")

#proc 7
proc7 = proc7[ , -c(9:12, 15:16)]
colnames(proc7)[9:10] = c("JOL", "RT")

#proc 8
proc8 = proc8[ , -c(9:14)]
colnames(proc8)[9:10] = c("JOL", "RT")

##PUT THEM BACK TOGETHER!
JOLs = rbind(proc3, proc7, proc5, proc8)

#sort
JOLs = JOLs[order(JOLs$ListNum, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Subject, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Block, decreasing = FALSE), ]

#recall
recall2 = C4[ - c(7, 9, 10)]
colnames(recall2)[8] = "Response" 

recall1 = C3[ - c(7, 10, 11)]
colnames(recall1)[8] = "Response" 

##put back together
recall = rbind(recall1, recall2)

#now combine JOLs and recall
combinedC = cbind(JOLs, recall)
combinedC = combinedC[ , -c(13:16)]

####Now do the same for D####
##First remove buffer trials
D = subset(D,
           D$ListNum > 5)
summary(D)

#add study or recall coding
D.recall = subset(D,
                  D$Procedure.Trial. == "recallproc1" | D$Procedure.Trial. == "recallproc2")
D.JOL1 = subset(D,
                D$Procedure.Trial. == "StudyProc3" | D$Procedure.Trial. == "StudyProc5")
D.JOL2 = subset(D,
                D$Procedure.Trial. == "StudyProc7" | D$Procedure.Trial. == "StudyProc8")

##Next, get things sorted
#jols
#block1
D.JOL1 = D.JOL1[order(D.JOL1$Subject, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$ListNum, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$Subject, decreasing = FALSE), ]
D.JOL1 = D.JOL1[order(D.JOL1$Block, decreasing = FALSE), ]

#block 2
D.JOL2 = D.JOL2[order(D.JOL2$Subject, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$ListNum, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$Subject, decreasing = FALSE), ]
D.JOL2 = D.JOL2[order(D.JOL2$Block, decreasing = FALSE), ]

#recall
D.recall = D.recall[order(D.recall$Subject, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$ListNum, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$Subject, decreasing = FALSE), ]
D.recall = D.recall[order(D.recall$Block, decreasing = FALSE), ]

#put back together
D.JOL = rbind(D.JOL2, D.JOL1)
D.JOL$Type = rep("JOL")

#sort the combined JOL
D.JOL = D.JOL[order(D.JOL$ListNum, decreasing = FALSE), ]
D.JOL = D.JOL[order(D.JOL$Subject, decreasing = FALSE), ]
D.JOL = D.JOL[order(D.JOL$Block, decreasing = FALSE), ]

#combined jols and recall
D.recall$Type = rep("recall")

D = rbind(D.recall, D.JOL)

##Now separate jol and recall trials
D.recall = subset(D,
                  D$Type == "recall")
D.JOL = subset(D,
               D$Type == "JOL")

##split JOL and recall by block
#JOLs

#drop unused columns (recall)
D.JOL = D.JOL[ , -c(13:15)]

#now subset
D1 = subset(D.JOL,
            D.JOL$Block == 1)
D2 = subset(D.JOL,
            D.JOL$Block == 2)

#Recall
#drop jol columns
D.recall = D.recall[ , -c(9:12, 16:19)]

#subset
D3 = subset(D.recall,
            D.recall$Block == 1)
D4 = subset(D.recall,
            D.recall$Block == 2)

#now get all jols and RTs/ recall responses into one column
#time for more subsetting!

##block 1
#study proc 3
proc3 = subset(D1,
               D1$Procedure.Trial. == "StudyProc3")
#study proc 7
proc7 = subset(D1,
               D1$Procedure.Trial. == "StudyProc7")

##block 2
#study proc 5
proc5 = subset(D2,
               D2$Procedure.Trial. == "StudyProc5")

#study proc 8
proc8 = subset(D2,
               D2$Procedure.Trial. == "StudyProc8")

##now start removing unused columns
#proc 3
proc3 = proc3[ , -c(11:16)]
colnames(proc3)[9:10] = c("JOL", "RT")

#proc 5
proc5 = proc5[ , -c(9:10, 13:16)]
colnames(proc5)[9:10] = c("JOL", "RT")

#proc 7
proc7 = proc7[ , -c(9:12, 15:16)]
colnames(proc7)[9:10] = c("JOL", "RT")

#proc 8
proc8 = proc8[ , -c(9:14)]
colnames(proc8)[9:10] = c("JOL", "RT")

##PUT THEM BACK TOGETHER!
JOLs = rbind(proc3, proc7, proc5, proc8)

#sort
JOLs = JOLs[order(JOLs$ListNum, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Subject, decreasing = FALSE), ]
JOLs = JOLs[order(JOLs$Block, decreasing = FALSE), ]

#recall
recall2 = D4[ - c(7, 9, 10)]
colnames(recall2)[8] = "Response" 

recall1 = D3[ - c(7, 10, 11)]
colnames(recall1)[8] = "Response" 

##put back together
recall = rbind(recall1, recall2)

#now combine JOLs and recall
combinedD = cbind(JOLs, recall)
combinedD = combinedD[ , -c(13:16)]

####Now put everything back together
combined = rbind(combinedA, combinedB, combinedC, combinedD)
#combined = rbind(C, D)

#Write to file
#remove '#' from linie 623 to write output to a csv file
#write.csv(combined, file = "processed 10_31.csv", row.names = FALSE) #change date on filename