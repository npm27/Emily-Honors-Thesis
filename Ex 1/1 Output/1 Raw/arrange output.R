#write a script to make set up for scoring easier
####set things up####
#load libraries
library(reshape)
library(dplyr)

####NOTES####
#Need to get pword 7 and pword 8 from e-prime file

#read in data
dat = read.csv("output 10_24_raw.csv")

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

#Now do B
B = subset(dat,
           dat$ExperimentName == "READ_JOL B")
B1 = subset(dat,
            dat$ExperimentName == "IS_JOL B")
B2 = subset(dat,
            dat$ExperimentName == "RL_JOL B")

B = rbind(B, B1, B2)

#next c
C = subset(dat,
           dat$ExperimentName == "READ_JOL C")
C1 = subset(dat,
            dat$ExperimentName == "IS_JOL C")
C2 = subset(dat,
            dat$ExperimentName == "RL_JOL C")

C = rbind(C, C1, C2)

#Finally, D
D = subset(dat,
           dat$ExperimentName == "READ_JOL D")
D1 = subset(dat,
            dat$ExperimentName == "IS_JOL D")
D2 = subset(dat,
            dat$ExperimentName == "RL_JOL D")

D = rbind(D, D1, D2)

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

##combine columns
A = cbind(A.JOL, A.recall)

#remove NA columns
summary(A)

A = A[ , -c(13:15, 26:29)]

#subset on block
table(A$Block)

A1 = subset(A,
            A$Block == 1)
A2 = subset(A,
            A$Block == 2)

#rename columns
colnames(A1)[9] = "JOL"
colnames(A1)[10] = "JOL_RT"
colnames(A1)[20] = "Recall_prompt"
colnames(A1)[23] = "Response"

#cut unused columns
A1 = A1[ , -c(11:12, 15:19, 21, 24:27)]

#now do the same for A2
#rename columns
colnames(A2)[11] = "JOL"
colnames(A2)[12] = "JOL_RT"
colnames(A2)[20] = "Recall_prompt"
colnames(A2)[25] = "Response"

#cut unused columns
A2 = A2[ , -c(9:10, 15:19, 21, 23:24, 26:27)]

##put the two blocks back together
A = rbind(A1, A2)

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

##combine columns
B = cbind(B.JOL, B.recall)

#remove NA columns
summary(B)

B = B[ , -c(13:15, 26:29)]

#subset on block
table(B$Block)

B1 = subset(B,
            B$Block == 1)
B2 = subset(B,
            A$Block == 2)

#rename columns
colnames(B1)[9] = "JOL"
colnames(B1)[10] = "JOL_RT"
colnames(B1)[20] = "Recall_prompt"
colnames(B1)[23] = "Response"

#cut unused columns
B1 = B1[ , -c(11:12, 15:19, 21, 24:27)]

#now do the same for B2 SOMETHING IS OFF BROUND HERE
#rename columns
colnames(B2)[11] = "JOL"
colnames(B2)[12] = "JOL_RT"
colnames(B2)[20] = "Recall_prompt"
colnames(B2)[25] = "Response"

#cut unused columns
B2 = B2[ , -c(9:10, 15:19, 21, 23:24, 26:27)]

##put the two blocks back together
B = rbind(B1, B2)

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

##combine columns
C = cbind(C.JOL, C.recall)

#remove NA columns
summary(C)

C = C[ , -c(13:15, 26:29)]

#subset on block
table(C$Block)

C1 = subset(C,
            C$Block == 1)
C2 = subset(C,
            C$Block == 2)

#rename columns
colnames(C1)[9] = "JOL"
colnames(C1)[10] = "JOL_RT"
colnames(C1)[20] = "Recall_prompt"
colnames(C1)[23] = "Response"

#cut unused columns
C1 = C1[ , -c(11:12, 15:19, 21, 24:27)]

#now do the same for 
#rename columns
colnames(C2)[11] = "JOL"
colnames(C2)[12] = "JOL_RT"
colnames(C2)[20] = "Recall_prompt"
colnames(C2)[25] = "Response"

#cut unused columns
C2 = C2[ , -c(9:10, 15:19, 21, 23:24, 26:27)]

##put the two blocks back together
C = rbind(C1, C2)

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

##combine columns
D = cbind(D.JOL, D.recall)

#remove NA columns
summary(D)

D = D[ , -c(13:15, 26:29)]

#subset on block
table(D$Block)

D1 = subset(D,
            D$Block == 1)
D2 = subset(D,
            D$Block == 2)

#rename columns
colnames(D1)[9] = "JOL"
colnames(D1)[10] = "JOL_RT"
colnames(D1)[20] = "Recall_prompt"
colnames(D1)[23] = "Response"

#cut unused columns
D1 = D1[ , -c(11:12, 15:19, 21, 24:27)]

#now do the same for 
#rename columns
colnames(D2)[11] = "JOL"
colnames(D2)[12] = "JOL_RT"
colnames(D2)[20] = "Recall_prompt"
colnames(D2)[25] = "Response"

#cut unused columns
D2 = D2[ , -c(9:10, 15:19, 21, 23:24, 26:27)]

##put the two blocks back together
D = rbind(D1, D2)

####Now put everything back together
combined = rbind(A, B, C, D)
#combined = rbind(C, D)

#Write to file
write.csv(combined, file = "processed 10_24.csv", row.names = FALSE)
