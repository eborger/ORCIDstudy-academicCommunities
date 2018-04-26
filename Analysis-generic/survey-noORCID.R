#!/usr/bin/env Rscript

############## SURVEY QESTIONS FOR PARTICIPANTS WHO DO NOT HAVE AN ORCID iD
#AllData <- read.csv("Test_20180413.csv", stringsAsFactors=FALSE)
#data <- AllData[-c(1:2),c(8,12:119)]
#data$count <- 1
noORCID <- subset(data, Q2.1==2)

#################### having heard about ORICD##############################
everHeardabout <- subset(noORCID, Q3.1 !="")
everHeardabout <- aggregate(everHeardabout$count, by=list(everHeardabout$Q3.1), FUN=sum, na.rm=T)
everHeardabout_plot <- plotSimpleQ(everHeardabout, "Q3.1")

#export output
everHeardabout$QID <- "Q3.1"
everHeardabout_exp <- left_join(x=everHeardabout, y=options, by=c("QID", "Group.1" = "option"))
names(everHeardabout)[names(everHeardabout) =="x"] <- "count"
write.table(everHeardabout,"results/everHeardabout.csv", sep=",")
ggsave("results/everHeardabout.png", plot = everHeardabout_plot, dpi=300)

#################### Convincing to sign up to ORCID ##############################
convincingSignup <- multiSelection(noORCID,"Q3.3")
convincingSignup <- aggregate(convincingSignup$count, by=list(convincingSignup$variable,convincingSignup$value), FUN=sum, na.rm=TRUE)
convincingSignup_plot <- plotMatrixQ(convincingSignup, "Q3.3")

#export output
convincingSignup_exp <- left_join(x=convincingSignup, y=questions, by=c("Group.1" = "QualtricsID"))
convincingSignup_exp$QID <- "Q5.1"
convincingSignup_exp <- left_join(x=convincingSignup_exp, y=options, by=c("QID"))
names(convincingSignup_exp)[names(convincingSignup_exp) =="x"] <- "count"
write.table(convincingSignup_exp,"results/convincingSignup.csv", sep=",")
ggsave("results/convincingSignup.png", plot = convincingSignup_plot, dpi=300)

#################### Learning about ORCID ##############################
learningORCID <- multiSelection(noORCID, "Q3.6")
learningORCID <- aggregate(learningORCID$count, by=list(learningORCID$variable), FUN=sum, na.rm=TRUE)
learningORCID_plot <-plotMCQ(learningORCID, "Q3.6") + theme(axis.title.x = element_blank())

#export output
learningORCID$QID <- "Q3.6"
learningORCID_exp <- left_join(x=learningORCID, y=questions, by=c("Group.1"="QualtricsID"))
names(learningORCID_exp)[names(learningORCID_exp) =="x"] <- "count"
write.table(learningORCID_exp,"results/learningORCID.csv", sep=",")
ggsave("results/learningORCID.png", plot = learningORCID_plot, dpi=300)

#################### benefitting from ORCID ##############################
benefit_noiD <- multiSelection(hasORCID,"Q5.1")
benefit_noiD <- aggregate(benefit_noiD$count, by=list(benefit_noiD$variable,benefit_noiD$value), FUN=sum, na.rm=TRUE)
benefit_noiD_plot <- plotMatrixQ(benefit_noiD, "Q5.1")

#export output
benefit_noiD_exp <- left_join(x=benefit_noiD, y=questions, by=c("Group.1" = "QualtricsID"))
benefit_noiD_exp$QID <- "Q5.1"
benefit_noiD_exp <- left_join(x=benefit_noiD_exp, y=options, by=c("QID"))
names(benefit_noiD_exp)[names(benefit_noiD_exp) =="x"] <- "count"
write.table(benefit_noiD_exp,"results/whoBenefits-iD.csv", sep=",")
ggsave("results/benefit.png", plot = benefit_noiD_plot, dpi=300)