#!/usr/bin/env Rscript

############## SURVEY QESTIONS FOR PARTICIPANTS WHO DO NOT HAVE AN ORCID iD
#AllData <- read.csv("Test_20180413.csv", stringsAsFactors=FALSE)
#data <- AllData[-c(1:2),c(8,12:119)]
#data$count <- 1
noORCID <- subset(data, Q2.1==2)

#################### having heard about ORICD##############################
everHeardabout <- as.data.frame(table(subset(data, Q3.1 !="")$Q3.1))
everHeardabout_plot <- plotSimpleQ(everHeardabout, "Q3.1")

#export output
everHeardabout <- left_join(x=everHeardabout, y=subset(options, QID=="Q3.1", select=c("optionText")), by=c("Var1" = "option"))
colnames(everHeardabout_exp) <- c("choice", "count", "Heard of ORCID")
write.table(subset(everHeardabout_exp, select=c("Heard of ORCID", "count"),"results/everHeardabout.csv", sep=",", row.names=FALSE))
ggsave("../results/everHeardabout.png", plot = everHeardabout_plot, dpi=300)

#################### Convincing to sign up to ORCID ##############################
convincingSignup <- multiSelection(noORCID,"Q3.3")
convincingSignup <- aggregate(convincingSignup$count, by=list(convincingSignup$variable,convincingSignup$value), FUN=sum, na.rm=TRUE)
convincingSignup_plot <- plotMatrixQ(convincingSignup, "Q3.3")

#export output
convincingSignup <- left_join(x=convincingSignup, y=questions, by=c("Group.1" = "QualtricsID"))
convincingSignup$QID <- "Q5.1"
convincingSignup <- left_join(x=convincingSignup, y=options, by=c("QID"))
names(convincingSignup)[names(convincingSignup) =="x"] <- "count"
write.table(subset(convincingSignup, select=c("Qtext", "subQtext", "optionText", "count"), "results/convincingSignup.csv", sep=",", row.names=FALSE))
ggsave("../results/convincingSignup.png", plot = convincingSignup_plot, dpi=300)

#################### Learning about ORCID ##############################
learningORCID <- multiSelection(noORCID, "Q4.1")
learningORCID <- aggregate(learningORCID$count, by=list(learningORCID$variable), FUN=sum, na.rm=TRUE)
learningORCID_plot <-plotMCQ(learningORCID, "Q4.1") + theme(axis.title.x = element_blank())

#export output
learningORCID <- left_join(x=learningORCID, y=questions, by=c("Group.1"="QualtricsID"))
names(learningORCID)[names(learningORCID) =="x"] <- "count"
write.table(subset(subset(learningORCID, select=c("Qtext", "subQtext", "count")),"../results/learningORCID.csv", sep=",", row.names=FALSE))
ggsave("../results/learningORCID.png", plot = learningORCID_plot, dpi=300)

#################### benefitting from ORCID ##############################
benefit_noiD <- multiSelection(noORCID,"Q5.1")
benefit_noiD <- aggregate(benefit_noiD$count, by=list(benefit_noiD$variable,benefit_noiD$value), FUN=sum, na.rm=TRUE)
benefit_noiD_plot <- plotMatrixQ(benefit_noiD, "Q5.1")

#export output
benefit_noiD <- left_join(x=benefit_noiD, y=questions, by=c("Group.1" = "QualtricsID"))
benefit_noiD$QID <- "Q5.1"
benefit_noiD <- left_join(x=benefit_noiD, y=options, by=c("QID"))
names(benefit_noiD_exp)[names(benefit_noiD) =="x"] <- "count"
write.table(subset(benefit_noiD, select=c("Qtext", "subQtext", "optionText", "count"),"results/whoBenefits-iD.csv", sep=",", row.names=FALSE))
ggsave("../results/benefit.png", plot = benefit_noiD_plot, dpi=300)
