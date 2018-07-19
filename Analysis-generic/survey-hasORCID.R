#!/usr/bin/env Rscript

############## SURVEY QESTIONS FOR PARTICIPANTS WITH ORCID iDs
#AllData <- read.csv("Test_20180413.csv", stringsAsFactors=FALSE)
#data <- AllData[-c(1:2),c(8,12:119)]
#data$count <- 1
hasORCID <- subset(data, Q2.1==1)

#################### time having an ORCID iD ################################
ownership <- as.data.frame(table(subset(data, Q2.2 !="")$Q2.2))
ownership_plot <- plotSimpleQ(ownership,"Q2.2")

#export output
ownership <- left_join(x=ownership, y=subset(options, QID =="Q2.1", select=c("option", "optionText")), by=c("Var1" = "option"))
colnames(ownership) <- c("ownership-choice", "count", "ownership")

write.table(subset(ownership_exp, select=c("optionText", "count")),"results/ORCID-ownershipTime.csv", sep=",",row.names=FALSE)
ggsave("results/ownership.png", plot=ownership_plot, dpi=300)

####################having heard about ORCID################################
heardFirst <- multiSelection(hasORCID, "Q2.3")
heardFirst<- aggregate(heardFirst$count, by=list(heardFirst$variable, heardFirst$value), FUN=sum, na.rm=TRUE)
heardFirst_plot <- plotMCQ(heardFirst, "Q2.3")

#export output
heardFirst <- left_join(x=heardFirst, y=questions, by=c("Group.1"="QualtricsID"))
names(heardFirst)[names(heardFirst) =="x"] <- "count"
write.table(subset(heardFirst, select=c("Qtext", "subQtext", "count")), "results/hearing-about-ORCID.csv", sep=",",row.names=FALSE )
ggsave("results/heardFirst.png", plot=heardFirst_plot, dpi=300)

####################ORCID mandate################################
mandatedORCID <- as.data.frame(table(subset(data, Q2.4 !="")$Q2.4))
mandatedORCID_plot <- plotSimpleQ(mandatedORCID, "Q2.4") + ggtitle("Did you get an ORCID iD because you had to?")

#export output
madatedORCID <- left_join(x=ownership, y=subset(options, QID =="Q2.4", select=c("option", "optionText")), by=c("Var1"="option"))
write.table(subset(madatedORCID, select=c("optionText", "count")), "results/mandatedORCID.csv", sep=",", row.names=FALSE)
ggsave("results/mandatedORCID.png", plot=mandatedORCID_plot, dpi=300)

#################### Motivation for ORCID signup ################################
ORCIDmotivation <- multiSelection(hasORCID, "Q2.6")
ORCIDmotivation<- aggregate(ORCIDmotivation$count, by=list(ORCIDmotivation$variable, ORCIDmotivation$value), FUN=sum, na.rm=TRUE)
ORCIDmotivation_plot <- plotMatrixQ(ORCIDmotivation, "Q2.6")

#export output
ORCIDmotivation <- left_join(x=ORCIDmotivation, y=subset(questions, select=c("QualtricsID", "subQtext")), by=c("Group.1" = "QualtricsID"))
ORCIDmotivation <- left_join(x=ORCIDmotivation, y=subset(options, QID == "Q2.6", select=c("optionText")), by=c("Group.2" = "option"))
names(ORCIDmotivation)[names(ORCIDmotivation) =="x"] <- "count"
write.table(subset(ORCIDmotivation, select=c("Qtext", "subQtext", "optionText", "count")),"results/ORCIDmotivation.csv", sep=",")
ggsave("results/ORCIDmotivation.png", plot=ORCIDmotivation_plot, dpi=300)

#################### request for clarification ##############################
clarification <- multiSelection(hasORCID, "Q2.9")
clarification <- aggregate(clarification$count, by=list(clarification$variable, clarification$value), FUN=sum, na.rm=TRUE)
clarification_plot <-plotMCQ(clarification, "Q2.9") + theme(axis.title.x = element_blank())

#export output
clarification <- left_join(x=clarification, y=questions, by=c("Group.1"="QualtricsID"))
names(clarification)[names(clarification) =="x"] <- "count"
write.table(subset(clarification, select=c("Qtext", "subQtext", "count")),"results/clarifyingORCID.csv", sep=",",row.names=FALSE)
ggsave("results/clarification.png", plot=clarification_plot, dpi=300)

#################### Adding information to ORCID record ##############################
populate <- multiSelection(hasORCID, "Q2.10")
populate <- aggregate(populate$count, by=list(populate$variable, populate$value), FUN=sum, na.rm=TRUE)
populate_plot <- plotMCQ(populate, "Q2.10") + theme(axis.title.x = element_blank())

#export output
populate <- left_join(x=populate, y=questions, by=c("Group.1"="QualtricsID"))
names(populate)[names(populate) =="x"] <- "count"
write.table(subset(populate, select=c("Qtext", "subQtext", "count")),"results/clarifyingORCID.csv", sep=",",row.names=FALSE)
ggsave("results/populateORCID.csv", plot=populate_plot, dpi=300)

#################### ORCID iDs in integration ##############################
connected <- as.data.frame(table(subset(data, Q2.11 !="")$Q2.11))
connected_plot <- plotSimpleQ(connected,"Q2.11")

connectionRoute <- as.data.frame(table(subset(data, Q2.12 !="")$Q2.12))
connectionRoute_plot <- plotSimpleQ(connectionRoute, "Q2.12") + coord_flip() + theme(axis.title.y = element_blank())

disconnected <- as.data.frame(table(subset(data, Q2.12 !="")$Q2.13))
disconnected_plot <- plotSimpleQ(disconnected,"Q2.13") + coord_flip() + theme(axis.title.y = element_blank())

#export output
connected <- left_join(x=connected, y=subset(options, QID=="Q2.11", select=c("option", "optionText")), by=c("Var1"="option"))
colnames(connected) <- c("choice", "count", "connected?")
write.table(subset(connected, select=c("connected?", "count")),"results/connectedORCID.csv", sep=",",row.names=FALSE)

connectionRoute <-left_join(x=connectionRoute, y=subset(options, QID=="Q2.12", select=c("option", "optionText")), by=c("Var1"="option"))
colnames(connectionRoute) <- c("choice", "count", "local connection")
write.table(subset(connectionRoute, select=c("local connection", "count")),"results/ORCIDconnectionRoute.csv", sep=",",row.names=FALSE)

disconnected <- left_join(x=connectionRoute, y=subset(options, QID=="Q2.13", select=c("option", "optionText")), by=c("Var1"="option"))
colnames(disconnected) <- c("choice", "count", "reason why not")
write.table(subset(disconnected, select=c("reason why not", "count")),"results/disconnectedORCID.csv", sep=",",row.names=FALSE)

ggsave("results/connectedORCID.png", plot = connected_plot, dpi=300)
ggsave("results/ORICDconnectionRoute.png", plot = connectionRoute_plot, dpi=300)
ggsave("results/disconnectedORCID.png", plot = disconnected_plot, dpi=300)

#################### Requests more information about ORCID uses cases#########################
useCaseFAQ <- multiSelection(hasORCID, "Q4.1")
useCaseFAQ <- aggregate(useCaseFAQ$count, by=list(useCaseFAQ$variable), FUN=sum, na.rm=TRUE)
useCaseFAQ_plot <-plotMCQ(useCaseFAQ, "Q4.1") + theme(axis.title.x = element_blank())

#export output
useCaseFAQ <- left_join(x=useCaseFAQ, y=questions, by=c("Group.1"="QualtricsID"))
names(useCaseFAQ)[names(useCaseFAQ) =="x"] <- "count"
write.table(subset(subset(useCaseFAQ, select=c("Qtext", "subQtext", "count")), select=c("Qtext", "subQtext", "count")),"../results/useCaseFAQs.csv", sep=",",row.names=FALSE)
ggsave("results/useCaseFAQ.png", plot = useCaseFAQ_plot, dpi=300)

#################### ORCID iD usage ##############################
ORCIDusage <- multiSelection(hasORCID,"Q2.14")
ORCIDusage <- aggregate(ORCIDusage$count, by=list(ORCIDusage$variable,ORCIDusage$value), FUN=sum, na.rm=TRUE)
ORCIDusage_plot <- plotMatrixQ(ORCIDusage, "Q2.14")

#export output
ORCIDusage <- left_join(x=ORCIDusage, y=questions, by=c("Group.1" = "QualtricsID"))
names(ORCIDusage)[names(ORCIDusage) =="x"] <- "count"
write.table(subset(ORCIDusage, select=c("Qtext", "subQtext", "optionText", "count")),"results/ORCIDusage.csv", sep=",",row.names=FALSE)
ggsave("results/ORCIDusage.png", plot = ORCIDusage_plot, dpi=300)


#################### recommending ORCID ##############################
recommendingORCID <- as.data.frame(table(subset(data, Q5.2 !="")$Q5.2))
recommendingORCID_plot <- plotSimpleQ(recommendingORCID, "Q5.2")

#export output
recommendingORCID <- left_join(x=recommendingORCID, y=subset(options, QID=="Q5.2", select=c("optionText")), by=c("Var1" = "option"))
colnames(recommendingORCID) <- c("choice", "count", "recommendation")
write.table(subset(recommendingORCID, select=c("Qtext", "count")),"results/recommendingORCID.csv", sep=",",row.names=FALSE)
ggsave("results/recommendingORCID.png", plot = recommendingORCID_plot, dpi=300)

#################### benefitting from ORCID ##############################
benefit_iD <- multiSelection(hasORCID,"Q5.1")
benefit_iD <- aggregate(benefit_iD$count, by=list(benefit_iD$variable,benefit_iD$value), FUN=sum, na.rm=TRUE)
benefit_iD_plot <- plotMatrixQ(benefit_iD, "Q5.1")

#export output
benefit_iD_exp <- left_join(x=benefit_iD, y=questions, by=c("Group.1" = "QualtricsID"))
benefit_iD_exp$QID <- "Q5.1"
benefit_iD_exp <- left_join(x=benefit_iD_exp, y=options, by=c("QID"))
names(benefit_iD_exp)[names(benefit_iD_exp) =="x"] <- "count"
write.table(benefit_iD_exp,"../results/whoBenefits-iD.csv", sep=",",row.names=FALSE)
ggsave("../results/benefit.png", plot = benefit_iD_plot, dpi=300)