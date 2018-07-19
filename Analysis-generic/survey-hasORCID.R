#!/usr/bin/env Rscript

############## SURVEY QESTIONS FOR PARTICIPANTS WITH ORCID iDs
#AllData <- read.csv("Test_20180413.csv", stringsAsFactors=FALSE)
#data <- AllData[-c(1:2),c(8,12:119)]
#data$count <- 1
hasORCID <- subset(data, Q2.1==1)

#################### time having an ORCID iD ################################
ownership <- subset(data, Q2.2 != "")
ownership <- aggregate(ownership$count, by=list(ownership$Q2.2), FUN=sum, na.rm=TRUE)
ownership_plot <- plotSimpleQ(ownership,"Q2.2")

#export output
ownership$QID <- "Q2.2"
ownership_exp <- left_join(x=ownership, y=options, by=c("QID", "Group.1"="option"))
names(ownership_exp)[names(ownership_exp) =="x"] <- "count"
write.table(subset(ownership_exp, select=c("optionText", "count")),"../results/ORCID-ownershipTime.csv", sep=",",row.names=FALSE)
ggsave("../results/ownership.png", plot=ownership_plot, dpi=300)

####################having heard about ORCID################################
heardFirst <- multiSelection(hasORCID, "Q2.3")
heardFirst<- aggregate(heardFirst$count, by=list(heardFirst$variable, heardFirst$value), FUN=sum, na.rm=TRUE)
heardFirst_plot <- plotMCQ(heardFirst, "Q2.3")

#export output
heardFirst$QID <- "Q2.3"
heardFirst_exp <- left_join(x=heardFirst, y=questions, by=c("Group.1"="QualtricsID"))
names(heardFirst_exp)[names(heardFirst_exp) =="x"] <- "count"
write.table(subset(heardFirst_exp, select=c("Qtext", "subQtext", "count")), "../results/hearing-about-ORCID.csv", sep=",",row.names=FALSE )
ggsave("../results/heardFirst.png", plot=heardFirst_plot, dpi=300)

####################ORCID mandate################################
mandatedORCID <- subset(hasORCID, Q2.4 !="")
mandatedORCID <- aggregate(mandatedORCID$count, by=list(mandatedORCID$Q2.4), FUN=sum, na.rm=TRUE)
mandatedORCID_plot <- plotSimpleQ(mandatedORCID, "Q2.4")

#export output
mandatedORCID$ID <- "Q2.4"
madatedORCID_exp <- left_join(x=ownership, y=options, by=c("QID", "Group.1"="option"))
names(madatedORCID_exp)[names(madatedORCID_exp) =="x"] <- "count"
write.table(subset(madatedORCID_exp, select=c("optionText", "count")), "../results/mandatedORCID.csv", sep=",", row.names=FALSE)
ggsave("../results/mandatedORCID.png", plot=mandatedORCID_plot, dpi=300)

#################### Motivation for ORCID signup ################################
ORCIDmotivation <- multiSelection(hasORCID, "Q2.6")
ORCIDmotivation<- aggregate(ORCIDmotivation$count, by=list(ORCIDmotivation$variable, ORCIDmotivation$value), FUN=sum, na.rm=TRUE)
ORCIDmotivation_plot <- plotMatrixQ(ORCIDmotivation, "Q2.6")
  
#export output
ORCIDmotivation_exp <- left_join(x=ORCIDmotivation, y=questions, by=c("Group.1" = "QualtricsID"))
ORCIDmotivation_exp$QID <- "Q2.6"
ORCIDmotivation_exp <- left_join(x=ORCIDmotivation_exp, y=options, by=c("QID"))
names(ORCIDmotivation_exp)[names(ORCIDmotivation_exp) =="x"] <- "count"
write.table(subset(ORCIDmotivation_exp, select=c("Qtext", "subQtext", "optionText", "count")),"../results/ORCIDmotivation.csv", sep=",")
ggsave("../results/ORCIDmotivation.png", plot=ORCIDmotivation_plot, dpi=300)

#################### request for clarification ##############################
clarification <- multiSelection(hasORCID, "Q2.9")
clarification <- aggregate(clarification$count, by=list(clarification$variable, clarification$value), FUN=sum, na.rm=TRUE)
clarification_plot <-plotMCQ(clarification, "Q2.9") + theme(axis.title.x = element_blank())

#export output
clarification$QID <- "Q2.9"
clarification_exp <- left_join(x=clarification, y=questions, by=c("Group.1"="QualtricsID"))
names(clarification_exp)[names(clarification_exp) =="x"] <- "count"
write.table(subset(clarification_exp, select=c("Qtext", "subQtext", "count")),"../results/clarifyingORCID.csv", sep=",",row.names=FALSE)
ggsave("../results/clarification.png", plot=clarification_plot, dpi=300)

#################### Adding information to ORCID record ##############################
populate <- multiSelection(hasORCID, "Q2.10")
populate <- aggregate(populate$count, by=list(populate$variable, populate$value), FUN=sum, na.rm=TRUE)
populate_plot <-plotMCQ(populate, "Q2.10") + theme(axis.title.x = element_blank())

#export output
populate$QID <- "Q2.10"
populate_exp <- left_join(x=populate, y=questions, by=c("Group.1"="QualtricsID"))
names(populate_exp)[names(populate_exp) =="x"] <- "count"
write.table(subset(populate_exp, select=c("Qtext", "subQtext", "count")),"../results/clarifyingORCID.csv", sep=",",row.names=FALSE)
ggsave("../results/populateORCID.csv", plot=populate_plot, dpi=300)

#################### ORCID iDs in integration ##############################
connected <- subset(hasORCID, Q2.11 !="")
connected <- aggregate(connected$count, by=list(connected$Q2.11), FUN=sum, na.rm=TRUE)
connected_plot <- plotSimpleQ(connected,"Q2.11")

connectionRoute <- subset(hasORCID, Q2.12 !="")
connectionRoute <- aggregate(connectionRoute$count, by=list(connectionRoute$Q2.12), FUN=sum, na.rm=TRUE)
connectionRoute_plot <- plotSimpleQ(connectionRoute, "Q2.12") + coord_flip() + theme(axis.title.y = element_blank())

disconnected <- subset(hasORCID, Q2.13 !="")
disconnected <- aggregate(disconnected$count, by=list(disconnected$Q2.13), FUN=sum, na.rm=TRUE)
disconnected_plot <- plotSimpleQ(disconnected,"Q2.13") + coord_flip() + theme(axis.title.y = element_blank())

#export output
connected$QID <- "Q2.11"
connected_exp <- left_join(x=connected, y=options, by=c("QID", "Group.1"="option"))
names(connected_exp)[names(connected_exp) =="x"] <- "count"
write.table(subset(connected_exp, select=c("Qtext", "count")),"../results/connectedORCID.csv", sep=",",row.names=FALSE)

connectionRoute$QID <- "Q2.12"
connectionRoute_exp <- left_join(x=connectionRoute, y=options, by=c("QID", "Group.1"="option"))
names(connectionRoute_exp)[names(connectionRoute_exp) =="x"] <- "count"
write.table(subset(connectionRoute_exp, select=c("Qtext", "count")),"../results/ORCIDconnectionRoute.csv", sep=",",row.names=FALSE)

disconnected$QID <- "Q2.13"
disconnected_exp <- left_join(x=disconnected, y=options, by=c("QID", "Group.1"="option"))
names(disconnected_exp)[names(disconnected_exp) =="x"] <- "count"
write.table(subset(disconnected_exp, select=c("Qtext", "count")),"../results/disconnectedORCID.csv", sep=",",row.names=FALSE)

ggsave("../results/connectedORCID.png", plot = connected_plot, dpi=300)
ggsave("../results/ORICDconnectionRoute.png", plot = connectionRoute_plot, dpi=300)
ggsave("../results/disconnectedORCID.png", plot = disconnected_plot, dpi=300)

#################### Requests more information about ORCID uses cases#########################
useCaseFAQ <- multiSelection(hasORCID, "Q4.1")
useCaseFAQ <- aggregate(useCaseFAQ$count, by=list(useCaseFAQ$variable), FUN=sum, na.rm=TRUE)
useCaseFAQ_plot <-plotMCQ(useCaseFAQ, "Q4.1") + theme(axis.title.x = element_blank())

#export output
useCaseFAQ$QID <- "Q4.1"
useCaseFAQ_exp <- left_join(x=useCaseFAQ, y=questions, by=c("Group.1"="QualtricsID"))
names(useCaseFAQ_exp)[names(useCaseFAQ_exp) =="x"] <- "count"
write.table(subset(subset(useCaseFAQ_exp, select=c("Qtext", "subQtext", "count")), select=c("Qtext", "subQtext", "count")),"../results/useCaseFAQs.csv", sep=",",row.names=FALSE)
ggsave("../results/useCaseFAQ.png", plot = useCaseFAQ_plot, dpi=300)

#################### ORCID iD usage ##############################
ORCIDusage <- multiSelection(hasORCID,"Q2.14")
ORCIDusage <- aggregate(ORCIDusage$count, by=list(ORCIDusage$variable,ORCIDusage$value), FUN=sum, na.rm=TRUE)
ORCIDusage_plot <- plotMatrixQ(ORCIDusage, "Q2.14")

#export output
ORCIDusage_exp <- left_join(x=ORCIDusage, y=questions, by=c("Group.1" = "QualtricsID"))
names(ORCIDusage_exp)[names(ORCIDusage_exp) =="x"] <- "count"
write.table(subset(ORCIDusage_exp, select=c("Qtext", "subQtext", "optionText", "count")),"../results/ORCIDusage.csv", sep=",",row.names=FALSE)
ggsave("../results/ORCIDusage.png", plot = ORCIDusage_plot, dpi=300)


#################### recommending ORCID ##############################
recommendingORCID <- subset(hasORCID, Q5.2 !="")
recommendingORCID <- aggregate(recommendingORCID$count, by=list(recommendingORCID$Q5.2), FUN=sum, na.rm=T)
recommendingORCID_plot <- plotSimpleQ(recommendingORCID, "Q5.2")

#export output
recommendingORCID$QID <- "Q5.2"
recommendingORCID_exp <- left_join(x=populatingORCID, y=options, by=c("QID", "Group.1" = "option"))
names(recommendingORCID_exp)[names(recommendingORCID_exp) =="x"] <- "count"
write.table(subset(recommendingORCID_exp, select=c("Qtext", "count")),"../results/recommendingORCID.csv", sep=",",row.names=FALSE)
ggsave("../results/recommendingORCID.png", plot = recommendingORCID_plot, dpi=300)

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