#!/usr/bin/env Rscript

#####################################LIBRARIES#####################################
library(ggplot2)
library(scales)
library(reshape)
library(gridExtra)
library(grid)
library(dplyr)

#####################################FUNCTIONS#####################################
#wrap labels
label_wrap_mod <- function(value, width = 30) {
  sapply(strwrap(as.character(value), width=width, simplify=FALSE), paste, collapse="\n")}

# functions to generate stacked summary datasets for multiple choice questions
multiSelection <- function(dataset, question)
{
  prefix <- question
  dataset <- dataset[, grep("_TEXT", colnames(dataset), value = TRUE, invert=TRUE)]
  prefix_stack <- melt(dataset, id.vars=c("ResponseId", "count"), measure.vars = grep(toString(prefix), names(dataset), value = TRUE))
  prefix_stack[prefix_stack == ""] <- NA # exclude cases without answer
  prefix_stack <- prefix_stack[complete.cases(prefix_stack),]
  prefix_stack
}

# Arrange graphs together with a single shard legend 
grid_arrange_shared_legend <- function(..., ncol = length(list(...)), nrow = 1, position = c("bottom", "right")) {
  plots <- list(...)
  position <- match.arg(position)
  g <- ggplotGrob(plots[[1]] + theme(legend.position = position))$grobs
  legend <- g[[which(sapply(g, function(x) x$name) == "guide-box")]]
  lheight <- sum(legend$height)
  lwidth <- sum(legend$width)
  gl <- lapply(plots, function(x) x + theme(legend.position="none"))
  gl <- c(gl, ncol = ncol, nrow = nrow)
  
  combined <- switch(position,
                     "bottom" = arrangeGrob(do.call(arrangeGrob, gl),
                                            legend,
                                            ncol = 1,
                                            heights = unit.c(unit(1, "npc") - lheight, lheight)),
                     "right" = arrangeGrob(do.call(arrangeGrob, gl),
                                           legend,
                                           ncol = 2,
                                           widths = unit.c(unit(1, "npc") - lwidth, lwidth)))
  grid.newpage()
  grid.draw(combined)
  
  # return gtable invisibly
  invisible(combined)
}

plotMCQ <- function(data, question){
  data$QID <- toString(question)
  data <- left_join(x=data, y=questions, by=c("Group.1" = "QualtricsID"))
  names(data)[names(data) =="x"] <- "count"
  plot <- ggplot(data, aes(x=label_wrap_mod(subQtext, width=35), y=count))+ coord_flip() +
    geom_bar(stat="Identity", width=0.8) +
    theme(axis.title.y = element_blank(),axis.line=element_line(color="gray90"), text = element_text(size=12), panel.background=element_rect(fill="white"),plot.title = element_text(hjust=0.5)) + 
    #scale_y_continuous(breaks=pretty_breaks(n=10)) + 
    geom_text(aes(label=count),position=position_stack(vjust=0.5), colour="white") +
    xlab("count")
  plot
}

plotSimpleQ <- function(data, question){
  data$QID <- toString(question)
  data <- left_join(x=data, y=options, by=c("QID", "Group.1" = "option"))
  plot <- ggplot(data, aes(x=label_wrap_mod(data$optionText, width=35), fill=data$optionText)) +
    geom_bar(stat="count", width=0.8) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.line=element_line(color="gray90"), text = element_text(size=12), panel.background=element_rect(fill="white"),plot.title = element_text(hjust=0.5)) + 
    scale_y_continuous(breaks=pretty_breaks(n=10)) + 
    geom_text(stat="count", aes(label=..count..),vjust=0, hjust=1.5, color="gray40")+
    geom_text(aes(label = scales::percent((..count..)/sum(..count..)), y= (..count..)/2), stat= "count", vjust =0, color="white")
  plot
}

plotMatrixQ <- function(data, question){
  data <- left_join(x=data, y=questions, by=c("Group.1"= "QualtricsID"))
  data <- left_join(x=data, y=options, by=c("QID","Group.2" ="option"))
  data$QID <- toString(question)
  plot <- ggplot(data, aes(label_wrap_mod(optionText, width=35),x,fill=optionText))+ coord_flip()+
    geom_bar(stat="Identity", position="stack",width=0.9, color="white") +
    theme(axis.line=element_line(color="gray90"),axis.title.x=element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(), legend.position="top", legend.title = element_blank(),panel.background=element_rect(fill="white"),text = element_text(size=12)) + 
    geom_text(aes(label=x),size=3, position=position_stack(vjust=0.5), color="white")+
    ylab("Count")
  plot
}
##################################### DATA #####################################
#AllData <- read.csv("Test_20180413.csv", stringsAsFactors=FALSE)
AllData <- read.csv("ORCIDstudy_QualtricsSurvey_generic-v2_20180420.csv", stringsAsFactors=FALSE)
#extract the data with just Q-code headers
data <- AllData[-c(1:2),c(8,12:118)]
data$count <- 1

# get a list of all questions and options of the survey
QualtricsID <- names(AllData[12:118])
Qualtricstext <- t(AllData[1,12:118])
QID <- NA
Qtext <- NA
subQID <- NA
subQtext <- NA
questions <- data.frame(QualtricsID, Qualtricstext, QID, Qtext, subQID, subQtext, row.names=NULL)
names(questions)[names(questions)=="X1"] <- "Qualtricstext"
questions$QID <- gsub("*_.*", "",questions$QualtricsID)
questions$Qtext <- gsub(" -.*","",questions$Qualtricstext)
questions$subQID <- gsub(".*_", "",questions$QualtricsID)
questions$subQtext <- gsub(".* - ","",questions$Qualtricstext)
#save the list to a csv file
write.table(questions,"surveyQuestions.csv", sep=",",row.names=FALSE)

#Drop-down menus for job roles and schools and single choice questions in qualtrics are not represented in the export
options <- read.csv("Options.csv")
options$subQID <- as.character(options$subQID)

######################## FREE TEXT ANSWERS ################### 
### Generate extract of answers to free text questions
#Q2.3_8_TEXT, Q2.6, Q2.7, Q2.8_11_TEXT, Q2.12_5_TEXT, Q2.14, Q2.15
#Q3.2_5_TEXT, Q3.4 
#Q4.1_31_TEXT
#Q6.3, Q6.4, Q6.6

# Create a file with the free-text answers to one of the questions
txtout <- function(data,colname)
{
  filename <- paste(toString(colname),".csv", sep="")
  colNum <- match(toString(colname), names(data))
  responded <- subset(data, data[colNum] !="")
  txtResponses <- responded[c(match("ResponseId", names(data)),match("Q2.1", names(data)),match("Q6.1", names(data)),match("Q6.2", names(data)),colNum)]
  write.csv(txtResponses,file= filename, row.names=FALSE)
}

# Print comments as R-output
printText <- function(colname)
{
  colNum <- match(toString(colname), names(data))
  text <- data[,colNum]
  text <- subset(text, text !="" & text !="n/a" & text !="N/A" & text !="none")
  text
}

#################### Response by department ################################
# incorporate data about staff/students per department to obtain percentages

deptResponse <- subset(data, Q6.1 !="")
deptResponse$Q6.2 <- gsub("1|2|3|4|7", "Staff", deptResponse$Q6.2)
deptResponse$Q6.2 <- gsub("5|6", "Staff", deptResponse$Q6.2)
#aggregate responses by department:
agg_deptResponse <- aggregate(deptResponse$count, by=list(deptResponse$Q6.1,deptResponse$Q6.2), FUN=sum, na.rm=TRUE)
agg_deptResponse$QID <- "Q6.1"
#label staff and students
agg_deptResponse <- left_join(x=agg_deptResponse, y=options, by=c("Group.1" = "option", "QID"))

deptResponse_plot <- ggplot(agg_deptResponse, aes(x=reorder(optionText, x, sum), y=x, fill=label_wrap_mod(Group.2, width=25))) + 
         coord_flip()+ geom_bar(stat="Identity", width=0.8) +
         scale_fill_manual(values = c("gray65","gray30"))+
         theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
               legend.title = element_blank(), text = element_text(size=14),panel.background=element_rect(fill="white")) +
         geom_text(aes(label= x),position=position_stack(vjust=0.5),size=3, color="white") +
         scale_y_continuous(breaks=pretty_breaks(n=10))+
         ylab("Count")

#plot ORCID iDs
deptORCIDs <- subset(data, Q6.1 !="")
agg_deptORCIDs <- aggregate(deptORCIDs$count, by=list(deptORCIDs$Q6.1, deptORCIDs$Q2.1), FUN=sum, na.rm=TRUE)
agg_deptORCIDs$QID1 <- "Q6.1"
agg_deptORCIDs$QID2 <- "Q2.1"
agg_deptORCIDs <- left_join(x=agg_deptORCIDs, y=options, by=c("Group.1" = "option", "QID1" = "QID"))
agg_deptORCIDs <- left_join(x=agg_deptORCIDs, y=options, by=c("Group.2" = "option", "QID2" = "QID"))

deptORCIDs_plot <- ggplot(agg_deptORCIDs, aes(y=x, x=optionText.x, fill=optionText.y)) + coord_flip() +
  geom_bar(stat="Identity", width=0.8) +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title = element_blank(), text = element_text(size=14),panel.background=element_rect(fill="white")) +
  geom_text(aes(label= x),position=position_stack(vjust=0.5),size=3, color="white")

#export output
names(agg_deptResponse)[names(agg_deptResponse) =="Group.1 | optionText"] <- "School"
names(agg_deptResponse)[names(agg_deptResponse) =="x"] <- "count"
write.table(agg_deptResponse,"results/responses-roles.csv", sep=",")
ggsave("results/deptResponse_plot.png", plot = deptResponse_plot, dpi=300)

names(agg_deptORCIDs)[names(agg_deptORCIDs) =="Group.1 | optionText.x"] <- "School"
names(agg_deptORCIDs)[names(agg_deptORCIDs) =="Group.2 | optionText.y"] <- "ORCID iD"
names(agg_deptORCIDs)[names(agg_deptORCIDs) =="x"] <- "count"
write.table(agg_deptORCIDs,"results/ORCID-dept.csv", sep=",")
ggsave("results/deptORCIDs.png", plot=deptORCIDs_plot, dpi=300)

#################### By role ################################
roleResponse <- subset(data, Q6.2 !="")
#[plot response rate by role
agg_roleResponse <- aggregate(roleResponse$count, by=list(roleResponse$Q6.2), FUN=sum, na.rm=TRUE)
roleResponse_plot <- plotSimpleQ(agg_roleResponse, "Q6.2") + theme(axis.title.y=element_blank()) + coord_flip()

# plot ORCID iDs
roleORCIDs <- subset(data, Q6.2 !="")
agg_roleORCIDs <- aggregate(roleORCIDs$count, by=list(roleORCIDs$Q6.2, roleORCIDs$Q2.1), FUN=sum, na.rm=TRUE)
agg_roleORCIDs$QID1 <- "Q6.2"
agg_roleORCIDs$QID2 <- "Q2.1"
agg_roleORCIDs <- left_join(x=agg_roleORCIDs, y=options, by=c("Group.1" = "option", "QID1"="QID"))
agg_roleORCIDs <- left_join(x=agg_roleORCIDs, y=options, by=c("Group.2" = "option", "QID2"="QID"))

roleORCIDs_plot <- ggplot(agg_roleORCIDs, aes(y=x, x=label_wrap_mod(optionText.x, width=30), fill=optionText.y)) + coord_flip() +
  geom_bar(stat="Identity", width=0.8) +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title = element_blank(), text = element_text(size=14),panel.background=element_rect(fill="white")) +
  geom_text(aes(label= x),position=position_stack(vjust=0.5),size=3, color="white")

#export output
agg_roleResponse$QID <- "Q6.2"
agg_roleResponse <- left_join(x=agg_roleResponse, y=options, by=c("QID", "Group.1" = "option"))
names(agg_roleResponse)[names(agg_roleResponse) =="x"] <- "count"
write.table(agg_roleResponse,"results/responses-roles.csv", sep=",")
ggsave("results/roleResponse.png", plot=roleResponse_plot, dpi=300)

names(agg_roleORCIDs)[names(agg_roleORCIDs) =="Group.1 | optionText.x"] <- "Role"
names(agg_roleORCIDs)[names(agg_roleORCIDs) =="Group.2 | optionText.y"] <- "ORCID iD"
names(agg_roleORCIDs)[names(agg_roleORCIDs) =="x"] <- "count"
write.table(agg_roleORCIDs,"results/ORCID-role.csv", sep=",")
ggsave("results/roleORCIDs.png", plot=roleORCIDs_plot, dpi=300)

#################### Response by age group #########################
#personal information, consider if this is required for analysis.


####################ID count################################
iDs <- subset(data, Q2.1!="")
iDs <- aggregate(iDs$count, by=list(iDs$Q2.1), FUN=sum, na.rm=T)
ORCIDiDs_plot <- plotSimpleQ(iDs, "Q2.1")
ggsave("results/ORCIDiDs.png", plot=ORCIDiDs_plot, dpi=300)
