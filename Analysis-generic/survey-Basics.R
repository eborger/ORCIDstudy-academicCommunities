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
#takes as attributes the string value to be wrapped, including categories in a plot, and the width
label_wrap_mod <- function(value, width = 30) {
  sapply(strwrap(as.character(value), width=width, simplify=FALSE), paste, collapse="\n")}

# functions to generate summary of ../results for multiple choice questions
# takes as attributes the dataframe and column name (question ID)
multiSelection <- function(dataset, question)
{
  prefix <- question
  dataset <- dataset[, grep("_TEXT", colnames(dataset), value = TRUE, invert=TRUE)]
  prefix_stack <- melt(dataset, id.vars=c("ResponseId", "count"), measure.vars = grep(toString(prefix), names(dataset), value = TRUE))
  prefix_stack[prefix_stack == ""] <- NA # exclude cases without answer
  prefix_stack <- prefix_stack[complete.cases(prefix_stack),]
  prefix_stack
}

# Arrange graphs together with a single shared legend 
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

# plot a multiple choice question as bar chart
# input is the data frame and column name
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

# plot a simple question as column chart
# input is the data frame and column name
plotSimpleQ <- function(data, question){
  data <- left_join(x=data, y=subset(options, QID == toString(question), select=c("option", "optionText")), by=c("Var1" = "option"))
  plot <- ggplot(data, aes(x=label_wrap_mod(data$optionText, width=35), y = Freq, fill=data$optionText)) +
    geom_bar(stat="identity", width=0.8) +
    theme(legend.position="none", axis.title.x = element_blank(), axis.line=element_line(color="gray90"), text = element_text(size=12), panel.background=element_rect(fill="white"),plot.title = element_text(hjust=0.5)) + 
    scale_y_continuous(breaks=pretty_breaks(n=10)) + 
    geom_text(aes(label= Freq),vjust=0, hjust=1.5, color="gray40")+
    geom_text(aes(label = scales::percent((Freq)/sum(Freq)), y= (Freq)/2), vjust =0, color="white")
  plot
}

# plot a matrix question as stacked 100% bar chart
# input is data frame and column name (question ID)
plotMatrixQ <- function(data, question){
  data <- left_join(x=data, y=questions, by=c("Group.1"= "QualtricsID"))
  data$QID <- toString(question)
  data <- left_join(x=data, y=options, by=c("QID", "Group.2" ="option"))
  plot <- ggplot(data, aes(label_wrap_mod(subQtext, width=35),x,fill=optionText))+ coord_flip()+
    geom_bar(stat="Identity", position="stack",width=0.9, color="white") +
    theme(axis.line=element_line(color="gray90"),axis.title.x=element_blank(),axis.ticks.x = element_blank(),axis.title.y = element_blank(), legend.position="top", legend.title = element_blank(),panel.background=element_rect(fill="white"),text = element_text(size=12)) + 
    geom_text(aes(label=x),size=3, position=position_stack(vjust=0.5), color="white")+
    ylab("Count")
  plot
}
##################################### DATA #####################################

AllData <- read.csv("data/ORCIDstudy_survey-generic-v3_num.csv", stringsAsFactors=FALSE)
#extract the response data
data <- AllData[-c(1:2),c(8, 11:length(AllData))]
data$count <- 1

# get a list of all questions and options of the survey based on the Qulatrics data export with question text
#AllData <- read.csv("data/ORCIDstudy_survey-generic-v3_txt.csv", stringsAsFactors=FALSE)
QualtricsID <- names(AllData[11:length(AllData)])
Qualtricstext <- t(AllData[1,11:length(AllData)])
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
write.table(questions,"data/surveyQuestions.csv", sep=",",row.names=FALSE)

#cleaning up...
rm(QualtricsID, Qualtricstext, QID, Qtext, subQID, subQtext)

#Drop-down menus for job roles and schools and single choice questions in qualtrics are not represented in the export
options <- read.csv("data/Options.csv")
options$option <- as.character(options$option)

######################## FREE TEXT ANSWERS ################### 
# Function to create a file with the free-text answers to one of the questions
txtout <- function(data,colname)
{
  filename <- paste("data/", colname,".csv", sep="")
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

### Generate extract of answers to free text questions
freetext <- c("Q2.3_12_TEXT", "Q2.5", "Q2.7", "Q2.8", "Q2.9_9_TEXT", "Q2.13_5_TEXT", "Q2.15", "Q3.2_6_TEXT", "Q3.4", "Q4.1_13_TEXT", "Q5.3", "Q6.3", "Q6.4")
for(i in freetext){
  txtout(data, i)
}

#################### Response by department ################################
# incorporate data about staff/students per department to obtain percentages

dept_data <- as.data.frame(table(subset(data, Q6.1 !="")$Q6.1, subset(data, Q6.1 !="")$Q6.2))

#uncomment and adjust to create categories for staff and students at higher level instead of job groups:
#dept_data$Q6.2 <- gsub("1|2|3|4|7", "Staff", dept_data$Q6.2)
#dept_data$Q6.2 <- gsub("5|6", "Staff", dept_data$Q6.2)

dept_data <- left_join(x=dept_data, y=subset(options, QID =="Q6.1", select=c("option", "optionText")), by=c("Var1" = "option"))
dept_data <- left_join(x=dept_data, y=subset(options, QID =="Q6.2", select=c("option", "optionText")), by=c("Var2" = "option"))

deptResponse_plot <- ggplot(dept_data, aes(x=reorder(optionText.x, Freq, sum), y=Freq, fill=label_wrap_mod(optionText.y, width=25))) + 
         coord_flip()+ geom_bar(stat="Identity", width=0.8) +
       #  scale_fill_manual(values = c("gray65","gray30"))+
         theme(legend.position="bottom", axis.title.y = element_blank(), 
               legend.title = element_blank(), text = element_text(size=12),panel.background=element_rect(fill="white")) +
         geom_text(aes(label= Freq),position=position_stack(vjust=0.5),size=3, color="white") +
         ylab("Count")

#plot ORCID iDs
dept_ORCIDs <- as.data.frame(table(subset(data, Q6.1 !="")$Q6.1, subset(data, Q6.1 !="")$Q2.1))
dept_ORCIDs <- left_join(x=dept_ORCIDs, y=subset(options, QID =="Q6.1", select=c("option", "optionText")), by=c("Var1" = "option"))
dept_ORCIDs <- left_join(x=dept_ORCIDs, y=subset(options, QID=="Q2.1", select=c("option", "optionText")), by=c("Var2" = "option"))

deptORCIDs_plot <- ggplot(dept_ORCIDs, aes(y=Freq, x=optionText.x, fill=optionText.y)) + coord_flip() +
  geom_bar(stat="Identity", width=0.8) +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title = element_blank(), text = element_text(size=14),panel.background=element_rect(fill="white")) +
  geom_text(aes(label= Freq),position=position_stack(vjust=0.5),size=3, color="white") + 
  ggtitle("Do you have an ORCID iD?")

#export output
colnames(dept_data) <- c("School-choice", "option-choice", "count", "question code", "School")
write.table(subset(agg_deptResponse, select=c("School", "count")),"../results/responses-depts.csv", sep=",", row.names = FALSE)
ggsave("results/deptResponse_plot.png", plot = deptResponse_plot, dpi=300)

colnames(dept_ORCIDs) <- c("School-choice", "ORCID-choice", "count", "School", "ORCID iD")
write.table(subset(agg_deptORCIDs, select=c("School", "ORCID iD", "count")),"../results/ORCID-dept.csv", sep=",", row.names = FALSE)
ggsave("results/deptORCIDs.png", plot=deptORCIDs_plot, dpi=300)

#################### By role ################################
job_data <- as.data.frame(table(subset(data, Q6.2 !="")$Q6.2))
job_data_plot <- ggplot(job_data, aes(x = optionText, y=Freq)) + geom_bar(stat="identity")+ 
  theme(axis.title.y=element_blank()) + coord_flip() + ylab("count")


# plot ORCID iDs
job_ORCIDs <- as.data.frame(table(subset(data, Q6.2 !="")$Q6.2, subset(data, Q6.2 !="")$Q2.1))
job_ORCIDs <- left_join(x=dept_ORCIDs, y=subset(options, QID =="Q6.2", select=c("option", "optionText")), by=c("Var1" = "option"))
job_ORCIDs <- left_join(x=dept_ORCIDs, y=subset(options, QID=="Q2.1", select=c("option", "optionText")), by=c("Var2" = "option"))

job_ORCIDs_plot <- ggplot(job_ORCIDs, aes(y=Freq, x=label_wrap_mod(optionText.x, width=30), fill=optionText.y)) + coord_flip() +
  geom_bar(stat="Identity", width=0.8) +
  theme(legend.position="bottom", axis.title.y = element_blank(),axis.title.x = element_blank(), 
        legend.title = element_blank(), text = element_text(size=14),panel.background=element_rect(fill="white")) +
  geom_text(aes(label= Freq),position=position_stack(vjust=0.5),size=3, color="white")

#export output
job_data <- left_join(x=job_data, y=subset(options, QID =="Q6.2", select=c("option", "optionText")), by=c("Var1" = "option"))
colnames(job_data) <- c("Job-choice", "count", "Job role")
write.table(subset(job_data, select=c("Job role", "count")),"../results/responses-roles.csv", sep=",", row.names = FALSE)
ggsave("results/roleResponse.png", plot=roleResponse_plot, dpi=300)

colnames(job_ORCIDs) <- c("Job-choice", "ORCID-choice", "count", "Job role", "ORCID iD", "ORCID iD")
write.table(subset(job_ORCIDs, select=c("Job role", "ORCID iD", "count")),"../results/ORCID-role.csv", sep=",", row.names = FALSE)
ggsave("results/roleORCIDs.png", plot=roleORCIDs_plot, dpi=300)

####################ID count################################

iDs <- as.data.frame(table(subset(data, Q2.1 !="")$Q2.1))
iDs_plot <- plotSimpleQ(iDs, "Q2.1")
iDs <- left_join(x=iDs, y=subset(options, QID =="Q2.1", select=c("option", "optionText")), by=c("Var1" = "option"))
ggsave("results/ORCIDiDs.png", plot=ORCIDiDs_plot, dpi=300)
