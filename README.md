# Scripts and data files for the analysis of the "ORCID adoption in academic communities" survey performed on qualtrics

## Data 
* 'survey data.csv' which will be named 'surveyData_[DATE].csv'
Each question is represented by a column, each row represents a respondent. MC quetsions are split into columns with MC options ('subquestions') named "QuestionID_subquestionID", e.g. "Q2.3_1"

* Options.csv
Table definining numeric codes for answers to each question
	* col 1: Qualtrics main question ID
	* col 2: option corresponding to the answer text
	* col 3: encoded text a respondent has chosen

* surveyQuestions.csv
File produced from the survey output bu survey-Basics.R. Describes the coding of questions and subquestions (MC answers)
	* col 1: Qualtrics ID
	* col 2: Qualtrics full question text (incl subquestion)
	* col 3: Qualtrics main question ID
	* col 4: subqeistion ID
	* col 5: subqiuestion text

## Scripts
Scripts are to be used together. Code is split thematically for clarity.
Main functions and contain a help attribute which can be accessed by typing 'attr(FOO, "help")' in the R console

* survey-Basics.R
	* defines necessary functions
	* reads in data
	* **creates the "surveyQuestions.csv" file**
	* creates plots about basic survey results(response rate, number of ORCID iDs)

* survey-hasORCID.R
	* R code to create plots form questions posed to participants who indivated that they have an ORCID iD

* survey-noORCID.R
	* R code to create plots from questions posed to participants who indicated that they do not have an ORCID iD.

* Survey-analysis.ipynb
python notebook replicating the survey analysis in python.