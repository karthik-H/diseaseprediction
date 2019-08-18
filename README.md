# DiseasePrediction
disease prediction using symptoms
# Features
⦁	  Uses multinomial naive bayes as classification model.
⦁	  Provide Restful api to intract with application.

# Data set 
all the disease and symptoms names are encoded in UMCL format for more refrence about UMCL :(https://www.nlm.nih.gov/research/umls/sourcereleasedocs/current/ICD10CM/index.html)

⦁	disease_symptom_mapping.csv file provide info about symptoms associated with disease

⦁	disease_symptom_mapping_training.csv file used to train model and has same content as disease_symptom_mapping.csv 

⦁	disease_symptom_dictionary.csv file has mapping for UMCL code and name associated with it

plumber.R file contains code for application working

Variables
⦁	Threshold - minimum threshold to predict one perticular disease e.g if threshold is specified as 20%. If probality of the predicted disease is more than 20% then model will predict that disease as the output else it will cluster set of diseases and give it as output.
⦁	Range - To specify how many disease to cluster if probality is below threshold.
Usage
⦁	clone the repository 
⦁	open plumber.R in R studio 
⦁	install required packages
⦁	click on Run api in Rstudio or use command 
Plumber::plumb(file='filepath')$run() this should start API

Packages Required
run following command in R console
⦁	install.packages("plumber") #to create Rest api
⦁	install.packages("naivebayes") # to create model
⦁	install.packages("tm") # create document term matrix
⦁	install.packages("magrittr") # R utility
