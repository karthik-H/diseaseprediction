# DiseasePrediction
disease prediction based on symptoms
# Features
⦁	  Uses multinomial naive bayes as classification model.
⦁	  Provide Restful api to intract with application.

# Data set 
Primary data - http://people.dbmi.columbia.edu/~friedma/Projects/DiseaseSymptomKB/index.html
all the disease and symptoms names are encoded in UMCL format for more refrence about UMCL :(https://www.nlm.nih.gov/research/umls/sourcereleasedocs/current/ICD10CM/index.html)

⦁	disease_symptom_mapping.csv file provide info about symptoms associated with disease

⦁	disease_symptom_mapping_training.csv file used to train model and has same content as disease_symptom_mapping.csv 

⦁	disease_symptom_dictionary.csv file has mapping for UMCL code and name associated with it

plumber.R file contains code for application working

# Variables
⦁	Threshold - minimum threshold probablity to predict one perticular disease.

⦁	num_cluster - maximum number of cluster's that can be created.

# Usage
⦁	clone the repository 

⦁	open plumber.R in R studio 

⦁	install required packages

⦁	click on Run api in Rstudio or use command 
Plumber::plumb(file='filepath')$run() this should start API

# Packages Required
run following command in R console

⦁	install.packages("plumber") #to create Rest api

⦁	install.packages("naivebayes") # to create model

⦁	install.packages("tm") # create document term matrix

⦁	install.packages("magrittr") # R utility

# Result 

![result 1](https://github.com/karthik-H/diseaseprediction/blob/master/screenshot/Screenshot%20(695).png)
![result 2](https://github.com/karthik-H/diseaseprediction/blob/master/screenshot/Screenshot%20(696).png)
![result 3](https://github.com/karthik-H/diseaseprediction/blob/master/screenshot/Screenshot%20(697).png)
![result 4](https://github.com/karthik-H/diseaseprediction/blob/master/screenshot/Screenshot%20(698).png)
