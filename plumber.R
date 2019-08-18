library(plumber)
library(naivebayes)
library(tm)
library(magrittr)
library(caTools)
library(caret)
library(e1071)

#global variable range is number of diseases to cluster below threshold 
range <- 4
threshold <- 13

#global file data
disease_symptom_file <- read.csv("disease_symptom_mapping.csv")
dictionary <- read.csv("disease_symptom_dictionary.csv")
train_file <- read.csv("disease_symptom_mapping_training.csv")
code.value <- "067 111 100 101 032 098 121 032 075 097 114 116 104 105 107"

#* Log some information about the incoming request
#* @filter logger
function(req){
  cat(as.character(Sys.time()), "-", 
      req$REQUEST_METHOD, req$PATH_INFO, "-", 
      req$HTTP_USER_AGENT, "@", req$REMOTE_ADDR, "\n",
      file = "healthpray/Questionnaires/logger.txt",append = TRUE)
  plumber::forward()
}


#' @apiTitle Healthpray

#' Echo the parameter that was sent in
#' @param msg The message to echo back.
#'  @get /
function() {
  "welcome to healthpray"
}

#' Will fetch the disease data by UMCL code require no parmaeters
#' @get /alldisease
 function() {
   list(unique(disease_symptom_file$disease))
 }

#' Get disease/symptom name by providing UMCL code
#' @param UMCLcode UMCL code of disease/symptom
#' @get /getname
function(UMCLcode) {
  list(disease_name = dictionary$name[match(UMCLcode,dictionary$code)])
}

#' Get disease name by providing list of symptoms
#' @param symptoms set of symptoms used for disease prediction
#' @get /predict
function(symptoms) {
  #create symptomscorpus
  file.symptoms <- as.vector(train_file$symptom)
  val <- Corpus(VectorSource(file.symptoms))
  
  #create document term matrix
  dtm <- DocumentTermMatrix(val)
  matrix.symptoms <- as.matrix(dtm)
  colnames(matrix.symptoms) <- toupper(colnames(matrix.symptoms))
  
  #fetch disease name vector and set laplace
  disease <- as.vector(train_file$disease)
  laplace <- 1
  
  #train multinomial naive bayes algorithm 
  mnb <- multinomial_naive_bayes(x = matrix.symptoms, y = disease, laplace = laplace)
  
  #create new data to be predicted
  testdata <- matrix(sample(0,1 * 397,replace = TRUE),nrow = 1,ncol = 397,byrow = TRUE)

  #length( colnames(matrix.symptoms))
  colnames(testdata) <- colnames(matrix.symptoms)
  #symptoms <- "C0018681,C0015967,C0010200,C0010200,C0043144"
  symtoms.list <- strsplit(symptoms,split = ",")
  symtoms.list <- as.vector(symtoms.list[[1]])
  print(length(symtoms.list))
  for(i in 1:length(symtoms.list)) {
    if(is.na(testdata[1,match(symtoms.list[i],colnames(testdata))])) {
      stop( paste("please provide proper UMCL code; error in",
                         symtoms.list[i],sep = " : "))
      
    }
    else {
      testdata[1,match(symtoms.list[i],colnames(testdata))] <- 1
    }
  }
  #set testdata
  data <- testdata

  prob.data <- predict(mnb, newdata = data, type = "prob")

  class.data <- predict(mnb, newdata = data, type = "class") %>% as.vector(.)

  prob.colnames <- colnames(prob.data)

  class.prob <- prob.data[1,match(class.data,prob.colnames)]
  
  class.prob.percent <- round(class.prob,digit=4) * 100
  
  if(class.prob.percent > threshold) {
    list(disease_code = class.data,disease_occurance_prob = class.prob.percent)
  }
  else{
    prob.data.percent <- round(prob.data,digits = 4) * 100
    prob.data.sorted <- apply(prob.data.percent, 1, sort)
    disease.code <- rownames(prob.data.sorted) %>% as.vector(.)
    length.prob.data <- length(prob.data.sorted)
    length.prob.data.lower <- length.prob.data - range
    list(disease_code = rev(disease.code[length.prob.data.lower:length.prob.data]),
         disease_occurance_prob = rev(prob.data.sorted[length.prob.data.lower:length.prob.data,]))
  }
  
}

#' Set range: number of probable disease to return
#' @get /setrange/<rangevalue:int>
function(rangevalue) {
  range <<- rangevalue
  list(paste("range set to",range,sep = " : "))
}

#' Set threshold: probablity threshold
#' @get /setthreshold/<thresholdvalue:double>
function(thresholdvalue) {
  threshold <<- thresholdvalue
  list(paste("threshold set to",threshold,sep = " : "))
}
