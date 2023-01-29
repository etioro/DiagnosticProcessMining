#Check if package pacman is installed. If so, load. If not install and load 
if (!require("pacman")) install.packages("pacman"); library(pacman)
#Use pacman to load required packages
p_load(tidyverse,dplyr,readxl,janitor,lubridate,ranger,caret,Metrics,tools4uplift,uplift,conflicted, rio,update = getOption("pac_update"))

# If you receive an error message: ' Failed to install/load: uplift', please follow the steps below to install
# Install the uplift package
install.packages("https://cran.r-project.org/src/contrib/Archive/uplift/uplift_0.3.5.tar.gz", repos=NULL, type='source')

# load the package
library(uplift)

# Specify that where the select or filter functions conflicts, prefer the dplyr version
conflict_prefer("select", "dplyr")
conflict_prefer("filter", "dplyr")

# Import the BPIC 17 event log from github and write to a dataframe
BPI_Challenge_2017 <- import("https://github.com/etioro/DiagnosticProcessMining/raw/main/BPI%20Challenge%202017.xlsx")

# Convert the event log column names to 'clean' names
BPI_Challenge_2017 <- BPI_Challenge_2017 %>% clean_names()

# View the event log
View(BPI_Challenge_2017)

# Import the data for single offer cases  from github
single_offer_cases <- import("https://raw.githubusercontent.com/etioro/DiagnosticProcessMining/main/BPIC17_all_single_offers.csv")
View(single_offer_cases)

# Create a list of unique single case ids
#unique_case_id <- single_offer_cases %>% select(case_id) %>% distinct() 

# Filter the event log to extract only single offer cases
single_offer_eventlog <- BPI_Challenge_2017 %>% filter(case_id %in% single_offer_cases$case_id)
#View the single offer event log
View(single_offer_eventlog)

# Extract a test case from the event log and use the remainder to build a training set.
# We do this, as for the purpose of this exercise, the online & offline phases are happening simultaneously
# However in reality we would split historically cases 70% or 80% for a training set and the remainder for the test set

# Extracting the testing event log from the BPIC 17 log
testing_df <- single_offer_eventlog %>% filter(case_id == "Application_1930715945") 

# Extracting the training event log from the BPIC 17 log
training_df <- single_offer_eventlog %>% filterfilter(case_id != "Application_1930715945") 

# Create a function to encode the event log based on tracelength, k
createEncodedLog <- function(log,k){
  #select stable attributes
  stable_att <- log %>% select(case_id,accepted,first_withdrawal_amount,monthly_cost,number_of_terms,case_requested_amount,offered_amount,credit_score) %>% mutate(accepted) %>% drop_na() %>% mutate(offer_amt_less_than_requested=if_else(offered_amount<case_requested_amount,TRUE,FALSE)) %>% distinct()
  #create frequency encoding
  freq_encoding <- log %>% filter(tracelength <= k) %>% group_by(case_id,activity) %>% count() %>% spread(activity,n) %>% replace(is.na(.),0) 
  # return the encoded log but joining the stable attribues and the freqiuency encoding
  encodedLog <- stable_att %>% left_join(freq_encoding,by='case_id') 
}

# test to see output of function
training_encoded <- createEncodedLog(training_df,10)
View(training_encoded)

#Create a sequence of tracelengths between 3 & 30 in steps of 3
seq_k <- seq(3,30,by=3)

# BUILD PREDICTIVE MODEL - Offline Phase
# Create a model for for different tracelengths
createModels <- function(k){
  #create an encoded log based on tracelenght
  df <- createEncodedLog(training_df,k) %>% ungroup() %>% select(-case_id)
  # change the outcomes to 'yes' or 'no' rather than TRUE or FALSE
  df <- df %>% mutate(accepted = if_else(accepted==TRUE,"yes","no"))
  #Specify the factor levels for the accepted variable 
  df$accepted <- factor(df$accepted, levels=c("yes","no"))
  # Create a random forest classification model for the tracelength
  rf_model <- caret::train(accepted ~ .,
                            data = df, 
                            method = "ranger", 
                            #metric = "ROC",
                            trControl = trainControl(method = "repeatedcv", number = 5, repeats = 3,classProbs = TRUE),
                            na.action = na.pass,
                            verbose = FALSE,
                            tuneLength=4)
}

#Create a set of predictive models
models <- lapply(seq_k,createModels)
# Would recommend saving the models as it takes a long time to run and if you need to recreate, it's best to re-load (rather than re-run).
# Also change the file path below to a valid local path for you
saveRDS(models,file ="Insert a local file path & name here")

# Use this function to re-load if required: models <- readRDS(filename)

#Create a function which given a tracelength, will return the most appropriate predictive model
returnModel <- function(k){
  i <- ifelse(between(k/3,0,10),round((k/3),digits=0),10)
  model <- models[i]
}

# test to see output of function
model_k <- returnModel(30)

#Create a function which given a partial trace, will return the predicted outcome.This is required in the online phase

returnPrediction <- function(p_trace){
  # find out the tracelength
  k <- nrow(p_trace)
  # Create an encoded log from the partial trace
  df <- p_trace %>% createEncodedLog(k) %>% ungroup() %>% select(-case_id)
  # Return the appropriate predictive model for the tracelength
  model_k <- returnModel(k)
  # Determine the columns in the dataframe used to build the model
  train_df <- training_df %>% createEncodedLog(k) %>% ungroup() %>% select(-case_id)
  missing_cols <- compare_df_cols(df,train_df) %>% filter(is.na(df)) %>% select(column_name) %>% mutate(n=0) %>% spread(column_name,n)
  # Add any columns to the encoded partial trace dataframe so it has the same columns as the dataframe used to build the model
  df <- cbind(df,missing_cols)
  # Predict the likely outcome of the partial trace
  pred <- predict(model_k,newdata=df) %>% as.data.frame() %>% rename(predicted_response=1)
  #Return a dataframe with the predicted augmented with certain attributes for the case
  final_df <- cbind(df,pred) %>% select(accepted,credit_score,predicted_response)
  
}

# test to see output of function - This is equivalen to the Online Phase
test_pred <- returnPrediction("Application_1930715945",6)

#BUILD PRESCRIPTIVE MODEL
# fit the uplift model
# Read the data file for single offer cases with credit_score = 0 and payback period either 120 or 126 months
zerocs_single_offer_cases <- import("https://raw.githubusercontent.com/etioro/DiagnosticProcessMining/main/BPIC17_payback_uplift.csv")

# Remove the partial case from the training set 
train <- zerocs_single_offer_cases %>% filter(case_id !="Application_1930715945")

# Create a dataframe with the partial trace
ptrace<- zerocs_single_offer_cases %>% filter(case_id =="Application_1930715945")

# fit the uplift model
fit1 <- upliftRF(accepted ~ offer_amt_less_than_requested + monthly_cost + offered_amount + first_withdrawal_amount + credit_score + requested_amount + payback_period + trt(pb_treat), 
                 data = train,
                 mtry = 3,
                 ntree = 100, 
                 split_method = "KL",
                 minsplit = 5, 
                 verbose = TRUE)

# print the fitted uplift model
print(fit1)

# predict the uplift for the test data set
pred <- predict(fit1, ptrace) %>% as.data.frame() %>% rename("prob_accept_if_extend"="pr.y1_ct1","prob_accept_if_not_extended" ="pr.y1_ct0")
View(pred)
