## Code to support video post 3 of Diagnostic Process Mining series
## Written by Niyi Ogunbiyi
## Note: the objective is to keep code simple and easy to follow. Hence each line is commented and the desired objective explained
## I recommend running each line individually and inspecting the output (rather than executing the script file in one go)
## Please read the comments carefully as it contains instructions and tips if you encounter problems

#Check if package pacman is installed. If so, load. If not install and load 
if (!require("pacman")) install.packages("pacman"); library(pacman)
#Use pacman to load required packages
p_load(stats,dplyr,readxl,janitor,tidyverse,broom,dagitty,rio,update = getOption("pac_update"))

## 1. CREATE THE INCOMPLETE SINGLE OFFER CASES FILE - This is a single row summary with the static variables for all single offer cases which are missing required documentation to complete the process 
# Import the BPIC 17 event log from github and write to a dataframe

BPI_Challenge_2017 <- import("https://github.com/etioro/DiagnosticProcessMining/raw/main/BPI%20Challenge%202017.xlsx")

# Convert the event log column names to 'clean' names
BPI_Challenge_2017 <- BPI_Challenge_2017 %>% clean_names()

# View the event log
View(BPI_Challenge_2017)

# Transform the event log to show cases and the number of times each activity has been executed
grouped_cases_wide <- BPI_Challenge_2017 %>% group_by(case_id,activity) %>% count() %>% spread(activity,n) 

# View the transformed event log
View(grouped_cases_wide)

# Filter for incomplete cases and add a field which indicates whether the customer has been chased .i.e. contacted more than twice without sending required document
incomplete_cases <- grouped_cases_wide %>% filter(!is.na(`W_Call incomplete files`)) %>% mutate_at(vars(c("O_Returned","O_Accepted")), ~replace_na(.,0)) %>% mutate(chased=if_else(`W_Call incomplete files`-O_Returned>=2,TRUE,FALSE))
View(incomplete_cases)

# Import the single cases file from github - this was created in the 2nd exercise (Define Problem).
single_offer_cases <- import("https://raw.githubusercontent.com/etioro/DiagnosticProcessMining/main/BPIC17_all_single_offers_0.csv")
View(single_offer_cases)

# Filter for incomplete single offer cases
incomplete_single_offer_cases <- incomplete_cases %>% filter(case_id %in% single_offer_cases$case_id)
View(incomplete_single_offer_cases)

# Drop activity count and enrich the incomplete single offer cases with static case attributes
incomplete_single_offer_cases <- incomplete_single_offer_cases %>% select(case_id,chased) %>% left_join(single_offer_cases,by='case_id')
View(incomplete_single_offer_cases)

# Add a new column indicating whether the client was offered an amount lesss than applied for
incomplete_single_offer_cases <- incomplete_single_offer_cases%>% mutate(offer_amt_less_than_requested=if_else(offered_amount < requested_amount,TRUE,FALSE))
View(incomplete_cases_other)

#Convert the columns 'chased' and 'offer_amt_less_than_requested' from boolean to numeric
incomplete_single_offer_cases$chased <- as.numeric(incomplete_single_offer_cases$chased)
incomplete_single_offer_cases$offer_amt_less_than_requested <- as.numeric(incomplete_single_offer_cases$offer_amt_less_than_requested)


# 2. CREATE THE "CHASE" MODEL .i.e. WHAT IS THE CAUSAL EFFECT OF CHASING CUSTOMERS ON ACCEPTANCE?
# Create a model which indicate the probability of the customer being chased based on credit score (.i.e. propensity score)
model_incomplete <- glm(chased ~ credit_score,data = incomplete_single_offer_cases,family=binomial(link="logit"))

# Create a dataframe augmented with the probability of the the customer being chased (among others)
chase_probabilities <- augment_columns(model_incomplete,incomplete_single_offer_cases,type.predict = "response") %>% rename(propensity = .fitted)

# View a few columns from the new dataframe
chase_probabilities %>% select(case_id,accepted,chased,propensity) %>% View()

# Calculate the inverse probability weights (.i.e. the strangeness score) based on the propensity score
chase_ipw <- chase_probabilities %>% mutate(ipw = (chased/propensity)+ (1-chased)/(1-propensity))

#View the inverse probability weights
View(chase_ipw)

# Create a model which indicates the causal effect using the inverse probability weights 
model_ipw <- lm(accepted ~ chased, data = chase_ipw, weights = ipw)

# View the causal effect and confidence interval. Note that the 'conf.int' parameter allows us to view the confidence interval for the causal effects
tidy(model_ipw,conf.int = TRUE)

#3. CREATE THE "LTR" MODEL - WHAT IS THE CAUSAL EFFECT OF OFFERING CUSTOMERS AN AMOUNT LESS THAN REQUESTED ON ACCEPTANCE?
# Create a model which indicate the probability of the customer being offered less than the amount requested based on credit score (.i.e. propensity score)
model_ltr <- glm(offer_amt_less_than_requested ~ credit_score,data = incomplete_single_offer_cases,family=binomial(link="logit"))

# Create a dataframe augmented with the probability of the the customer being offered less than the amount requested 
ltr_probabilities <- augment_columns(model_ltr,incomplete_single_offer_cases,type.predict = "response") %>% rename(propensity = .fitted)

# Calculate the inverse probability weights (.i.e. the strangeness score) based on the propensity score
ltr_ipw <- ltr_probabilities %>% mutate(ipw = (offer_amt_less_than_requested/propensity)+ (1-offer_amt_less_than_requested)/(1-propensity))

#View the inverse probability weights
View(ltr_ipw)

# Create a model which indicates the causal effect using the inverse probability weights 
model_ltr <- lm(accepted ~ offer_amt_less_than_requested, data = ltr_ipw, weights = ipw)

# View the causal effect and confidence interval.
tidy(model_ltr,conf.int = TRUE)
