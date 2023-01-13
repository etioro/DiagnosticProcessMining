## Code to support video post 2 of Diagnostic Process Mining series
## Written by Niyi Ogunbiyi
## Note: the objective is to keep code simple and easy to follow. Hence each line is commented and the desired objective explained
## I recommend running each line individually and inspecting the output (rather than executing the script file in one go)
## Please read the comments carefully as it contains instructions and tips if you encounter problems

#Check if 'pacman' package is installed. If so, load. If not install and load 
if (!require("pacman")) install.packages("pacman"); library(pacman)
#Use pacman to load required packages
p_load(priceR,dplyr,readxl,janitor,tidyr,rio,update = getOption("pac_update"))

## 1. CREATE THE SINGLE OFFER CASES FILE - This is a single row summary with the static variables for all single offer cases 
# Import the BPIC 17 event log from github and write to a dataframe

BPI_Challenge_2017 <- import("https://github.com/etioro/DiagnosticProcessMining/raw/main/BPI%20Challenge%202017.xlsx")

# Convert the event log column names to 'clean' names
BPI_Challenge_2017 <- BPI_Challenge_2017 %>% clean_names()

# View the event log
View(BPI_Challenge_2017)

# Filter only activities pertaining to offers
event_log <- BPI_Challenge_2017 %>% filter(event_origin=="Offer") %>% mutate(offer.id = if_else(is.na(offer_id),event_id,offer_id))

#Count the number of offers per application
offer_per_app <- event_log %>% filter(!is.na(accepted))%>% distinct(case_id,offer.id,accepted) %>% group_by(case_id,offer.id,accepted) %>% count()

# Calculate the average offer per application 
avg_offer_per_app <- offer_per_app %>% group_by(case_id,accepted) %>% count()

#View the average offer per application
View(avg_offer_per_app)

# Create a dataframe showing the number of accepted & decline offers per application 
accepted_spread <- avg_offer_per_app %>% ungroup %>% spread(accepted,n)  

# View the dataframe
View(accepted_spread)

# Create an extra column which indicates whether the application had multiple offer
accepted_spread <- accepted_spread %>% replace(is.na(.),0) %>% mutate(multiple_offer=if_else(`FALSE` + `TRUE`>1,TRUE,FALSE))
# View the updated dataframe
View(accepted_spread)

# Filter out applications had multiple offers. Retain only single offer applications
single_offer <- accepted_spread %>% filter(multiple_offer==FALSE)

#View single offer applications
View(single_offer)

#Create a single row summary of each case with static attributes
single_offer_cases <- BPI_Challenge_2017 %>%  filter(case_id %in% single_offer$case_id) %>% select(case_id,accepted,first_withdrawal_amount,monthly_cost,number_of_terms,case_requested_amount,offered_amount,credit_score) %>% drop_na() 

#View single offer applications
View(single_offer_cases)

#Rename a couple of fields to make them more intuitive
single_offer_cases <- single_offer_cases %>% rename("requested_amount"="case_requested_amount","payback_period"="number_of_terms")

#Change the 'accepted' field to a numeric value (as opposed to boolean)
single_offer_cases$accepted <- as.numeric(single_offer_cases$accepted)

## 2. CALCULATE THE IMPACT OF THE PROBLEM AND BENEFITS CASE

# Calculate the number of accepted vs declined offers
count_accepted <- single_offer_cases %>% group_by(accepted) %>% summarize(count=n())
View(count_accepted)

# View % of accepted offers (Q1). Note: this will be printed to the console (rather than stored in a variable ) 
count_accepted$count[2]/(count_accepted$count[1] + count_accepted$count[2]) * 100

#calculate project interest income. Assume 1% earned on each EUR lent (See Q3)
benefits_case <- single_offer_cases %>% group_by(accepted) %>% summarize(projected_interest_income=.01*sum(offered_amount))
View(benefits_case)

#Format the projected interest income to 1 decimal place 
benefits_case$projected_interest_income <- format_currency(benefits_case$projected_interest_income,"€",digits=1)






