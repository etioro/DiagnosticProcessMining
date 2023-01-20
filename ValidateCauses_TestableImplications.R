## Code to support video post 4 of Diagnostic Process Mining series. This checks the testable implications
## Written by Niyi Ogunbiyi
## Note: the objective is to keep code simple and easy to follow. Hence each line is commented and the desired objective explained
## I recommend running each line individually and inspecting the output (rather than executing the script file in one go)
## Please read the comments carefully as it contains instructions and tips if you encounter problems

#Check if package pacman is installed. If so, load. If not install and load 
if (!require("pacman")) install.packages("pacman"); library(pacman)
#Use pacman to load required packages
p_load(dagitty,lavaan,rio,update = getOption("pac_update"))

#create the  DAG object
model <- dagitty('dag {
  bb="0,0,1,1"
  accepted [outcome,pos="0.648,0.505"]
  chased [exposure,pos="0.393,0.512"]
  credit_score [adjusted,pos="0.372,0.053"]
  first_withdrawal_amount [pos="0.309,0.304"]
  monthly_cost [pos="0.155,0.333"]
  offer_amt_less_than_requested [pos="0.571,0.271"]
  payback_period [pos="0.436,0.276"]
  chased -> accepted
  credit_score -> chased
  credit_score -> first_withdrawal_amount
  credit_score -> monthly_cost
  credit_score -> offer_amt_less_than_requested
  credit_score -> payback_period
  first_withdrawal_amount -> accepted
  monthly_cost -> accepted
  offer_amt_less_than_requested -> accepted
  payback_period -> accepted
}')

#plot the DAG
plot(model)

# Import the incomplete single cases file from github - this was created in the 3rd exercise (Find Causes).
incomplete_single_offer_cases <- import("https://raw.githubusercontent.com/etioro/DiagnosticProcessMining/main/BPIC17_incomplete_single_offers.csv")
View(incomplete_single_offer_cases)

# Remove extra variable in the dataframe but not in the DAG model (e.g. case_id)
df_cov <- incomplete_single_offer_cases %>% ungroup() %>% select(-case_id,-requested_amount,-offered_amount)
View(df_cov)

# Create a variable table
vt <- varTable(df_cov)

#Compute the correlation matrix
cov <- lavCor(df_cov)

#View the covariates dataframe
View(cov)

# Run the testable implications to check if data supports the assumptions in the DAG
localTests(x=model,sample.cov = cov, sample.nobs = min(vt$nobs))
