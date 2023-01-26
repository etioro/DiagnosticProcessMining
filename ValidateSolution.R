#Check if package pacman is installed. If so, load. If not install and load 
if (!require("pacman")) install.packages("pacman"); library(pacman)

# I have used the R Uplift library to perform uplift modelling. 
# However this library has now been archived so will need to be installed from the CRAN archive
# Install the uplift package
install.packages("https://cran.r-project.org/src/contrib/Archive/uplift/uplift_0.3.5.tar.gz", repos=NULL, type='source')

#Use pacman to load required packages
p_load(tools4uplift,uplift,rio, update = getOption("pac_update"))

# Read the data file for single offer cases with credit_score = 0 and payback period either 120 or 126 months from github
zerocs_single_offer_cases <- import("https://raw.githubusercontent.com/etioro/DiagnosticProcessMining/main/BPIC17_payback_uplift.csv")

# set a random seed for reproducibility
set.seed(1988)

#Split the file into a training and test set 
split.data1 <- SplitUplift(data = zerocs_single_offer_cases, p = 0.7, group = c("accepted", "pb_treat"))

#Extract the training data set
train <- split.data1[[1]]

#Extract the test data set
test <- split.data1[[2]]

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
pred <- predict(fit1, test)

# Assess the performance for of the uplift model
perf <- performance(pred[, 1], pred[, 2], test$accepted, test$pb_treat, direction = 1)

#Create and plot a Qini curve for the uplift model
Q <- qini(perf, plotit = TRUE)

#Print out the Qini score
Q