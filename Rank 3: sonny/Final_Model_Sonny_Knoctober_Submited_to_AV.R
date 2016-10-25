library(readr)
library(dplyr)
library(caret)
library(xgboost)
library(randomForest)
library(lubridate)
library(gbm)

train <- read_csv("Train.csv")
H1 <- read_csv("First_Health_Camp_Attended.csv")
H1$"NA" <- NULL 

H2 <- read_csv("Second_Health_Camp_Attended.csv")
H3 <- read_csv("Third_Health_Camp_Attended.csv")

Camp_details <- read_csv("Health_Camp_Detail.csv")
patient <- read_csv("Patient_Profile.csv")
test <- read_csv("Test.csv")

train$isTrain <- T
test$isTrain <- F

df_all <- bind_rows(train, test)

df_all <- left_join(df_all, H1, by = c("Patient_ID", "Health_Camp_ID"))
df_all <- left_join(df_all, H2, by = c("Patient_ID", "Health_Camp_ID"))
df_all <- left_join(df_all, H3, by = c("Patient_ID", "Health_Camp_ID"))
df_all <- left_join(df_all, Camp_details, by = c("Health_Camp_ID"))
df_all <- left_join(df_all, patient, by = c("Patient_ID"))

#Create the desired Target Column
df_all$Outcome <- ifelse(!is.na(df_all$Health_Score) | !is.na(df_all$"Health Score") | (df_all$Number_of_stall_visited > 0), 1 ,0)
df_all$Outcome <- ifelse(is.na(df_all$Outcome), 0, df_all$Outcome )

gen_feature_oneHot <- function(column, data) {
  
  deltaData <- select(data, -get(column))
  data <- select(data, get(column))
  dummies <- dummyVars(~ . -1, data = data)
  df2 <- predict(dummies, newdata = data)
  
  df2 <- cbind(deltaData, df2)
  return(df2)
}

featureEngg <- function(dat) {
  #Drop these 
  for (i in c("Donation", 
              "Health_Score", 
              "Health Score", 
              "Number_of_stall_visited",
              "Last_Stall_Visited_Number")) {
    print(i)
    dat[[i]] <- NULL
  }

  #Convert the Date fields into date format
  for (i in c("Registration_Date", 
              "Camp_Start_Date",
              "Camp_End_Date",
              "First_Interaction")) {
    print(i)
    dat[[i]] <- dmy(dat[[i]])
  }
  
  dat$feat_durationOfCamp <- as.numeric(difftime(dat$Camp_End_Date , dat$Camp_Start_Date, units = "days"))
  dat$feat_didUserRegisterBeforeEventStarts <- ifelse(dat$Registration_Date < dat$Camp_Start_Date, 1, 0)
  dat$feat_daysLeftForEventSinceRegistraion <- as.numeric(difftime(dat$Camp_End_Date, dat$Registration_Date, units = "days"))
  dat$feat_ratio_daysLeftForEventSinceRegistraion_by_durationOfCamp <- dat$feat_daysLeftForEventSinceRegistraion / dat$feat_durationOfCamp
  
  dat$feat_weekdayEvent <- wday(dat$Camp_Start_Date)
  dat$feat_weekOfEvent <- week(dat$Camp_Start_Date)
  dat$feat_monthOfEvent <- month(dat$Camp_Start_Date)
  dat$feat_quarterOfEvent <- quarter(dat$Camp_Start_Date)

  dat$feat_weekdayRegistration <- wday(dat$Registration_Date)
  #dat$feat_weekOfRegistration <- week(dat$Registration_Date)
  dat$feat_monthOfRegistration <- month(dat$Registration_Date)
  #dat$feat_quarterOfRegistration <- quarter(dat$Registration_Date)
  
  dat$feat_weekdayEndDate <- wday(dat$Camp_End_Date)
  dat$feat_weekOfEndDate <- week(dat$Camp_End_Date)
  dat$feat_monthOfEndDate <- month(dat$Camp_End_Date)
  #dat$feat_quarterOfEndDate <- quarter(dat$Camp_End_Date)
  
  
  dat$feat_sum_of_socialMediaShares <- rowSums(dat[, c("Online_Follower",
                                                       "LinkedIn_Shared",
                                                       "Twitter_Shared",
                                                       "Facebook_Shared")], na.rm = T)
  dat$Income <- as.numeric(ifelse(dat$Income == "None", -9999, dat$Income))
  
  dat$Education_Score <- as.numeric(ifelse(dat$Education_Score == "None", -9999, dat$Education_Score))
  
  dat$Age <- as.numeric(ifelse(dat$Age == "None", -9999, dat$Age))
  #Recalculate Age
  dat$Age <- ifelse(dat$Age != -9999, 
                    dat$Age + as.numeric(difftime(dat$Registration_Date, dat$First_Interaction, units = "days"))/365, 
                    dat$Age)
  
  dat$feat_agebin <- NA
  dat$feat_agebin <- ifelse(dat$Age > 39, 1, dat$feat_agebin)
  dat$feat_agebin <- ifelse(dat$Age > 30 & dat$Age <= 39, 2, dat$feat_agebin)
  dat$feat_agebin <- ifelse(dat$Age > 20 & dat$Age <= 30, 3, dat$feat_agebin)
  dat$feat_agebin <- ifelse(dat$Age < 20, 4, dat$feat_agebin)

  dat$feat_daysBetweenFirstInteraction_and_registration <- as.numeric(difftime(dat$Registration_Date , dat$First_Interaction, units = "days"))
  dat$feat_daysBetweenFirstInteraction_and_EventStart <- as.numeric(difftime(dat$Camp_Start_Date , dat$First_Interaction, units = "days"))

  #Feature of Football in  each event
  dat %>%
    group_by(Health_Camp_ID) %>%
    summarise(feat_CountOfEventsFootfall = n()) -> df_temp
  dat <- left_join(dat, df_temp, by = "Health_Camp_ID")  

  #Feature of How many events has the patient registered
  dat %>%
    group_by(Patient_ID) %>%
    summarise(feat_CountOfPatientVisits = n()) -> df_temp
  dat <- left_join(dat, df_temp, by = "Patient_ID")  
  
  #Feature of how many days have elapsed since the last registration made by any patient
  dat %>%
    group_by(Patient_ID) %>%
    arrange(Registration_Date) %>%
    mutate(feat_elapseDays = as.numeric(difftime(lead(Registration_Date), Registration_Date, units = "days"))) %>%
    ungroup() -> dat
  dat$feat_elapseDays <- ifelse(is.na(dat$feat_elapseDays) , -9999, dat$feat_elapseDays)
  
  #OneHot Encode all Categorical variables
  OneHotList <- c("Category1",
                  "Category2",
                  "Category3",
                  "Income",
                  "City_Type",
                  "Var1",
                  "Employer_Category")
  for (i in OneHotList) {
    cat("One Hot Features ", i, "\n")
    dat <- gen_feature_oneHot(i, dat)
  }

  
  #Drop any columns with no variation
  for (i in names(dat)) {
    if (length(unique(dat[[i]])) <= 1) {
      cat("Dropping no variation column - ", i, "\n")
      dat[[i]] <- NULL
    }
  }
  
  return(dat)
}

df_all <- featureEngg(df_all)

#Split back to train and test
train <- df_all[df_all$isTrain == T, ]
test <- df_all[df_all$isTrain == F, ]

TARGET = "Outcome"
DropList = c("Patient_ID", 
             "Health_Camp_ID",
             "isTrain",
             "Registration_Date",
             "Camp_Start_Date",
             "Camp_End_Date",
             "First_Interaction",
             TARGET)
ETA <- 0.01
MAX_DEPTH <- 2
SUB_SAMPLE <- 0.8
MIN_CHILD_WEIGHT <- 1
COL_SAMPLE <- 0.7
GAMMA <- 0
seed <- c(1000, 5000) #Any 2 random seeds
BOOSTER <- "gbtree" #  "gblinear" "gbtree"
nrounds <- 3800

X_train <- train 
Y_train <- train[[TARGET]]

p <- test$Patient_ID
h <- test$Health_Camp_ID

for (i in DropList) {
  cat("Dropping", i, "\n")
  X_train[[i]] <- NULL
  test[[i]] <- NULL
}

EVAL_METRIC <- "auc"
OBJECTIVE <- "binary:logistic"
BOOSTER <- BOOSTER
nthread <- parallel::detectCores()
isMaximize  <- T
EARLY_STOPPING <- 50
print.every.n <- 10
param <- list(
  objective           = OBJECTIVE,
  booster             = BOOSTER,
  eval_metric         = EVAL_METRIC,
  eta                 = ETA,
  max_depth           = MAX_DEPTH,
  subsample           = SUB_SAMPLE,
  min_child_weight    = MIN_CHILD_WEIGHT,
  colsample_bytree    = COL_SAMPLE,
  gamma               = GAMMA,
  nthread             = nthread,
  num_parallel_tree   = 1
)

dtrain <- xgb.DMatrix(  data = data.matrix(X_train),
                        label = data.matrix(Y_train),
                        missing = NA)
watchlist <- list(train = dtrain)

test_target_xgb <- rep(0, nrow(test))
for (s in seed) {
  set.seed(s)
  cat("########## XGB Seed ", s, "\n")
  bst <- xgb.train(             params              = param,
                                data                = dtrain,
                                nrounds             = nrounds,
                                verbose             = 1,
                                print.every.n       = print.every.n,
                                early.stop.round    = EARLY_STOPPING,
                                watchlist           = watchlist,
                                maximize            = isMaximize
  )
  tmp <- predict(bst, data.matrix(test), missing=NA)
  test_target_xgb <- tmp + test_target_xgb
}
test_target_xgb <- test_target_xgb / length(seed)
probs <- as.data.frame(matrix(test_target_xgb, nrow=nrow(test), byrow = TRUE))

#GBM
ntree <- 2200
test_target_gbm <- rep(0, nrow(test))
for (s in seed) {
  set.seed(s)
  cat("########## GBM Seed ", s, "\n")
  bst <- gbm.fit(x = X_train, 
                 y = Y_train,
                 distribution = "bernoulli",
                 n.trees = ntree,
                 interaction.depth = 3,
                 n.minobsinnode = 10,
                 bag.fraction = 0.8,
                 shrinkage = 0.01)
  tmp <- predict(bst, test, n.trees = ntree, type = "response")
  test_target_gbm <- tmp + test_target_gbm 
}
test_target_gbm <- test_target_gbm / length(seed)

final_test <- data.frame(Patient_ID = p,
                         Health_Camp_ID = h,
                         Outcome1 = probs$V1,
                         Outcome2 = test_target_gbm)

#Build a rank average ensemble
final_test <- mutate(final_test, rank1 = dense_rank((Outcome1)))
final_test <- mutate(final_test, rank2 = dense_rank((Outcome2)))
final_test$WeightedScore <- (final_test$rank1 * 0.5 + final_test$rank2 * 0.5)
final_test$WeightedScore <- final_test$WeightedScore / max(final_test$WeightedScore)

final_test$Outcome <- final_test$WeightedScore
write_csv(final_test[, c("Patient_ID", "Health_Camp_ID", "Outcome")], "Sub_final.csv")

