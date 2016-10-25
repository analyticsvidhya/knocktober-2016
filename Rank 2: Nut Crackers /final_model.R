# Analytics Vidhya
# Knocktober
# Team : Nut Crackers
# Members : Naveen Kumar Kaveti & Suprit Saha

# Load Required packages
# =========================================================================
package_names <- c("lubridate","tidyr","Metrics","dplyr")

loadPackage <- function(pkg)
{
  if(missing(pkg) || !is.character(pkg))
  {
    stop("Package not correctly entered !!!")
  }
  new.pkg <- pkg[!(pkg %in% installed.packages()[, "Package"])]
  if(length(new.pkg)) 
  {
    install.packages(new.pkg, dependencies = TRUE)
  }
  sapply(pkg, require, character.only = TRUE)
  cat("Packages Loaded !!!")
}
suppressPackageStartupMessages(suppressWarnings(loadPackage(package_names)))


# Reading datasets
# =================================================================================
FHCA <- read.csv("First_Health_Camp_Attended.csv",header = TRUE,na.strings = "")
HCD <- read.csv("Health_Camp_Detail.csv",header = TRUE,na.strings = "")
PP <- read.csv("Patient_Profile.csv",header = TRUE,na.strings = "")
SHCA <- read.csv("Second_Health_Camp_Attended.csv",header = TRUE,na.strings = "")
THCA <- read.csv("Third_Health_Camp_Attended.csv",header = TRUE,na.strings = "")
Train <- read.csv("Train.csv",header = TRUE,na.strings = "")

# Reading test data
# =================================================================================
Test <- read.csv("Test.csv",header = TRUE,na.strings = "")

# Feature extraction from existing datasets
# =================================================================================
Train <- left_join(Train, PP, by = "Patient_ID")
Train <- left_join(Train, HCD, by = "Health_Camp_ID")
Train <- left_join(Train, FHCA, by = c("Patient_ID", "Health_Camp_ID"))
Train <- left_join(Train, SHCA, by = c("Patient_ID", "Health_Camp_ID"))
Train <- left_join(Train, THCA, by = c("Patient_ID", "Health_Camp_ID"))

Test <- left_join(Test, PP, by = "Patient_ID")
Test <- left_join(Test, HCD, by = "Health_Camp_ID")

# Dropping variables 
# =================================================================================
Train$Category3 <- NULL 
Test$Category3 <- NULL
Train$Donation <- NULL

Train$X <- NULL
Train$Last_Stall_Visited_Number <- NULL


# Defining Target variable
# =================================================================================
Train$Y <- ifelse((is.na(Train$Health_Score) & is.na(Train$Health.Score) & Train$Number_of_stall_visited <1),0,1)
Train$Y[is.na(Train$Y)] <- 0

table(Train$Y)


# Feature engineering
# ==================================================================================
Cleansing <- function(df)
{
  if(length(formals(Cleansing)) != nargs())
  {
    stop("Check for missing arguments !!!")
  }
  if(is.character(df))
  {
    df <- eval(parse(text = df))
  }
  if(missing(df) | !is.data.frame(df) )
  {
    stop("Enter valid data frame !!!")
  }
  
  df$Camp_Start_Date <- as.Date(df$Camp_Start_Date, format = "%d-%b-%y")
  df$CSD_Day <- lubridate::day(df$Camp_Start_Date)
  df$CSD_Mon <- lubridate::month(df$Camp_Start_Date)
  df$CSD_Year <- lubridate::year(df$Camp_Start_Date)
  
  df$Camp_End_Date <- as.Date(df$Camp_End_Date, format = "%d-%b-%y")
  df$CED_Day <- lubridate::day(df$Camp_End_Date)
  df$CED_Mon <- lubridate::month(df$Camp_End_Date)
  df$CED_Year <- lubridate::year(df$Camp_End_Date)
  
  df$Camp_Duration <- difftime(df$Camp_End_Date, df$Camp_Start_Date, units = c("days"))
  df$Camp_Duration <- as.numeric(df$Camp_Duration)
  
  df$Registration_Date <- as.Date(df$Registration_Date, format = "%d-%b-%y")
  if(sum(is.na(df$Registration_Date)) > 0){
    df$Registration_Date[is.na(df$Registration_Date)] <- df$Camp_Start_Date[is.na(df$Registration_Date)] + days(round(df$Camp_Duration[is.na(df$Registration_Date)]/2))
  }
  df$Reg_Day <- lubridate::day(df$Registration_Date)
  df$Reg_Mon <- lubridate::month(df$Registration_Date)
  df$Reg_Year <- lubridate::year(df$Registration_Date)
  
  df$Online_Follower <- as.factor(df$Online_Follower)
  df$LinkedIn_Shared <- as.factor(df$LinkedIn_Shared)
  df$Twitter_Shared <- as.factor(df$Twitter_Shared)
  df$Facebook_Shared <- as.factor(df$Facebook_Shared)
  
  levels(df$Income)[levels(df$Income) == "None"] <- 7
  
  df$Education_Score <- as.character(df$Education_Score)
  df$Education_Score[df$Education_Score == "None"] <- 0
  df$Education_Score <- as.numeric(df$Education_Score)
  df$Education_Score[df$Education_Score == 0] <- median(df$Education_Score[df$Education_Score != 0])
  
  df$Age <- as.character(df$Age)
  df$Age[df$Age == "None"] <- 0
  df$Age <- as.numeric(df$Age)
  df$Age[df$Age == 0] <- median(df$Age[df$Age != 0])
  
  df$First_Interaction <- as.Date(df$First_Interaction, format = "%d-%b-%y")
  df$FI_Day <- lubridate::day(df$First_Interaction)
  df$FI_Mon <- lubridate::month(df$Registration_Date)
  # df$FI_Year <- year(df$Registration_Date) # Equals to some other variable (Perfect multi-collinearity)
  
  levels(df$City_Type) <- c(1:length(levels(df$City_Type)))
  
  levels(df$Employer_Category) <- c(1:length(levels(df$Employer_Category)))
  
  levels(df$Category1) <- c(1:length(levels(df$Category1)))
  
  levels(df$Category2) <- c(1:length(levels(df$Category2)))
  
  df$Reg_Year <- factor(df$Reg_Year, levels = c(2003, 2004, 2005, 2006, 2007))
  
  df$CSD_Year <- factor(df$CSD_Year, levels = c(2003, 2004, 2005, 2006, 2007))
  
  return(df)
}
Train <- Cleansing(Train)
Test <- Cleansing(Test)

# Recency & Frequency variables
# ========================================================================
tab1 <- rbind(Train[, c("Patient_ID", "Registration_Date")], Test[, c("Patient_ID", "Registration_Date")])

tab2 <- as.data.frame(tab1 %>% group_by(Patient_ID) %>% summarise(PRRD = max(Registration_Date)))
tab2$Recency <- as.integer(difftime(max(Train$Camp_End_Date, Test$Camp_End_Date), tab2$PRRD, units = "days"))
tab2$PRRD <- NULL


tab3 <- as.data.frame(table(tab1$Patient_ID))
colnames(tab3) <- c("Patient_ID", "Frequency")
tab3$Patient_ID <- as.integer(as.character(tab3$Patient_ID))

Train <- left_join(Train, tab2, by = c("Patient_ID"))
Train <- left_join(Train, tab3, by = c("Patient_ID"))
Test <- left_join(Test, tab2, by = c("Patient_ID"))
Test <- left_join(Test, tab3, by = c("Patient_ID"))

Train$Recency <- as.integer(Train$Recency)
Test$Recency <- as.integer(Test$Recency)

# Time Difference features
# =================================================================================
Train$Lag <- as.integer(difftime(Train$Registration_Date, Train$Camp_Start_Date, units = "days"))
Train$Lag2 <- as.integer(difftime(Train$Registration_Date, Train$Camp_End_Date, units = "days"))
Train$Lag3 <- as.integer(difftime(Train$Registration_Date, Train$First_Interaction, units = "days"))

Test$Lag <- as.integer(difftime(Test$Registration_Date, Test$Camp_Start_Date, units = "days"))
Test$Lag2 <- as.integer(difftime(Test$Registration_Date, Test$Camp_End_Date, units = "days"))
Test$Lag3 <- as.integer(difftime(Test$Registration_Date, Test$First_Interaction, units = "days"))


# Assigning probabilities for each Age group
# ==================================================================================
library(classInt)
temp <- classIntervals(Train$Age, 10, style = "fixed", fixedBreaks = c(30, 40, 50, 60, 70, 80))
Train$Age_Bucket <- as.factor(findCols(temp))

temp2 <- classIntervals(Test$Age, 10, style = "fixed", fixedBreaks = c(30, 40, 50, 60, 70, 80))
Test$Age_Bucket <- as.factor(findCols(temp2))

Age_of <- as.data.frame(Train %>% group_by(Age_Bucket) %>% summarise(Age_of = mean(as.integer(as.character(Y)))))

Train <- left_join(Train, Age_of, by = "Age_Bucket")
Test <- left_join(Test, Age_of, by = "Age_Bucket")

# Feature set
# ===================================================================================
Features <- Train[, c("LinkedIn_Shared", "Income", "Education_Score", "Age", "City_Type", "Employer_Category", "Category1", "Category2", "CSD_Mon", "CED_Year", "Camp_Duration", "Reg_Year", "Recency", "Frequency", "Age_of", "Lag", "Lag2", "Lag3", "Y")]

Train_XY <- data.frame(Features)
Train_XY$Y <- as.factor(Train_XY$Y)


# Using GBM from h2o package
# =============================================================
library(h2o)
h2o.init()

train_h2o <- as.h2o(Train_XY) # Creating h2o dataframe

splits <- h2o.splitFrame(
  train_h2o,           ##  splitting the H2O frame we read above
  c(0.6,0.2),   ##  create splits of 60% and 20%; 
  seed=1234)    ##  setting a seed will ensure reproducible results (not R's seed)

train <- h2o.assign(splits[[1]], "train.hex")   
valid <- h2o.assign(splits[[2]], "valid.hex")
holdout <- h2o.assign(splits[[3]], "test.hex")

gbm <- h2o.gbm(
  training_frame = train,        ## the H2O frame for training
  validation_frame = valid,      ## the H2O frame for validation (not required)
  x=c(1:18),                        ## the predictor columns, by column index
  y=19,                          ## the target index (what we are predicting)
  model_id = "gbm_covType1",     ## name the model in H2O
  seed = 2000000)              ## Set the random seed for reproducability

summary(gbm)                   ## View information about the model.

# holdout predictions
# ===================================================================
pred_test_gbm <- predict(gbm, holdout)
pred_test_gbm <- data.frame(outcome = as.data.frame(holdout[19]), prob = as.data.frame(pred_test_gbm)[, "p1"])
auc(pred_test_gbm[,1], pred_test_gbm[,2])

# Test Data predictions
# ==================================================================
Test_h2o <- as.h2o(Test)
pred_Test_gbm <- predict(gbm, Test_h2o)
result <- as.data.frame(pred_Test_gbm)
result <- data.frame(Patient_ID = Test$Patient_ID, Health_Camp_ID = Test$Health_Camp_ID, Outcome = result$p1)
write.csv(result, file = "Predictions_GBM.csv", row.names = FALSE)

