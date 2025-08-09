# Step by step guide for missing data imputation

# install packages if required
requiredPackages <- c("readxl, tidyverse,
                      lubridate, ggplot2, data.table")
for (package in requiredPackages) { #Installs packages if not yet installed
  if (!requireNamespace(package, quietly = TRUE))
    install.packages(package)
}

# upload packges
library(readxl)
library(tidyverse) # get for tibble
library(lubridate)
library(ggplot2)
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)

# temporarily turn off warnings
options(warn=0)


# Import and data preprocessing

#set working directory
path_ <- "C:/Users/valer/Desktop/R_project/Project 5/data.xlsx"


# loading data containing test and reference datasheets 
loading_data <- function(path_, sheet_) {
  
  # reading file
  data <- read_excel(path = path_, sheet = sheet_)
  
  # convert all types to numeric to enable further calculations 
  data <- data |> 
    mutate_all(as.numeric)
  sum(is.na(data))
  return (data)
  
}

# dataset for the test medicinal product
data_test <- loading_data(path_, "test_rand")
data_test

# values will not be imputed once all rows contain NAs, and corresponding times
# will be dropped

col_subjects <- ncol(data_test) - 1

# identify missing times that contain subjects with all missing data
missing_times <- 
  data_test[rowSums(is.na(data_test)) == col_subjects, ]$'Time'
df_non_missing <- data_test[data_test$"Time" != missing_times, ]
df_non_missing

# data preprocessing from wide to long format
data_test_long <- melt(setDT(df_non_missing), id.vars = c("Time"), 
             variable.name = "Subject", value.name = "Concentration")
data_test_long



# create time series plots for all subject
p_all_subjects <- ggplot(data_test_long, aes(x=Time, y=Concentration)) + geom_line(aes(colour=Subject), size = 1) + 
  xlab("Time, hours")+
  ylab("Concentration, ng/ml") +
  ggtitle('Individual subject plots') + 
  theme(axis.line = element_line(color = "lightblue"), axis.text = element_text(color = "black"), 
        plot.title = element_text(hjust = .5),
        panel.grid.minor = element_blank(), panel.grid.major = element_blank(),
        panel.border = element_rect(color = "lightblue", fill = NA),
        panel.background = element_rect(fill = NA), text = element_text(size=10))

plot(p_all_subjects)
# 
# The rise, peak and decay vary across subjects which might indicate inter-subject variation. In
# other words, the rate of absorption and elimination for the drug is different for each
# subject


# create time series plots for subjects with missing data
missing_data_subjects <- c("Subject_1", "Subject_5")
df_missing <- df_non_missing |> 
  select('Time', missing_data_subjects)

# show the table with missing subjects
df_missing

# Case 1: Two data points missing from the entire dataset. 
# First missing data point is for Subject 1
# at time 0.330 hours; subject is selected at random using R. 
# Second missing data point is for Subject 5 at
# time 3 and time 23 hours; subject is selected at random using R.


# missingness will be computed by subsetting the dataset by two datasets:
# absorption and elimination phases

# identify the absorption phase (end of phase, time value) for all subjects
data_test
# times for individual subjects


# get the list of subjects to iterate over them later
subjects_list <- colnames(data_test)[2:ncol(data_test)]
subjects_list

# store Tmax values for all subjects
Tmax_values <- NULL

# iterate over subjects
for (subject in subjects_list) {
  
  data_temp <- data_test |> 
    select(1, subject)
  # find Cmax values
  Cmax <- max(data_temp[subject], na.rm = TRUE)
  # find Tmax values corresponding to Cmax
  Tmax <- filter(data_temp, data_temp[subject] == Cmax)$Time
  # store Tmax in a vector
  Tmax_values <- c(Tmax_values, Tmax)
}

# cut off value which will define absorption and elimination phase
Tmax_median <- median(Tmax_values)
# identify the absorption phase dataset
df_abs <- data_test[data_test['Time'] <= Tmax_median, ]
# data preprocessing from wide to long format
df_abs_long <- melt(setDT(df_abs), id.vars = c("Time"), 
                       variable.name = "Subject", value.name = "Concentration")

# identify the elimination phase
df_elim <- data_test[data_test['Time'] >= Tmax_median, ]
# data preprocessing from wide to long format
df_elim_long <- melt(setDT(df_elim), id.vars = c("Time"), 
                       variable.name = "Subject", value.name = "Concentration")

# identify subset for quadratic imputation
# the cutoff will be 2 hours timepoint (base on the observation of all plots)
data_quadr <- data_test[data_test['Time'] <= 2, ]
# data preprocessing from wide to long format
data_quadr_long <- melt(setDT(data_quadr), id.vars = c("Time"), 
                       variable.name = "Subject", value.name = "Concentration")

# Models description (absorption phase)
df_abs
# Population model with time
#fit linear regression model using 'x' as predictor and 'y' as response variable
model_abs <- lm(Concentration ~ Time, data=df_abs_long)
summary(model_abs)
# Multiple R-squared:  0.5654

# Quadratic model
#create a new variable for hours2
data_quadr_long
data_quadr_long$Time2 <- data_quadr_long$Time^2
#fit quadratic regression model
quadraticModel <- lm(Concentration ~ Time + Time2, data=data_quadr_long)
#view model summary
summary(quadraticModel)
# Multiple R-squared:  0.3082
# R-squared is larger for the linear model rather than quadratic


# Population model with time
#fit linear regression model using 'x' as predictor and 'y' as response variable
model_elim <- lm(Concentration ~ Time, data=df_elim_long)
summary(model_elim)
# Multiple R-squared:  0.6198

# Quadratic model
#create a new variable for hours2
data_quadr_long
data_quadr_long$Time2 <- data_quadr_long$Time^2
#fit quadratic regression model
quadraticModel <- lm(Concentration ~ Time + Time2, data=data_quadr_long)
#view model summary
summary(quadraticModel)
# Multiple R-squared:  0.3082
# R-squared is larger for the linear model rather than quadratic


# imputation of missing values

# Subject_1 should be imputed by the absorption linear model

df_non_missing
# use model to predict points value
selected_subject <- c('Subject_1')
df_subject1 <- df_non_missing |> 
  select(1, selected_subject)
df_subject1

predict(model_abs, df_subject1)


# Subject_5 should be imputed by the elimination linear model

df_non_missing
# use model to predict points value
selected_subject <- c('Subject_5')
df_subject5 <- df_non_missing |> 
  select(1, selected_subject)
df_subject5

predict(model_elim, df_subject5)







