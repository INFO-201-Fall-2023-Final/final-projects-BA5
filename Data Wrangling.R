#Name: BA5

library(dplyr)
library(stringr)
library(ggplot2)

mental_df <- read.csv("mental-disorders.csv") 
sub_df <- read.csv("prevalence-by-mental-and-substance-use-disorder.csv")

# join the two data
mental_depress_df <- left_join(mental_df, sub_df, by = c("Entity", "Year","Code"))

# Create a new categorical variable
# Calculate the mean of the prevalence percentage of mental disorders
mean_mental <- mean(mental_depress_df$Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE)

# Function to categorize prevalence
categorize_mental <- function(percent) {
  if (percent < mean_mental) {
    return('Low')
  } else {
    return('High')
  }
}

# Apply the function to create a new column
mental_depress_df$Prevalence_mental_Category <- sapply(mental_depress_df$Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., categorize_mental)

# Create a new numerical variable
# Percentage of Depressive disorder in mental health problem
per_depress <- (mental_depress_df$Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent./mental_depress_df$Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent.) * 100
mental_depress_df$Percent_depress <- per_depress

# Create a summarization data frame
# summary the mean of mental disorders and depressive disorders prevalence
sum_df <- summarise(group_by(mental_depress_df, Year), avg_mental_disorders = mean(Prevalence...Mental.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE),avg_depress_disorders = mean(Prevalence...Depressive.disorders...Sex..Both...Age..Age.standardized..Percent., na.rm = TRUE))

# export as csv
# write.csv(mental_depress_df, file = "~/Desktop/final/mental_depress.csv", row.names = FALSE)

