install.packages("dplyr")
install.packages("openxlsx")
install.packages("ROSE")
library(openxlsx)            
library(dplyr)
library(ROSE)


install.packages("openxlsx")
library(openxlsx) 
mydata <- read.xlsx("E:/Aiub 12th Semester/Data Science/Sample Read Dataset/Main Project2/DataScience_Mid_Project/Midterm_Dataset.xlsx")
mydata
View(mydata)



str(mydata)
mydata



summary(mydata)
mydata



colSums(is.na(mydata))
sum(is.na(mydata))



lapply(mydata, function(col) which(is.na(col)))


missing_counts <- colSums(is.na(mydata))
barplot(missing_counts,
        main = "Missing Values in each attribute",
        xlab = "Attributes",
        ylab = "Number of missing values",
        col = "skyblue",
        las = 2)  



install.packages("ggplot2")
library(ggplot2)


missingData <- data.frame(
  Missing = c("Available", "Missing"),
  Gender = c(sum(!is.na(mydata$Gender)), sum(is.na(mydata$Gender)))
)


ggplot(missingData, aes(x = Missing, y = Gender, fill = Missing)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Available" = "green", "Missing" = "red")) + 
  labs(
    title = "Missing Instances of Gender",
    x = "Data Availability",
    y = "Gender"
  ) +
  theme_minimal()





missingData_Age <- data.frame(
  Status = c("Available", "Missing"),
  Count = c(sum(!is.na(mydata$Age)), sum(is.na(mydata$Age)))
)

ggplot(missingData_Age, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Available" = "green", "Missing" = "red")) +
  labs(
    title = "Missing Data in Age",
    x = "Data Availability",
    y = "Count"
  ) +
  theme_minimal()






missingData_AcademicPressure <- data.frame(
  Status = c("Available", "Missing"),
  Count = c(sum(!is.na(mydata$Academic_Pressure)), sum(is.na(mydata$Academic_Pressure)))
)

ggplot(missingData_AcademicPressure, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Available" = "green", "Missing" = "red")) +
  labs(
    title = "Missing Data in Academic Pressure",
    x = "Data Availability",
    y = "Count"
  ) +
  theme_minimal()






missingData_StudySatisfaction <- data.frame(
  Status = c("Available", "Missing"),
  Count = c(sum(!is.na(mydata$Study_Satisfaction)), sum(is.na(mydata$Study_Satisfaction)))
)

ggplot(missingData_StudySatisfaction, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Available" = "green", "Missing" = "red")) +
  labs(
    title = "Missing Data in Study Satisfaction",
    x = "Data Availability",
    y = "Count"
  ) +
  theme_minimal()






missingData_SleepDuration <- data.frame(
  Status = c("Available", "Missing"),
  Count = c(sum(!is.na(mydata$Sleep_Duration)), sum(is.na(mydata$Sleep_Duration)))
)

ggplot(missingData_SleepDuration, aes(x = Status, y = Count, fill = Status)) +
  geom_bar(stat = "identity") +
  scale_fill_manual(values = c("Available" = "green", "Missing" = "red")) +
  labs(
    title = "Missing Data in Sleep Duration",
    x = "Data Availability",
    y = "Count"
  ) +
  theme_minimal()





missing_rows <- which(rowSums(is.na(mydata)) > 0)
missing_rows

dataz <- is.na(mydata)
view(mydata)



mydata_remove_missing <- na.omit(mydata)
mydata_remove_missing





mydata[] <- lapply(mydata, function(x) {
  if (is.numeric(x)) {
    x[is.na(x)] <- mean(x, na.rm = TRUE)
    x <- round(x) 
  }
  return(x)
})
mydata



mydata[] <- lapply(mydata, function(x) {
  if (is.character(x) || is.factor(x)) {  
    x[is.na(x)] <- names(which.max(table(x)))  
  }
  return(x)
})
mydata




forward_fill <- mydata
forward_fill[] <- lapply(forward_fill, function(column) {
  zoo::na.locf(column, na.rm = FALSE)
})
forward_fill




backward_fill <- mydata
backward_fill [] <- lapply(backward_fill, function(column) {
  zoo::na.locf(column, na.rm = FALSE, fromLast = TRUE)
})
backward_fill



numeric_means <- sapply(mydata, function(col) {
  if (is.numeric(col)) {
    mean(col, na.rm = TRUE)  
  } else {
    NA  
  }
})
numeric_means





numeric_median <- sapply(mydata, function(col) {
  if (is.numeric(col)) {
    median(col, na.rm = TRUE)  
  } else {
    NA
  }
})
numeric_median




numeric_var <-sapply(mydata, function(x) {
  if (is.numeric(x)) {
    var(x, na.rm = TRUE)
  } else {
    NA 
  }
})
numeric_var




numeric_sd <-sapply(mydata, function(x) {
  if (is.numeric(x)) {
    sd(x, na.rm = TRUE)  
  } else {
    NA 
  }
})
numeric_sd




duplicated_rows <- mydata[duplicated(mydata) | duplicated(mydata, fromLast = TRUE), ]
duplicated_rows



mydata <-distinct(mydata)
mydata




result <- lapply(mydata, function(x) {
  if (is.factor(x) | is.character(x)) {
    table(x)
  }
})
result



install.packages("ROSE")
library("ROSE")

names(mydata) <- make.names(names(mydata), unique = TRUE)
mydata$Gender <- as.factor(mydata$Gender)
undersampled_data <- ovun.sample(Gender ~ ., data = mydata, method = "under", seed = 123)$data
table(undersampled_data$Gender)






names(mydata) <- make.names(names(mydata), unique = TRUE)
mydata$Gender <- as.factor(mydata$Gender)
oversampled_data <- ovun.sample(Gender ~ ., data = mydata, method = "over", seed = 123)$data
table(oversampled_data$Gender)





errors <- lapply(mydata, function(col) {
  unique(col)
})
errors


names(mydata) <- make.names(names(mydata), unique = TRUE)
mydata$Have_you_ever_had_suicidal_thoughts.. <- gsub("Yess", "Yes", mydata$Have_you_ever_had_suicidal_thoughts..)
mydata$Have_you_ever_had_suicidal_thoughts.. <- gsub("Noo", "No", mydata$Have_you_ever_had_suicidal_thoughts..)
unique(mydata$Have_you_ever_had_suicidal_thoughts..)




outliers_iqr <- lapply(mydata[sapply(mydata, is.numeric)], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  which(x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR))
})
outliers_iqr




boxplot(mydata$Age, main = "Boxplot for Age", col = "skyblue")
boxplot(mydata$Academic_Pressure, main = "Boxplot for Academic Presssure", col = "skyblue")



mydata[sapply(mydata, is.numeric)] <- lapply(mydata[sapply(mydata, is.numeric)], function(x) {
  Q1 <- quantile(x, 0.25, na.rm = TRUE)
  Q3 <- quantile(x, 0.75, na.rm = TRUE)
  IQR <- Q3 - Q1
  x[x < (Q1 - 1.5 * IQR) | x > (Q3 + 1.5 * IQR)] <- median(x, na.rm = TRUE)
  return(x)
})
table(mydata$Age)
table(mydata$Academic_Pressure)




filter_data1 <- filter(mydata, Age >= 18 & Age <= 60)
filter_data1


filter_data2 <- filter(mydata, Age >= 18 & Age <= 60 & Study_Hours > 5)
filter_data2

filter_data3 <- filter(mydata, Age >= 18 & Age <= 60 & Study_Hours < 5)
filter_data3


mydata$Gender <- factor(mydata$Gender, levels = c("Male", "Female"),labels = c(1,2))
table(mydata$Gender)


mydata$Sleep_Duration <- factor(mydata$Sleep_Duration, 
                                levels = c("Less than 5 hours", "5-6 hours", "7-8 hours", "More than 8 hours"),labels = c(4,5.5,7.5,9))

table(mydata$Sleep_Duration)

mydata$Academic_Pressure <- factor(mydata$Academic_Pressure,levels = c(1, 2, 3, 4, 5),
                                            labels = c("Very Low", "Low", "Moderate", "High", "Very High"))
table(mydata$Academic_Pressure)




minMax <- function(x) {
  (x - min(x)) / (max(x) - min(x))
}
mydata$Gender <- as.numeric(as.factor(mydata$Gender))

mydata$Gender <- minMax(mydata$Gender)

