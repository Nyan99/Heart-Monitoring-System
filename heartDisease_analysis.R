# heart_disease_analysis.R

library(ggplot2)

# Load the heart disease dataset
heart_disease <- read.csv("C:/Users/User/OneDrive/Documents/KD24203 DMW/Assignment Project/heart_disease_dataset.csv")  # Replace with your dataset path

# View the first few rows
head(heart_disease)

# Summary statistics
summary(heart_disease)

# Structure of the dataset
str(heart_disease)

# Number of rows and columns
dim(heart_disease)

# Check for missing values in each column
colSums(is.na(heart_disease))

# Example: Extracting titles from names if applicable (assuming you have a name field)
# heart_disease$Title <- gsub('(.*, )|(\\..*)', '', heart_disease$Name)

# Plot: Heart disease by gender
p1 <- ggplot(heart_disease, aes(x = Gender, fill = factor(Heart.Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Disease by Gender", x = "Gender", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p1)

# Plot: Heart disease by age group
heart_disease$age_group <- cut(heart_disease$Age, breaks = seq(0, 100, by = 10))
p2 <- ggplot(heart_disease, aes(x = age_group, fill = factor(Heart.Disease))) +
  geom_bar(position = "dodge") +
  labs(title = "Heart Disease by Age Group", x = "Age Group", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p2)

# Plot: Cholesterol levels by heart disease status
p3 <- ggplot(heart_disease, aes(x = Cholesterol, fill = factor(Heart.Disease))) +
  geom_histogram(binwidth = 20, position = "dodge") +
  labs(title = "Cholesterol Levels by Heart Disease Status", x = "Cholesterol", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p3)

# Plot: Blood pressure by heart disease status
p4 <- ggplot(heart_disease, aes(x = Blood.Pressure, fill = factor(Heart.Disease))) +
  geom_histogram(binwidth = 10, position = "dodge") +
  labs(title = "Blood Pressure by Heart Disease Status", x = "Blood Pressure", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p4)

# Additional example plots
# Plot: Heart rate by heart disease status
p5 <- ggplot(heart_disease, aes(x = Heart.Rate, fill = factor(Heart.Disease))) +
  geom_histogram(binwidth = 5, position = "dodge") +
  labs(title = "Heart Rate by Heart Disease Status", x = "Heart Rate", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p5)

# Plot: Exercise hours by heart disease status
p6 <- ggplot(heart_disease, aes(x = Exercise.Hours, fill = factor(Heart.Disease))) +
  geom_histogram(binwidth = 1, position = "dodge") +
  labs(title = "Exercise Hours by Heart Disease Status", x = "Exercise Hours", y = "Count") +
  scale_fill_discrete(name = "Heart Disease", labels = c("No", "Yes"))

# Display the plot
print(p6)

