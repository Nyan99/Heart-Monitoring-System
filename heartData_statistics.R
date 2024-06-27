# Load the heart disease dataset
heart_disease <- read.csv("C:/Users/User/OneDrive/Documents/KD24203 DMW/Assignment Project/heart_disease_dataset.csv")  # Replace with your dataset path

# Summary statistics
summary(heart_disease$Age)
summary(heart_disease$Cholesterol)

# Counts and proportions
table(heart_disease$Gender)
table(heart_disease$Heart.Disease)
prop.table(table(heart_disease$Heart.Disease))

# Correlation
# Note: Ensure all columns used in cor() are numeric
cor(heart_disease[, c("Age", "Cholesterol", "Blood.Pressure", "Heart.Rate", "Blood.Sugar")])

# Group-wise statistics
aggregate(Age ~ Heart.Disease, data = heart_disease, FUN = mean)
aggregate(Cholesterol ~ Gender, data = heart_disease, FUN = median)

# Frequency tables
table(heart_disease$Gender, heart_disease$Heart.Disease)
table(heart_disease$Smoking, heart_disease$Heart.Disease)

