# Loading the dataset

data=read.csv('C:/Users/Dell Latitude 3301/Downloads/ICT583 s2 2025 dataset.csv', sep=",", header=TRUE)

# Understanding the dataset
head(data)
names(data)
str(data)



# Data Pre-Processing
# Finding the missing values for each variable
install.packages("dplyr")
library(dplyr)
data %>% is.na() %>% colSums()

# Alternative Approach
f <- function(x) sum(is.na(x))
apply(data, 2, f)
lapply(data, f)
sapply(data, f)

# Encoding the continuous variables
data <- data %>%
  mutate(
    Age         = as.numeric(Age),
    body_height = as.numeric(body_height),
    body_weight = as.numeric(body_weight),
    waist       = as.numeric(waist),
    BMI         = as.numeric(BMI),
    MNAa_tot    = as.numeric(MNAa_tot),
    MNAb_tot    = as.numeric(MNAb_tot)
  )

# Encoding the categorical variables
data <- data %>%
  mutate(
    Mobility        = as.factor(Mobility),
    Education_ID    = as.factor(Education_ID),
    MNAa_q3         = as.factor(MNAa_q3),
    Hyperlipidaemia = as.factor(Hyperlipidaemia)
  )


# Handling missing data
data <- na.omit(data)

# After cleaning data check
data %>% is.na() %>% colSums()
str(data)



# Exploratory data analysis
# Box Plot for the BMI by MMSE_class (output variable; 0 = not at risk, 1 = at risk)
library(ggplot2)
ggplot(data, aes(x = factor(MMSE_class), y = BMI)) +
  geom_boxplot()

# Scatterplots of numeric variables
# Age vs BMI
ggplot(data, aes(x = Age, y = BMI)) +
  geom_point()

# Histogram of Age 
ggplot(data, aes(Age)) +
  geom_histogram(binwidth = 1)

# Correlation exploration
numeric_cols <- c("Age", "BMI", "Waist", "MNAa_tot", "MNAb_tot")
pairs(data[, numeric_cols], lower.panel = NULL)
cor(data[, numeric_cols])

library(dplyr)
cor(data[, numeric_cols]) %>% round(3)



# Prediction modelling
formula <- MMSE_class ~ Age + BMI + waist + MNAa_tot + MNAb_tot

# Logistic Regression Model
LGModel <- glm(formula, data = data, family = binomial(logit))
summary(LGModel)

LGModel1 <- glm(MMSE_class ~ Age + MNAa_tot, data = data, family = binomial(logit))
summary(LGModel1)

# Logistic Regression Model Tip Probability
LGModel1Predprobablistic <- predict(LGModel1, data, type = "response")
LGModel1Pred <- round(predict(LGModel1, data, type = "response"))

# Confusion matrix
library(caret)
confusionMatrix(table(LGModel1Pred, data[, "MMSE_class"]))

# ROC curve
library(ROCR)
pred <- prediction(LGModel1Predprobablistic, data[, "MMSE_class"])
perf <- performance(pred, "tpr", "fpr")
plot(perf, colorize = FALSE)

# Decision Tree
library(rpart)
DT_model <- rpart(formula, data = data, method = "class", control = rpart.control(cp = 0))

# Predict class labels
DT_pred <- predict(DT_model, data, type = "class")

# Evaluate with confusion matrix
confusionMatrix(table(DT_pred, data[, "MMSE_class"]))

