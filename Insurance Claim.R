library(lattice)
library(MASS)
library(ggplot2)
library(dplyr)
library(robustbase)
library(psych)
library(ltm)
library(corrplot)

data <- insurance_data

head(data)

# mencari nama-nama kolom dari dataset
colnames(data)

# mencari informasi mengenai setiap kolom 
str(data)

# mencari descriptive statistic dari dataset
summary(data)
print(describe(data), digits=2)

# melihat apakah terdapat missing value
colSums(is.na(data))

# mengisi missing value pada kolom age dengan mean dari age tersebut
mean(data$age, na.rm = TRUE)

data$age[is.na(data$age)] <- mean(data$age, na.rm = TRUE)
head(data)

# mengubah tipe data kolom age menjadi integer
data$age <- as.integer(data$age)
head(data)

# mencari modus untuk mengisi missing value dari kolom region
find_mode <- function(x) {
  u <- unique(x)
  tab <- tabulate(match(x, u))
  u[tab == max(tab)]
}

find_mode(data$region)

# mengisi missing value pada kolom region dengan 'southeast' yang
# merupakan modus
data$region[is.na(data$region)] <- "southeast"
head(data,20)

# menghilangkan kolom index dan PatientID karena tidak relevan
data = subset(data, select = -c(index, PatientID))
head(data)

# mengelompokkan bmi yang ada menjadi 4 kategori, yaitu 'underweight',
# 'normal', 'overweight', dan 'obesity'
bmicat <- function(x){
  if (x < 18.5){
    return ("underweight")
  } else if (x >= 18.5 && x < 25){
    return ("normal")
  } else if (x >= 25 && x < 30){
    return ("overweight")
  } else if (x >= 30){
    return ("obesity")
  }
}

data <- data %>% mutate(bmi_cat = sapply(bmi, bmicat))
head(data)

# mengelompokkan kolom usia menjadi 4 kategori, yaitu 'children', 'young adults',
# 'adults', dan 'elderly'
agecat <- function(x){
  if(x >= 0 && x < 17){
    return ("children")
  } else if (x >= 17 && x < 31){
    return ("young adults")
  } else if (x >= 31 && x < 60){
    return ("adults")
  } else if (x >= 60){
    return ("elderly")
  }
}

data <- data %>% mutate(age_cat = sapply(age, agecat))
head(data)

# mengelompokkan kolom blood pressure menjadi 4 kategori, yaitu 'low blood pressure',
# 'normal', 'prehypertension', dan 'hypertension'
bpcat <- function(x){
  if(x < 80){
    return ("low blood pressure")
  } else if (x >= 80 && x < 121){
    return ("normal")
  } else if (x >= 121 && x < 140){
    return ("prehypertension")
  } else if (x >= 140){
    return ("hypertension")
  }
}

data <- data %>% mutate(bloodpressure_cat = sapply(bloodpressure, bpcat))
head(data)

# visualisasi
# visualisasi kolom gender
ggplot(data, aes(x = gender))+geom_bar()+labs(x = "Gender", y = "Count") 
# visualisasi kolom diabetic
ggplot(data, aes(x = diabetic))+geom_bar()+labs(x = "Diabetic", y = "Count") 
# visualisasi kolom children
ggplot(data, aes(x = children))+geom_bar()+labs(x = "Children", y = "Count") 
# visualisasi kolom smoker
ggplot(data, aes(x = smoker))+geom_bar()+labs(x = "Smoker", y = "Count") 
# visualisasi kolom region
ggplot(data, aes(x = region))+geom_bar()+labs(x = "Region", y = "Count") 
# visualisasi kolom bmi category
ggplot(data, aes(x = bmi_cat))+geom_bar()+labs(x = "BMI Category", y = "Count") 
# visualisasi kolom age category
ggplot(data, aes(x = age_cat))+geom_bar()+labs(x = "Age Category", y = "Count") 
# visualisasi kolom blood pressure cateogory
ggplot(data, aes(x = bloodpressure_cat))+geom_bar()+labs(x = "Blood Pressure Category", y = "Count") 

# visualisasi hubungan antara diabetic dengan bmi category
barplot(table(data$diabetic, data$bmi_cat), 
        main = "BMI Category and Diabetic",
        xlab = "BMI Category", 
        ylab = "Count",
        legend.text = rownames(table(data$diabetic, data$bmi_cat)),
        col = c("orange", "lightblue"),
        beside = TRUE,
        ylim = c(0,400))

# visualisasi hubungan antara claim dengan children
boxplot(claim ~ children, data = data, col = c("blue", "red"), 
        xlab = "Number of Children", ylab = "Claims")

# visualisasi hubungan antara claim dengan bmi category
boxplot(claim ~ bmi_cat, data = data, col = c("blue", "red"), 
        xlab = "BMI Category", ylab = "Claims")

# visualisasi hubungan antara claim dengan smoking
boxplot(claim ~ smoker, data = data, col = c("blue", "red"), 
        xlab = "Smoker", ylab = "Claims")

# visualisasi hubungan antara claim dengan blood pressure
boxplot(claim ~ bloodpressure_cat, data = data, col = c("blue", "red"), 
        xlab = "Blood Pressure Category", ylab = "Claims")

# visualisasi hubungan antara gender dengan bmi category
barplot(table(data$gender, data$bmi_cat), 
        main = "BMI Category and Gender",
        xlab = "BMI Category", 
        ylab = "Count",
        legend.text = rownames(table(data$gender, data$bmi_cat)),
        col = c("orange", "lightblue"),
        beside = TRUE,
        ylim = c(0,400))

# visualisasi hubungan antara region dengan bmi category
barplot(table(data$region, data$bmi_cat), 
        main = "BMI Category and Region",
        xlab = "BMI Category", 
        ylab = "Count",
        legend.text = rownames(table(data$region, data$bmi_cat)),
        col = c("orange", "lightblue", "lightgreen","yellow"),
        beside = TRUE,
        ylim = c(0,300))

# visualisasi hubungan antara smoker dengan blood pressure category
barplot(table(data$smoker, data$bloodpressure_cat), 
        main = "Blood Pressure and Smoker",
        xlab = "Blood Pressure", 
        ylab = "Count",
        legend.text = rownames(table(data$smoker, data$bloodpressure_cat)),
        col = c("lightgreen","yellow"),
        beside = TRUE,
        ylim = c(0,1200))

# visualisasi hubungan antara blood pressure dengan bmi
ggplot(data, aes(x = data$bloodpressure, y = data$bmi)) +
  geom_point() +
  scale_x_continuous(limits = c(80, 150),
  breaks = c(80,90,100,110,120,130,140,150)) +
  labs(title = "Blood Pressure and BMI", x = "Blood Pressure", y = "BMI")

# correlation
# insurance claim and age - low negative to no
plot(data$age, data$claim)
age <- cor(data$age,data$claim, method = "spearman")
age

# insurance claim and gender - low negative to no
gender <- biserial.cor(data$claim, data$gender)
gender

# insurance claim and bmi - low positive
plot(data$bmi, data$claim)
bmi <- cor(data$bmi, data$claim, method = "spearman")
bmi

# insurance claim and blood pressure - positive moderate 
plot(data$bloodpressure,data$claim)
bp <- cor(data$bloodpressure, data$claim, method = "spearman")
bp

# insurance claim and diabetic - low positive to no
diabetic <- biserial.cor(data$claim, data$diabetic)
diabetic

# insurance claim and children - low positive to no
plot(data$children,data$claim)
child <- cor(data$children, data$claim, method = "spearman")
child

# insurance claim and smoker - high negative
smoke <- biserial.cor(data$claim, data$smoker)
smoke

# menyatukan hasil correlation yang ada menjadi sebuah tabel
corr_table = matrix(c(age, gender, bmi, bp, diabetic, child, smoke), ncol = 7, byrow = TRUE)
colnames(corr_table) = c('age','gender','bmi','bloodpressure','diabetic','children','smoker')
rownames(corr_table) = c('claim')
final = as.table(corr_table)
final

corrplot(final, method = "color", col=colorRampPalette(c("white","lightgreen","blue","black"))(100))

# statistical test
# T test - male and female
male_claim <- data$claim[data$gender == "male"]
female_claim <- data$claim[data$gender == "female"]

result <- t.test(male_claim, female_claim)
print(result)

# T test - diabetic and not diabetic
diabet_claim <- data$claim[data$diabetic == "Yes"]
not_diabet_claim <- data$claim[data$diabetic == "No"]

result <- t.test(diabet_claim, not_diabet_claim)
print(result)

# T test - smoking and not smoking
smoking_claim <- data$claim[data$smoker == "No"]
not_smoking_claim <- data$claim[data$smoker == "Yes"]

result <- t.test(smoking_claim, not_smoking_claim)
print(result)

# ANOVA - region
res <- aov(claim ~ region, data = data)
print(summary(res))

# ANOVA - BMI
res <- aov(claim ~ bmi_cat, data = data)
print(summary(res))

# Mann-Whitney U test
two_child <- data$claim[data$children == 2]
three_child <- data$claim[data$children == 3]

hasil <- wilcox.test(two_child, three_child)
print(hasil)

# Kruskal Wallis
kw <- kruskal.test(claim ~ bloodpressure, data = data)
print(kw)

# predictive modeling
# random forest
library(randomForest)

set.seed(123)
train_index <- sample(1:nrow(data), 0.7 * nrow(data))
train_data <- data[train_index, ]
test_data <- data[-train_index, ]

rf_model <- randomForest(claim ~ ., data = data, ntree = 100)
print(rf_model)

predictions <- predict(rf_model, test_data)

mae <- mean(abs(test_data$claim - predictions))
mse <- mean((predictions - test_data$claim)^2)
rmse <- sqrt(mse)
print(paste("Mean Absolute Error (MAE):", mae))
print(paste("Root Mean Squared Error (RMSE):", rmse))
plot(predictions,test_data$claim)
