attach(workplace_wellness_dataset)

## continuous variables
summary(Age)
hist(workplace_wellness_dataset$Age, main="Age Distribution", xlab="Age", col="lightblue", border="black",freq = FALSE)
lines(density(workplace_wellness_dataset$Age), col="red", lwd=2)

boxplot(workplace_wellness_dataset$Age, horizontal = TRUE, main = " Boxplot of Age", xlab = "Age")

summary(Pre_Stress_Level)
summary(Post_Stress_Level)
boxplot(workplace_wellness_dataset$Pre_Stress_Level, workplace_wellness_dataset$Post_Stress_Level, names=c("Pre-Stress", "Post-Stress"), main="Stress Levels Before and After Program",col="lightgreen")

summary(Pre_Exercise_Frequency)
summary(Post_Exercise_Frequency)
boxplot(workplace_wellness_dataset$Pre_Exercise_Frequency, workplace_wellness_dataset$Post_Exercise_Frequency, names=c("Pre-Ex_Fre", "Post-Ex_Fre"), main="Exercise Frequency Before and After Program",col="yellow")

summary(Pre_BMI)
summary(Post_BMI)
boxplot(workplace_wellness_dataset$Pre_BMI, workplace_wellness_dataset$Post_BMI, names=c("Pre-BMI", "Post-BMI"), main="BMI Value Before and After Program",col="red")

summary(Pre_Mental_Health_Score)
summary(Post_Mental_Health_Score)
boxplot(workplace_wellness_dataset$Pre_Mental_Health_Score, workplace_wellness_dataset$Post_Mental_Health_Score, names=c("Pre-MentalHealth", "Post-MentalHealth"), main="Mental Health Before and After Program",col="pink")

## categorical variables
# Calculate the frequency distribution for Gender
gender_dist <- table(data$Gender)

# Create the pie chart
pie(gender_dist, main="Gender Distribution", col=rainbow(length(gender_dist)), labels=paste(names(gender_dist), "\n", gender_dist))

# Calculate the frequency distribution for Department
department_dist <- table(data$Department)

# Create the pie chart
pie(department_dist, main="Department Distribution", col=rainbow(length(department_dist)), labels=paste(names(department_dist), "\n", department_dist))

# Calculate the frequency distribution for Job Role
jobrole_dist <- table(data$Job_Role)

# Create the pie chart
pie(jobrole_dist, main="Job Role Distribution", col=rainbow(length(jobrole_dist)), labels=paste(names(jobrole_dist), "\n", jobrole_dist))

summary(Pre_Smoking_Status)
summary(Post_Smoking_Status)
table(Pre_Smoking_Status)
table(Post_Smoking_Status)

## Statistical Analysis
shapiro.test(workplace_wellness_dataset$Pre_Stress_Level)
shapiro.test(workplace_wellness_dataset$Post_Stress_Level)

shapiro.test(workplace_wellness_dataset$Pre_Exercise_Frequency)
shapiro.test(workplace_wellness_dataset$Post_Exercise_Frequency)

##stress level wilcox test
wilcox.test(workplace_wellness_dataset$Pre_Stress_Level,workplace_wellness_dataset$Post_Stress_Level,alternative = "greater",paired = T, conf.level = 0.95)

##Exercise freqeuncy wilcox test
wilcox.test(Post_Exercise_Frequency,Pre_Exercise_Frequency,mu=0,alternative = "greater",paired = T,conf.level = 0.95)

## objective 2
##(1)
library(dplyr)
library(ggplot2)

summary_stats <- workplace_wellness_dataset %>% group_by(workplace_wellness_dataset$Department) %>%summarize(mean_pre_bmi = mean(Pre_BMI),sd_pre_bmi = sd(Pre_BMI),median_pre_bmi = median(Pre_BMI), )
summary_stats1 <- workplace_wellness_dataset %>%group_by(workplace_wellness_dataset$Department) %>%summarize( mean_post_bmi = mean(Post_BMI),sd_post_bmi = sd(Post_BMI),median_post_bmi = median(Post_BMI),  )

print(summary_stats)
print(summary_stats1)

ggplot(workplace_wellness_dataset, aes(x = Department, y = Pre_BMI, fill = Department)) +geom_boxplot() +labs(title = "Pre-BMI by Department")
ggplot(workplace_wellness_dataset, aes(x = Department, y = Post_BMI, fill = Department)) +geom_boxplot() +labs(title = "Post-BMI level by Department")

kruskal.test(Post_BMI~Department)
#(2)
summary_stats <- workplace_wellness_dataset %>% group_by(workplace_wellness_dataset$Department) %>%summarize(mean_pre_stress = mean(Pre_Stress_Level),sd_pre_stress = sd(Pre_Stress_Level),median_pre_stress = median(Pre_Stress_Level), )
summary_stats1 <- workplace_wellness_dataset %>%group_by(workplace_wellness_dataset$Department) %>%summarize( mean_post_stress = mean(Post_Stress_Level),sd_post_stress = sd(Post_Stress_Level),median_post_stress = median(Post_Stress_Level),  )

print(summary_stats)
print(summary_stats1)

ggplot(workplace_wellness_dataset, aes(x = Department, y = Pre_Stress_Level, fill = Department)) +geom_boxplot() +labs(title = "Pre-stress level by Department")
ggplot(workplace_wellness_dataset, aes(x = Department, y = Post_Stress_Level, fill = Department)) +geom_boxplot() +labs(title = "Post-stress level by Department")

kruskal.test(Post_Stress_Level~Department)

# (3)
summary_stats <- workplace_wellness_dataset %>% group_by(workplace_wellness_dataset$Department) %>%summarize(mean_pre_Exer = mean(Pre_Exercise_Frequency),sd_pre_Exer = sd(Pre_Exercise_Frequency),median_pre_Exer = median(Pre_Exercise_Frequency), )
summary_stats1 <- workplace_wellness_dataset %>%group_by(workplace_wellness_dataset$Department) %>%summarize( mean_post_Exer = mean(Post_Exercise_Frequency),sd_post_Exer = sd(Post_Exercise_Frequency),median_post_Exer = median(Post_Exercise_Frequency),  )

print(summary_stats)
print(summary_stats1)

ggplot(workplace_wellness_dataset, aes(x = Department, y = Pre_Exercise_Frequency, fill = Department)) +geom_boxplot() +labs(title = "Pre-Exercise frequency level by Department")
ggplot(workplace_wellness_dataset, aes(x = Department, y = Post_Exercise_Frequency, fill = Department)) +geom_boxplot() +labs(title = "Post-Exercise frequency level by Department")

kruskal.test(Post_Exercise_Frequency~Department)

#(4)
summary_stats <- workplace_wellness_dataset %>% group_by(workplace_wellness_dataset$Department) %>%summarize(mean_pre_mental = mean(Pre_Mental_Health_Score),sd_pre_mental = sd(Pre_Mental_Health_Score),median_pre_mental = median(Pre_Mental_Health_Score), )
summary_stats1 <- workplace_wellness_dataset %>%group_by(workplace_wellness_dataset$Department) %>%summarize( mean_post_mental = mean(Post_Mental_Health_Score),sd_post_metal = sd(Post_Mental_Health_Score),median_post_mental = median(Post_Mental_Health_Score),  )

print(summary_stats)
print(summary_stats1)

ggplot(workplace_wellness_dataset, aes(x = Department, y = Pre_Mental_Health_Score, fill = Department)) +geom_boxplot() +labs(title = "Pre-mental health score by Department")
ggplot(workplace_wellness_dataset, aes(x = Department, y = Post_Mental_Health_Score, fill = Department)) +geom_boxplot() +labs(title = "Post-mental health score by Department")

kruskal.test(Post_Mental_Health_Score~Department)

## objective 3
## (1)
stress_change <- workplace_wellness_dataset$Post_Stress_Level - workplace_wellness_dataset$Pre_Stress_Level
pre_exercise_stress_corr <- cor.test(workplace_wellness_dataset$Pre_Exercise_Frequency,stress_change )
print(pre_exercise_stress_corr)

post_exercise_stress_corr <- cor.test(workplace_wellness_dataset$Post_Exercise_Frequency,stress_change )
print(post_exercise_stress_corr)

## (2)
BMI_change <- workplace_wellness_dataset$Post_BMI - workplace_wellness_dataset$Pre_BMI
pre_exercise_BMI_corr <- cor.test(workplace_wellness_dataset$Pre_Exercise_Frequency,BMI_change )
print(pre_exercise_BMI_corr)

post_exercise_BMI_corr <- cor.test(workplace_wellness_dataset$Post_Exercise_Frequency,BMI_change )
print(post_exercise_BMI_corr)

## (3)
mental_change <- workplace_wellness_dataset$Post_Mental_Health_Score - workplace_wellness_dataset$Pre_Mental_Health_Score
pre_exercise_mental_corr <- cor.test(workplace_wellness_dataset$Pre_Exercise_Frequency,mental_change )
print(pre_exercise_mental_corr)

post_exercise_mental_corr <- cor.test(workplace_wellness_dataset$Post_Exercise_Frequency,mental_change )
print(post_exercise_mental_corr)

## smoking status vs health outcomes
## smoking statsu vs change in health outcomes
# Load ggplot2 library
library(ggplot2)

ggplot(workplace_wellness_dataset, aes(x = Pre_Smoking_Status, y = Pre_Stress_Level)) +geom_bar(stat = "summary", fun = "median", fill = "blue") + ggtitle("Median Pre-Stress Level by Smoking Status") +xlab("Smoking Status") +  ylab("Median Pre-Stress Level")
ggplot(workplace_wellness_dataset, aes(x = Post_Smoking_Status, y = Post_Stress_Level)) +geom_bar(stat = "summary", fun = "median", fill = "green") +ggtitle("Median Post-Stress Level by Smoking Status") +xlab("Smoking Status") + ylab("Median Post-Stress Level")

wilcox.test(Post_Stress_Level~Post_Smoking_Status)
wilcox.test(Pre_Stress_Level~Pre_Smoking_Status)

##SMOKING STATUS VS BMI
ggplot(workplace_wellness_dataset, aes(x = Pre_Smoking_Status, y = Pre_BMI)) +geom_bar(stat = "summary", fun = "median", fill = "blue") +ggtitle("Median Pre-BMI Level by Smoking Status") + xlab("Smoking Status") + ylab("Median Pre-BMI Level")
ggplot(workplace_wellness_dataset, aes(x = Post_Smoking_Status, y = Post_BMI)) +geom_bar(stat = "summary", fun = "median", fill = "green") +ggtitle("Median Post-BMI Level by Smoking Status") +xlab("Smoking Status") + ylab("Median Post-BMI Level")

wilcox.test(Post_BMI~Post_Smoking_Status)
wilcox.test(Pre_BMI~Pre_Smoking_Status)

##SMOKING STATUS VS Mental health score
ggplot(workplace_wellness_dataset, aes(x = Pre_Smoking_Status, y = Pre_Mental_Health_Score)) +geom_bar(stat = "summary", fun = "median", fill = "blue") +ggtitle("Median Pre-mental health score Level by Smoking Status") +xlab("Smoking Status") +  ylab("Median Pre-mental health score Level")
ggplot(workplace_wellness_dataset, aes(x = Post_Smoking_Status, y = Post_Mental_Health_Score)) +geom_bar(stat = "summary", fun = "median", fill = "green") +ggtitle("Median Post-mental health score Level by Smoking Status") +xlab("Smoking Status") + ylab("Median Post-mental health score Level")

wilcox.test(Post_Mental_Health_Score~Post_Smoking_Status)
wilcox.test(Pre_Mental_Health_Score~Pre_Smoking_Status)







