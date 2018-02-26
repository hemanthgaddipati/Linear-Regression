# Exploratory data analysis
summary(emp_data)
# Graphical exploration
hist(Salary_Data$YearsExperience)
hist(Salary_Data$Salary)  
boxplot(Salary_Data$YearsExperience, horizontal = T)
boxplot(Salary_Data$Salary, horizontal = T)
qqnorm(Salary_Data$YearsExperience)
qqline(Salary_Data$YearsExperience)
qqnorm(Salary_Data$Salary)
qqline(Salary_Data$Salary)

#2nd moment business decisions
var(Salary_Data$YearsExperience)
sd_experience <- sqrt(var(Salary_Data$YearsExperience))
show(sd_experience)
var(Salary_Data$Salary)
sd_salary <- sqrt(var(Salary_Data$Salary))
show(sd_salary)

# 3rd and 4th moment business decisions

skewness(Salary_Data$YearsExperience)
kurtosis(Salary_Data$YearsExperience)
skewness(Salary_Data$Salary)
kurtosis(Salary_Data$Salary)

#data seems to be normal so lets proceed with regression model

##Scatter plot 
plot(Salary_Data$YearsExperience, Salary_Data$Salary, main="scatter plot", xlab = "experience", ylab ="salary", pch=20)
##determining the strength of the scatter plot
cor(Salary_Data$YearsExperience, Salary_Data$Salary)

# now we got the direction,strength and leniarity so lets build the model

model1 <- lm(Salary_Data$Salary ~ Salary_Data$YearsExperience)
summary(model1)


#our model has R^2 value = 0.95 so lets build CI and PI
confint(model1,level = 0.95)
predict(model1,interval = "predict")

