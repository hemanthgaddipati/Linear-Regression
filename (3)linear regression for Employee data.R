# Exploratory data analysis
summary(emp_data)
# Graphical exploration
hist(emp_data$Salary_hike)
hist(emp_data$Churn_out_rate)  
boxplot(emp_data$Salary_hike, horizontal = T)
boxplot(emp_data$Churn_out_rate, horizontal = T)
qqnorm(emp_data$Salary_hike)
qqline(emp_data$Salary_hike)
qqnorm(emp_data$Churn_out_rate)
qqline(emp_data$Churn_out_rate)

#2nd moment business decisions
var(emp_data$Salary_hike)
sd_shike <- sqrt(var(emp_data$Salary_hike))
show(sd_shike)
var(emp_data$Churn_out_rate)
sd_COR <- sqrt(var(emp_data$Churn_out_rate))
show(sd_COR)

# 3rd and 4th moment business decisions

skewness(emp_data$Salary_hike)
kurtosis(emp_data$Salary_hike)
skewness(emp_data$Churn_out_rate)
kurtosis(emp_data$Churn_out_rate)

#data seems to be normal so lets proceed with regression model

##Scatter plot 
plot(emp_data$Salary_hike, emp_data$Churn_out_rate, main="scatter plot", xlab = "salary hike", ylab ="churn out rate", pch=20)
##determining the strength of the scatter plot
cor(emp_data$Churn_out_rate , emp_data$Salary_hike)

# now we got the direction,strength and leniarity so lets build the model

model1 <- lm(emp_data$Churn_out_rate ~ emp_data$Salary_hike)
summary(model1)

attach(emp_data)
model2 <- lm(log(Churn_out_rate)~ log(Salary_hike))
summary(model2)

#our model has R^2 value = 0.87 so lets build CI and PI
confint(model1,level = 0.95)
predict(model1,interval = "predict")

