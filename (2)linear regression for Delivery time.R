# Exploratory data analysis
summary(delivery_time)
# Graphical exploration
hist(delivery_time$`Delivery Time`)
hist(delivery_time$`Sorting Time`)  
boxplot(delivery_time$`Delivery Time`, horizontal = T)
boxplot(delivery_time$`Sorting Time`, horizontal = T)
qqnorm(delivery_time$`Delivery Time`)
qqline(delivery_time$`Delivery Time`)
qqnorm(delivery_time$`Sorting Time`)
qqline(delivery_time$`Sorting Time`)

#2nd moment business decisions
var(delivery_time$`Delivery Time`)
sd_dt <- sqrt(var(delivery_time$`Delivery Time`))
show(sd_dt)
var(delivery_time$`Sorting Time`)
sd_st <- sqrt(var(delivery_time$`Sorting Time`))
show(sd_st)

# 3rd and 4th moment business decisions

skewness(delivery_time$`Delivery Time`)
kurtosis(delivery_time$`Delivery Time`)
skewness(delivery_time$`Sorting Time`)
kurtosis(delivery_time$`Sorting Time`)

#data seems to be normal so lets proceed with regression model

##Scatter plot 
plot(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`,main="scatter plot", xlab = "calories consumed", ylab ="weight gained", pch=20)
##determining the strength of the scatter plot
cor(delivery_time$`Delivery Time`,delivery_time$`Sorting Time`)

# now we got the direction,strength and leniarity so lets build the model

model1 <- lm(delivery_time$`Delivery Time`~ delivery_time$`Sorting Time`)
summary(model1)

attach(delivery_time)
model2 <- lm(log(`Delivery Time`)~sqrt(`Sorting Time`))
summary(model2)

#our model has R^2 value = 0.73 so lets build CI and PI
confint(model1,level = 0.95)
predict(model1,interval = "predict")

