# Exploratory data analysis
  summary(calories_consumed)
# Graphical exploration
  hist(calories_consumed$`Calories Consumed`)
  hist(calories_consumed$`Weight gained (grams)`)  
  boxplot(calories_consumed$`Weight gained (grams)`, horizontal = T)
  boxplot(calories_consumed$`Calories Consumed`, horizontal = T)
  qqnorm(calories_consumed$`Calories Consumed`)
  qqline(calories_consumed$`Calories Consumed`)
  qqnorm(calories_consumed$`Weight gained (grams)`)
  qqline(calories_consumed$`Weight gained (grams)`)
  boxplot(calories_consumed$`Weight gained (grams)`, horizontal = T)

#2nd moment business decisions
  var(calories_consumed$`Weight gained (grams)`)
  sd_weight <- sqrt(var(calories_consumed$`Weight gained (grams)`))
  show(sd_weight)
  var(calories_consumed$`Calories Consumed`)
  sd_calories <- sqrt(var(calories_consumed$`Calories Consumed`))
  show(sd_calories)
  
# 3rd and 4th moment business decisions
  
  boxplot(calories_consumed$`Calories Consumed`, horizontal = T)
  skewness(calories_consumed$`Weight gained (grams)`)
  kurtosis(calories_consumed$`Weight gained (grams)`)
  skewness(calories_consumed$`Calories Consumed`)
  kurtosis(calories_consumed$`Calories Consumed`)

#data seems to be normal so lets proceed with regression model
  
  ##Scatter plot 
    plot(calories_consumed$`Weight gained (grams)`,calories_consumed$`Calories Consumed`,main="scatter plot", xlab = "calories consumed", ylab ="weight gained", pch=20)
  ##determining the strength of the scatter plot
    cor(calories_consumed$`Weight gained (grams)`,calories_consumed$`Calories Consumed`)
    
# now we got the direction,strength and leniarity so lets build the model
    
    model1 <- lm(calories_consumed$`Weight gained (grams)`~ calories_consumed$`Calories Consumed`)
    summary(model1)
#our model has R^2 value > 0.8 so we built a strong model lets build CI and PI
    confint(model1,level = 0.95)
    predict(model1,interval = "predict")

    
        