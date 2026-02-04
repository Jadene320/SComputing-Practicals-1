#Practical One
##Question 1 

head(airquality)
air_data <- na.omit(airquality) #Remove NA valies from airquality data set and save an df

sum(!complete.cases(airquality))

##Ozone Statistics
min(air_data$Ozone)
max(air_data$Ozone)
mean(air_data$Ozone)
sd(air_data$Ozone)

##Temp Statistics
min(air_data$Temp)
max(air_data$Temp)
mean(air_data$Temp)
sd(air_data$Temp)

#Question 3
head(cars)

rm(list = ls())
df <- cars
Y <- df$dist
speed <- df$speed 
X <- cbind(1, speed)
b <- solve(t(X) %*% X)  %*% t(X) %*% Y ## %*% means perform matrix multiplication
print(b)

model <- lm(dist ~ speed, data= cars)
summary(model)

