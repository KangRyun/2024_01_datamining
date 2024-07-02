library(dplyr)

### 실제값
true.value <- PlantGrowth$weight
true.value %>% length

true.value |> length()


##### 1
### 예측값
pred.value <- rnorm(30, 5, 0.7)


### 오차
error.value <- true.value- pred.value

### MAE
MAE <- mean(abs(error.value))

### MSE
MSE <- mean(error.value^2)

### RMSE
RMSE <- sqrt(MSE)

res.1 <- c(MAE, MSE, RMSE)
### 예측값
pred.value <- rnorm(30, 5.5, 0.5)


### 오차
error.value <- true.value- pred.value

### MAE
MAE <- mean(abs(error.value))

### MSE
MSE <- mean(error.value^2)

### RMSE
RMSE <- sqrt(MSE)
res.2 <- c(MAE, MSE, RMSE)

