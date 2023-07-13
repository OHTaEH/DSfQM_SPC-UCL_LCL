install.packages("readxl")
install.packages("gridExtra")
install.packages("MCMCpack")
install.packages("dbscan")
install.packages("ggplot2")
install.packages("corrplot")
install.packages("gmodels")
install.packages("mvtnorm")
install.packages("MASS")
install.packages("mixtools")
install.packages("rpart")
install.packages("dplyr")
install.packages("e1071")
install.packages("Epi")
install.packages("ade4")
install.packages("factoextra")
install.packages("kdensity")
install.packages("caret")
install.packages("glmnet")
install.packages("isotree")
install.packages("cusum")
install.packages("ROCR")
install.packages("party")
install.packages("tidyverse")
install.packages("pROC")
install.packages("manipulate")
install.packages("doMC")
install.packages("randomForest")
install.packages("stringr")
install.packages("car")
install.packages("solitude")
install.packages("h2o")
install.packages("rgl")
library(gridExtra)
library(MCMCpack)
library(dbscan)
library(ggplot2) 
library(corrplot)
library(gmodels)
library(mvtnorm)
library(MASS)
library(mixtools)
library(rpart)
library(dplyr)
library(e1071)
library(Epi)
library(ade4)
library(factoextra)
library(kdensity)
library(data.table)
library(randomForest)
library(caret)
library(glmnet)
library(isotree)
library(pROC)
library(readxl)
library(dplyr)
library(cusum)
library(ROCR)
library(party)
library(tidyverse)
library(pROC)
library(manipulate)
library(doMC)
library(randomForest)
library(stringr)
library(car)
library(solitude)
library(h2o)
library(rgl)

rm(list=ls())

######################
##### Uniformity #####
######################

data2 = read_excel("/Users/ot/R_DSfQM/spc/Uniformity_Data.xlsx")
data2 = as.data.frame(data2)
head(data2)
data2
dim(data2)

# (2)-a 정규성 검정

# qq플롯을 통한 정규성 확인
# qq플롯 그림으로 시각화를 하였을 때
# 정규분포를 따를 것 같지 않아보인다
qqnorm(data2$Uniformity)
qqline(data2$Uniformity)

# 샤피로 윌콕슨 테스트를 통한 정규성 확인
# h0 : 정규분포 따른다
# h1 : 정규분포를 따르지 않는다
# p값이 0.002901로 0.05보다 작은 값이 나왔다
# 따라서 h0를 기각하는 것이 더 적절하다고 본다
# 그러므로 정규분포를 따른다고 보기 어렵다
shapiro.test(data2$Uniformity)

# 콜모고로프 스미르노프 테스트를 통한 정규성 검정
# 표본 크기가 작아서 이거보다는 샤피로가 더 낫기는 함
# 그래도 걍 해봤는데 이번에는 p값이 0.05보다 크게나옴
# 망ㅋ
# 워닝메세지 보니까 같은 숫자 있으면 안되나보다,,,

ks.test(data2$Uniformity, "pnorm", 
        mean = mean(data2$Uniformity), sd = sd(data2$Uniformity),
        alternative = "two.sided")

## Uniformity를 n번째에서 n-1번째꺼 빼준거의 절댓값
data2_1 = data2[2:30,2]
data2_0 = data2[1:29,2]
MR_data2 = abs(data2_1 - data2_0)
MR_data2

# qq플롯을 통한 정규성 확인
# qq플롯 그림으로 시각화를 하였을 때
# 정규분포를 따를 것 같지 않아보인다
qqnorm(MR_data2)
qqline(MR_data2)

# 얘도안됨
shapiro.test(MR_data2)

# 어쩐지 정규분포 안따르게들 생겼다...
hist(MR_data2)
hist(data2$Uniformity)

# 로그변환
# uniformity를 로그변환 시킨 값을 uni_log라는 변수로 만들어서
# 기존 데이터인 data2에다가 추가함
data2 <- transform(data2, 
                   uni_log = log(data2$Uniformity))

#잘 추가되어서 들어갔나 확인 
data2

#히스토그램 그려보기
hist(data2$uni_log)

# qq플롯 그림(이거만보고 판단하기가 아직은 좀 애매함)
qqnorm(data2$uni_log)
qqline(data2$uni_log)

# 로그 변환시킨 값에 대해서 샤피로 정규성 검정을 해봤더니
# p값이 0.05보다 크므로 uni_log는 정규분포 따른다고 볼 수 있겠다
shapiro.test(data2$uni_log)


## 제곱근 변환
data2 <- transform(data2, 
                   uni_sqrt = sqrt(data2$Uniformity))

hist(data2$uni_sqrt)

qqnorm(data2$uni_sqrt)
qqline(data2$uni_sqrt)

shapiro.test(data2$uni_sqrt)

# 제곱근 변환도 정규분포를 따르는 것 같으나
# 샤피로 테스트의 p값이 로그변환에서 더 크게 나타나는 것으로 보아
# 로그 변환된 값이 좀 더 정규분포에 가까운 것으로 보임

# rm(list=ls())

####### (2)-b ########
## 우선 먼저 기존의 Uniformity 값으로 시도해보기

wafers = read_excel("/Users/ot/R_DSfQM/spc/Uniformity_Data.xlsx")
wafers = as.data.frame(wafers)
wafers
dim(wafers)

### X_bar in Table ###
x_bar_phaseI = mean(wafers[,2])
x_bar_phaseI = round(x_bar_phaseI, 3)
x_bar_phaseI

### MR ###
x_1_phaseI = wafers[2:30,2]
x_0_phaseI = wafers[1:29,2]
MR_phaseI = abs(x_1_phaseI - x_0_phaseI)
MR_phaseI

### MR_bar ###
MR_bar_phaseI = mean(MR_phaseI)
MR_bar_phaseI = round(MR_bar_phaseI, 4)
MR_bar_phaseI

# UCL of I chart
d2 = 1.128
UCL_I = x_bar_phaseI + 3*MR_bar_phaseI/d2
UCL_I = round(UCL_I, 3)
UCL_I

# LCL of I chart
LCL_I = x_bar_phaseI - 3*MR_bar_phaseI/d2
LCL_I = round(LCL_I, 3)
LCL_I


### plot of I chart ### x_bar
par(mai = c(1, 1, 0.5, 2))
plot(wafers[,2], xlab = "wafer", ylab = "Individual value",
     pch = 19, col = 4, type = "o",
     xlim = c(0, 30), ylim = c(0, 35),
     lwd = 3, mgp = c(1.5, 0.5, 0))

abline(h = LCL_I) # LCL 라인의 숫자를 그래프에 나타내기 
axis(4, at = LCL_I, las = 2, tck = 0,
     labels = paste("LCL=", round(LCL_I, 3)),
     mgp = c(0.5, 0.2, 0))

abline(h = x_bar_phaseI)
axis(4, at = x_bar_phaseI, las = 2, tck = 0,
     labels = paste("Mean=", round(x_bar_phaseI, 3)),
     mgp = c(2, 0.2, 0))

abline(h = UCL_I)  # UCL 라인의 숫자를 그래프에 나타내기 
axis(4, at = UCL_I, las = 2, tck = 0,
     labels = paste("UCL=", round (UCL_I, 3)),
     mgp = c(2, 0.2, 0))




### UCL of MR chart ###
D4 = 3.267
UCL_MR = D4*MR_bar_phaseI
UCL_MR = round (UCL_MR, 3)
UCL_MR

# LCL of MR chart
D3 = 0
LCL_MR = D3*MR_bar_phaseI
LCL_MR

### plot of MR chart ###
par(mai = c(1, 1, 0.5, 2))
plot(MR_phaseI, xlab = "R_bar", ylab = "Moving range",
     pch = 19, col = 4, type = "o",
     xlim = c(0, 30), ylim = c(0, 25),
     lwd = 3, mgp = c(1.5, 0.5, 0))

abline(h = LCL_MR)
axis(4, at = LCL_MR, las = 2, tck = 0,
     labels = paste("LCL=", round(LCL_MR, 3)),
     mgp = c(2, 0.2, 0))

abline(h = MR_bar_phaseI)
axis(4, at = MR_bar_phaseI, las = 2, tck = 0,
     labels = bquote(bar(R) == .(round(MR_bar_phaseI, 4))),
     mgp = c(2, 0.2, 0))

abline(h = UCL_MR)
axis(4, at = UCL_MR, las = 2, tck = 0,
     labels = paste("UCL=", round(UCL_MR, 3)),
     mgp = c(2, 0.2, 0))


