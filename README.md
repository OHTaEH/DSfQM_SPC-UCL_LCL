> rm(list=ls())
> data2 = read_excel("/Users/R_DSfQM/spc/Uniformity_Data.xlsx")
                                                                                                                                                          
> data2 = as.data.frame(data2)
> head(data2)
  Wafer Uniformity
1     1         11
2     2         16
3     3         22
4     4         14
5     5         34
6     6         22
> data2
   Wafer Uniformity
1      1         11
2      2         16
3      3         22
4      4         14
5      5         34
6      6         22
7      7         13
8      8         11
9      9          6
10    10         11
11    11         11
12    12         23
13    13         14
14    14         12
15    15          7
16    16         15
17    17         16
18    18         12
19    19         11
20    20         18
21    21         14
22    22         13
23    23         18
24    24         12
25    25         13
26    26         12
27    27         15
28    28         21
29    29         21
30    30         14
> dim(data2)
[1] 30  2
> # qq플롯을 통한 정규성 확인
> # qq플롯 그림으로 시각화를 하였을 때
> # 정규분포를 따를 것 같지 않아보인다
> qqnorm(data2$Uniformity)
> qqline(data2$Uniformity)
> # 샤피로 윌콕슨 테스트를 통한 정규성 확인
> # h0 : 정규분포 따른다
> # h1 : 정규분포를 따르지 않는다
> # p값이 0.002901로 0.05보다 작은 값이 나왔다
> # 따라서 h0를 기각하는 것이 더 적절하다고 본다
> # 그러므로 정규분포를 따른다고 보기 어렵다
> shapiro.test(data2$Uniformity)

	Shapiro-Wilk normality test

data:  data2$Uniformity
W = 0.88054, p-value = 0.002901

> ks.test(data2$Uniformity, "pnorm", 
+         mean = mean(data2$Uniformity), sd = sd(data2$Uniformity),
+         alternative = "two.sided")

	Asymptotic one-sample Kolmogorov-Smirnov test

data:  data2$Uniformity
D = 0.17626, p-value = 0.3089
alternative hypothesis: two-sided

Warning message:
In ks.test.default(data2$Uniformity, "pnorm", mean = mean(data2$Uniformity),  :
  ties should not be present for the Kolmogorov-Smirnov test
> ## Uniformity를 n번째에서 n-1번째꺼 빼준거의 절댓값
> data2_1 = data2[2:30,2]
> data2_0 = data2[1:29,2]
> MR_data2 = abs(data2_1 - data2_0)
> MR_data2
 [1]  5  6  8 20 12  9  2  5  5  0 12  9  2  5  8  1  4  1  7  4  1  5  6  1  1  3  6  0  7
> # qq플롯을 통한 정규성 확인
> # qq플롯 그림으로 시각화를 하였을 때
> # 정규분포를 따를 것 같지 않아보인다
> qqnorm(MR_data2)
> qqline(MR_data2)
> # 얘도안됨
> shapiro.test(MR_data2)

	Shapiro-Wilk normality test

data:  MR_data2
W = 0.88585, p-value = 0.004565

> # 어쩐지 정규분포 안따르게들 생겼다...
> hist(MR_data2)
> hist(data2$Uniformity)
> # 로그변환
> # uniformity를 로그변환 시킨 값을 uni_log라는 변수로 만들어서
> # 기존 데이터인 data2에다가 추가함
> data2 <- transform(data2, 
+                    uni_log = log(data2$Uniformity))
> #잘 추가되어서 들어갔나 확인 
> data2
   Wafer Uniformity  uni_log
1      1         11 2.397895
2      2         16 2.772589
3      3         22 3.091042
4      4         14 2.639057
5      5         34 3.526361
6      6         22 3.091042
7      7         13 2.564949
8      8         11 2.397895
9      9          6 1.791759
10    10         11 2.397895
11    11         11 2.397895
12    12         23 3.135494
13    13         14 2.639057
14    14         12 2.484907
15    15          7 1.945910
16    16         15 2.708050
17    17         16 2.772589
18    18         12 2.484907
19    19         11 2.397895
20    20         18 2.890372
21    21         14 2.639057
22    22         13 2.564949
23    23         18 2.890372
24    24         12 2.484907
25    25         13 2.564949
26    26         12 2.484907
27    27         15 2.708050
28    28         21 3.044522
29    29         21 3.044522
30    30         14 2.639057
> #히스토그램 그려보기
> hist(data2$uni_log)
> # qq플롯 그림(이거만보고 판단하기가 아직은 좀 애매함)
> qqnorm(data2$uni_log)
> qqline(data2$uni_log)
> # 로그 변환시킨 값에 대해서 샤피로 정규성 검정을 해봤더니
> # p값이 0.05보다 크므로 uni_log는 정규분포 따른다고 볼 수 있겠다
> shapiro.test(data2$uni_log)

	Shapiro-Wilk normality test

data:  data2$uni_log
W = 0.95218, p-value = 0.1933

> ## 제곱근 변환
> data2 <- transform(data2, 
+                    uni_sqrt = sqrt(data2$Uniformity))
> hist(data2$uni_sqrt)
> qqnorm(data2$uni_sqrt)
> qqline(data2$uni_sqrt)
> shapiro.test(data2$uni_sqrt)

	Shapiro-Wilk normality test

data:  data2$uni_sqrt
W = 0.93482, p-value = 0.066

> wafers = read_excel("/Users/ot/R_DSfQM/spc/Uniformity_Data.xlsx")
                                                                                                                                                          
> wafers = as.data.frame(wafers)
> wafers
   Wafer Uniformity
1      1         11
2      2         16
3      3         22
4      4         14
5      5         34
6      6         22
7      7         13
8      8         11
9      9          6
10    10         11
11    11         11
12    12         23
13    13         14
14    14         12
15    15          7
16    16         15
17    17         16
18    18         12
19    19         11
20    20         18
21    21         14
22    22         13
23    23         18
24    24         12
25    25         13
26    26         12
27    27         15
28    28         21
29    29         21
30    30         14
> dim(wafers)
[1] 30  2
> ### X_bar in Table ###
> x_bar_phaseI = mean(wafers[,2])
> x_bar_phaseI = round(x_bar_phaseI, 3)
> x_bar_phaseI
[1] 15.067
> ### MR ###
> x_1_phaseI = wafers[2:30,2]
> x_0_phaseI = wafers[1:29,2]
> MR_phaseI = abs(x_1_phaseI - x_0_phaseI)
> MR_phaseI
 [1]  5  6  8 20 12  9  2  5  5  0 12  9  2  5  8  1  4  1  7  4  1  5  6  1  1  3  6  0  7
> ### MR_bar ###
> MR_bar_phaseI = mean(MR_phaseI)
> MR_bar_phaseI = round(MR_bar_phaseI, 4)
> MR_bar_phaseI
[1] 5.3448
> # UCL of I chart
> d2 = 1.128
> UCL_I = x_bar_phaseI + 3*MR_bar_phaseI/d2
> UCL_I = round(UCL_I, 3)
> UCL_I
[1] 29.282
> # LCL of I chart
> LCL_I = x_bar_phaseI - 3*MR_bar_phaseI/d2
> LCL_I = round(LCL_I, 3)
> LCL_I
[1] 0.852
> ### plot of I chart ### x_bar
> par(mai = c(1, 1, 0.5, 2))
> plot(wafers[,2], xlab = "wafer", ylab = "Individual value",
+      pch = 19, col = 4, type = "o",
+      xlim = c(0, 30), ylim = c(0, 35),
+      lwd = 3, mgp = c(1.5, 0.5, 0))
> abline(h = LCL_I) # LCL 라인의 숫자를 그래프에 나타내기 
> axis(4, at = LCL_I, las = 2, tck = 0,
+      labels = paste("LCL=", round(LCL_I, 3)),
+      mgp = c(0.5, 0.2, 0))
> abline(h = x_bar_phaseI)
> axis(4, at = x_bar_phaseI, las = 2, tck = 0,
+      labels = paste("Mean=", round(x_bar_phaseI, 3)),
+      mgp = c(2, 0.2, 0))
> abline(h = UCL_I)  # UCL 라인의 숫자를 그래프에 나타내기 
> axis(4, at = UCL_I, las = 2, tck = 0,
+      labels = paste("UCL=", round (UCL_I, 3)),
+      mgp = c(2, 0.2, 0))
> ### UCL of MR chart ###
> D4 = 3.267
> UCL_MR = D4*MR_bar_phaseI
> UCL_MR = round (UCL_MR, 3)
> UCL_MR
[1] 17.461
> # LCL of MR chart
> D3 = 0
> LCL_MR = D3*MR_bar_phaseI
> LCL_MR
[1] 0
> ### plot of MR chart ###
> par(mai = c(1, 1, 0.5, 2))
> plot(MR_phaseI, xlab = "R_bar", ylab = "Moving range",
+      pch = 19, col = 4, type = "o",
+      xlim = c(0, 30), ylim = c(0, 25),
+      lwd = 3, mgp = c(1.5, 0.5, 0))
> abline(h = LCL_MR)
> axis(4, at = LCL_MR, las = 2, tck = 0,
+      labels = paste("LCL=", round(LCL_MR, 3)),
+      mgp = c(2, 0.2, 0))
> abline(h = MR_bar_phaseI)
> axis(4, at = MR_bar_phaseI, las = 2, tck = 0,
+      labels = bquote(bar(R) == .(round(MR_bar_phaseI, 4))),
+      mgp = c(2, 0.2, 0))
> abline(h = UCL_MR)
> axis(4, at = UCL_MR, las = 2, tck = 0,
+      labels = paste("UCL=", round(UCL_MR, 3)),
+      mgp = c(2, 0.2, 0))
> 
