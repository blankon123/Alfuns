install.packages(c('car','xlsx'))
library(xlsx)
library(car)

data <- read.xlsx(file = 'perempuan.xls',sheetIndex = 1)
fit <- lm(AU~AGE+BMI, data=data)
summary(fit)

# Normalitas dari residual
# qq plot for studentized resid
qqPlot(fit, main="QQ Plot")
# distribution of studentized residuals
library(MASS)
sresid <- studres(fit) #karena relatif mengikuti garis lurus, maka model relatif normal
#gak pake uji saphirowilk dll karena yang diuji udah langsung model, kalo saphirowilk dll kan yang diuji 1 variabel, bukan 1 model
hist(sresid, freq=FALSE, 
     main="Distribution of Studentized Residuals")
xfit<-seq(min(sresid),max(sresid),length=40) 
yfit<-dnorm(xfit) 
lines(xfit, yfit)#plot menunjukkan relatif normal

# Untuk mengecek Homoskedastisitas
# Homoskedastis itu artinya varian dari y-cap relatif sama terhadap seluruh nilai x
# non-constant error variance test
ncvTest(fit)
# plot studentized residuals vs. fitted values 
spreadLevelPlot(fit) #relatif Random artinya 

# Untuk mengecek Multikolinieritas
# Multikol itu menunjukkan adanya hubungan antara variabel2 independen di regresi
# Asumsi yang dibutuhkan untuk Regresi yang baik adalah tidak adanya multikol
vif(fit) # Variance Inflation Factors 
sqrt(vif(fit)) > 2 # kalo akarnya > 2 brarti ada multikol, BMI+AGE tidak ada multikol

# Evaluate Nonlinearity
# component + residual plot 
crPlots(fit)
# Ceres plots 
ceresPlots(fit)

# Untuk mengecek Autokorelasi
# Autokol itu menunjukkan adanya hubungan antara antar nilai variabel dependen di time-series
# Asumsi yang dibutuhkan untuk Regresi yang baik adalah tidak adanya autokol
# Tes pake durbin-watson
durbinWatsonTest(fit) #nilai antara 0-4 , 2 berarti tidak ada autokol. 1.96 bagus,tidak ada autokol
