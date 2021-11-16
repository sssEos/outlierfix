library(outlierfix)
dt <- data.frame(age= c(23, 26, 21, 31, 38, 34, 25,11, 32, 36 ,38, 35 ,30 ,35, 20),income=c(1200,1400,1350,1600,7580,3620,2500,4230,4830,3820,5230,3360,2860,3120,2970))
fixout_conti(dt,col_name="income")
fixout_conti(dt,col_name="income",iden.m = "asymmetric")
fixout_conti(dt,col_name="age", rangeLU = c(20,35))

dt <- data.frame(price= c(150,20,65,28,75,3,4,22,16,25,28,30,35,47),sales = c(20,60,58,67,2750,2,58,67,32,35,45,67,78,65))
fixout_conti(dt,col_name="price", log=TRUE)
fixout_conti(dt,col_name="price", iden.m="winsorize")

