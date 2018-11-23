#simulate returns under normal distribution
#set up initial values
library('quantmod') 
library(data.table)
#We use 'Wells Fargo Small Company Growth Inst (WSCGX)'fund as an example
getSymbols("WSCGX",src="yahoo",from = '2018-01-01', auto.assign = T)#load historical fund data from Yahoo finance
WSCGX
df <- WSCGX[,c(4)] #substract adj.close 
df <- data.table(df)
setnames(df,c('WSCGX.Close'),c('Close'))
#calculate NAV
df[, nav:= Close/shift(Close,1)]
df[1,nav:= 1]
df[2:.N,rtn:= nav-1]
df
#calculate portfolio's mean and stand deviation
ws.mean = mean(df[[3]],na.rm =TRUE)
ws.sigma = sd(df[[3]],na.rm=TRUE)
#simulate portfolio's return under normal distribution
NumDays = length(df$Close) #number of days
rtn.simu <- data.table(matrix(NA,nrow = NumDays))
#simulate 200 times
for (i in 1:200){
   rtn.simu[[i]] <- rnorm(NumDays,mean=ws.mean,sd=ws.sigma)
}
#calculate the annual return of 200 samples
library(matrixStats)
rtn.simu.acc = lapply(rtn.simu,function(x) product(x+1)-1) #accumlative return
rtn.simu.annual = lapply(rtn.simu.acc,function(x) (x/NumDays+1)^252-1) #annualized return
rtn.simu.summary = data.table(rtn.simu.acc,rtn.simu.annual)
rtn.simu.summary
#plot simulated returns' histagram
hist(as.numeric(rtn.simu.annual), 
     main="Histogram for fund's annualized returns", 
     xlab="annualized return", 
     border="#00CDCD", 
     col="#008B9B",
     xlim =c(-0.5,2))

