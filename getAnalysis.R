dirs <- list.dirs("./Data", recursive = F)

dirs <- rev(dirs)

intr = 30
allSec = 2*60*60
interSec = 1.5*60*60
nDayLag1 = 5
nDayLag = 50

Methods <- c("LWAP", "sWAP", "LWAPS", "LWAPF", "MWAP", "MWAPS", "MWAPF", "M1WAP","M1WAPF","M1WAPS","M2WAP","M2WAPS","M2WAPF", "TWAP")

# Strategy <- function(x, V, s, threshold = 0.9){
#     xsum <- cumsum(x)
#     t = which(xsum/V > threshold) [1]
#     if(is.na(t) || t > 450){
#         t = 450
#     }
#     d <- c((x/V)[1:t], (1-(xsum/V)[t])*Normalize(tail(s, 480 - t)))
#     # d <- c((x/V)[1:t], rep((1-(xsum/V)[t])/(480 - t), 480 - t))
#     return(d)
# }

Strategy1 <- function(x,V,s) {
    d <- x/V
    for (t in 241:479) {
        d[t] = d[t]*(1-sum(d[1:(t-1)])) / sum(s[t:480] / 480)
    }
    d[480] = 1- sum(d[1:479])
    return(d)
}

Strategy2 <- function(x,V,s) {
    d <- x/V
    for (t in 241:470) {
        d[t] = d[t]*(1-sum(d[1:(t-1)])) / sum(s[t:480] / 480)
    }
    d[471:480] <- (1 - sum(d[1:470])) /10 # hyper parameter
    return(d)
}


Strategy3 <- function(x,V,s) {
    d <- x/V
    for (t in 241:471) {
        d[t] = d[t]*(1-sum(d[1:(t-1)])) / sum(s[t:480] / 480)
    }
    d[471:480] = tail(seq(d[470], (1 - sum(d[1:470])) / 5 - d[471], length.out = 11) ,-1)
    return(d)
}

Normalize <- function(x){
    x/sum(x)
}

Slot = c(cumsum(rep(1,allSec/intr)), (interSec+allSec)/intr+cumsum(rep(1,allSec/intr)))

firstdiff <- function(x) {
    shifted <- c(0,x[1:(length(x)-1)])
    x-shifted
}

library(tidyr)
library(dplyr)
library(glmnet)
library(lubridate)
library(forecast)

getL <- function(x,y,t){
    a = (mean(x)*mean(y) - mean(x*y)) /( mean(x)*mean(x) - mean(x*x))
    b = mean(y) - a* mean(x)
    return(a*t+b)
}


RollLinear <- function(x, n = 5){
    t = rep(0,n)
    for(i in (n+1) : length(x)){
        var <- getL(1:n,x[(i-n):(i-1)],n+1)
        t<-append(t,var)
    }
    return(t)
}

RollMean <- function(x,n = 5){
    t = rep(0,n)
    for(i in (n+1) : length(x)){
        var <- mean(x[(i-n):(i-1)])
        t<-append(t,var)
    }
    return(t)
}



for (dir in dirs){
    files <- list.files(dir, full.names = T)
    for (file in files) { # 请注意异常处理
        # get Basic Frame
        tmpDf <- read.csv(file, header = F)
        colnames(tmpDf) <- c("MarketCode","SecuritiesCode","DateTime","Price","TurnOver","Volume")
        tmpDf$Date <- format(as.Date(tmpDf$DateTime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
        tmpDf <- tmpDf[order(as.Date(tmpDf$Date, format="%Y-%m-%d")),] 
        CallAuction <- subset(tmpDf, strptime(DateTime, "%Y-%m-%d %H:%M:%S") < strptime(paste(Date,"09:30:00"), "%Y-%m-%d %H:%M:%S"), select = c(Date, Volume))
        colnames(CallAuction) <- c("Date", "CallAuction") 
        CallAuction <- aggregate(CallAuction ~ Date, CallAuction, sum)
        tmpDf <- subset(tmpDf, strptime(DateTime, "%Y-%m-%d %H:%M:%S") > strptime(paste(Date,"09:30:00"), "%Y-%m-%d %H:%M:%S"))
        # get all half sec volume 
        df <- data.frame(
            Date = as.character(),
            Slot = as.integer(),
            Price = as.numeric(),
            Volume = as.integer()
        )
        for (date in unique(tmpDf$Date)){
            tmp <- subset(tmpDf, Date == date)
            if ( nrow(tmp) < 50 ) next
            tmp$DateTime <- as.POSIXct(as.character(tmp$DateTime), format =  "%Y-%m-%d %H:%M:%S")
            start1 = strptime(paste(date, "09:30:00"),"%Y-%m-%d %H:%M:%S")
            start2 = strptime(paste(date, "13:00:00"),"%Y-%m-%d %H:%M:%S")
            TimeStamp <- c(start1 + cumsum(rep(intr,allSec/intr)), start2 + cumsum(rep(intr,allSec/intr)))
            i = as.integer(tmp$DateTime - start1)
            j = as.integer(TimeStamp - start1)
            Volume <- cumsum(tmp$Volume)
            Volume <- firstdiff(approx(c(0,i),c(0,Volume),xout = j)$y)
            Price <- approx(c(0,i),c(mean(tmp$Price),tmp$Price),xout = j)$y
            df <-rbind.data.frame(df, cbind.data.frame(date, Slot, Price, Volume))
        }
        hMinV <- aggregate(Volume ~ date + Slot, FUN = mean, data = df)
        hMinV <- spread(hMinV, Slot, Volume)
        hMinP <- aggregate(Price ~ date + Slot, FUN = mean, data = df)
        hMinP <- spread(hMinP, Slot, Price)
        # The XY data
        tmp <- aggregate(Volume ~ Date, tmpDf, sum) # Volume of every day
        tmp <- merge(tmp, CallAuction)
        if (nrow(tmp) < 80) next
        tmp$M <- RollMean(tmp$Volume)
        tmp$L <- RollLinear(tmp$Volume)
        dM <- tail(abs(tmp$M - tmp$Volume) / tmp$Volume, -(nDayLag + nDayLag1))
        dL <- tail(abs(tmp$L - tmp$Volume) / tmp$Volume, -(nDayLag + nDayLag1))
        tmp$Wday <- as.factor(wday(tmp$Date))
        # X <- model.matrix(~. + 0, tmp[,3:6])
        X <- as.matrix(tmp[,3:5])
        Y <- tmp$Volume
        # Model 1 diagnosis loop 
        M1pred <- rep(0,length(Y))
        coef <- matrix(0, ncol = 4, nrow = length(Y))
        for (i in (nDayLag1 + 1 + nDayLag):length(Y)){
            Xnew = X[(i - nDayLag):(i-1),]
            Ynew = Y[(i - nDayLag):(i-1)]
            cvfit = cv.glmnet(Xnew,Ynew, type.measure = "mse", nfolds = 3, alpha = 1)
            M1pred[i] <- predict(cvfit ,newx = t(X[i,]) , s = "lambda.min")
            coef[i,]<- as.vector(coef(cvfit, s = "lambda.min"))
        }
        coef <- tail(cbind(M1pred, coef), -(nDayLag + nDayLag1))
        write.csv(coef,paste0("./Result/",substr(file,8,18),"CoefM1.csv"))
        dM1 <- tail(abs(M1pred - tmp$Volume) / tmp$Volume, -(nDayLag + nDayLag1))
        tmp$M1 <- M1pred
        # Wdat move M2 
        w <- tmp %>% group_by(Wday) %>% summarise(wmean = mean(Volume, na.rm = T))
        w <- as.data.frame(w)$wmean
        Y <- Y - w[wday(tmp$Date) - 1]
        M2pred <- rep(0,length(Y))
        coef <- matrix(0, ncol = 4, nrow = length(Y))
        for (i in (nDayLag1 + 1 + nDayLag):length(Y)){
            Xnew = X[(i - nDayLag):(i-1),]
            Ynew = Y[(i - nDayLag):(i-1)]
            cvfit = cv.glmnet(Xnew,Ynew, type.measure = "mse", nfolds = 3, alpha = 1)
            M2pred[i] <- predict(cvfit ,newx = t(X[i,]) , s = "lambda.min")
            coef[i,]<- as.vector(coef(cvfit, s = "lambda.min"))
        }
        tmp$M2 <- M2pred
        tmp$M2 <- tmp$M2 + w[wday(tmp$Date) - 1]
        coef <- tail(cbind(tmp$M2, coef), -(nDayLag + nDayLag1))
        write.csv(coef,paste0("./Result/",substr(file,8,18),"CoefM2.csv"))
        dM2 <- tail(abs(tmp$M2 - tmp$Volume) / tmp$Volume, -(nDayLag + nDayLag1))
        #summary
        alld <- data.frame(dL, dM, dM1, dM2) # check length
        colnames(hMinV)[1]<-"Date"
        hMinV <- merge(hMinV, tmp)
        colnames(hMinP)[1]<-"Date"
        hMinP <- hMinP[hMinP$Date %in% hMinV$Date,] # !pay a attention!
        write.csv(alld,paste0("./Result/",substr(file,8,18),"alldM.csv") )
        fResults <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        sResults <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        LResults <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        MResults <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M1Results <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M2Results <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        TResults <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        LResultsF <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        MResultsF <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M1ResultsF <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M2ResultsF <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        LResultsS <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        MResultsS <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M1ResultsS <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        M2ResultsS <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        Best <- matrix(data = 1/480, ncol = 480, nrow = nrow(hMinV))
        for (i in (nDayLag + nDayLag1 + 1):nrow(hMinV)){ # 把所有东西都放在这里
            best <- Normalize(t(hMinV[i,2:481]))
            s = apply(as.matrix(hMinV[(i-6):(i-1),2:481]),2,mean, na.rm = T)
            head <- s[1]
            midhead <- s[241]
            s = s / sum(s) * 480
            ARm <- Arima(as.numeric(head(hMinV[i-1,2:481])/ s, 240), order = c(1,0,0))
            hMinPred1 <- sapply(t(hMinV[i,2:240]) / s[1:239],function(x) ARm$coef[1]*(x-ARm$coef[2])+ARm$coef[2])*s[2:240]
            hMinPred1 <- c(head,hMinPred1)
            ARm <- Arima(as.numeric(tail(hMinV[i-1,2:481])/ s , 240), order = c(1,0,0))
            hMinPred2 <- sapply(t(hMinV[i,242:480]) / s[241:479] ,function(x) ARm$coef[1]*(x-ARm$coef[2])+ARm$coef[2])* s[242:480]
            hMinPred2 <- c(midhead,hMinPred2)
            hMinPred <- c(hMinPred1,hMinPred2)
            fResults[i,] <- abs(hMinPred - t(hMinV[i,2:481]))
            sResults[i,] <- abs(s / 480)
            LResults[i,] <- abs(Strategy1(hMinPred, hMinV$L[i],s))
            MResults[i,] <- abs(Strategy1(hMinPred, hMinV$M[i],s))
            M1Results[i,] <- abs(Strategy1(hMinPred, hMinV$M1[i],s))
            M2Results[i,] <- abs(Strategy1(hMinPred, hMinV$M2[i],s))
            LResultsF[i,] <- abs(Strategy2(hMinPred, hMinV$L[i],s))
            MResultsF[i,] <- abs(Strategy2(hMinPred, hMinV$M[i],s))
            M1ResultsF[i,] <- abs(Strategy2(hMinPred, hMinV$M1[i],s))
            M2ResultsF[i,] <- abs(Strategy2(hMinPred, hMinV$M2[i],s))
            LResultsS[i,] <- abs(Strategy3(hMinPred, hMinV$L[i],s))
            MResultsS[i,] <- abs(Strategy3(hMinPred, hMinV$M[i],s))
            M1ResultsS[i,] <- abs(Strategy3(hMinPred, hMinV$M1[i],s))
            M2ResultsS[i,] <- abs(Strategy3(hMinPred, hMinV$M2[i],s))
            Best[i,] <- best
            TResults[i,]<- abs(1/480)
        }
        fResults <- tail(fResults,-55)
        write.csv(fResults,paste0("./Result/",substr(file,8,18),"forcastResult.csv"))
        Price <- tail(hMinP[,2:481],-55)
        Best <- tail(Best, -55)
        write.csv(Best,paste0("./Result/",substr(file,8,18),"Best.csv"))
        VWAP <- Best * Price
        write.csv(Price,paste0("./Result/",substr(file,8,18),"VWAP.csv"))
        for (Methodstr in Methods) {
          Result <- tail(eval(parse(text = gsub("WAP", "Results", Methodstr))),-55)
          Method <- as.data.frame(abs (Result - Best)* Price / VWAP) # change !
          colnames(Method) <- 1:480
          Method$Date <- tail(hMinP[,1],-55)
          Method <- gather(Method, Slot, Slip, -Date)
          write.csv(Method,paste0("./Result/",substr(file,8,18),Methodstr,".csv"), row.names = F)
        }
        print(paste0("Done", substr(file,8,18)))
    }
}

