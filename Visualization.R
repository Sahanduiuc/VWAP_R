Dirs <- list.dirs(path = "./Result", recursive = F)

library(tidyr)
library(dplyr)
library(ggplot2)
library(stringi)

gnh = 10 # group name head

gnt = 11 # group name tail

# Daily Volume Error
alldM <- data.frame()
for(Dir in Dirs) {
    files <- list.files(path = Dir, full.names = T)
    alldMfiles<-files[str_detect(files,"alldM")]
    for(file in alldMfiles){
        tmp <- read.csv(file, header = T)
        tmp <-gather(tmp, Model, Measure, -X)
        tmp <- cbind.data.frame(substr(Dir,gnh,gnt),tmp)
        alldM <- rbind(alldM,tmp)
    }
}

colnames(alldM)<-c("Group","Date","Model","Measure")
alldMt <- ggplot(alldM,aes(x = Group, y = Measure))+geom_boxplot(aes(colour = Model))+ggtitle("Daily Volume Estimation Error")
alldMt <- alldMt + scale_y_continuous(limit = c(0,5))
ggsave("AllD.jpg", alldMt ,width = 30, height = 15, units = "cm")


Methods <- c("LWAP", "sWAP", "LWAPS", "LWAPF", "MWAP", "MWAPS", "MWAPF", "M1WAP","M1WAPF","M1WAPS","M2WAP","M2WAPS","M2WAPF", "TWAP")


getMethod <- function(x){
    substr(x,21,nchar(x)-4)
}

getCode <- function(x){
    substr(x,13,20)
}

getGroup <- function(x){
    substr(x,10,11)
}

AllMinList <- data.frame()
AllAllWAP <- data.frame()

for(Dir in Dirs) {
    AllWAP <- data.frame()
    files <- list.files(path = Dir, full.names = T)
    WAPfiles <- files[str_detect(files,"WAP")]
    Group <- getGroup(WAPfiles[1])
    for(file in WAPfiles) {
        Code <- getCode(file)
        VWAP <- read.csv(paste0(substr(file,1,20),"VWAP.csv"),header = T, row.names = 1)
        tmp <- read.csv(file, header = T)
        if (ncol(tmp) != 3) next
        tmp <- spread(tmp, Slot, Slip)
        tmp[,2:481] <- as.matrix(tmp[,2:481]) * as.matrix(VWAP) / rowSums(VWAP)
        tmp <- gather(tmp, Slot, Slip, -Date)
        tmp <- aggregate(Slip ~ Date, tmp , sum)
        tmp <- tmp[is.finite(tmp$Slip),]
        Method <- getMethod(file)
        df <- cbind.data.frame(Group,Code,Method,tmp)
        AllWAP <- rbind.data.frame(df,AllWAP)
    }
    AllAllWAP <- rbind.data.frame(AllWAP,AllAllWAP)
    # t <- ggplot(AllWAP,aes(x = Group, y = Slip, colour = Method))+ geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits = c( 0,quantile(AllWAP$Slip, 0.9, na.rm = T)))
    # ggsave(paste("./Summary/",Group,"AllWAP.jpg"),t)
    # MS <- AllWAP %>% group_by(Code,Method) %>% summarize(WAPMean = mean(Slip, na.rm = T) , WAPSD = sd(Slip, na.rm = T))
    # write.csv(MS,paste("./Summary/",Group,"MS.csv"))
    # MinList <- MS %>% group_by(Code) %>% summarize(MinMethod = Method[which.min(WAPMean)])
    # MinList$Group <- Group
    # AllMinList<-rbind.data.frame(AllMinList,MinList)
    print(Dir)
}

t <- ggplot(AllAllWAP,aes(x = Group, y = Slip, colour = Method))+ geom_boxplot(outlier.shape=NA) + scale_y_continuous(limits = c( 0,3))
t <- t + ggtitle("Method Comparison over 11 Groups")
ggsave(paste("./Summary/","1AllWAP.jpg"),t, width = 40, height = 10)


write.csv(AllMinList,"MinList.csv")