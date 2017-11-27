Mdir <- list.dirs(path = "./AllData/201709", recursive = F)

d = 0

AllDf <- matrix(0 , ncol = 21, nrow = 3807)

nday = length(Mdir)

getStock <- function(x) {
  toupper(substr(x,1,8))
}

for(dir in Mdir){
    files <- list.files(dir, full.names = F)
    if ( d == 0 ) {
      Stocks <- getStock(files)
    }
    d = d + 1
    for ( file in files ){
        tmpDf = read.csv(paste0(dir,"/",file), header = T, stringsAsFactors=FALSE)
        Date <- format(as.Date(tmpDf$DateTime, format="%Y-%m-%d %H:%M:%S"), "%Y-%m-%d")
        tmpDf <- subset(tmpDf, strptime(DateTime, "%Y-%m-%d %H:%M:%S") > strptime(paste(Date,"09:30:00"), "%Y-%m-%d %H:%M:%S"))
        TotalVolume <- sum(tmpDf$Volume)
        i = match(toupper(substr(file,1,8)), Stocks)
        AllDf[i,d] = TotalVolume
    }
}

Dat<- rowMeans(AllDf,na.rm = T)

Out<-cbind.data.frame(Stocks,Dat)
colnames(Out)<-c("SecuritesCode","VolumeSum")
write.csv(Out,"VolumeSum.csv",row.names = F)
