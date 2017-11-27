dat <- read.csv("Sample3.csv", header = T)

library(stringr)

codeList <- dat$SecuritesCode

Mdirs <- list.dirs(path = "./AllData", recursive = F)
for (Dirs in Mdirs) {
    dirs <- list.dirs(path = Dirs, recursive = F)
    for(dir in dirs) {
        files <- list.files(dir, full.names = F)
        for ( file in files) {
            if (substr(toupper(file),1,8) %in% codeList) {
                tmpDf = read.csv(paste0(dir,"/",file), header = T, stringsAsFactors=FALSE)
                i = dat$Group[match(substr(toupper(file),1,8),codeList)]
                write.table(tmpDf, paste0("./Data/",str_pad(i, 2, pad = "0"),"/",substr(toupper(file),1,8),".csv"), row.names = F, col.names = F, append = T,sep = ",", fileEncoding = "UTF-8")
            }
        }
    }
}
