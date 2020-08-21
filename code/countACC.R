setwd("D:/SyncDocs/MyJob/Converg/manuscript_JEPG/data")
#setwd("//VUW/Personal$/Homes/S/shangl/My Documents/NCKU/converg/results")
dat <- read.table("ANDsd3.txt", header=T, na.strings="NA", sep="\t")
cn <- cbind("left","right","single","both")
write.table(cn, file = "ANDcountACC.txt", append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)
ord <- c(1:46)
for (n in ord){
  dat.AND <- subset(dat, dat$Correct == 1 & dat$Subject == n)
  
  HH.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 1)
  HX.AND <- subset(dat.AND, dat.AND$Channel1 == 1 & dat.AND$Channel2 == 0)
  XH.AND <- subset(dat.AND, dat.AND$Channel1 == 0 & dat.AND$Channel2 == 1)
  b<-nrow(HH.AND)
  l<-nrow(HX.AND)
  r<-nrow(XH.AND)
  s<-l+r
  vals<-cbind(l,r,s,b)
  write.table(vals, file = "ANDcountACC.txt", append = TRUE, sep="\t",
              row.names = FALSE, col.names = FALSE)
}