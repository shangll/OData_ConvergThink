library(sft)
setwd("D:/SyncDocs/MyJob/converg/results")
dat.AND <- read.table("ANDsd3rep.txt", header=T)

for (n in 1:46){
  dat <- subset(dat.AND, dat.AND$Correct == 1 & dat.AND$Subject == n)
  
  HXa <- subset(dat, dat$Channel1==1 & dat$Channel2==0)
  XHa <- subset(dat, dat$Channel1==0 & dat$Channel2==1)
  HHa <- subset(dat, dat$Channel1==1 & dat$Channel2==1)
  HX <- HXa$RT
  XH <- XHa$RT
  HH <- HHa$RT
  
  rt <- list(HH,HX,XH)
  tvec <- sort(unique(c(HX,XH,HH)))
  tm <- sort(unique(c(rt,recursive = TRUE)))
  nc <- length(rt) - 1
  cr <- vector("list", length(rt))
  for (i in 1:length(rt)) {cr[[i]] <- rep(1, length(rt[[i]]))}
  
  denomand <- estimateNAK(rt[[1]], cr[[1]])
  numerand <- estimateUCIPand(RT = rt[1+(1:nc)], CR = cr[1+(1:nc)])
  andct <- numerand$K(tm)/denomand$K(tm)
  andct[is.nan(andct)] <- NA
  andct[is.infinite(andct)] <- NA
  andct[andct==0] <- NA
  
  naset <- which(is.na(andct))
  andctdel <- andct[-which(is.na(andct))]
  tm <- tm[-naset]
  ctfinal <- approxfun(tm, andctdel)
  rt <- rt[-naset]
  cr <- cr[-naset]
  rmtestnew <- ucip.test(rt, cr, OR = FALSE)
  
  matplot(tvec,ctfinal(tvec),
          type='p', lty=1, main="Example Capacity Functions", xlab="Time",
          ylab="C(t)")
  abline(1,0)
  
  #rmtestnew[1]
  #rmtestnew[2]
  
  cz <- rmtestnew[1]
  pv <- rmtestnew[2]
  pvs <- format(pv, scientific = FALSE)
  vals <- cbind(cz,pvs)
  write.table(vals, file = "cztestrep.txt", append = TRUE, sep="\t",
              row.names = FALSE, col.names = FALSE)
}