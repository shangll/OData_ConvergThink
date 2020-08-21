setwd("D:/SyncDocs/MyJob/converg/data")
dat <- read.table("AccANDrepANS.txt",header=T,na.strings="NA",sep="\t",encoding='GBK')
til <- cbind("Subject","Condition","RT","Correct","Channel1","Channel2",
             "Ans","Cue","Cue2")
write.table(til, file = "AccANDnonRep.txt",append = TRUE, sep="\t",
            row.names = FALSE, col.names = FALSE)

ord <- c(1:3,5:46)
for (sbj in ord){
  subjDat <- subset(dat,dat$Subject==sbj)
  leftTrial <- subset(subjDat,subjDat$Channel1==1 & subjDat$Channel2==0)
  rightTrial <- subset(subjDat,subjDat$Channel1==0 & subjDat$Channel2==1)
  bothTrial <- subset(subjDat,subjDat$Channel1==1 & subjDat$Channel2==1)
  ansal <- leftTrial$Ans
  ansar <- rightTrial$Ans
  ansab <- bothTrial$Ans
  l <- leftTrial$Cue
  r <- rightTrial$Cue
  bl <- bothTrial$Cue2
  br <- bothTrial$Cue
  ansl <- c()
  ansr <- c()
  ansb <- c()
  left <- c()
  right <- c()
  bothl <- c()
  bothr <- c()
  
  for (al in 1:length(ansal))
  {
    ansl[al] = as.character(ansal[al])
  }
  for (ar in 1:length(ansar))
  {
    ansr[ar] = as.character(ansar[ar])
    
  }
  for (ab in 1:length(ansab))
  {
    ansb[ab] = as.character(ansab[ab])
  }
  for (h in 1:length(l))
  {
    left[h] = as.character(l[h])
  }
  for (n in 1:length(r))
  {
    right[n] = as.character(r[n])
  }
  for (k in 1:length(bl))
  {
    bothl[k] = as.character(bl[k])
  }
  for (m in 1:length(br))
  {
    bothr[m] = as.character(br[m])
  }
  
  singleCue <- c(right,left)
  singleAns <- c(ansr,ansl)
  
  for (i in 1:(length(singleAns)-1))
  {
    for (j in (i+1):length(singleAns))
    {
      if ((i != j) & (singleAns[i] == singleAns[j]) & 
          (singleCue[i] == singleCue[j]))
        subjDat$Correct[j] = 0
    }
  }
  
  for (i in 1:(length(ansb)-1))
  {
    for (j in (i+1):length(ansb))
    {
      if ((i != j) & (ansb[i] == ansb[j]) & 
          (((bothl[i] == bothl[j]) & (bothr[i] == bothr[j])) |
          ((bothr[i] == bothl[j]) & (bothl[i] == bothr[j]))))
        subjDat$Correct[length(singleAns)+j] = 0
    }
  }

  write.table(subjDat, file = "AccANDnonRep.txt",append = TRUE, sep="\t",
              row.names = FALSE, col.names = FALSE)

}