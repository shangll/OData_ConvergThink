setwd("D:\\Usr\\Documents\\NCKU\\new")
dat <- read.table("Sig2.5ANDdel.txt", header=T)

highCRAT <- c(17,19,34,42,45,8,20,24,25,26,7,15) 
lowCRAT <- c(16,30,18,11,28,37,44,1,3,6,31,43)
highPST <- c(35,10,32,21,29,17,6,7,24,8)
lowPST <- c(20,30,23,14,39,16,31,15,11,28,34)

# high CRAT
hc <- data.frame()

for (j in 1:length(highCRAT))
{
for (i in 1:23316)
{
if (dat$Subject[i] == highCRAT[j])
{
  hc = rbind(hc, dat[i,])
}
}
}
write.table(hc, file = "D:\\Usr\\Documents\\NCKU\\new\\HighCRAT.txt", 
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

# low CRAT
lc <- data.frame()
for (j in 1:length(lowCRAT))
{
for (i in 1:23316)
{
  if (dat$Subject[i] == lowCRAT[j])
  {
    lc = rbind(lc, dat[i,])
  }
}
}
  
write.table(lc, file = "D:\\Usr\\Documents\\NCKU\\new\\LowCRAT.txt", 
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

# high PST
hp <- data.frame()
for (j in 1:length(highPST))
{
  for (i in 1:23316)
  {
    if (dat$Subject[i] == highPST[j])
    {
      hp = rbind(hp, dat[i,])
    }
  }
}

write.table(hp, file = "D:\\Usr\\Documents\\NCKU\\new\\HighPST.txt", 
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)

#low PST
lp <- data.frame()
for (j in 1:length(lowPST))
{
  for (i in 1:23316)
  {
    if (dat$Subject[i] == lowPST[j])
    {
      lp = rbind(lp, dat[i,])
    }
  }
}
write.table(lp, file = "D:\\Usr\\Documents\\NCKU\\new\\LowPST.txt", 
            append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)