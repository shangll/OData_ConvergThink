setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data")
dat = read.table(file = "ansRep.csv", head = TRUE, na.strings = "NA", sep = ",")

for (i in 19:30)
{
  accData = subset(dat, dat$Subject == i & dat$Correct == 1)
  redundant = accData$ans
  count = 0
  for (k in 1:(length(redundant)-1))
  {
    for (j in (k+1):length(redundant))
    {
      if (redundant[k] == redundant[j])
      {
        count = count + 1
      }
    }
  }
  print(count)
}