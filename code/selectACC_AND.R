setwd("C:\\Users\\shangll\\Documents\\NCKU\\sub\\data_spl")
dat = read.table(file = "a47.csv", head = TRUE, na.strings = "NA", sep = ",")

ans = dat$ansa
l = dat$stim
r = dat$stimb
left = c( )
right = c( )

for (h in 1:length(l))
{
  left[h] = as.character(l[h])
}
for (n in 1:length(r))
{
  right[n] = as.character(r[n])
}


for (i in 1:(length(ans)-1))
{
  for (j in (i+1):length(ans))
    {
      if ( i != j & ans[i] == ans[j] & 
           ((left[i] == left[j] & right[i] == right[j]) | 
            (right[i] == left[j] & left[i] == right[j])))
      dat$Correct[j] = 0
    }
}
write.table(dat, file = "C:\\Users\\shangll\\Documents\\NCKU\\sub\\data_spl\\ansa47.csv", sep=",")