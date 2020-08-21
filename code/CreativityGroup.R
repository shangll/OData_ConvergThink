setwd("D:\\SL_NCKU\\new")
dat = read.table(file='accAndSig3Del.txt', head=T)
for (i in 1:22163)
{
  if (dat[i,]$Subject == 8 || dat[i,]$Subject == 17 || dat[i,]$Subject == 19 || dat[i,]$Subject == 20 || dat[i,]$Subject == 24 || dat[i,]$Subject == 25 || dat[i,]$Subject == 26 || dat[i,]$Subject == 34 || dat[i,]$Subject == 42 || dat[i,]$Subject == 45)
    {
      write.table(dat[i,], file = "D:\\SL_NCKU\\new\\AndCRAThigh.txt", append = TRUE, sep="\t", row.names = FALSE, col.names = FALSE)
    }
}