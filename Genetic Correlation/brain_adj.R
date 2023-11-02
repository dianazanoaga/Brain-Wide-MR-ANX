
rm(list=ls())

library(readxl)
library("xlsx")
library(purrr)
library(plyr)
library("stringr")

finngen_brain_corr_all <- read_excel("Downloads/finngen_brain_corr_all.xlsx")
brain_brain_summaryfile <- read_excel("brain_brain_summaryfile.xlsx")
brain_brain_summaryfile$p_adj <- brain_brain_summaryfile$p
sig <- brain_brain_summaryfile[brain_brain_summaryfile$p_adj <0.05,] # 86 out of 156 are significant

for(i in 1:156){
  
  input_string1<- brain_brain_summaryfile$p1[i]
  number1 <- str_extract(input_string1, "[0-9]+")
  prefix <- "/gpfs/gibbs/pi/polimanti/diana/output/"
  suffix <- ".proc.txt.out.sumstats.gz"
  new_string1 <- paste0(prefix, number1, suffix)
  brain_brain_summaryfile$`IDP p1`[i] <- finngen_brain_corr_all$`IDP short name`[finngen_brain_corr_all$p2==new_string1]
  input_string2 <- brain_brain_summaryfile$p2[i]
  number2 <- str_extract(input_string2, "[0-9]+")
  prefix <- "/gpfs/gibbs/pi/polimanti/diana/output/"
  suffix <- ".proc.txt.out.sumstats.gz"
  new_string2 <- paste0(prefix, number2, suffix)
  brain_brain_summaryfile$`IDP p2`[i] <- finngen_brain_corr_all$`IDP short name`[finngen_brain_corr_all$p2==new_string2]
  i=i+1
  
}

write.xlsx(brain_brain_summaryfile, file="correlation_between_brains.xlsx")

