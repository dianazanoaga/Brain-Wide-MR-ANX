#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(readxl)
library(data.table)

batch_args <- read_excel(args[1])
batch_args <- t(batch_args)
print(batch_args[1])

for (i in seq_along(batch_args)) {
  id <- sub(".proc.txt.*", "", batch_args[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/brain_analysis/input/", "", id)
  brain_all <- fread(i)
  brain_all <- as.data.frame(brain_all)
  write.csv(brain_all, file=paste0("/gpfs/gibbs/pi/polimanti/diana/files_format/", id, ".txt")
}
