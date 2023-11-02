#!/usr/bin/env Rscript

args = commandArgs(trailingOnly=TRUE)

library(MRlap)
library(remotes)
library(readxl)
library(data.table)
library("plyr")
library(MRInstruments)
library(ieugwasr)
library("ragg")
library(Cairo)
library(WriteXLS)
library(writexl)

uk_all_snp_rename <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")
uk_all_snp_rename <- rename(uk_all_snp_rename, c("alt" = "ref", "ref"= "alt"))

head(uk_all_snp_rename)

batch_args <- read_excel(args[1])
batch_args <- t(batch_args)
print(seq_along(batch_args))


for (i in seq_along(batch_args)) {
  
  brain_all <- fread(batch_args[i])
  brain_all <- as.data.frame(brain_all)
  brain_all_rename <- brain_all
  brain_all_rename <- rename(brain_all_rename, c("n" = "N", "a1"="a2", "a2" = "a1"))
  

  id <- sub(".txt.*", "", batch_args[i])
  id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format/", "", id)
  k <- paste0("brain", id)
  print(k)
  
  brain_all_rename_sig <- brain_all_rename[brain_all_rename$pvalue < 5*10^(-8),]
  uk_all_snp_rename_sig <- uk_all_snp_rename[uk_all_snp_rename$pval_EUR < 5*10^(-8),]
  print(dim(uk_all_snp_rename_sig)[1])
  print(dim(brain_all_rename_sig)[1])
  
  head(uk_all_snp_rename)
  
  A = MRlap(exposure = uk_all_snp_rename,
            exposure_name = "uk_anxiety",
            outcome = brain_all_rename,
            outcome_name = k,
            ld = "/gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr",
            hm3 = "/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist",
            MR_threshold= 1*10^(-5))
  
  
  A_Mr <- A$MRcorrection
  A_Mr <- as.data.frame(A_Mr)
  A_Mr <- A_Mr[1,-4]
  A_Mr_fin <- cbind(A_Mr, A$LDSC,A$GeneticArchitecture)
  A_Mr_fin$"exposure" <- "anxiety"
  A_Mr_fin$"outcome" <- k
  
  
  B = MRlap(exposure = brain_all_rename,
            exposure_name = k,
            outcome = uk_all_snp_rename,
            outcome_name = "uk_anxiety",
            ld = "/gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr",
            hm3 = "/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist",
            MR_threshold= 1*10^(-5))
  
  
  B_Mr <- B$MRcorrection
  B_Mr <- as.data.frame(B_Mr)
  B_Mr <- B_Mr[1,-4]
  B_Mr_fin <- cbind(B_Mr, B$LDSC,B$GeneticArchitecture)
  B_Mr_fin$"exposure" <- k
  B_Mr_fin$"outcome" <- "anxiety"
  
  list_data <- list("anx_brain" = A_Mr_fin ,"brain_anx" = B_Mr_fin)
  
  write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/ukbioa1a2", id, ".xlsx"))
  
  
}