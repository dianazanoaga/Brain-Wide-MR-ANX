#!/usr/bin/env Rscript


library(readxl)
library(data.table)
library("plyr")
library("dplyr")
library(meta)
library(purrr)
library(metafor)
library(rio) # per leggere piu sheets di R
library(WriteXLS)
library(writexl)
library(MendelianRandomization)
library(tidyverse)
library(TwoSampleMR)



my_list = c("/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0953.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/0043.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1961.txt",
            "/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1511.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/1437.txt","/gpfs/gibbs/pi/polimanti/diana/files_format_nuovi/3915.txt")

ukbio_all <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")

# read all the brains
for(i in 1:length(my_list)) {  # assign function within loop
  
  brain <- fread(my_list[i])
  #brain_sig_relaxed <- brain[brain$pvalue < 1*10^(-5)]
  assign(paste0("brain_", i), format_data(brain, 
                                          type="exposure", 
                                          phenotype_col = "brain",
                                          snp_col = "rsid",
                                          beta_col = "beta",
                                          se_col = "se",
                                          effect_allele_col = "a2",
                                          other_allele_col = "a1",
                                          pval_col = "pvalue"))
}



# find snps where at least one is less tha threshold significant 
common_rows <- intersect(intersect(intersect(intersect(intersect(brain_1$SNP, brain_3$SNP), brain_4$SNP), brain_5$SNP), brain_2$SNP), brain_6$SNP) #0

dt_all <- as.data.frame(matrix(ncol=13))
colnames(dt_all) <- colnames(brain_2)

for(i in c(1:1205886)){
  print(i)
  x = min(brain_1$pval.exposure[i], brain_2$pval.exposure[i],brain_3$pval.exposure[i],
          brain_4$pval.exposure[i], brain_5$pval.exposure[i],brain_6$pval.exposure[i])
  
  if(x < 1*10^(-5)){
    
    if(x == brain_1$pval.exposure[i]){
      dt_all <- rbind(dt_all, brain_1[i,])
    } else if(x == brain_2$pval.exposure[i]){
      dt_all <- rbind(dt_all, brain_2[i,])
    } else if(x == brain_3$pval.exposure[i]){
      dt_all <- rbind(dt_all, brain_3[i,])
    } else if(x == brain_4$pval.exposure[i]){
      dt_all <- rbind(dt_all, brain_4[i,])
    } else if(x == brain_4$pval.exposure[i]){
      dt_all <- rbind(dt_all, brain_5[i,])
    } else{
      dt_all <- rbind(dt_all, brain_6[i,])
    }
  }
}


dt_all_sig <- unique(dt_all[dt_all$pval.exposure < 1*10^(-5),])

# clump data!
dt_all_sig_clump <- clump_data(dt_all_sig)
write_xlsx(dt_all_sig_clump, "/gpfs/gibbs/pi/polimanti/diana/multivariable_exp_input_clump_harm.xlsx")


# take only the selected snps from the original dataset
for(i in 1:6){
  
  assign(paste0("brain_selected_snps_", i), get(paste0("brain_", i))[get(paste0("brain_", i))$SNP %in% dt_all_sig_clump$SNP,])
}

# select these snps also from the mvp and HARMONIZE
for(i in 1:6) {
  
  print(i)
  assign(paste0("ukbio_out_",i), read_outcome_data(
    snps = get(paste0("brain_selected_snps_", i))$SNP,
    sep = ",",
    filename = "/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt",
    snp_col = "rsid",
    beta_col = "beta",
    se_col = "se",
    effect_allele_col = "alt.x",
    other_allele_col = "ref.x",
    pval_col = "pval_EUR"))
  

  
  assign(paste0("b_ukbio_",i, "_har"), harmonise_data(
    exposure_dat = get(paste0("brain_selected_snps_", i)), 
    outcome_dat = get(paste0("ukbio_out_", i))
    
  ))
}

# Take only colonne che vogliamo
for(i in 1:6){
  assign(paste0("b_ukbio_short_", i, "_har"), get(paste0("b_ukbio_", i, "_har"))[,c(1:7, 16, 17, 24, 25)])
}


common_rows_new <- list(b_ukbio_short_1_har, b_ukbio_short_2_har, b_ukbio_short_3_har,
                        b_ukbio_short_4_har, b_ukbio_short_5_har, b_ukbio_short_6_har)

common_rows_new <- as.data.frame(common_rows_new)

# START MULTIVARIABLE

m1 = matrix(common_rows_new$beta.exposure)
m2 =matrix(common_rows_new$beta.exposure.1)
m3 =matrix(common_rows_new$beta.exposure.2)
m4 =matrix(common_rows_new$beta.exposure.3)
m5 =matrix(common_rows_new$beta.exposure.4)
m6 = matrix(common_rows_new$beta.exposure.5)

m1s = matrix(common_rows_new$se.exposure)
m2s =matrix(common_rows_new$se.exposure.1)
m3s =matrix(common_rows_new$se.exposure.2)
m4s =matrix(common_rows_new$se.exposure.3)
m5s =matrix(common_rows_new$se.exposure.4)
m6s = matrix(common_rows_new$se.exposure.5)



MRMVInputObject <- mr_mvinput(bx = cbind(m1,m2,m3,m4,m5,m6),
                              bxse = cbind(m1s, m2s, m3s, m4s, m5s, m6s),
                              by = common_rows_new$beta.outcome,
                              byse = common_rows_new$se.outcome,
                              snps = common_rows_new$SNP)

MR_multi <- mr_mvivw(MRMVInputObject)
MR_multi

mvp_multivariable_result <- data.frame("exposure" = MR_multi@Exposure, 
                                       "outcome" = rep("mvp", 6),
                                       "estimate" = MR_multi@Estimate,
                                       "std error" = MR_multi@StdError,
                                       "low_CI" = MR_multi@CILower,
                                       "high_CI" = MR_multi@CIUpper,
                                       "pvalue" = MR_multi@Pvalue,
                                       "snp" = rep(MR_multi@SNPs,6))

write_xlsx(mvp_multivariable_result, "/gpfs/gibbs/pi/polimanti/diana/ukbio_multivariableMr_harm.xlsx")



