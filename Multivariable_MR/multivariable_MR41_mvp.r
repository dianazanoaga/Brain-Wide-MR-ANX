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

mvp_all <- fread("/gpfs/gibbs/pi/polimanti/diana/gad_analysis/input_gad/dbGAP_GAD2eurMVP")


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



# all 6
common_rows <- intersect(intersect(intersect(intersect(intersect(brain_1$SNP, brain_3$SNP), brain_4$SNP), brain_5$SNP), brain_2$SNP), brain_6$SNP) #0
b_1 <- brain_1[brain_1$SNP %in% common_rows,]
b_2 <- brain_2[brain_2$SNP %in% common_rows,]
b_3 <- brain_3[brain_3$SNP %in% common_rows,]
b_4 <- brain_4[brain_4$SNP %in% common_rows,]
b_5 <- brain_5[brain_5$SNP %in% common_rows,]
b_6 <- brain_6[brain_6$SNP %in% common_rows,]

dt_all <- as.data.frame(matrix(ncol=13))
colnames(dt_all) <- colnames(b_2)

for(i in c(1:1205886)){
  print(i)
  x = min(b_1$pval.exposure[i], b_2$pval.exposure[i],b_3$pval.exposure[i],
          b_4$pval.exposure[i], b_5$pval.exposure[i],b_6$pval.exposure[i])
  
  if(x < 1*10^(-5)){
    
    if(x == b_1$pval.exposure[i]){
      dt_all <- rbind(dt_all, b_1[i,])
    } else if(x == b_2$pval.exposure[i]){
      dt_all <- rbind(dt_all, b_2[i,])
    } else if(x == b_3$pval.exposure[i]){
      dt_all <- rbind(dt_all, b_3[i,])
    } else if(x == b_4$pval.exposure[i]){
      dt_all <- rbind(dt_all, b_4[i,])
    } else if(x == b_5$pval.exposure[i]){
      dt_all <- rbind(dt_all, b_5[i,])
    } else{
      dt_all <- rbind(dt_all, b_6[i,])
    }
  }
}



dt_all_sig <- unique(dt_all[dt_all$pval.exposure < 1*10^(-5),])

dt_all_sig_clump <- clump_data(dt_all_sig)
head(dt_all_sig)
write_xlsx(dt_all_sig_clump, "/gpfs/gibbs/pi/polimanti/diana/multivariable_exp_input_clump.xlsx")


# START MULTIVARIABLE
mvp_only_selected_snp <- mvp_all[mvp_all$rsid %in% dt_all_sig_clump$SNP,]
sum(duplicated(mvp_only_selected_snp))

exp_1 <- brain_1[brain_1$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7,4,5)]
exp_2 <- brain_2[brain_2$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7,4,5)]

mvp_only_selected_snp_ordered <- mvp_only_selected_snp[order(mvp_only_selected_snp$rsid), c(3,8,9, 5,6)]

exp_1 <- brain_1[brain_1$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]
exp_2 <- brain_2[brain_2$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]
exp_3 <- brain_3[brain_3$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]
exp_4 <- brain_4[brain_4$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]
exp_5 <- brain_5[brain_5$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]
exp_6 <- brain_6[brain_6$SNP %in% dt_all_sig_clump$SNP, c(2, 6, 7)]




mvp_only_selected_snp_ordered <- mvp_only_selected_snp[order(mvp_only_selected_snp$rsid), c(3,8,9)]


exp_1_ordered <- exp_1[order(exp_1$SNP),]
exp_2_ordered <- exp_2[order(exp_2$SNP),]
exp_3_ordered <- exp_3[order(exp_3$SNP),]
exp_4_ordered <- exp_4[order(exp_4$SNP),]
exp_5_ordered <- exp_5[order(exp_5$SNP),]
exp_6_ordered <- exp_6[order(exp_6$SNP),]


mvp_mult <- rename(mvp_only_selected_snp_ordered, c("SNP" = "rsid", "outcome.beta" = "EFFECT", "outcome.se" = "SE"))
exp_1_ordered <- rename(exp_1, c("exposure_1.beta" = "beta.exposure", "exposure_1.se" = "se.exposure"))
exp_2_ordered <- rename(exp_2, c("exposure_2.beta" = "beta.exposure", "exposure_2.se" = "se.exposure"))
exp_3_ordered <- rename(exp_3, c("exposure_3.beta" = "beta.exposure", "exposure_3.se" = "se.exposure"))
exp_4_ordered <- rename(exp_4, c("exposure_4.beta" = "beta.exposure", "exposure_4.se" = "se.exposure"))
exp_5_ordered <- rename(exp_5, c("exposure_5.beta" = "beta.exposure", "exposure_5.se" = "se.exposure"))
exp_6_ordered <- rename(exp_6, c("exposure_6.beta" = "beta.exposure", "exposure_6.se" = "se.exposure"))





data_frames <- list(mvp_mult, exp_1_ordered, exp_2_ordered, exp_3_ordered,
                    exp_4_ordered, exp_5_ordered, exp_6_ordered)

common_rows_new <- reduce(data_frames, inner_join, by = "SNP")
common_rows_new <- as.data.frame(common_rows_new)

m1 = matrix(common_rows_new[,4])
m2 =matrix(common_rows_new[,6])
m3 =matrix(common_rows_new[,8])
m4 =matrix(common_rows_new[,10])
m5 =matrix(common_rows_new[,12])
m6 = matrix(common_rows_new[,14])

m1s = matrix(common_rows_new[,5])
m2s =matrix(common_rows_new[,7])
m3s =matrix(common_rows_new[,9])
m4s =matrix(common_rows_new[,11])
m5s =matrix(common_rows_new[,13])
m6s = matrix(common_rows_new[,15])


MRMVInputObject <- mr_mvinput(bx = cbind(m1,m2,m3,m4,m5,m6),
                              bxse = cbind(m1s, m2s, m3s, m4s, m5s, m6s),
                              by = common_rows_new$outcome.beta,
                              byse = common_rows_new$outcome.se,
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

write_xlsx(mvp_multivariable_result, "/gpfs/gibbs/pi/polimanti/diana/mvp_multivariableMr.xlsx")




