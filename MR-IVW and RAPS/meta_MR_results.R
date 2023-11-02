#!/usr/bin/env Rscript

library(readxl)
library(data.table)
library("plyr")
library(meta)
library(purrr)
library(metafor)
library(rio) # per leggere piu sheets di R
library(WriteXLS)
library(writexl)

id <- c("0039", "0705", "0715", "0772", "0800", "0802", "0862", "0952", "0974", "1444", "3190", "3386", "3446", 
        "0043", "0399", "0490", "0709", "0878", "0962", "1511", "1904", "1961", "2246", "2676", "3181", "3384",
        "3473", "3824", "2128", "0507", "0810", "0953", "1437", "1682", "1948", "2627", "3057", "3379", "3402", "3478", "3915")

dataset_anx_brain <- data.frame(matrix(ncol=1, nrow= 41))
dataset_brain_anx <- data.frame(matrix(ncol=1, nrow= 41))

for(i in c(1:41)){

  mvp_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/mvp_all/", id[i], ".xlsx")
  finngen_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/finn_all/", id[i], ".xlsx")
  uk_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/ukbio_all/", id[i], ".xlsx")
  
  mvp_anx_brain<- import_list(mvp_file)$anx_brain
  mvp_brain_anx<- import_list(mvp_file)$brain_anx
  
  finngen_anx_brain <- import_list(finngen_file)$anx_brain
  finngen_brain_anx <-import_list(finngen_file)$brain_anx
  
  ukbio_anx_brain <- import_list(uk_file)$anx_brain
  ukbio_brain_anx <- import_list(uk_file)$brain_anx
 
  # ANX - Brain IDP 
  dataset_anx_brain$"exposure"[i] =  "anxiety"
  dataset_anx_brain$"outcome"[i] =  id[i]
  dataset_anx_brain$"mvp_n"[i] =  mvp_anx_brain[9, 6]
  dataset_anx_brain$"mvp_b"[i] <- mvp_anx_brain[9, 7]
  dataset_anx_brain$"mvp_se"[i] <- mvp_anx_brain[9, 8]
  dataset_anx_brain$"mvp_p"[i] <- mvp_anx_brain[9, 9]
  dataset_anx_brain$"finn_n"[i] <- finngen_anx_brain[9, 6]
  dataset_anx_brain$"finn_b"[i]  <- finngen_anx_brain[9, 7]
  dataset_anx_brain$"finn_se"[i]  <- finngen_anx_brain[9, 8]
  dataset_anx_brain$"finn_p"[i]  <- finngen_anx_brain[9, 9]
  dataset_anx_brain$"ukbio_n"[i]  <- ukbio_anx_brain[1, 3]
  dataset_anx_brain$"ukbio_b"[i]  <- ukbio_anx_brain[1, 5]
  dataset_anx_brain$"ukbio_se"[i] <- ukbio_anx_brain[1, 6]
  dataset_anx_brain$"ukbio_p"[i]  <- ukbio_anx_brain[1, 7]
  
  # Brain IDP - ANX 
  dataset_brain_anx$"exposure"[i]  <- id[i]
  dataset_brain_anx$"outcome"[i]  <- "anxiety"
  dataset_brain_anx$"mvp_n"[i]  <- mvp_brain_anx[9, 6]
  dataset_brain_anx$"mvp_b"[i] <- mvp_brain_anx[9, 7]
  dataset_brain_anx$"mvp_se"[i] <- mvp_brain_anx[9, 8]
  dataset_brain_anx$"mvp_p"[i]  <- mvp_brain_anx[9, 9]
  dataset_brain_anx$"finn_n"[i]  <- finngen_brain_anx[9, 6]
  dataset_brain_anx$"finn_b"[i] <- finngen_brain_anx[9, 7]
  dataset_brain_anx$"finn_se"[i] <- finngen_brain_anx[9, 8]
  dataset_brain_anx$ "finn_p"[i]  <- finngen_brain_anx[9, 9]
  dataset_brain_anx$"ukbio_n"[i] <- ukbio_brain_anx[1, 3]
  dataset_brain_anx$"ukbio_b"[i]  <- ukbio_brain_anx[1, 5]
  dataset_brain_anx$"ukbio_se"[i]  <- ukbio_brain_anx[1, 6]
  dataset_brain_anx$"ukbio_p"[i]  <- ukbio_brain_anx[1, 7]
  
  # META ANALYSIS OF ANX - BRAIN 
  
  beta <- c(mvp_anx_brain$b[9], finngen_anx_brain$b[9], ukbio_anx_brain$corrected_effect)
  se <- c(mvp_anx_brain$se[9], finngen_anx_brain$se[9], ukbio_anx_brain$corrected_effect_se)
  data <- data.frame(r = beta, se = se)
  meta_result_reml <- rma.uni(r, sei = se , data = data)
  meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
  meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
  
  dataset_anx_brain$"meta_b_reml"[i] <- meta_result_reml$beta
  dataset_anx_brain$"meta_se_reml"[i] <- meta_result_reml$se
  dataset_anx_brain$"meta_pval_reml"[i] <- meta_result_reml$pval
  dataset_anx_brain$"meta_het_reml"[i] <- meta_result_reml$QEp
  
  dataset_anx_brain$"meta_b_common"[i] <- meta_result_common$beta
  dataset_anx_brain$"meta_se_common"[i] <- meta_result_common$se
  dataset_anx_brain$"meta_pval_common"[i] <- meta_result_common$pval
  dataset_anx_brain$"meta_het_common"[i] <- meta_result_common$QEp
  
  dataset_anx_brain$"meta_b_dl"[i] <- meta_result_dl$beta
  dataset_anx_brain$"meta_se_dl"[i] <- meta_result_dl$se
  dataset_anx_brain$"meta_pval_dl"[i] <- meta_result_dl$pval
  dataset_anx_brain$"meta_het_dl"[i] <- meta_result_dl$QEp
  
  
  # META ANALYSIS OF BRAIN - ANX 
  
  beta <- c(mvp_brain_anx$b[9], finngen_brain_anx$b[9], ukbio_brain_anx$corrected_effect)
  se <- c(mvp_brain_anx$se[9], finngen_brain_anx$se[9], ukbio_brain_anx$corrected_effect_se)
  data <- data.frame(r = beta, se = se)
  meta_result_reml <- rma.uni(r, sei = se , data = data)
  meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
  meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
  
  dataset_brain_anx$"meta_b_reml"[i] <- meta_result_reml$beta
  dataset_brain_anx$"meta_se_reml"[i] <- meta_result_reml$se
  dataset_brain_anx$"meta_pval_reml"[i] <- meta_result_reml$pval
  dataset_brain_anx$"meta_het_reml"[i] <- meta_result_reml$QEp
  
  dataset_brain_anx$"meta_b_common"[i] <- meta_result_common$beta
  dataset_brain_anx$"meta_se_common"[i] <- meta_result_common$se
  dataset_brain_anx$"meta_pval_common"[i] <- meta_result_common$pval
  dataset_brain_anx$"meta_het_common"[i] <- meta_result_common$QEp
  
  dataset_brain_anx$"meta_b_dl"[i] <- meta_result_dl$beta
  dataset_brain_anx$"meta_se_dl"[i] <- meta_result_dl$se
  dataset_brain_anx$"meta_pval_dl"[i] <- meta_result_dl$pval
  dataset_brain_anx$"meta_het_dl"[i] <- meta_result_dl$QEp
  
}

dataset_anx_brain$"p_adj_reml" <- p.adjust(dataset_anx_brain$"meta_pval_reml" , method="fdr")
dataset_brain_anx$"p_adj_reml" <- p.adjust(dataset_brain_anx$"meta_pval_reml" , method="fdr")

dataset_anx_brain$"p_adj_common" <- p.adjust(dataset_anx_brain$"meta_pval_common" , method="fdr")
dataset_brain_anx$"p_adj_common" <- p.adjust(dataset_brain_anx$"meta_pval_common" , method="fdr")

dataset_anx_brain$"p_adj_dl" <- p.adjust(dataset_anx_brain$"meta_pval_dl" , method="fdr")
dataset_brain_anx$"p_adj_dl" <- p.adjust(dataset_brain_anx$"meta_pval_dl" , method="fdr")

dataset_anx_brain$"p_adj_reml_het" <- p.adjust(dataset_anx_brain$"meta_het_reml" , method="fdr")
dataset_brain_anx$"p_adj_reml_het" <- p.adjust(dataset_brain_anx$"meta_het_reml" , method="fdr")

dataset_anx_brain$"p_adj_common_het" <- p.adjust(dataset_anx_brain$"meta_het_common" , method="fdr")
dataset_brain_anx$"p_adj_common_het" <- p.adjust(dataset_brain_anx$"meta_het_common" , method="fdr")

dataset_anx_brain$"p_adj_dl_het" <- p.adjust(dataset_anx_brain$"meta_het_dl" , method="fdr")
dataset_brain_anx$"p_adj_dl_het" <- p.adjust(dataset_brain_anx$"meta_het_dl" , method="fdr")

write_xlsx(dataset_anx_brain[, 2:33], "/gpfs/gibbs/pi/polimanti/diana/meta_MR_anx_brain_all41.xlsx")
write_xlsx(dataset_brain_anx[, 2:33], "/gpfs/gibbs/pi/polimanti/diana/meta_MR_brain_anx_all41.xlsx")
