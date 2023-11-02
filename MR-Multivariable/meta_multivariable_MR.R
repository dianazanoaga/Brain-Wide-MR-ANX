library(meta)
library(readxl)
library(data.table)
library("plyr")
library(meta)
library(purrr)
library(metafor)
library(rio) # per leggere piu sheets di R
library(WriteXLS)
library(writexl)

mvp_multivariable_result <- read_excel("/gpfs/gibbs/pi/polimanti/diana/mvp_multivariableMr_harm.xlsx")
finn_multivariable_result <- read_excel("/gpfs/gibbs/pi/polimanti/diana/finn_multivariableMr_harm.xlsx")
ukbio_multivariable_result<- read_excel("/gpfs/gibbs/pi/polimanti/diana/ukbio_multivariableMr_harm.xlsx")

mvp_multivariable_result <- as.data.frame(mvp_multivariable_result)
finn_multivariable_result <- as.data.frame(finn_multivariable_result)
ukbio_multivariable_result <- as.data.frame(ukbio_multivariable_result)

dataset <- data.frame(matrix(ncol=1, nrow= 6))
dataset$"brain_n" <- c("0953", "0043", "1961", "1511", "1437", "3915")

for(i in c(1:6)){
  
  dataset$"exposure"[i]  <- mvp_multivariable_result$exposure[i]
  dataset$"mvp_n_multi"[i]  <- mvp_multivariable_result[i, 8]
  dataset$"mvp_b_multi"[i] <- mvp_multivariable_result[i, 3]
  dataset$"mvp_se_multi"[i] <- mvp_multivariable_result[i, 4]
  dataset$"mvp_p"[i]  <- mvp_multivariable_result[i, 7]
  dataset$"finn_n_multi"[i]  <- finn_multivariable_result[i, 8]
  dataset$"finn_b_multi"[i] <- finn_multivariable_result[i, 3]
  dataset$"finn_se_multi"[i] <- finn_multivariable_result[i, 4]
  dataset$ "finn_p_multi"[i]  <- finn_multivariable_result[i, 7]
  dataset$"ukbio_snp_multi"[i] <- ukbio_multivariable_result[i, 8]
  dataset$"ukbio_b_multi"[i]  <- ukbio_multivariable_result[i, 3]
  dataset$"ukbio_se_multi"[i]  <- ukbio_multivariable_result[i, 4]
  dataset$"ukbio_p_multi"[i]  <- ukbio_multivariable_result[i, 7]
  
}

dataset <- dataset[,-1]

for(i in c(1:6)){
  
  beta <- c(dataset$mvp_b_multi[i], dataset$finn_b_multi[i], dataset$ukbio_b_multi[i])
  se <- c(dataset$mvp_se_multi[i], dataset$finn_se_multi[i], dataset$ukbio_se_multi[i])
  data <- data.frame(r = beta, se = se)
  meta_result_reml <- rma.uni(r, sei = se , data = data)
  meta_result_common <- rma.uni(r, sei = se , data = data,method= "EE")
  meta_result_dl <- rma.uni(r, sei = se , data = data, method = "DL")
  
  
  dataset$"meta_b_reml"[i] <- meta_result_reml$beta
  dataset$"meta_se_reml"[i] <- meta_result_reml$se
  dataset$"meta_pval_reml"[i] <- meta_result_reml$pval
  dataset$"meta_het_reml"[i] <- meta_result_reml$QEp
  
  dataset$"meta_b_common_multi"[i] <- meta_result_common$beta
  dataset$"meta_se_common_multi"[i] <- meta_result_common$se
  dataset$"meta_pval_common_multi"[i] <- meta_result_common$pval
  dataset$"meta_het_common_multi"[i] <- meta_result_common$QEp
  
  dataset$"meta_b_dl"[i] <- meta_result_dl$beta
  dataset$"meta_se_dl"[i] <- meta_result_dl$se
  dataset$"meta_pval_dl"[i] <- meta_result_dl$pval
  dataset$"meta_het_dl"[i] <- meta_result_dl$QEp
  
}

dataset$"p_adj_reml" <- p.adjust(dataset$"meta_pval_reml" , method="fdr")

dataset$"p_adj_common_multi" <- p.adjust(dataset$"meta_pval_common" , method="fdr")

dataset$"p_adj_dl" <- p.adjust(dataset$"meta_pval_dl" , method="fdr")

dataset$"p_adj_reml_het" <- p.adjust(dataset$"meta_het_reml" , method="fdr")

dataset$"p_adj_common_het_multi" <- p.adjust(dataset$"meta_het_common" , method="fdr")

dataset$"p_adj_dl_het" <- p.adjust(dataset$"meta_het_dl" , method="fdr")

write_xlsx(dataset, "/gpfs/gibbs/pi/polimanti/diana/meta_multivariable_MR_harm.xlsx")

