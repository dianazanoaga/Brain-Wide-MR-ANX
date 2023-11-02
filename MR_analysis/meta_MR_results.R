

library(readxl)
library(data.table)
library("plyr")
library(meta)
library(purrr)
library(metafor)
library(rio) # per leggere piu sheets di R
library(WriteXLS)
library(writexl)



id <- c("0039", "0705", "0715", "0772", "0800", "0802", "0862", "0952", "0974", "1444", "3190", "3386", "3446")
length(id)

dataset_anx_brain <- data.frame(matrix(ncol=1, nrow= 13))
dataset_brain_anx <- data.frame(matrix(ncol=1, nrow= 13))


for(i in c(1:13)){
print(id[i])
  
mvp_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/mvp/", id[i], ".xlsx")
finngen_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/finngen/", id[i], ".xlsx")
uk_file <- paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/ukbio/ukbio", id[i], ".xlsx")

mvp_anx_brain<- import_list(mvp_file)$anx_brain
mvp_brain_anx<- import_list(mvp_file)$brain_anx

finngen_anx_brain <- import_list(finngen_file)$anx_brain
finngen_brain_anx <-import_list(finngen_file)$brain_anx

ukbio_anx_brain <- import_list(uk_file)$anx_brain
ukbio_brain_anx <- import_list(uk_file)$brain_anx

# anx su brain
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
dataset_anx_brain$"ukbio_b"[i]  <- -ukbio_anx_brain[1, 5]
dataset_anx_brain$"ukbio_se"[i] <- ukbio_anx_brain[1, 6]
dataset_anx_brain$"ukbio_p"[i]  <- ukbio_anx_brain[1, 7]


#brain su anx 
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
dataset_brain_anx$"ukbio_b"[i]  <- -ukbio_brain_anx[1, 5]
dataset_brain_anx$"ukbio_se"[i]  <- ukbio_brain_anx[1, 6]
dataset_brain_anx$"ukbio_p"[i]  <- ukbio_brain_anx[1, 7]


dataset_brain_anx$"ukbio_b"[8]  <- -0.078649478
dataset_brain_anx$"ukbio_se"[8]  <- 0.02993014



  # META ANALYSIS DI ANX SU BRAIN 


beta <- c(mvp_anx_brain$b[9], finngen_anx_brain$b[9], -ukbio_anx_brain$corrected_effect)
se <- c(mvp_anx_brain$se[9], finngen_anx_brain$se[9], ukbio_anx_brain$corrected_effect_se)
data <- data.frame(r = beta, se = se)
meta_result <- rma.uni(r, sei = se , data = data)
#summary(meta_result)

dataset_anx_brain$"meta_b"[i] <- meta_result$beta
dataset_anx_brain$"meta_se"[i] <- meta_result$se
dataset_anx_brain$"meta_pval"[i] <- meta_result$pval
dataset_anx_brain$"meta_het"[i] <- meta_result$QEp

# META ANALYSIS DI brain su anx 

beta <- c(mvp_brain_anx$b[9], finngen_brain_anx$b[9], -ukbio_brain_anx$corrected_effect)
se <- c(mvp_brain_anx$se[9], finngen_brain_anx$se[9], ukbio_brain_anx$corrected_effect_se)
data <- data.frame(r = beta, se = se)
meta_result_1 <- rma.uni(r, sei = se , data = data)
  
dataset_brain_anx$"meta_b"[i] <- meta_result_1$beta
dataset_brain_anx$"meta_se"[i] <- meta_result_1$se
dataset_brain_anx$"meta_pval"[i] <- meta_result_1$pval
dataset_brain_anx$"meta_het"[i] <- meta_result_1$QEp


}


dataset_anx_brain$"p_adj" <- p.adjust(dataset_anx_brain$"meta_pval" , method="fdr")
dataset_brain_anx$"p_adj" <- p.adjust(dataset_brain_anx$"meta_pval" , method="fdr")

p <- dataset_brain_anx[-c(11,13),]
p <- rbind(dataset_anx_brain, dataset_brain_anx)
p$"padj" <- p.adjust(p$meta_pval)
write_xlsx(dataset_anx_brain[, 2:20], "/gpfs/gibbs/pi/polimanti/diana/meta_MR_anx_brain.xlsx")
write_xlsx(dataset_brain_anx[, 2:20], "/gpfs/gibbs/pi/polimanti/diana/meta_MR_brain_anx.xlsx")


