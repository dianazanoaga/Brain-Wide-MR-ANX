
library(MRlap)
library(remotes)
library(readxl)
library(data.table)
library("plyr")
library(MRInstruments)
library(ieugwasr)
library("ragg")
library(Cairo)


uk_all <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all.txt")
uk_all <- as.data.frame(uk_all)


brain_all <- fread("/gpfs/gibbs/pi/polimanti/diana/files_format/0039.txt")
brain_all <- as.data.frame(brain_all)

i = "/gpfs/gibbs/pi/polimanti/diana/files_format/0039.txt"

head(uk_all)
head(brain_all)

snplist <- fread("/gpfs/gibbs/pi/polimanti/Pan.UKB.rsid")    
snplist <- as.data.frame(snplist)

w_hm3 <- fread("/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist")
eur_w_ld 


snplist$chr <- as.integer(snplist$chr)
head(snplist)
head(uk_all)
str(snplist)
str(uk_all)

uk_all_snp <- merge(x=uk_all, y=snplist, by.x= c("chr", "pos"), by.y= c("chr", "pos"))
head(uk_all_snp)

uk_all_snp[uk_all_snp$rsid=="rs10875231",]
uk_all[uk_all$chr==1 & uk_all$pos == 100000012,]
snplist[snplist$rsid=="rs10875231",]
snplist[snplist$rsid=="rs3094315",]

write.csv(uk_all_snp, file="/gpfs/gibbs/pi/polimanti/diana/uk_all_snp.txt")

uk_all_snp <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp.txt")

head(uk_all_snp)
head(brain_all) 


uk_all_snp_p <- uk_all_snp[,c("chr", "pos", "rsid", "ref.x", "alt.x", "se_EUR", "beta_EUR")]
n<- dim(uk_all_snp_p)[1]
uk_all_snp_p$"N" <- rep(40648, n)
str(uk_all_snp_p)
head(uk_all_snp_p)
uk_all_snp_rename <- uk_all_snp_p
uk_all_snp_rename <- rename(uk_all_snp_rename, c("ref.x" = "alt", "alt.x" = "ref", "se_EUR"= "se", "beta_EUR"="beta"))

write.csv(uk_all_snp_rename, file="/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")

# GOOD ONE TO LOAD EVERYTIME IS "/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt"

uk_all_snp_rename <- fread("/gpfs/gibbs/pi/polimanti/diana/uk_all_snp_rename.txt")

g <- sum(is.na(uk_all_snp_rename$beta))
g <- sum(is.na(uk_all_snp_rename$rsid))

j <- sum(is.na(brain_all_rename$a1))

h<- sum(is.na(uk_all_snp_rename_no_null$alt))


uk_all_snp_rename_no_null <- uk_all_snp_rename[uk_all_snp_rename$beta != 0,]
  
head(brain_all)
brain_all_rename <- brain_all
brain_all_rename <- rename(brain_all_rename, c("n" = "N", "a1" = "a2", "a2" ="a1"))


brain_all_rename_sig <- brain_all_rename[brain_all_rename$pvalue < 5*10^(-8),]
uk_all_snp_rename_sig <- uk_all_snp_rename_no_null[uk_all_snp_rename_no_null$pval_EUR < 5*10^(-8),]
print(uk_all_snp_rename_sig)

merge_brain_anx <- merge(x=brain_all_rename_sig, y=uk_all_snp_rename_no_null, by.x="rsid" , by.y="rsid")

str(brain_all_rename)
str(uk_all_snp_rename_no_null)

A = MRlap(exposure = uk_all_snp_rename_no_null,
          exposure_name = "uk_anxiety",
          outcome = brain_all_rename,
          outcome_name = "brain",
          ld = "/gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr",
          hm3 = "/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist",
          save_logfiles = TRUE,
          MR_threshold= 1*10^(-5))


dim(merge_brain_anx)[1]


A_Mr <- A$MRcorrection
A_Mr <- as.data.frame(A_Mr)
A_Mr <- A_Mr[1,-4]
A_Mr_fin <- cbind(A_Mr, A$LDSC,A$GeneticArchitecture)
A_Mr_fin$"exposure" <- "anxiety"
A_Mr_fin$"outcome" <- i

if(dim(merge_brain_anx)[1] < 5){
  
  B = MRlap(exposure = brain_all_rename,
            exposure_name = "brain",
            outcome = uk_all_snp_rename_no_null,
            outcome_name = "uk_anxiety",
            ld = "/gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr",
            hm3 = "/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist",
            save_logfiles = TRUE,
            MR_threshold= 1*10^(-5))
  
} else {
  
  B = MRlap(exposure = brain_all_rename,
            exposure_name = "brain",
            outcome = uk_all_snp_rename_no_null,
            outcome_name = "uk_anxiety",
            ld = "/gpfs/ysm/project/polimanti/mz575/ldsc/eur_w_ld_chr",
            hm3 = "/gpfs/ysm/project/polimanti/mz575/ldsc/w_hm3.snplist",
            MR_threshold= 5*10^(-8),
            save_logfiles = TRUE)
}





B_Mr <- B$MRcorrection
B_Mr <- as.data.frame(B_Mr)
B_Mr <- B_Mr[1,-4]
ABMr_fin <- cbind(B_Mr, B$LDSC,B$GeneticArchitecture)
B_Mr_fin$"exposure" <- "anxiety"
B_Mr_fin$"outcome" <- i


id <- sub(".txt.*", "", batch_args[i])
id <- sub(".*/gpfs/gibbs/pi/polimanti/diana/files_format", "", id)
write_xlsx(list_data, paste0("/gpfs/gibbs/pi/polimanti/diana/MR_results/ukbio", id, ".xlsx"))

