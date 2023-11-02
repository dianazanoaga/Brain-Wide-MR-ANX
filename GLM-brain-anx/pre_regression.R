library(remotes)
library(readxl)
library(data.table)
library(dplyr)
library(meta)
library(purrr)
library(metafor)
library(rio) 
library(WriteXLS)
library(writexl)

h <- fread("/gpfs/gibbs/pi/polimanti/UKBiobank/Brain_MRI/ukb671130.csv")
head(h)

eid <- h$eid

right_0043 <- h[, h$`25799-2.0`]
right_0043 <- as.data.frame(right_0043)
right_0043$"ID" <- eid

left_0043 <- h[, h$`25798-2.0`]
left_0043 <- as.data.frame(left_0043)
left_0043$"ID" <- eid

right_0953 <- h[, h$`27558-2.0`]
right_0953 <- as.data.frame(right_0953)
right_0953$"ID" <- eid

left_0953 <- h[, h$`27336-2.0`]
left_0953 <- as.data.frame(left_0953)
left_0953$"ID" <- eid

anx <- fread("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/allpops_pc_cov_ukbbridge.txt", fill=TRUE)
anx_da_usare <- anx[anx$related == "false", c(1,24:28)]

data_frames_0043r <- list(anx_da_usare, right_0043)
anx_right_0043 <- reduce(data_frames_0043r, inner_join, by = "ID")
sum(!is.na(anx_right_0043$right_0043)) #32892
anx_right_0043_fin <- anx_right_0043[!is.na(anx_right_0043$right_0043),]

data_frames_0043l <- list(anx_da_usare, left_0043)
anx_left_0043 <- reduce(data_frames_0043l, inner_join, by = "ID")
sum(!is.na(anx_left_0043$left_0043)) #32892
anx_left_0043_fin <- anx_left_0043[!is.na(anx_left_0043$left_0043),]

data_frames_0953r <- list(anx_da_usare, right_0953)
anx_right_0953 <- reduce(data_frames_0953r, inner_join, by = "ID")
sum(!is.na(anx_right_0953$right_0953)) #33124
anx_right_0953_fin <- anx_right_0953[!is.na(anx_right_0953$right_0953),]

data_frames_0953l <- list(anx_da_usare, left_0953)
anx_left_0953 <- reduce(data_frames_0953l, inner_join, by = "ID")
sum(!is.na(anx_left_0953$left_0953)) #33124
anx_left_0953_fin <- anx_left_0953[!is.na(anx_left_0953$left_0953),]

my_list <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/anx_symptoms")

lista_numero_col <- data.frame(matrix(nrow=28, ncol=1))

for(i in c(1:28)){
  
  assign(paste0("anx_", my_list[i]), fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/anx_symptoms/",my_list[i])))
  n <- dim(get(paste0("anx_", my_list[i])))[2]
  assign(paste0("anx_", my_list[i]), rename(get(paste0("anx_", my_list[i])), c("ID" = "eid")))
  lista_numero_col$"anx_symp"[i] <- my_list[i]
  lista_numero_col$n_col[i] <- n

}

tab_finale_0043r <- as.data.frame(matrix(nrow=28, ncol =1))

head(anx_left_0043_fin)

for(i in c(1:28)){
  
  print(i)
  data_frames_0043r_new <- list(anx_right_0043_fin, get(paste0("anx_", my_list[i])))
  assign(paste0("0043_anx_right_", my_list[i]), reduce(data_frames_0043r_new, inner_join, by = "ID"))
  assign(paste0("0043_anx_right_", my_list[i]), as.data.frame(get(paste0("0043_anx_right_", my_list[i]))))
  assign(paste0("0043_anx_right_", my_list[i], "_fin"), get(paste0("0043_anx_right_", my_list[i]))[!is.na(get(paste0("0043_anx_right_", my_list[i]))[,8]),])

  
  tab_finale_0043r$"name"[i] <- paste0("0043_anx_right_", my_list[i], "_fin")
  tab_finale_0043r$"n_id"[i] <- paste0(dim(get(paste0("0043_anx_right_", my_list[i], "_fin")))[1])
  
}
head(`0043_anx_right_20417.csv.gz_fin`)

tab_finale_0043l <- as.data.frame(matrix(nrow=28, ncol =1))

for(i in c(1:28)){
  
  print(i)
  data_frames_0043l_new <- list(anx_left_0043_fin, get(paste0("anx_", my_list[i])))
  assign(paste0("0043_anx_left_", my_list[i]), reduce(data_frames_0043l_new, inner_join, by = "ID"))
  assign(paste0("0043_anx_left_", my_list[i]), as.data.frame(get(paste0("0043_anx_left_", my_list[i]))))
  assign(paste0("0043_anx_left_", my_list[i], "_fin"), get(paste0("0043_anx_left_", my_list[i]))[!is.na(get(paste0("0043_anx_left_", my_list[i]))[,8]),])
  
  
  tab_finale_0043l$"name"[i] <- paste0("0043_anx_left_", my_list[i], "_fin")
  tab_finale_0043l$"n_id"[i] <- paste0(dim(get(paste0("0043_anx_left_", my_list[i], "_fin")))[1])
  
}

head(`0043_anx_right_20417.csv.gz_fin`)
head(`0043_anx_left_20417.csv.gz_fin`)

tab_finale_0953r <- as.data.frame(matrix(nrow=28, ncol =1))

for(i in c(1:28)){
  
  print(i)
  data_frames_0953r_new <- list(anx_right_0953_fin, get(paste0("anx_", my_list[i])))
  assign(paste0("0953_anx_right_", my_list[i]), reduce(data_frames_0953r_new, inner_join, by = "ID"))
  assign(paste0("0953_anx_right_", my_list[i]), as.data.frame(get(paste0("0953_anx_right_", my_list[i]))))
  assign(paste0("0953_anx_right_", my_list[i], "_fin"), get(paste0("0953_anx_right_", my_list[i]))[!is.na(get(paste0("0953_anx_right_", my_list[i]))[,8]),])
  
  
  tab_finale_0953r$"name"[i] <- paste0("0953_anx_right_", my_list[i], "_fin")
  tab_finale_0953r$"n_id"[i] <- paste0(dim(get(paste0("0953_anx_right_", my_list[i], "_fin")))[1])
  
}

tab_finale_0953l <- as.data.frame(matrix(nrow=28, ncol =1))


for(i in c(1:28)){
  
  print(i)
  data_frames_0953l_new <- list(anx_left_0953_fin, get(paste0("anx_", my_list[i])))
  assign(paste0("0953_anx_left_", my_list[i]), reduce(data_frames_0953l_new, inner_join, by = "ID"))
  assign(paste0("0953_anx_left_", my_list[i]), as.data.frame(get(paste0("0953_anx_left_", my_list[i]))))
  assign(paste0("0953_anx_left_", my_list[i], "_fin"), get(paste0("0953_anx_left_", my_list[i]))[!is.na(get(paste0("0953_anx_left_", my_list[i]))[,8]),])
  
  
  tab_finale_0953l$"name"[i] <- paste0("0953_anx_left_", my_list[i], "_fin")
  tab_finale_0953l$"n_id"[i] <- paste0(dim(get(paste0("0953_anx_left_", my_list[i], "_fin")))[1])
  
}

# STANDARDIZE BRAIN DAT (MEAN 0 SD 1), SEX FROM 0-1 TO 1-2, -121 E -818 TO NA

for(i in c(1:26)){
  
  assign(paste0("0043_anx_right_", my_list[i], "_finfin"), get(paste0("0043_anx_right_", my_list[i], "_fin")))
  assign(paste0("0043_anx_right_", my_list[i], "_finfin"), as.data.frame(get(paste0("0043_anx_right_", my_list[i], "_finfin"))))
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  #print(my_data)
  my_data[,3][my_data[,3] == 0] <- 2
  my_data[,4]<- my_data[,2] * my_data[,3]
  my_data[,5]<- my_data[,2] * my_data[,2]
  my_data[,6]<- my_data[,2] * my_data[,2] * my_data[,3] 
  my_data[,7] <- scale(my_data[,7] ) 
  my_data[,8][my_data[,8] == -121 | my_data[,8] == -818] <- NA
  #print(my_data)
  assign(paste0("0043_anx_right_", my_list[i], "_finfin"), my_data)
  
}
  
for(i in c(1:26)){
  
  assign(paste0("0043_anx_left_", my_list[i], "_finfin"), get(paste0("0043_anx_left_", my_list[i], "_fin")))
  assign(paste0("0043_anx_left_", my_list[i], "_finfin"), as.data.frame(get(paste0("0043_anx_left_", my_list[i], "_finfin"))))
  my_var <- paste0("0043_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  #print(my_data)
  my_data[,3][my_data[,3] == 0] <- 2
  my_data[,4]<- my_data[,2] * my_data[,3]
  my_data[,5]<- my_data[,2] * my_data[,2]
  my_data[,6]<- my_data[,2] * my_data[,2] * my_data[,3] 
  my_data[,7] <- scale(my_data[,7] ) 
  my_data[,8][my_data[,8] == -121 | my_data[,8] == -818] <- NA
  #print(my_data)
  assign(paste0("0043_anx_left_", my_list[i], "_finfin"), my_data)
  
}

for(i in c(1:26)){
  
  assign(paste0("0953_anx_right_", my_list[i], "_finfin"), get(paste0("0953_anx_right_", my_list[i], "_fin")))
  assign(paste0("0953_anx_right_", my_list[i], "_finfin"), as.data.frame(get(paste0("0953_anx_right_", my_list[i], "_finfin"))))
  my_var <- paste0("0953_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  #print(my_data)
  my_data[,3][my_data[,3] == 0] <- 2
  my_data[,4]<- my_data[,2] * my_data[,3]
  my_data[,5]<- my_data[,2] * my_data[,2]
  my_data[,6]<- my_data[,2] * my_data[,2] * my_data[,3] 
  my_data[,7] <- scale(my_data[,7] ) 
  my_data[,8][my_data[,8] == -121 | my_data[,8] == -818] <- NA
  #print(my_data)
  assign(paste0("0953_anx_right_", my_list[i], "_finfin"), my_data)
  
}

for(i in c(1:26)){
  
  assign(paste0("0953_anx_left_", my_list[i], "_finfin"), get(paste0("0953_anx_left_", my_list[i], "_fin")))
  assign(paste0("0953_anx_left_", my_list[i], "_finfin"), as.data.frame(get(paste0("0953_anx_left_", my_list[i], "_finfin"))))
  my_var <- paste0("0953_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  #print(my_data)
  my_data[,3][my_data[,3] == 0] <- 2
  my_data[,4]<- my_data[,2] * my_data[,3]
  my_data[,5]<- my_data[,2] * my_data[,2]
  my_data[,6]<- my_data[,2] * my_data[,2] * my_data[,3] 
  my_data[,7] <- scale(my_data[,7] ) 
  my_data[,8][my_data[,8] == -121 | my_data[,8] == -818] <- NA
  #print(my_data)
  assign(paste0("0953_anx_left_", my_list[i], "_finfin"), my_data)
  
}

#regression

for(i in c(1:26)){

my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
my_data <- get(my_var)
assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  right_0043 + age + sex + age_sex + age2 + age2_sex , data = my_data,  family = 'binomial'))
var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
dt <- get(var)
dt_d <- summary(dt)$coefficients
write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043right/', "0043_anx_right_", my_list[i], ".xlsx"))

}


for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0043_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  left_0043 + age + sex + age_sex + age2 + age2_sex , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043left/', "0043_anx_left_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0953_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  right_0953 + age + sex + age_sex + age2 + age2_sex , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0953right/', "0953_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0953_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  left_0953 + age + sex + age_sex + age2 + age2_sex , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0953left/', "0953_anx_left_", my_list[i], ".xlsx"))
  
}

my_list_0043l <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043left")

all_cov_0043left.results <- data.frame(matrix(nrow=26, ncol=5))

i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043left/",my_list_0043l[i]))
colnames(all_cov_0043left.results) <- colnames(prova)
  
for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043left/",my_list_0043l[i]))
  all_cov_0043left.results[i,] <- my_var[2,]
  
}

write.csv(all_cov_0043left.results ,file= "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/all_cov_0043left.result.xlsx")

# new 

my_list_0043r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043right")

all_cov_0043right.results <- data.frame(matrix(nrow=26, ncol=5))

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0043right/",my_list_0043r[i]))
  all_cov_0043right.results[i,] <- my_var[2,]
  
}

write.csv(all_cov_0043right.results ,file= "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/all_cov_0043right.result.xlsx")

 # new 

my_list_0953l <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0953left")

all_cov_0953left.results <- data.frame(matrix(nrow=26, ncol=5))

i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0953left/",my_list_0953l[i]))
colnames(all_cov_0953left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_all_cov_0953left/",my_list_0953l[i]))
  all_cov_0953left.results[i,] <- my_var[2,]
  
}

write.csv(all_cov_0953left.results ,file= "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/all_cov_0953left.result.xlsx")


#ONLY AGE SEX 
#regression

i = 1

for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ right_0043 + age + sex , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d, paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right/', "0043_anx_right_", my_list[i], ".xlsx"))
  
}


for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0043_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ left_0043 + age + sex  , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left/', "0043_anx_left_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0953_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  right_0953 + age + sex  , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right/', "0953_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  assign(paste0("0953_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  left_0953 + age + sex  , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left/', "0953_anx_left_", my_list[i], ".xlsx"))
  
}

my_list_0043l <- list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left")
all_cov_0043left.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left/",my_list_0043l[i]))
colnames(all_cov_0043left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left/",my_list_0043l[i]))
  all_cov_0043left.res <- my_var[2,]
  file <- my_list_0043l[i]
  all_cov_0043left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043left.res <- rename(all_cov_0043left.res, c("UKB_IDP" = "V1"))
  all_cov_0043left.results <- rbind(all_cov_0043left.results, all_cov_0043left.res)
  
}

all_cov_0043left.results <- all_cov_0043left.results[,c(6,1,2,3,4,5)]
write.csv(all_cov_0043left.results ,file= "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043left.result.xlsx")

# new 

my_list_0043r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right")
all_cov_0043right.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right/",my_list_0043r[i]))
colnames(all_cov_0043right.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right/",my_list_0043r[i]))
  all_cov_0043right.res <- my_var[2,]
  file <- my_list_0043r[i]
  all_cov_0043right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043right.res <- rename(all_cov_0043right.res, c("UKB_IDP" = "V1"))
  all_cov_0043right.results <- rbind(all_cov_0043right.results, all_cov_0043right.res)
  
}
all_cov_0043right.results <- all_cov_0043right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0043right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043right.result.xlsx")

# new 

my_list_0953l <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left")
all_cov_0953left.results <- data.frame(matrix(nrow=0, ncol=5))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left/",my_list_0953l[i]))
colnames(all_cov_0953left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left/",my_list_0953l[i]))
  all_cov_0953left.res <- my_var[2,]
  file <- my_list_0953l[i]
  all_cov_0953left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953left.res <- rename(all_cov_0953left.res, c("UKB_IDP" = "V1"))
  all_cov_0953left.results <- rbind(all_cov_0953left.results, all_cov_0953left.res)
  
}
all_cov_0953left.results <- all_cov_0953left.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953left.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953left.result.xlsx")

# new 

my_list_0953r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right")
all_cov_0953right.results <- data.frame(matrix(nrow=0, ncol=5))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right/",my_list_0953r[i]))
colnames(all_cov_0953right.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right/",my_list_0953r[i]))
  all_cov_0953right.res <- my_var[2,]
  file <- my_list_0953r[i]
  all_cov_0953right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953right.res <- rename(all_cov_0953right.res, c("UKB_IDP" = "V1"))
  all_cov_0953right.results <- rbind(all_cov_0953right.results, all_cov_0953right.res)
  
}

all_cov_0953right.results <- all_cov_0953right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953right.result.xlsx")

all <- rbind(all_cov_0953right.results, all_cov_0953left.results,  all_cov_0043right.results, all_cov_0043left.results)
all$"adj_pvalue" <- p.adjust(all$`Pr(>|z|)`, method = "fdr")
write_xlsx(all,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_all.result.xlsx" )
