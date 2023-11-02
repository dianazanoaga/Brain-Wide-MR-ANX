
#ONLY AGE SEX 
#regression

i = 1

for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 1,]
  assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ right_0043 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d, paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_1sex/', "0043_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 2,]
  assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ right_0043 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d, paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_2sex/', "0043_anx_right_", my_list[i], ".xlsx"))
  
}


for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 1,]
  assign(paste0("0043_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ left_0043 + age  , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_1sex/', "0043_anx_left_", my_list[i], ".xlsx"))
  
}


for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 2,]
  assign(paste0("0043_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ left_0043 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_2sex/', "0043_anx_left_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 1,]
  assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ right_0043 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d, paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_1sex/', "0043_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0043_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 2,]
  assign(paste0("0043_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~ right_0043 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0043_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d, paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_2sex/', "0043_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 1,]
  assign(paste0("0953_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  right_0953 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_1sex/', "0953_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_right_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 2,]
  assign(paste0("0953_anx_right_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  right_0953 + age , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_right_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_2sex/', "0953_anx_right_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 1,]
  assign(paste0("0953_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  left_0953 + age  , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_1sex/', "0953_anx_left_", my_list[i], ".xlsx"))
  
}

for(i in c(1:26)){
  
  my_var <- paste0("0953_anx_left_", my_list[i], "_finfin")
  my_data <- get(my_var)
  my_data <- my_data[my_data$sex == 2,]
  assign(paste0("0953_anx_left_", my_list[i], "_reg_all"), glm(as.factor(my_data[,8]) ~  left_0953 + age  , data = my_data,  family = 'binomial'))
  var <- paste0("0953_anx_left_", my_list[i], "_reg_all")
  dt <- get(var)
  dt_d <- summary(dt)$coefficients
  write.csv(dt_d ,file=paste0('/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_2sex/', "0953_anx_left_", my_list[i], ".xlsx"))
  
}

#PREPARAR FILES

# new 

my_list_0043r <- list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_1sex")
all_cov_0043right.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_1sex/",my_list_0043r[i]))
colnames(all_cov_0043right.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_1sex/",my_list_0043r[i]))
  all_cov_0043right.res <- my_var[2,]
  file <- my_list_0043r[i]
  all_cov_0043right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043right.res <- rename(all_cov_0043right.res, c("UKB_IDP" = "V1"))
  all_cov_0043right.results <- rbind(all_cov_0043right.results, all_cov_0043right.res)
  
}
all_cov_0043right.results <- all_cov_0043right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0043right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043right_1sex.result.xlsx")

# new 

my_list_0043r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_2sex")
all_cov_0043right.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_2sex/",my_list_0043r[i]))
colnames(all_cov_0043right.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043right_2sex/",my_list_0043r[i]))
  all_cov_0043right.res <- my_var[2,]
  file <- my_list_0043r[i]
  all_cov_0043right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043right.res <- rename(all_cov_0043right.res, c("UKB_IDP" = "V1"))
  all_cov_0043right.results <- rbind(all_cov_0043right.results, all_cov_0043right.res)
  
}
all_cov_0043right.results <- all_cov_0043right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0043right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043right_2sex.result.xlsx")



my_list_0043l <- list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_1sex")
all_cov_0043left.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_1sex/",my_list_0043l[i]))
colnames(all_cov_0043left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_1sex/",my_list_0043l[i]))
  all_cov_0043left.res <- my_var[2,]
  file <- my_list_0043l[i]
  all_cov_0043left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043left.res <- rename(all_cov_0043left.res, c("UKB_IDP" = "V1"))
  all_cov_0043left.results <- rbind(all_cov_0043left.results, all_cov_0043left.res)
  
}

all_cov_0043left.results <- all_cov_0043left.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0043left.results,  "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043left_1sex.result.xlsx")



my_list_0043l <- list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_2sex")
all_cov_0043left.results <- data.frame(matrix(nrow=0, ncol=6))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_2sex/",my_list_0043l[i]))
colnames(all_cov_0043left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0043left_2sex/",my_list_0043l[i]))
  all_cov_0043left.res <- my_var[2,]
  file <- my_list_0043l[i]
  all_cov_0043left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0043left.res <- rename(all_cov_0043left.res, c("UKB_IDP" = "V1"))
  all_cov_0043left.results <- rbind(all_cov_0043left.results, all_cov_0043left.res)
  
}

all_cov_0043left.results <- all_cov_0043left.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0043left.results, "/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043left_2sex.result.xlsx")


# new 

my_list_0953l <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_1sex")
all_cov_0953left.results <- data.frame(matrix(nrow=0, ncol=5))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_1sex/",my_list_0953l[i]))
colnames(all_cov_0953left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_1sex/",my_list_0953l[i]))
  all_cov_0953left.res <- my_var[2,]
  file <- my_list_0953l[i]
  all_cov_0953left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953left.res <- rename(all_cov_0953left.res, c("UKB_IDP" = "V1"))
  all_cov_0953left.results <- rbind(all_cov_0953left.results, all_cov_0953left.res)
  
}
all_cov_0953left.results <- all_cov_0953left.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953left.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953left_1sex.result.xlsx")


my_list_0953l <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_2sex")
all_cov_0953left.results <- data.frame(matrix(nrow=0, ncol=5))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_2sex/",my_list_0953l[i]))
colnames(all_cov_0953left.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953left_2sex/",my_list_0953l[i]))
  all_cov_0953left.res <- my_var[2,]
  file <- my_list_0953l[i]
  all_cov_0953left.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953left.res <- rename(all_cov_0953left.res, c("UKB_IDP" = "V1"))
  all_cov_0953left.results <- rbind(all_cov_0953left.results, all_cov_0953left.res)
  
}
all_cov_0953left.results <- all_cov_0953left.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953left.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953left_2sex.result.xlsx")


# new 

my_list_0953r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_1sex")
all_cov_0953right.results <- data.frame(matrix(nrow=0, ncol=5))
i = 1
prova <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_1sex/",my_list_0953r[i]))
colnames(all_cov_0953right.results) <- colnames(prova)

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_1sex/",my_list_0953r[i]))
  all_cov_0953right.res <- my_var[2,]
  file <- my_list_0953r[i]
  all_cov_0953right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953right.res <- rename(all_cov_0953right.res, c("UKB_IDP" = "V1"))
  all_cov_0953right.results <- rbind(all_cov_0953right.results, all_cov_0953right.res)
  
}

all_cov_0953right.results <- all_cov_0953right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953right_1sex.result.xlsx")

my_list_0953r <-list.files("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_2sex")
all_cov_0953right.results <- data.frame(matrix(nrow=0, ncol=5))

for(i in c(1:26)){
  
  my_var <- fread(paste0("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/summary_agesex_0953right_2sex/",my_list_0953r[i]))
  all_cov_0953right.res <- my_var[2,]
  file <- my_list_0953r[i]
  all_cov_0953right.res$"UKB_anx" <- sub(".*_([0-9]+)\\..*", "\\1", file)
  all_cov_0953right.res <- rename(all_cov_0953right.res, c("UKB_IDP" = "V1"))
  all_cov_0953right.results <- rbind(all_cov_0953right.results, all_cov_0953right.res)
  
}

all_cov_0953right.results <- all_cov_0953right.results[,c(6,1,2,3,4,5)]
write_xlsx(all_cov_0953right.results ,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953right_2sex.result.xlsx")

a <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953right_1sex.result.xlsx")
b <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953left_1sex.result.xlsx")
c <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043right_1sex.result.xlsx")
d <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043left_1sex.result.xlsx")

a2 <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953right_2sex.result.xlsx")
b2 <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0953left_2sex.result.xlsx")
c2 <-read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043right_2sex.result.xlsx")
d2 <- read_excel("/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_0043left_2sex.result.xlsx")

all <- rbind(a, b, c, d)
all$"adj_pvalue" <- p.adjust(all$`Pr(>|z|)`, method = "fdr")
write_xlsx(all,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_1.result.xlsx")

all2 <- rbind(a2, b2, c2, d2)
all2$"adj_pvalue" <- p.adjust(all2$`Pr(>|z|)`, method = "fdr")
write_xlsx(all2,"/gpfs/gibbs/pi/polimanti/diana/ultime_analisi/agesex_2.result.xlsx")
