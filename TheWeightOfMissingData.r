# CASEN 2022
rm(list = ls())

# librerías ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,rio,tidyverse,ggplot2,kableExtra,
               readxl,xtable)


# options -----------------------------------------------------------------
options(scipen=999)


# data --------------------------------------------------------------------
dataCASEN2022=import("data/Base de datos Casen 2022 STATA.dta")

dataCASEN2022 = dataCASEN2022 %>% 
  dplyr::mutate(
    age_cat = case_when(
      edad <= 15 ~ "<= 15",
      edad %in% 15:24 ~ "15-24",
      edad %in% 25:44 ~ "25-44",
      edad %in% 45:64 ~ "45-64",
      edad >= 65 ~ "65 or older"),
    educ_5t = case_when(
      esc <= 3 ~ "<= 3",
      esc>= 4 & esc<=7 ~ "4-7",
      esc>= 8 & esc<=11 ~ "8-11",
      esc==12 ~ "12",
      esc>= 13 ~ "at least 13"
    ),
    educ_2t = case_when(
      esc <= 11 ~ "<= 11",
      esc >= 12 ~ ">= 12"
    ),
    educ_na <- case_when(
      is.na(esc) ~ "Missing",
      TRUE ~"Observed"
    )
  )
dataCASEN2022$sexo[dataCASEN2022$sexo==1]="Male"
dataCASEN2022$sexo[dataCASEN2022$sexo==2]="Female"

# labor ------------------------------------------------------------
labor2 <- dataCASEN2022 %>% 
  dplyr::filter(o15 %in% c(3,4,5,6,7,8)) %>% # labor
  dplyr::mutate(y1_cat = case_when(
    y1==-88 ~ "No answer",
    y1==0 ~ "No received income",
    TRUE ~ "Received income"
  )) %>% 
  dplyr::mutate(y1_imputed = dplyr::case_when(
    y1 == -88 & !is.na(y0101c) ~ "Imputed income",
    y1 == -88 & is.na(y0101c) ~ "No observed and no imputed income",
    y1 == 0 ~ "No received income",
    !(y1 %in% c(-88,0)) ~ "Received income"
  ))


l <- labor2

l$IINCOME = 0
l$IINCOME[l$y1_cat=="No received income"] = 1
l$IINCOME[l$y1_cat=="Received income"] = 2
l$educ_5t[l$educ_5t==45477] = "4-7"
l$educ_5t[l$educ_5t==45604] = "8-11"
l$IESC=0
l$IESC[!is.na(l$educ_5t)]=1
l$IESC <- as.factor(l$IESC)
l$SCHOOL = 0
l$SCHOOL[l$educ_5t=="<= 3"] = 1
l$SCHOOL[l$educ_5t=="4-7"] = 2
l$SCHOOL[l$educ_5t=="8-11"] = 3
l$SCHOOL[l$educ_5t=="12"] = 4
l$SCHOOL[l$educ_5t=="at least 13"] = 5
l$SCHOOL<-as.factor(l$SCHOOL)



l$IINCOME2 <- 0
l$IINCOME2[l$IINCOME =='1' | l$IINCOME =='2'] <- 1 #solo es missing el efectivamente missing

table(l$IINCOME, l$IINCOME2)

z.cas <- round(prop.table(table(l$IINCOME2)), digits=3)
z.cas

z.educ <- round(prop.table(table(l$IESC, useNA = "always")), digits=3)
z.educ

z <- round(prop.table(table(l$IINCOME2, l$IESC)), digits=4)
z


l.1c <- subset(l,l$IINCOME2=='1') 
l.10 <- subset(l,l$IINCOME2=='1' & l$IESC=='0') 
l.11 <- subset(l,l$IINCOME2=='1' & l$IESC=='1') 
l.01 <- subset(l,l$IINCOME2=='0' & l$IESC=='1') 

l.111 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='1')
l.112 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='2')
l.113 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='3')
l.114 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='4')
l.115 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='5')


####### decile  P(y | z=1)

a <- sort(l.1c$y1)
b <- unique(a)
t <- cumsum(table(a))/length(a)
t
cas.1c <- data.frame(t)
cas.1c[[1]]

data <- cbind.data.frame(b, cas.1c[[1]])
colnames(data) <- c("Puntaje", "Frecuencia")
cas.1c <- data

q <- matrix(0,11,1)
q[1,1]  <- 0.0
q[2,1]  <- 0.1
q[3,1]  <- 0.2
q[4,1]  <- 0.3
q[5,1]  <- 0.4
q[6,1]  <- 0.5
q[7,1]  <- 0.6                  
q[8,1]  <- 0.7                  
q[9,1]  <- 0.8                  
q[10,1] <- 0.9                  
q[11,1] <- 1                  
q

q.cas1c <- matrix(0,11,19)
q.cas1c

for (i in 1:11){
  q.cas1c[i,1] <- q[i,1]
  q.cas1c[i,2] <- cas.1c$Puntaje[ which(cas.1c$Frecuencia >=q[i,1])[1]]
}

q.cas1c

#### income classification in P(y | z=1)

l.10$ingreso <- 0

for (i in 1:10){ # decil indicator
l.10$ingreso[l.10$y1 >= q.cas1c[i,2] & l.10$y1 < q.cas1c[i+1,2]] <- i
}

z.10 <- round(prop.table(table(l.10$ingreso, useNA = "always")), digits=3)
z.10

# 3th clumn  matrix q.cas1c

for (i in 2:11){
  q.cas1c[i,3] <- z.10[i-1]
}

q.cas1c

######### income classification od P(y | z=1,w=1, e=i) for i=1,2,3,4,5

l.111$ingreso <- 0

for (i in 1:10){
  l.111$ingreso[l.111$y1 >= q.cas1c[i,2] & l.111$y1 < q.cas1c[i+1,2]] <- i
}

z.111 <- round(prop.table(table(l.111$ingreso, useNA = "always")), digits=3)
z.111

# P(y | x=1,z=1,e=1) in 4th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,4] <- z.111[i-1]
}

q.cas1c

##
l.112$ingreso <- 0

for (i in 1:10){
  l.112$ingreso[l.112$y1 >= q.cas1c[i,2] & l.112$y1 < q.cas1c[i+1,2]] <- i
}

z.112 <- round(prop.table(table(l.112$ingreso, useNA = "always")), digits=3)
z.112

# P(y | x=2,z=1,e=1) in 5th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,5] <- z.112[i-1]
}

q.cas1c

##
l.113$ingreso <- 0

for (i in 1:10){
  l.113$ingreso[l.113$y1 >= q.cas1c[i,2] & l.113$y1 < q.cas1c[i+1,2]] <- i
}

z.113 <- round(prop.table(table(l.113$ingreso, useNA = "always")), digits=3)
z.113

# P(y | x=3,z=1,e=1) in 6th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,6] <- z.113[i-1]
}

q.cas1c

##

l.114$ingreso <- 0

for (i in 1:10){
  l.114$ingreso[l.114$y1 >= q.cas1c[i,2] & l.114$y1 < q.cas1c[i+1,2]] <- i
}

z.114 <- round(prop.table(table(l.114$ingreso, useNA = "always")), digits=3)
z.114

# P(y | x=4,z=1,e=1) in 7th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,7] <- z.114[i-1]
}

q.cas1c


##

l.115$ingreso <- 0

for (i in 1:10){
  l.115$ingreso[l.115$y1 >= q.cas1c[i,2] & l.115$y1 < q.cas1c[i+1,2]] <- i
}

l.115$ingreso[l.115$y1=='25000000'] <- 10

z.115 <- round(prop.table(table(l.115$ingreso, useNA = "always")), digits=3)
z.115

# P(y | x=5,z=1,e=1) in 8th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,8] <- z.115[i-1]
}

q.cas1c



###############  P(e=g | z=1,w=1) y P(e=g | z=0, w=1) forn g=1,2,3,4,5

zx.11 <- round(prop.table(table(l.11$SCHOOL, useNA = "always")), digits=3)
zx.11

zx.01 <- round(prop.table(table(l.01$SCHOOL, useNA = "always")), digits=3)
zx.01

educ <- matrix(0,5,3)
educ

for (i in 1:5){
  educ[i,1] <- i
  educ[i,2] <- zx.11[i]
  educ[i,3] <- zx.01[i]
}

educ




############ 9th column of q.cas1c is 1-P(y | z=1,e=0)

for (i in 2:11){
  q.cas1c[i,9] <- 1-q.cas1c[i,3]
}

q.cas1c

##### columns 10 to 14:  lb  for e=1, .. e=5; 

for (e in 1:5){
  for (i in 2:11){

    num <- q.cas1c[i,e+3]*educ[e,2]*z[4]
    den <- educ[e,3]*z[3]+educ[e,2]*z[4] + q.cas1c[i,9]*z[2] + z[1]
    
    q.cas1c[i,9+e] <- round(num/den,digits=3)
  }
}

q.cas1c

###### columns  15 to 19 ub for e=1, ... , e=5

for (e in 1:5){
  for (i in 2:11){
    
    num <- q.cas1c[i,e+3]*educ[e,2]*z[4] + educ[e,3]*z[3]+z[1] + q.cas1c[i,3]*z[2] 
    den <- educ[e,3]*z[3]+educ[e,2]*z[4] + q.cas1c[i,3]*z[2] + z[1]
    
    q.cas1c[i,14+e] <- round(num/den,digits=3)
  }
}

q.cas1c



############## classification imputed income conditional schooling 
### respect to decile of P(y|=1)

l2 <- labor2
l2$IINCOME = 0
l2$IINCOME[l2$y1_cat=="No received income"] = 1
l2$IINCOME[l2$y1_cat=="Received income"] = 2
l2$educ_5t[l2$educ_5t==45477] = "4-7"
l2$educ_5t[l2$educ_5t==45604] = "8-11"
l2$IESC=0
l2$IESC[!is.na(l2$educ_5t)]=1
l2$IESC <- as.factor(l2$IESC)
l2$SCHOOL = 0
l2$SCHOOL[l2$educ_5t=="<= 3"] = 1
l2$SCHOOL[l2$educ_5t=="4-7"] = 2
l2$SCHOOL[l2$educ_5t=="8-11"] = 3
l2$SCHOOL[l2$educ_5t=="12"] = 4
l2$SCHOOL[l2$educ_5t=="at least 13"] = 5
l2$SCHOOL<-as.factor(l2$SCHOOL)

l2$y0101c[l2$IINCOME=='1'] <- 0 

l3 <- subset(l2,l2$y0101c != 'NA')


l.e1 <- subset(l3,l3$SCHOOL=='1')
l.e2 <- subset(l3,l3$SCHOOL=='2')
l.e3 <- subset(l3,l3$SCHOOL=='3')
l.e4 <- subset(l3,l3$SCHOOL=='4')
l.e5 <- subset(l3,l3$SCHOOL=='5')


q.imp <- matrix(0,10,5)
q.imp


l.e1$ingreso <- 0

for (i in 1:10){
  l.e1$ingreso[l.e1$y0101c >= q.cas1c[i,2] & l.e1$y0101c < q.cas1c[i+1,2]] <- i
}


z.e1 <- round(prop.table(table(l.e1$ingreso, useNA = "always")), digits=3)
z.e1

##  e=1

for (i in 1:10){
  q.imp[i,1] <- z.e1[i]
}

q.imp

#e=2

l.e2$ingreso <- 0

for (i in 1:10){
  l.e2$ingreso[l.e2$y0101c >= q.cas1c[i,2] & l.e2$y0101c < q.cas1c[i+1,2]] <- i
}


z.e2 <- round(prop.table(table(l.e2$ingreso, useNA = "always")), digits=3)
z.e2


for (i in 1:10){
  q.imp[i,2] <- z.e2[i]
}

q.imp


#e=3

l.e3$ingreso <- 0

for (i in 1:10){
  l.e3$ingreso[l.e3$y0101c >= q.cas1c[i,2] & l.e3$y0101c < q.cas1c[i+1,2]] <- i
}


z.e3 <- round(prop.table(table(l.e3$ingreso, useNA = "always")), digits=3)
z.e3


for (i in 1:10){
  q.imp[i,3] <- z.e3[i]
}

q.imp

##e=4

l.e4$ingreso <- 0

for (i in 1:10){
  l.e4$ingreso[l.e4$y0101c >= q.cas1c[i,2] & l.e4$y0101c < q.cas1c[i+1,2]] <- i
}

z.e4 <- round(prop.table(table(l.e4$ingreso, useNA = "always")), digits=3)
z.e4


for (i in 1:10){
  q.imp[i,4] <- z.e4[i]
}

q.imp


##e=5

l.e5$ingreso <- 0

for (i in 1:10){
  l.e5$ingreso[l.e5$y0101c >= q.cas1c[i,2] & l.e5$y0101c <= q.cas1c[i+1,2]] <- i
}

z.e5 <- round(prop.table(table(l.e5$ingreso, useNA = "always")), digits=3)
z.e5


for (i in 1:10){
  q.imp[i,5] <- z.e5[i]
}

q.imp


