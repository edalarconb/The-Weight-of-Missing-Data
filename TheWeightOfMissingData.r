# CASEN 2022
rm(list = ls())

# libraries ---------------------------------------------------------------
if (!require("pacman")) install.packages("pacman")
pacman::p_load(dplyr,rio,tidyverse,ggplot2,kableExtra,
               readxl,xtable,purrr,ineq,scales)


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



# variables z and w -------------------------------------------------------
l <- labor2

l$IINCOME = 0
l$IINCOME[l$y1_cat=="No received income"] = 1
l$IINCOME[l$y1_cat=="Received income"] = 2

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
l$IINCOME2[l$IINCOME =='1' | l$IINCOME =='2'] <- 1 #these income categories are not missing

#z distribution
z.cas <- round(prop.table(table(l$IINCOME2)), digits=3)
z.cas

#w distribution
z.educ <- round(prop.table(table(l$IESC, useNA = "always")), digits=3)
z.educ

#joint distribution z,w (pi_00 to pi_11 in the document)
z <- round(prop.table(table(l$IINCOME2, l$IESC)), digits=4)
z



# sutsets for computations: notation -------------------------------------
# l.ij denotes the data set where the index 'i' denotes if the income is observed (i=1) or not (i=0)
#   and the index 'j' denotes if the schooling is observed (j=1) or not (j=0)
#   Thus, the data set l.11 denotes all cases where both income and schooling are observed.
# When only one variable is considered, the notation for the data set is 
#  l.cj for j=0,1: when the schooling is considered
#  l.ic for i=0,1: when the income is considered
# Data set with name l.ijx for x=1,2,3,4,5 correspond to l.ij filtered by the schooling level x


l.1c <- subset(l,l$IINCOME2=='1') #observed income dataset
l.10 <- subset(l,l$IINCOME2=='1' & l$IESC=='0') #observed income and missing schooling
l.11 <- subset(l,l$IINCOME2=='1' & l$IESC=='1') #observed income and observed schooling
l.01 <- subset(l,l$IINCOME2=='0' & l$IESC=='1') #missing income and observed schooling

#observed income and schooling by school level
l.111 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='1')
l.112 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='2')
l.113 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='3')
l.114 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='4')
l.115 <- subset(l,l$IINCOME2=='1' & l$IESC=='1' & l$SCHOOL=='5')



# P(y|z=1): income distribution among observed ----------------------------------


# * p(y| z=1) -------------------------------------------------------------
a <- sort(l.1c$y1)
b <- unique(a)
t <- cumsum(table(a))/length(a)
t
cas.1c <- data.frame(t)
cas.1c[[1]]

data <- cbind.data.frame(b, cas.1c[[1]])
colnames(data) <- c("Puntaje", "Frecuencia")
cas.1c <- data
cas.1c_plot <- cas.1c
cas.1c_plot$lb = cas.1c$Frecuencia*z.cas[2]
cas.1c_plot$ub = cas.1c$Frecuencia*z.cas[2]+z.cas[1]


# Figure 1 ----------------------------------------------------------------

cas.1c_plot <- cas.1c
cas.1c_plot$lb = cas.1c$Frecuencia*z.cas[2]
cas.1c_plot$ub = cas.1c$Frecuencia*z.cas[2]+z.cas[1]

library(tidyr)

cas.1c_long <- cas.1c_plot %>%
  pivot_longer(cols = c(Frecuencia, lb, ub), 
               names_to = "Series", 
               values_to = "Value")

ggplot(cas.1c_long %>% dplyr::filter(Puntaje<=3000000),
       aes(x=Puntaje, y=Value, color=Series)) +
  geom_point() +
  xlab("Income")+ylab("Quantile")+
scale_color_manual(values = c("lb" = "red", 
                              "Frecuencia" = "green", 
                              "ub" = "blue"),
                   labels = c("lb" = "Lower Bound",
                              "Frecuencia" = "Observed Income", 
                              "ub" = "Upper Bound"),
                   breaks = c("lb", "Frecuencia", "ub")) +
  labs(color = NULL)+
  theme_bw()



# Figure 2 ----------------------------------------------------------------
l.11e <- list()
l.11e[[1]] <- l.111$y1
l.11e[[2]] <- l.112$y1
l.11e[[3]] <- l.113$y1
l.11e[[4]] <- l.114$y1
l.11e[[5]] <- l.115$y1

alist=blist=ttlist=list()
alist <- lapply(l.11e,function(h) sort(h))
blist <- lapply(alist,function(h) unique(h))
ttlist <- lapply(alist,function(h) cumsum(table(h))/length(h))
t
cas.1c.list <- lapply(ttlist,function(h) data.frame(h))


data.list <- lapply(1:5,function(h) cbind.data.frame(blist[[h]], cas.1c.list[[h]][[1]]))
for(ll in 1:5){
  colnames(data.list[[ll]]) <- c("Puntaje", "Frecuencia")
  cas.1c.list[[ll]] <- data.list[[ll]]
  cas.1c.list[[ll]]$Educational = paste0("Educational level ",ll)
}
cas.1c.list.plot = do.call('rbind',cas.1c.list)

ggplot(data=cas.1c.list.plot %>% dplyr::filter(Puntaje<=3000000),
       aes(x=Puntaje,y=Frecuencia,color=Educational))+
  geom_point()+
  xlab("Income")+ylab("Quantile")+
  theme_bw()
  
# quantiles p(y| z=1) -----------------------------------------------------

# decile  P(y | z=1)

#matrix to store quantiles
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
  q.cas1c[i,1] <- q[i,1] #column 1: alpha
  q.cas1c[i,2] <- cas.1c$Puntaje[ which(cas.1c$Frecuencia >=q[i,1])[1]] #column 2: quantile, ld column in Table 4
}

q.cas1c

#### income classification in P(y | z=1)

l.10$ingreso <- 0

for (i in 1:10){ # decil indicator
l.10$ingreso[l.10$y1 >= q.cas1c[i,2] & l.10$y1 < q.cas1c[i+1,2]] <- i
}

z.10 <- round(prop.table(table(l.10$ingreso, useNA = "always")), digits=3)
z.10 

# C column in Table 4

for (i in 2:11){
  q.cas1c[i,3] <- z.10[i-1]
}

q.cas1c

######### income classification of P(y | z=1,w=1, e=i) for i=1,2,3,4,5

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

q.cas1c #column C1 in Table 4

##
l.112$ingreso <- 0

for (i in 1:10){
  l.112$ingreso[l.112$y1 >= q.cas1c[i,2] & l.112$y1 < q.cas1c[i+1,2]] <- i
}

z.112 <- round(prop.table(table(l.112$ingreso, useNA = "always")), digits=3)
z.112#column C2 in Table 4

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
z.113#column C3 in Table 4

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
z.114#column C4 in Table 4

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
z.115#column C5 in Table 4

# P(y | x=5,z=1,e=1) in 8th column of q.cas1c

for (i in 2:11){
  q.cas1c[i,8] <- z.115[i-1]
}

q.cas1c

# Table 5
###############  P(e=g | z=1,w=1) and P(e=g | z=0, w=1) for g=1,2,3,4,5

zx.11 <- round(prop.table(table(l.11$SCHOOL, useNA = "always")), digits=3)
zx.11

zx.01 <- round(prop.table(table(l.01$SCHOOL, useNA = "always")), digits=3)
zx.01

educ <- matrix(0,5,3)
educ

for (i in 1:5){
  educ[i,1] <- i
  educ[i,2] <- zx.11[i+1]
  educ[i,3] <- zx.01[i+1]
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



### classification imputed income conditional schooling 
# respect to decile of P(y|=1)

l2 <- labor2
l2$IINCOME = 0
l2$IINCOME[l2$y1_cat=="No received income"] = 1
l2$IINCOME[l2$y1_cat=="Received income"] = 2
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



# Figure 3 ----------------------------------------------------------------
schooling_levels <- c("At most 3 years", "Between 4 and 7","Between 8 and 11","12 years","At least 13")

ib.limits <- lapply(1:5,function(h) data.frame(d = 1:10,
                                               Imputed = q.imp[,h],
                                               LB = q.cas1c[2:11,(h+9)],
                                               UB = q.cas1c[2:11,(h+14)],
                                               Schooling = schooling_levels[h]))
ib.limits.plot <- do.call('rbind',ib.limits)
ib.limits.plot$Schooling = factor(ib.limits.plot$Schooling,
                                  levels = schooling_levels)
ib.limits.plot$interval = paste0("P(l", ib.limits.plot$d-1, " ≤ y < l", ib.limits.plot$d, "| e)")

library(latex2exp)

latex_labels <- with(ib.limits.plot,
                     sprintf(
                       "$P\\bigl(l_{%d}\\le y < l_{%d}\\mid e\\bigr)$",
                       d-1, d
                     )
)
expr_labels <- TeX(latex_labels)


colors <- c("At most 3 years" = "red",
            "Between 4 and 7" = "blue",
            "Between 8 and 11" = "green",
            "12 years" = "purple",
            "At least 13" = "orange")


ggplot(ib.limits.plot, aes(x = interval, group = Schooling, color = Schooling)) +
  geom_errorbar(aes(ymin = LB, ymax = UB),
                position = position_dodge(width = 0.7),
                width = 0.2, linewidth = 1) +
  geom_point(aes(y = Imputed, shape = "Imputed value"),
             position = position_dodge(width = 0.7),
             size = 2, show.legend = TRUE) +
  scale_color_manual(values = colors, name = "Years of schooling (e)") +
  scale_shape_manual(values = 5, name = "", labels = "Imputed value") +
  labs(y = "Identification bounds", x = NULL) +
  scale_x_discrete(
    position = "top",
    labels   = expr_labels
  ) +
  theme_minimal(base_size = 14) +
  theme(
    axis.text.x.top = element_text(
      color  = "black",
      size   = 12,
      face   = "bold",
      margin = margin(t = 10),   
      vjust  = 1                 
    ),
    axis.text.x.bottom  = element_blank(),
    axis.ticks.x.bottom = element_blank(),
    panel.grid.major.x  = element_blank(),
    panel.grid.minor.x  = element_blank()
  ) +
  geom_vline(xintercept = seq(1.5, nrow(ib.limits.plot) - 0.5, by = 1),
             linetype = "dashed", color = "grey60")




# Gini Index --------------------------------------------------------------
data.casen = labor2
prop.noObs=as.numeric(prop.table(table(data.casen$y1_cat))[1])

data.obs=data.casen %>% 
  dplyr::filter(y1_cat%in%c("Received income","No received income"))
data.imp=data.casen %>% 
  filter(y1_imputed%in%c("Imputed income",
                         "Received income","No received income"))


obs.es=(data.obs$y1-min(data.obs$y1))/(max(data.obs$y1)-min(data.obs$y1)) ##scaled data
gini_coef.obs <- ineq(obs.es, type = "Gini")
p=1-prop.noObs

muLB=p*mean(obs.es)
muUB=p*mean(obs.es)+max(obs.es)*(1-p)

mu=seq(muLB,muUB, length=10000)
mu0=(mu-p*mean(obs.es))/(1-p)


lbGini <- mapply(function(mu_i, mu0_i) {
  p^2 * gini_coef.obs + (1 / mu_i) * p * (1 - p) * mean(abs(obs.es - mu0_i))
}, mu, mu0)

ubGini <- mapply(function(mu_i, mu0_i) {
  p^2 * gini_coef.obs +
    (1 / mu_i) *( p * (1 - p) * ((1 - mu0_i) * mean(obs.es) + mu0_i * (1 - mean(obs.es))) +
                    (1 - p)^2 * mu0_i * (1 - mu0_i))
}, mu, mu0)

gini_bounds <- data.frame(mu = mu, mu0 = mu0, lbGini = lbGini, ubGini = ubGini)


gini_bounds <- data.frame(mu = mu*(max(data.obs$y1)-min(data.obs$y1))+min(data.obs$y1), 
                          lbGini = lbGini, ubGini = ubGini)

gini.imp=ineq(data.imp$y0101c, type = "Gini")

points.plot <- data.frame(
  mu = c(mean(data.obs$y1), mean(data.imp$y0101c, na.rm = TRUE)),
  gini = c(gini_coef.obs, gini.imp),
  grupo = c("Observed data", "Imputed data")
)


ggplot(gini_bounds, aes(x = mu / 1000)) +
  geom_ribbon(aes(ymin = lbGini, ymax = ubGini, fill = "Identification region"), alpha = 0.5) +
  geom_line(aes(y = lbGini), color = "darkblue", linetype = "dashed") +
  geom_line(aes(y = ubGini), color = "darkblue", linetype = "dashed") +
  geom_point(data = points.plot, aes(x = mu / 1000, y = gini, color = grupo), size = 2) +
  scale_color_manual(values = c("Observed data" = "black", 
                                "Imputed data" = "red")) +
  scale_fill_manual(values = c("Identification region" = "lightblue")) +
  coord_cartesian(ylim = c(0.25, 0.9)) +
  scale_x_continuous(
    breaks = pretty(gini_bounds$mu / 1000, n = 8)
  ) +
  labs(
    x = expression(paste(mu, " (in thousands)")),
    y = "Gini coefficient",
    color = NULL,
    fill = NULL
  ) +
  theme_minimal() +
  theme(
    axis.text = element_text(size = 12),      
    axis.title = element_text(size = 12),     
    legend.text = element_text(size = 12),    
    plot.title = element_text(size = 12, face = "bold")  
  )

#### plausible Gini with observed mean
mu=mean(obs.es) ##scaled mean
mu0=(mean(obs.es)-p*mean(obs.es))/(1-p)

lbGini=p^2*gini_coef.obs+1/mu*p*(1-p)*mean(abs(obs.es-mu0))
ubBini=p^2*gini_coef.obs+1/mu*p*(1-p)*((1-mu0) * mean(obs.es) + mu0 *(1-mean(obs.es)))+
  (1-p)^2*mu0*(1-mu0)
