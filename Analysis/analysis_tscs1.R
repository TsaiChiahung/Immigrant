## This is the replication files for the analysis of TSCS 2017 survey data in Tsai, Tsung-han, Chia-hung Tsai and Chi Huang (2018). "Different Immigrants, Same Attitudes? A Bayesian Bivariate Ordered Probit Analysis of Public Opinion in Taiwan."
## Before running, change the working directory in line 14 to the appropriate one.
##
## Require the following file:
## (1) tscs162_20170214_committee.dta: TSCS 2017 survey data

## CLEAN UP
rm(list=ls());

#######################################################################################
## THIS TELLS R TO ADD ~/Users/ttsai/Rlibs TO THE LIST OF DIRECTORIES FOR R PACKAGES.
.libPaths("~/Rlibs");
#.libPaths();

## LOAD PACKAGES
library(haven);
library(R2jags);
library(questionr);

#######################################################################################
## SET WORKING DIRECTORY
#setwd("/Users/ttsai/Work/Dataset/TSCS/");  # Run On Mac

## LOAD THE DATA
TSCS2017 <- read_dta(file="tscs162_20170214_committee.dta");
dim(TSCS2017);
TSCS2017[1:10,1:10];
#summary(TSCS2017);


###########################################################################################
########################    SET UP THE DATA AND RECODE VARIABLES    #######################

attach(TSCS2017);

#####################################  OUTCOME VARIABLES  ##################################
## Foreign Professionals to become citizens
table(f4);

re_f4 <- f4;
re_f4[which(f4 >= 94)] <- NA;
re_f4[which(f4 == 1)] <- 4;
re_f4[which(f4 == 2)] <- 3;
re_f4[which(f4 == 3)] <- 2;
re_f4[which(f4 == 4)] <- 1;

table(re_f4);
sum(table(re_f4));
summary(re_f4);

TSCS2017$re_f4 <- re_f4;

##
f4b <- re_f4;
f4b[which(re_f4 == 1)] <- 0;
f4b[which(re_f4 == 2)] <- 0;
f4b[which(re_f4 == 3)] <- 1;  # agree
f4b[which(re_f4 == 4)] <- 1;

table(f4b);
sum(table(f4b));
summary(f4b);

TSCS2017$f4b <- f4b;


## migrant laborers to become citizens
table(f5);

re_f5 <- f5;
re_f5[which(f5 >= 94)] <- NA;
re_f5[which(f5 == 1)] <- 4;
re_f5[which(f5 == 2)] <- 3;
re_f5[which(f5 == 3)] <- 2;
re_f5[which(f5 == 4)] <- 1;

table(re_f5);
sum(table(re_f5));
summary(re_f5);

TSCS2017$re_f5 <- re_f5;

##
f5b <- re_f5;
f5b[which(re_f5 == 1)] <- 0;
f5b[which(re_f5 == 2)] <- 0;
f5b[which(re_f5 == 3)] <- 1;  # agree
f5b[which(re_f5 == 4)] <- 1;

table(f5b);
sum(table(f5b));
summary(f5b);

TSCS2017$f5b <- f5b;


#####################################  EXPLANATORY VARIABLES  ##################################
## More Governmental Regulations
table(c3);

re_c3 <- c3;
re_c3[which(c3 >= 97)] <- NA;
re_c3 <- re_c3 - 1;

table(re_c3);
sum(table(re_c3));
summary(re_c3);

TSCS2017$re_c3 <- re_c3;


## Problems regarding Foreign Laborers
table(e2f);

re_e2f <- e2f;
re_e2f[which(e2f >= 94)] <- NA;
re_e2f[which(e2f == 1)] <- 3;
re_e2f[which(e2f == 2)] <- 3;
re_e2f[which(e2f == 3)] <- 2;
re_e2f[which(e2f == 4)] <- 1;
re_e2f[which(e2f == 5)] <- 1;

re_e2f <- factor(re_e2f, labels=c("a_Unsuccessful","b_Neither","c_Successful"), ordered=FALSE);

table(re_e2f);
sum(table(re_e2f));
summary(re_e2f);

TSCS2017$re_e2f <- re_e2f;


## Tolerance of foreigners
table(f3);

re_f3 <- f3;
re_f3[which(f3 >= 94)] <- NA;
re_f3[which(f3 == 1)] <- 1;
re_f3[which(f3 == 2)] <- 1;
re_f3[which(f3 == 3)] <- 0;
re_f3[which(f3 == 4)] <- 0;

table(re_f3);
sum(table(re_f3));
summary(re_f3);

TSCS2017$re_f3 <- re_f3;


## Government Respecting Human Rights
table(f6);

re_f6 <- f6;
re_f6[which(f6 >= 97)] <- NA;
re_f6[which(f6 == 1)] <- 1;
re_f6[which(f6 == 2)] <- 1;
re_f6[which(f6 == 3)] <- 0;
re_f6[which(f6 == 4)] <- 0;

table(re_f6);
sum(table(re_f6));
summary(re_f6);

TSCS2017$re_f6 <- re_f6;


## Country Economic Status
table(g1);

re_g1 <- g1;
re_g1[which(g1 >= 92)] <- NA;
re_g1[which(g1 == 1)] <- 2;
re_g1[which(g1 == 2)] <- 3;
re_g1[which(g1 == 3)] <- 1;

re_g1 <- factor(re_g1, labels=c("a_Same","b_Better","c_Worse"), ordered=FALSE);

table(re_g1);
sum(table(re_g1));
summary(re_g1);

TSCS2017$re_g1 <- re_g1;


## Household Economic Status
table(g3);

re_g3 <- g3;
re_g3[which(g3 >= 93)] <- NA;
re_g3[which(g3 == 1)] <- 2;
re_g3[which(g3 == 2)] <- 3;
re_g3[which(g3 == 3)] <- 1;

re_g3 <- factor(re_g3, labels=c("a_Same","b_Better","c_Worse"), ordered=FALSE);

table(re_g3);
sum(table(re_g3));
summary(re_g3);

TSCS2017$re_g3 <- re_g3;

## Social Status
table(j2);

re_j2 <- j2;
re_j2[which(j2 >= 97)] <- NA;

table(re_j2);
sum(table(re_j2));
summary(re_j2);

TSCS2017$re_j2 <- re_j2;

## Unemployment
table(j3);

re_j3 <- j3;
re_j3[which(j3 == 98)] <- NA;
re_j3[which(j3 == 1)] <- 0;
re_j3[which(j3 == 2)] <- 1;
re_j3[which(j3 == 3)] <- 1;
re_j3[which(j3 == 4)] <- 0;
re_j3[which(j3 == 5)] <- 1;
re_j3[which(j3 == 6)] <- 1;
re_j3[which(j3 == 7)] <- 1;
re_j3[which(j3 == 8)] <- 0;
re_j3[which(j3 == 10)] <- 1;
re_j3[which(j3 == 11)] <- 1;
re_j3[which(j3 == 12)] <- 1;
re_j3[which(j3 == 13)] <- 0;
re_j3[which(j3 == 15)] <- 0;
re_j3[which(j3 == 16)] <- 0;

table(re_j3);
sum(table(re_j3));
summary(re_j3);

TSCS2017$re_j3 <- re_j3;


## Unemployment2
table(j4);

re_j4 <- j4;
re_j4[which(j4 == 98)] <- NA;
re_j4[which(j4 == 1)] <- 0;
re_j4[which(j4 == 2)] <- 0;
re_j4[which(j4 == 3)] <- 0;
re_j4[which(j4 == 4)] <- 1;
re_j4[which(j4 == 5)] <- 1;
re_j4[which(j4 == 6)] <- 1;

table(re_j4);
sum(table(re_j4));
summary(re_j4);

TSCS2017$re_j4 <- re_j4;


## Occupation: ISCO 2008
table(j6b5);

occup <- c();

occup[which(j6b5 >= 9996)] <- NA;  ##
occup[which(j6b5 < 1000)] <- 10;  ## military
occup[which(j6b5 >= 1000 & j6b5 < 2000)] <- 1;  ## government, CEO
occup[which(j6b5 >= 2000 & j6b5 < 3000)] <- 2;  ## professionals
occup[which(j6b5 >= 3000 & j6b5 < 4000)] <- 3;  ## assistant professionals
occup[which(j6b5 >= 4000 & j6b5 < 5000)] <- 4;  ## 
occup[which(j6b5 >= 5000 & j6b5 < 6000)] <- 5;  ## sales, service
occup[which(j6b5 >= 6000 & j6b5 < 7000)] <- 6;  ## skilled agraculture
occup[which(j6b5 >= 7000 & j6b5 < 8000)] <- 7;  ## skilles workers
occup[which(j6b5 >= 8000 & j6b5 < 9000)] <- 8;  ## low-skilled workers
occup[which(j6b5 >= 9000 & j6b5 < 9990)] <- 9;  ## workers

table(occup);
sum(table(occup));
summary(occup);

TSCS2017$occup <- occup;


## Occupation2
occup2 <- occup;

occup2[which(occup == 10)] <- 1;  ## military
occup2[which(occup == 1 | occup == 2)] <- 2;  ## professionals
occup2[which(occup == 3 | occup == 6 | occup == 7 | occup == 8)] <- 3;  ## skilled
occup2[which(occup == 4 | occup == 5 | occup == 9)] <- 4;  ## low-skilled

occup2 <- factor(occup2, labels=c("a_military", "b_pro","c_skilled","d_low-skilled"), ordered=FALSE);

table(occup2);
sum(table(occup2));
summary(occup2);

TSCS2017$occup2 <- occup2;


## Income
table(j16);

re_j16 <- j16;
re_j16[which(j16 >= 97)] <- NA;
re_j16[which(j16 >= 1 & j16 <= 4)] <- 1;  ## 0-2.9
re_j16[which(j16 >= 5 & j16 <= 7)] <- 2;  ## 3-5.9
re_j16[which(j16 >= 8 & j16 <= 10)] <- 3;  ## 6-8.9
re_j16[which(j16 >= 11 & j16 <= 13)] <- 4;  ## 9-11.9
re_j16[which(j16 >= 14 & j16 <= 23)] <- 5;  ## 12-

re_j16 <- factor(re_j16, labels=c("a_0-2.9", "b_3-5.9", "c_6-8.9", "d_9-11.9", "e_12+"), ordered=TRUE);

table(re_j16);
sum(table(re_j16));
summary(re_j16);

TSCS2017$re_j16 <- re_j16;


## Education
table(a13);

edu <- a13;
edu[which(a13 == 1)] <- 1;  ## illiteracy
edu[which(a13 >= 2 & a13 <= 5)] <- 2;  ## elementary and junior high
edu[which(a13 >= 6 & a13 <= 9)] <- 3;  ## senior high
edu[which(a13 >= 10 & a13 <= 16)] <- 4;  ## college
edu[which(a13 >= 17 & a13 <= 22)] <- 5;  ## university

edu <- factor(edu, labels=c("a_Illiteracy", "b_Junior", "c_Senior", "d_College", "University"), ordered=TRUE);

table(edu);
sum(table(edu));
summary(edu);

TSCS2017$edu <- edu;


## Gender
table(a1);

female <- a1 - 1;

table(female);
sum(table(female));
summary(female);

TSCS2017$female <- female;



detach(TSCS2017);

##  SAVE THE NEW DATA SET
#write.csv(TSCS2017, "/Users/ttsai/Work/Dataset/TSCS/tscs2017.csv", row.names=FALSE);



###########################################################################################
########################    Descriptive Statistics    #######################
## LOAD THE DATA
TSCS2017 <- read.csv(file="tscs2017.csv", header=TRUE);
dim(TSCS2017);

attach(TSCS2017);

##################################
table(re_f4);
freq.table <- round(wtd.table(x=re_f4, weights=wr5, na.show=TRUE), 0);
freq.table;
round(prop.table(freq.table)*100, 2);
sum(freq.table);

##
table(re_f5);
freq.table <- round(wtd.table(x=re_f5, weights=wr5, na.show=TRUE), 0);
freq.table;
round(prop.table(freq.table)*100, 2);
sum(freq.table);

##
table(re_f3);
freq.table <- round(wtd.table(x=re_f3, weights=wr5, na.show=TRUE), 0);
freq.table;
round(prop.table(freq.table)*100, 2);
sum(freq.table);


##  Cross-tabulation: occupation
##
freq.table <- round(wtd.table(x=occup2, y=re_f4, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);

##
freq.table <- round(wtd.table(x=occup2, y=re_f5, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);


##  Cross-tabulation: income
##
freq.table <- round(wtd.table(x=re_j16, y=re_f4, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);

##
freq.table <- round(wtd.table(x=re_j16, y=re_f5, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);


##  Cross-tabulation: tolerance
##
f3a <- c();
f3a[which(f3 >= 94)] <- NA;
f3a[which(f3 == 1)] <- 4;
f3a[which(f3 == 2)] <- 3;
f3a[which(f3 == 3)] <- 2;
f3a[which(f3 == 4)] <- 1;

freq.table <- round(wtd.table(x=f3a, y=re_f4, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);

##
freq.table <- round(wtd.table(x=f3a, y=re_f5, weights=wr5, na.show=FALSE), 0);
freq.table;
rowSums(freq.table);
round(freq.table/rowSums(freq.table)*100, 2);


detach(TSCS2017);



###########################################################################################
########################    Model    #######################
##########################################################################
## LOAD THE DATA
TSCS2017 <- read.csv(file="tscs2017.csv", header=TRUE);
dim(TSCS2017);


##########################################################################
## DEALING WITH MISSING VALUES
######################################################################################
s.data <- TSCS2017[,c("re_f4", "re_f5", "re_f3", "re_g1", "re_g3", "re_j2", "re_j3", "occup2", "re_j16", "re_e2f", "edu", "female")];
summary(s.data);
dim(s.data);

keep.d <- complete.cases(s.data[,3:11]);

t.data <- s.data[keep.d,];
dim(t.data);


###############   BIVARIATE ORDERED PROBIT MODEL   ######################
##
#attach(t.data);

## 
y1 <- as.numeric(t.data$re_f4);
y2 <- as.numeric(t.data$re_f5);


##  create dummy variables for occupations; a_military as the reference
table(t.data$occup2);
occup2_dummy <- outer(t.data$occup2, c("a_military", "b_pro","c_skilled","d_low-skilled"), '==') + 0;
colnames(occup2_dummy) <- c("a_military", "b_pro","c_skilled","d_low-skilled");

t.data$occup2_dummy <- occup2_dummy[,-1];


##  create dummy variables for Country Economic Status; Worse as the reference
table(t.data$re_g1);
re_g1_dummy <- outer(t.data$re_g1, c("a_Same","b_Better","c_Worse"), '==') + 0;
colnames(re_g1_dummy) <- c("a_Same","b_Better","c_Worse");

t.data$g1_dummy <- re_g1_dummy[,-1];


##  create dummy variables for Household Economic Status; Worse as the reference
table(t.data$re_g3);
re_g3_dummy <- outer(t.data$re_g3, c("a_Same","b_Better","c_Worse"), '==') + 0;
colnames(re_g3_dummy) <- c("a_Same","b_Better","c_Worse");

t.data$g3_dummy <- re_g3_dummy[,-1];


##  create dummy variables for Income; 0-2.9 as the reference
table(t.data$re_j16);
re_j16_dummy <- outer(t.data$re_j16, c("a_0-2.9", "b_3-5.9", "c_6-8.9", "d_9-11.9", "e_12+"), '==') + 0;
colnames(re_j16_dummy) <- c("a_0-2.9", "b_3-5.9", "c_6-8.9", "d_9-11.9", "e_12+");

t.data$j16_dummy <- re_j16_dummy[,-1];


##  create dummy variables for Problems regarding Foreign Laborers; a_Unsuccessful as the reference
table(t.data$re_e2f);
re_e2f_dummy <- outer(t.data$re_e2f, c("a_Unsuccessful","b_Neither","c_Successful"), '==') + 0;
colnames(re_e2f_dummy) <- c("a_Unsuccessful","b_Neither","c_Successful");

t.data$e2f_dummy <- re_e2f_dummy[,-1];


##  create dummy variables for ecudation; a_Illiteracy as the reference
table(t.data$edu);
edu_dummy <- outer(t.data$edu, c("a_Illiteracy","b_JunHigh", "c_SenHigh", "d_Pro", "e_Uni"), '==') + 0;
colnames(edu_dummy) <- c("a_Illiteracy","b_JunHigh", "c_SenHigh", "d_Pro", "e_Uni");

t.data$edu_dummy <- edu_dummy[,-1];


##    EQUATION 1
X <- cbind(t.data$occup2_dummy, t.data$g1_dummy, t.data$g3_dummy, t.data$j16_dummy, t.data$re_f3, t.data$re_j2, t.data$re_j3, t.data$edu_dummy);

##    EQUATION 2
Z <- cbind(t.data$occup2_dummy, t.data$g1_dummy, t.data$g3_dummy, t.data$j16_dummy, t.data$re_f3, t.data$re_j2, t.data$re_j3, t.data$edu_dummy, t.data$e2f_dummy);

N <- dim(t.data)[1];
K <- dim(X)[2];
M <- dim(Z)[2];
L <- 4;

#detach(t.data);


## SPECIFY DATA PARAMETERS
data1 <- list("y1", "y2", "N", "K", "M", "X", "Z", "L");


init.gam <- rbind(seq(-1,1,length=L-1), seq(-1,1,length=L-1) );


# INITIAL VALUES; LARGE DISPREAD
inits1 <- list(
	list(rho=-0.5, B=rep(0,K), G=rep(0.5,M), gam=init.gam ),
	list(rho=0, B=rep(-0.5,K), G=rep(-0.5,M), gam=init.gam ),
	list(rho=0.5, B=rep(0.5,K), G=rep(0,M), gam=init.gam )
    );

parameters1 <- c("B", "G", "rho", "gamma");

##  MARKOV CHAIN MONTE CARLO
n.chains <- length(inits1);
n.iteration <- 100000;
frac.discard <- 0.5;  # FRACTION OF SAMPLES DISCARDED
n.discard <- n.iteration*frac.discard; n.discard;  # BURNIN-IN PERIOD
n.thining <- 10;
n.saved.chin <- (n.iteration - n.discard)/n.thining; n.saved.chin;  # SAMPLES AFTER THINING SAVED FOR EACH CHAIN
n.saved.chin*n.chains;  # TOTAL SAMPLES SAVED

## SET THE SEED, WHICH TELLS US WHERE THE RANDOM PROCESS STARTS.
set.seed(201889);

## SET WORKING DIRECTORY WHERE THE BUGS/JAGS FILE IS IN MY MACBOOK PRO
#setwd("/Users/ttsai/Work/Working_Papers/Immigration/BOPM_code/");
#getwd();

## HAND-OFF TO JAGS
bml1 <- jags(model.file="BOPM.R",
             data = data1,
             inits = inits1,
             parameters.to.save = parameters1,
             n.chains = n.chains,   # (DEFAULT: 1)
             n.iter = n.iteration,
             n.burnin = n.discard,
             n.thin = n.thining,
             DIC = TRUE
             );

print(bml1);

##  PROCESS THE MCMC RESULTS
mcmc.out <- as.mcmc(bml1);

##  CONVERGENCE DIAGNOSTICS
gelman.diag(mcmc.out);
geweke.diag(mcmc.out);

##  SAVE THE RESULTS
out.matrix <- as.matrix(mcmc.out);
dim(out.matrix);
#out.matrix[1,];


B <- out.matrix[,c(1,11:18,2:10)];
G <- out.matrix[,c(20,31,33:39,21:30,32)];
gamma <- out.matrix[,40:45];
r <- out.matrix[,46];

out.para <- cbind(B, G, gamma, r);
dim(out.para);


##  SAVE THE RESULTS
#write.csv(out.para, "/Users/ttsai/Work/Working_Papers/Immigration/bopm_para.csv", row.names=FALSE);



######################################################
##
##  LOAD AND REOPORT THE RESULTS
##
######################################################
## LODING FUNCTIONS
#source("/Users/ttsai/WashU/R note/TsaiFunctions.R");


## SET WORKING DIRECTORY
#setwd("/Users/ttsai/Work/Working_Papers/Immigration/");  # Run On Mac

## LOAD THE DATA
#est.para <- read.csv(file="bopm_para.csv", header=TRUE);
#dim(est.para);


########################  REPORT ESTIMATES  #######################
#par1 <- cbind(round(apply(est.para, 2, mean), 3), HPDint(est.para, prob=0.9)$HPDinterval);
#par1;



