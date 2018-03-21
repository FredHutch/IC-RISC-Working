# get_functions.R

library(readxl)
library(plyr)
library(tidyverse)
library(ggplot2)
library(cowplot)
library(viridis)
library(ggthemes)
library(ggExtra)
library(scales)
library(xtable)
library(markdown)
library(htmltools)
library(rsconnect)
library(shiny)
library(shinyBS)
library(shinythemes)
# library(assertthat)
# library(car)

get_betas= function(sim) {     # READS AND RETURNS rr/se BASED ON SIM STATUS ====
  # assert_that(sim==0 | sim==1)
  newbees= read_excel("./required_files/relative risk documentation v2.xlsx")
  newbees= mutate(newbees, beta= abs(beta))     # convert betas to all positive 
                                                # (to be consistent with input)
  newbees= select(newbees, Risk_Factor, rflevel_cat, beta, betase, SIM_Status)
  newbees= filter(newbees, SIM_Status==sim)
  
  return(newbees)
}

IREA= function(x, demo_, sim_) {  # AGE/RACE/SEX SPECIFIC INCIDENDE
  # x is (agenow-62): uses coeff from incidence_mortality_models.R

  irea_age= ifelse((demo_== "WM" & sim_== 0), 
                    ar_incid_NEG_WM$coefficients[1] + 
                    ar_incid_NEG_WM$coefficients[2] * x + 
                    ar_incid_NEG_WM$coefficients[3] * x^2 +
                    ar_incid_NEG_WM$coefficients[4] * x^3, 
            ifelse((demo_== "WM" & sim_== 1), 
                    ar_incid_POS_WM$coefficients[1] + 
                    ar_incid_POS_WM$coefficients[2] * x +
                    ar_incid_POS_WM$coefficients[3] * x^2 + 
                    ar_incid_POS_WM$coefficients[4] * x^3, 
            ifelse((demo_== "WF" & sim_== 0), 
                    ar_incid_NEG_WF$coefficients[1] + 
                    ar_incid_NEG_WF$coefficients[2] * x +
                    ar_incid_NEG_WF$coefficients[3] * x^2 + 
                    ar_incid_NEG_WF$coefficients[4] * x^3, 
            ifelse((demo_== "WF" & sim_== 1), 
                    ar_incid_POS_WF$coefficients[1] + 
                    ar_incid_POS_WF$coefficients[2] * x + 
                    ar_incid_POS_WF$coefficients[3] * x^2 + 
                    ar_incid_POS_WF$coefficients[4] * x^3, 
            ifelse((demo_== "BM" & sim_== 0), 
                    ar_incid_NEG_BM$coefficients[1] + 
                    ar_incid_NEG_BM$coefficients[2] * x + 
                    ar_incid_NEG_BM$coefficients[3] * x^2 + 
                    ar_incid_NEG_BM$coefficients[4] * x^3, 
            ifelse((demo_== "BM" & sim_== 1), 
                    ar_incid_POS_BM$coefficients[1] + 
                    ar_incid_POS_BM$coefficients[2] * x + 
                    ar_incid_POS_BM$coefficients[3] * x^2 + 
                    ar_incid_POS_BM$coefficients[4] * x^3, 
                    0))))))

  return(as.numeric(irea_age))
}

MXEA= function(x, demo) {   # estimates age-specific mortality rate from model ====
  # assert_that(demo== "WM" | demo=="WF" | demo=="BM")
  if(demo== "WM"){
    mxea_age= ar_mort_WM$coefficients[1] + 
    ar_mort_WM$coefficients[2] * x + 
    ar_mort_WM$coefficients[3] * x^2 +
    ar_mort_WM$coefficients[4] * x^3
  } else if (demo== "WF") {
    mxea_age= ar_mort_WF$coefficients[1] + 
    ar_mort_WF$coefficients[2] * x + 
    ar_mort_WF$coefficients[3] * x^2 +
    ar_mort_WF$coefficients[4] * x^3
  } else if (demo== "BM") {
    mxea_age= ar_mort_WF$coefficients[1] + 
    ar_mort_BM$coefficients[2] * x + 
    ar_mort_BM$coefficients[3] * x^2 +
    ar_mort_BM$coefficients[4] * x^3
  } else mxea_age= 0
  
return(as.numeric(mxea_age))
}

project_risk2 <- function(beta, AR, age.c, demog, sim_status, delta){  ## project risk ====
  RR= exp(sum(beta))
  up_to_now= 0
  for(i in 0:delta-1) {
    term1= IREA(age.c+ i, demog, sim_status) * (1-AR) * RR / 100000
    term2 = 0
    for(j in 0:i) {
      term2a= IREA(age.c+ j, demog, sim_status) * (1-AR) * RR / 100000
      term2b= MXEA(age.c+ j, demog) / 100000
      term2= term2 + term2a + term2b
     }
    up_to_now= up_to_now + term1* exp(-term2)
  }
  return(up_to_now * 1000)     # per thousand is what is graphed
}

compute_var <- function(epsilon, beta, PAR_pert, age.c, demog, ## variance ====
                        sim_status, delta, se) {
  
  ## PAR=matrix of PAR- and PAR+ (pertubated PARs) - calculated and saved in par.R
  
  numPar=length(beta)
  risktemp <- matrix(0,2,numPar)
  
  for (j in 1:numPar) {
    beta_minus=beta
    beta_plus=beta
    beta_minus[j] <- ifelse(beta_minus[j]==0,beta_minus[j],beta_minus[j] - epsilon)
    beta_plus[j] <- ifelse(beta_plus[j]==0,beta_plus[j],beta_plus[j] + epsilon)
    risktemp[1,j] <- project_risk2(beta_minus,PAR_pert$PAR_comb_minus[j], 
                                   age.c, demog, sim_status, delta) 
    risktemp[2,j] <- project_risk2(beta_plus,PAR_pert$PAR_comb_plus[j], 
                                   age.c, demog, sim_status, delta)
  }
  
  grad <- rep(0,numPar)
  grad = (abs(risktemp[2,] - risktemp[1,])/(2*epsilon)) 
  var <- sum(se^2 * grad^2)
  
  return(var)
}
