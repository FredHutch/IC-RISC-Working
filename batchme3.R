# batchme3.R

project_yrs= 10   ## defines period over which absolute risk is calculated

beacon_imp$risk10= NA

# Calculate 10 year risk for each record====

for(i in 1:nrow(beacon_imp)) {
  parm_list1= beacon_imp[i,]
  indx <- sapply(parm_list1, is.factor)
  parm_list1[indx] <- lapply(parm_list1[indx], function(x) as.numeric(as.character(x)))
  
  sim_status= 0
  agenow= parm_list1$age
  age= agenow- 62
  sex= parm_list1$sex
  race= parm_list1$race
  
  demog= ifelse(sex==1 & race==1,"WM", 
                ifelse(sex==2 & race==1, "WF",
                  ifelse(sex==1 & race==2, "BM", "NA")))
  
  parm_list2= gather(parm_list1, Risk_Factor, parm_val, site:segment.length)
  
  beta_list2= get_betas(sim_status)
  
  beta_list3= merge(parm_list2, beta_list2, 
                    by.x=c("Risk_Factor", "parm_val"), 
                    by.y= c("Risk_Factor", "rflevel_cat"))
  if(sim_status==0) {
    PAR_prod= PAR_prod_neg
    PAR_pert= PAR_pert_neg
  }
  if(sim_status==1) {
    PAR_prod= PAR_prod_pos
    PAR_pert= PAR_pert_pos
  }
  dataToUse = merge(PAR_pert, beta_list3, "Risk_Factor")
  
  mario_IR10= project_risk2(dataToUse$beta,PAR_prod, agenow-62, demog, sim_status, project_yrs)
  beacon_imp$risk10[i]= mario_IR10
}

source("batch_plot.R")

