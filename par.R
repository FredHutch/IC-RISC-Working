# par.R
# calculates par and writes matrix of perturbations (PAR_pert.RData)
# runs once and saves files

epsilon= 0.001

for(simsim in 0:1) {

  RiskFactorData = read_excel("./required_files/relative risk documentation v2.xlsx")
  # SELECT ON SIM STATUS
  RiskFactorData= filter(RiskFactorData, use_me== 1 & SIM_Status== simsim) 
  RF = select(RiskFactorData[!is.na(RiskFactorData$prev),], Risk_Factor, beta, betase, prev)
  RF = mutate(RF, beta= abs(beta))  #convert betas to positive to be consistent with input
  
  RF1=mutate(RF, RR_x_prev=(exp(beta)*prev),
             RR_x_prev_minus=(exp(beta - epsilon) *prev),
             RR_x_prev_plus=(exp(beta + epsilon) *prev))
  
  RF1$RR_x_prev_minus = with(RF1, ifelse(beta==0,RR_x_prev, RR_x_prev_minus))
  RF1$RR_x_prev_plus = with(RF1, ifelse(beta==0,RR_x_prev, RR_x_prev_plus))       
  
  PAR_ind=ddply(RF1,.(Risk_Factor), summarise, 
                PAR= (sum(RR_x_prev)-1)/sum(RR_x_prev),
                PAR_minus= (sum(RR_x_prev_minus)-1)/sum(RR_x_prev_minus),
                PAR_plus= (sum(RR_x_prev_plus)-1)/sum(RR_x_prev_plus))
  
  if(simsim==0) {
    PAR_prod_neg= 1-prod((1-PAR_ind$PAR))
    PAR_pert_neg= mutate(PAR_ind, 
                    PAR_comb_minus= 1- ( prod(1-PAR)/(1-PAR)*(1-PAR_minus)),
                    PAR_comb_plus = 1- ( prod(1-PAR)/(1-PAR)*(1-PAR_plus)))
    save(PAR_pert_neg, file= "PAR_pert_neg.RData")
    } else if(simsim==1) {
        PAR_prod_pos= 1-prod((1-PAR_ind$PAR))
        PAR_pert_pos= mutate(PAR_ind, 
                    PAR_comb_minus= 1- ( prod(1-PAR)/(1-PAR)*(1-PAR_minus)),
                    PAR_comb_plus = 1- ( prod(1-PAR)/(1-PAR)*(1-PAR_plus))  )
        save(PAR_pert_pos, file= "PAR_pert_pos.RData")
    }
}
