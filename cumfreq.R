
bea_filt = filter(beacon_imp_exp, cc2==0)
# bea_filt = filter(beacon_imp_exp, cc2==1)

risks= bea_filt$risk10
breaks = c(0, 0.6, 1.3, 2, 4,6,8,10,12,14,16,18) 
risks.cut = cut(risks, breaks, right=FALSE) 
risks.freq = table(risks.cut)
risks.cumfreq = cumsum(risks.freq)
myrisks= data.frame(cbind(risks.cumfreq))
myrisks$num_scope= nrow(bea_filt)- risks.cumfreq
myrisks$scope_frac= myrisks$num_scope/nrow(bea_filt)
myrisks$threshold= c(0.6, 1.3, 2, 4,6,8,10,12,14,16,18)

myrisks.controls= myrisks
# myrisks.cases= myrisks

View(myrisks.controls)
# View(myrisks.cases)

# tres2= merge(tres, myrisks.controls)
# tres2= mutate(tres2, test1= specificity+ scope_frac)
