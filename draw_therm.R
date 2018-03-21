#draw_therm.R

draw_therm= function(sim, agenow, minenv, maxenv, demog) {

graphratio= 11.0    # DETERMINES RELATIVE HEIGHT OF THERMOMETER
pixeldensity= 220   # NUMBER OF PIXELS TO "LOOK GOOD"
  
beta_list= get_betas(sim)

# Define risk spacE ====

x=seq(minenv, maxenv, length= pixeldensity/3)
y=seq(minenv, maxenv, length= pixeldensity)

riskfield= merge(x, y, by= NULL)
riskfield= dplyr::rename(riskfield, b_fix= x, b_env= y)
riskfield= mutate(riskfield, IR5= exp(b_env))

maxIR5= max(riskfield$IR5)

# Produce background thermometer ====

me_begin= 0.14 # defines portion of color spectrum (0-1) to use
me_end= 1.0

p1= 
  ggplot(riskfield, aes(b_fix, IR5, color= IR5)) +
  theme_tufte(base_family="") +
  theme(axis.title=element_text(face="bold",
        size="18", color="brown")) +
  theme(plot.title=element_text(face="bold",
        size="18", color="black")) +
  theme(legend.position= "none") +
  geom_point() +
  coord_fixed(ratio= graphratio) +
  scale_color_viridis(option = "B", discrete= FALSE, 
                      begin= me_begin, end= me_end, name=NULL) +
  labs(list(y= "EA incidence rate\n", x= "")) +
  scale_x_continuous(trans= "identity", breaks= NULL) + 
  scale_y_continuous(trans= "log10", 
                     breaks= c(0.5, 1, 2, 5, 10, 20, 50, 100, 200), 
                     labels= c(0.5, 1, 2, 5, 10, 20, 50, 100, 200))

# Add comparison diseases ====

disease_comp= read_excel("required_files/mortality_comparisons.xlsx")
disease_comp= gather(disease_comp, demogroup, incidence10, WM:BM)

# PLACE TEXT SLIGHTLY RIGHT OF MID-THERMOMETER
disease_comp= mutate(disease_comp, xleft= (maxenv- minenv)*0.50+ minenv) 
disease_comp= mutate(disease_comp, xright= maxenv)
disease_comp= mutate(disease_comp, yright= incidence10)
disease_comp= mutate(disease_comp, yleft= incidence10)

# Filter on age and demographics
# IGNORE INCIDENCE RATES OUTSIDE OF THERMOMETER RANGE

# agenow= recode(agenow, "40:44=42; 45:49=47; 50:54=52; 55:59=57; 
#                60:64=62; 65:69=67; 70:74=72; 75:79=77; else=NA")

agenow= case_when(
  agenow< 45 ~ 42,
  agenow< 50 ~ 47,
  agenow< 55 ~ 52,
  agenow< 60 ~ 57,
  agenow< 65 ~ 62,
  agenow< 70 ~ 67,
  agenow< 75 ~ 72,
  agenow< 80 ~ 77,
  TRUE ~ as.numeric(NA)
)

disease_comp2= filter(disease_comp, age== agenow, demogroup == demog, 
                      incidence10<= exp(maxenv), incidence10>= exp(minenv))

p1= p1 + 
  geom_text(data= disease_comp2, 
            mapping= aes(x= xleft, y= yleft, color= maxIR5* 1.1, 
                         label= paste0(disease_comp2$disease)),
            hjust=0, nudge_x= +0.05, nudge_y= 0.04, size=3.6) +
  geom_segment(data= disease_comp2, 
               mapping= aes(x=xleft, xend=xright, y= yleft, yend= yright, 
                            color= maxIR5* 0.9), 
               size= 0.7, linetype= 1)

return(p1)
}
