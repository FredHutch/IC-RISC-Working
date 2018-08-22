#incidence_mortality_models.R       SOURCED BY SERVER.R

source("get_functions.R")

agerate= read_excel("./input_data/stratified_EA_rates.xlsx")
agerate$age.c= agerate$age-62
agerate= mutate(agerate, ACM_XEA_wm= ACM_wm - mortality_wm_EA,
                ACM_XEA_wf= ACM_wf - mortality_wf_EA,
                ACM_XEA_bm= ACM_bm - mortality_bm_EA)

####################  Fit Models (Mortality is overall mortality minus EA-specific mortality====
#WHITE MALES

ar_incid_NEG_WM = lm(agerate$rate_sim_neg_wm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_incid_POS_WM = lm(agerate$rate_sim_pos_wm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_mort_WM = lm(agerate$ACM_XEA_wm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))

#WHITE FEMALES

ar_incid_NEG_WF = lm(agerate$rate_sim_neg_wf ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_incid_POS_WF = lm(agerate$rate_sim_pos_wf ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_mort_WF = lm(agerate$ACM_XEA_wf ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))

#BLACK MALES

ar_incid_NEG_BM = lm(agerate$rate_sim_neg_bm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_incid_POS_BM = lm(agerate$rate_sim_pos_bm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))
ar_mort_BM = lm(agerate$ACM_XEA_bm ~ agerate$age.c + I(agerate$age.c^2) + I(agerate$age.c^3))

####################  Display fits ====

p_incid_NEG_BOTH= ggplot(agerate, aes(x=age)) +
  xlim(40,90) +
  scale_y_continuous(breaks=seq(0,50,10)) +
  geom_point(aes(y= rate_sim_neg_wm), shape= 16, size=2, color="blue") +
  stat_smooth(aes(y= fitted(ar_incid_NEG_WM)), method = 'lm', formula =  y ~ poly(x,3), color="blue") +

  geom_point(aes(y= rate_sim_neg_wf), shape= 16, size=2, color="red") +
  stat_smooth(aes(y= fitted(ar_incid_NEG_WF)), method = 'lm', formula =  y ~ poly(x,3), color="red") +

  geom_point(aes(y= rate_sim_neg_bm), shape= 16, size=2, color="purple") +
  stat_smooth(aes(y= fitted(ar_incid_NEG_BM)), method = 'lm', formula =  y ~ poly(x,3), color="purple") +

  labs(title = "EA incidence - SIM neg") +
  theme(plot.title = element_text(size=14, margin = margin(4, 0, 10, 0)),
        panel.grid.major.y = element_line(colour = "gray", size=0.50),
        panel.grid.minor.y = element_line(colour= "gray", size= 0.50)) +
  xlab(NULL) +
  ylab("Rate per 100,000")

p_incid_POS_BOTH= ggplot(agerate, aes(x=age)) +

  xlim(40,90) +
  scale_y_continuous(breaks=seq(0,1800,200)) +
  expand_limits(y=c(0,1000)) +
  geom_point(aes(y= rate_sim_pos_wm), shape= 16, size=2) +
  stat_smooth(aes(y= fitted(ar_incid_POS_WM)), method = 'lm', formula =  y ~ poly(x,3), color="blue") +

  geom_point(aes(y= rate_sim_pos_wf), shape= 16, size=2, color="red") +
  stat_smooth(aes(y= fitted(ar_incid_POS_WF)), method = 'lm', formula =  y ~ poly(x,3), color="red") +

  geom_point(aes(y= rate_sim_pos_bm), shape= 16, size=2, color="purple") +
  stat_smooth(aes(y= fitted(ar_incid_POS_BM)), method = 'lm', formula =  y ~ poly(x,3), color="purple") +
  labs(title = "EA incidence - SIM pos") +
  theme(plot.title = element_text(size=14, margin = margin(4, 0, 10, 0)),
        panel.grid.major.y = element_line(colour = "gray", size=0.50),
        panel.grid.minor.y = element_line(colour= "gray", size= 0.50)) +
  xlab(NULL) +
  ylab(NULL)

p_mortal_BOTH= ggplot(agerate, aes(x=age)) +

  xlim(40,90) +
  scale_y_continuous(breaks=seq(0,16000,2000)) +
  geom_point(aes(y= ACM_wm), shape= 16, size=2) +
  stat_smooth(aes(y= fitted(ar_mort_WM)), method = 'lm', formula =  y ~ poly(x,3), color="blue") +

  geom_point(aes(y= ACM_wf), shape= 16, size=2, color="red") +
  stat_smooth(aes(y= fitted(ar_mort_WF)), method = 'lm', formula =  y ~ poly(x,3), color="red") +

  geom_point(aes(y= ACM_bm), shape= 16, size=2, color="purple") +
  stat_smooth(aes(y= fitted(ar_mort_BM)), method = 'lm', formula =  y ~ poly(x,3), color="purple") +

  labs(title = "All Cause Mortality\n(except EA)") +
  theme(plot.title = element_text(size=14, margin = margin(4, 0, 10, 0)),
        panel.grid.major.y = element_line(colour = "gray", size=0.50)) +
  xlab("Age") +
  ylab("Rate per 100,000")

p1= ggdraw() +
  draw_plot(p_incid_NEG_BOTH, 0, 0.5, 0.5, 0.5) +
  draw_plot(p_incid_POS_BOTH, 0.5, 0.5, 0.5, 0.5) +
  draw_plot(p_mortal_BOTH, 0.20, 0.04, 0.60, 0.46) +
  draw_plot_label(c("white/male", "black/male", "white/female"),
                  c(0.12, 0.12, 0.12), c(0.90, 0.88, 0.86),
                  colour=c("blue", "purple", "red"), size= 11, hjust= 0) +
  # draw_plot_label("*all rates per 100,000 per year",
  #                 .02, .05, size=11, hjust= 0) 
  draw_plot_label("*dots represent source data; lines are fitted models used in calculations",
                  .02, .03, size=11, hjust= 0)

p1
