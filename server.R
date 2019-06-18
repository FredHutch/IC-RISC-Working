#server.R
#LICENSE====
# ### License
# 
# #### Academic and non-profit use
# 
# IC-RISC software is made available for *academic and other non-profit use* under the 2-Clause BSD License:
#   
#   Copyright 2018 Thomas L Vaughan
# 
# Redistribution and use in source and binary forms, with or without modification, are permitted provided that the following conditions are met:
#   
#   1. Redistributions of source code must retain the above copyright notice, this list of conditions and the following disclaimer.
# 
# 2. Redistributions in binary form must reproduce the above copyright notice, this list of conditions and the following disclaimer in the documentation and/or other materials provided with the distribution.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT HOLDER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.
# 
# #### Commercial use
# 
# IC-RISC software will be made available for commercial use under a negotiated license.
# 
# #### Contact
# 
# For academic and other non-profit use, download software at: https://github.com/FredHutch/IC-RISC-Working
# 
# For commercial use, contact Fred Hutchinson Cancer Center, Business Development & Strategy (206.667.4304)

#Read remainer of source files====
source("get_functions.R") # includes libraries
source("par.R")
source("incidence_mortality_models.R")
source("draw_therm.R")
source("draw_happyplot.R")

# Begin shinyServer====
shinyServer(function(input, output) {

###############################  RESPOND TO REQUESTS FOR HELP ====
  observeEvent(input$rf_help, {
    showNotification(includeMarkdown("markdown_files/rf_help.md"),  type= "default", duration= 8)
  })

  observeEvent(input$pf_help, {
    showNotification(includeMarkdown("markdown_files/pf_help.md"),  type= "default", duration= 8)
  })

  observeEvent(input$bg_help, {
    showNotification(includeMarkdown("markdown_files/bg_help.md"),  type= "default", duration= 8)
  })

  observeEvent(input$cf_help, {
    showNotification(includeMarkdown("markdown_files/cf_help.md"),  type= "default", duration= 8)
  })

###############################  OUTPUT MYRISK ====

  output$myrisk = renderPlot({
      withProgress(expr = {  # Adds progress bar to whole renderplot

# READ INPUT FROM UI ====

      project_yrs= 10   ## defines period over which absolute risk is calculated

      sex_race= as.integer(input$sex_race)
      demog= ifelse((sex_race==0),"WM",
             ifelse(sex_race==1, "BM",
             ifelse(sex_race==2, "WF", "BF")))

      agenow= input$age
      age= agenow-62

      bmi= case_when(
        input$bmic< 25 ~ 0,
        input$bmic< 30 ~ 1,
        input$bmic< 35 ~ 2,
        input$bmic< 40 ~ 3,
        input$bmic< 60 ~ 4,
        TRUE ~ as.numeric(NA)
        )

      physical.activity= case_when(       # reverses phys activity input
        input$exercise== 4 ~ 0,
        input$exercise== 3 ~ 1,
        input$exercise== 2 ~ 2,
        input$exercise== 1 ~ 3,
        TRUE ~ as.numeric(NA)
        )

      smoking= as.integer(input$cig2)
      reflux= as.integer(input$refluxfreq)

      nsaids= ifelse(input$nsaid==1, 0, 1)    #flips to inverse
      statins= ifelse(input$statin==1, 0, 1)  #flips to inverse

      family.history= as.integer(input$famhx)

      biopsy.abn= as.integer(input$biopsy)
      segment.length= as.integer(input$segment)

      sim_status= as.integer(input$simstatus)
      screen.neg= ifelse(input$simstatus==0, 1, 0)  # 1-> has been screened and found to be SIM negative
                                                    # 0-> either not screened or SIM positive
      screen.neg= ifelse(screen.neg==1, 0, 1)    #this reverses screen.neg because protective 1= SIM pos or unknown

      if(sim_status!= 1) {     #if not SIM positive, then biomarkers must = 0
        biopsy.abn= 0
        segment.length= 0
        sim_status= 0
      }

# draw thermomenter & calculates Mario 10 year risk and CI ====

      minenv= log(0.40)     # define upper and lower bounds of thermometer
      maxenv= log(540)
      p1= draw_therm(sim_status, agenow, minenv, maxenv, demog)

# prepare input data for merging with betas (creates parm_list2)

      parm_list1= data.frame(reflux, smoking, bmi, nsaids, statins, physical.activity,
                             family.history, biopsy.abn, segment.length, screen.neg)

      temp_parm_names= colnames(parm_list1)
      parm_list2= as.data.frame(t(parm_list1))
      rownames(parm_list2)= NULL
      parm_list2$Risk_Factor= temp_parm_names
      parm_list2= dplyr::rename(parm_list2, parm_val= V1)

# get beta coefficients and standard errors; merge with exposure levels (parm list)

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
      
# Risk estimate (project_risk)

      mario_ebeta= sum(dataToUse$beta)
      mario_RR= exp(mario_ebeta)

      mario_IR5= project_risk2(dataToUse$beta, PAR_prod, agenow-62, demog, sim_status, project_yrs)
# View(mario_IR5)
# Variance estimate (compute_var) (epsilon set in "par.R")

      var_est=compute_var(epsilon, dataToUse$beta, PAR_pert, agenow-62, demog, sim_status, project_yrs, dataToUse$betase)
      se_est= var_est^0.5

      mario_IR5_upperCI= mario_IR5 + 1.96* se_est
      mario_IR5_lowerCI= mario_IR5 - 1.96* se_est

# Place mario data on thermometer ====

      mario_IR5_therm= min(mario_IR5, 501)  #SET MAXIMUM RATE SHOWN ON THERMOMETER
      mario_IR5_therm= max(mario_IR5, 0.50) #SET MINIMUM RATE
      mario_IR5_upperCI_therm= min(mario_IR5_upperCI, 501)
      mario_IR5_lowerCI_therm= max(mario_IR5_lowerCI, 0.50)

# Define colors and shapes and labels====
      if(mario_IR5 > 500) {
        finalrisk= "> 500"
        ntreatc= "> 1 in 2"
      } else if(mario_IR5 < 1.0) {
        finalrisk= "< 1"
        ntreatc= "< 1 in 1,000"
      } else {
        finalrisk= as.character(round(mario_IR5,1))
        ntreatn= as.character((round(1000/mario_IR5, 0)))
        ntreatc= paste0("1 in ", ntreatn)
      }

      if(mario_IR5 > 500) {         # adjust color if incidence over max
        mpc= exp(maxenv) *  0.5
      } else {
        mpc = exp(maxenv) * 1.0
        }

      if(mario_IR5 > 100) {         # color
        mycolor= "#cc0000"          #red
      } else if(mario_IR5 > 20) {
        mycolor= "purple"
      } else if(mario_IR5 > 5) {
        mycolor= "blue"
      } else {
        mycolor= "darkgreen"          #green
        # mycolor= "#336600"          #green
      }

    risklabel= paste0("Estimated 10-year risk")
    risklabel2= paste0(finalrisk, " per 1,000  ", "(", ntreatc, " people)")
    # risklabel3= paste0(ntreatc, " people")

    mario_gscore= (maxenv- minenv)*0.16+ minenv   # ADJUSTS LEFT-MOST EDGE OF  POINTER
    meme= data.frame(mario_gscore, mario_IR5_therm,     # NEEDS TO BE DATA FRAME TO WORK WITH GGPLOT
                     risklabel, age, mpc,
                     mario_IR5_lowerCI_therm, mario_IR5_upperCI_therm)

    if(mario_IR5>500) {
      myshape= "\u2191"          # Up arrow
    }
    else if(mario_IR5>= 0.5) {
      myshape= "\u25C4"   # Triangle
    }
    else {
      myshape= "\u2193"                            # Down arrow
    }

    p1= p1 +      # p1 initially drawn in draw_therm.R
      geom_point(data= meme,
                 mapping= aes(x= mario_gscore, y= ifelse(mario_IR5_therm> 501, 440, mario_IR5_therm), colour= mpc),
                 size=16, shape= myshape)

# Add confidence bar ====

    if(mario_IR5>= 0.6 & mario_IR5<=650) {
      p1= p1 +
      geom_segment(data=meme,
                   mapping= aes(x= minenv+0.08, y= mario_IR5_upperCI_therm,
                                xend= minenv+0.08, yend= mario_IR5_lowerCI_therm, colour= mpc),
                                size=2.2, alpha=0.8)
    }

# Draw happyplot and combine the graphs ====

      pphappy= draw_happyplot(round(mario_IR5))
      foot1= "- Vertical bar at tip of pointer indicates confidence band\n"
      foot2= "- Death rates for selected causes are specific for age/sex/race\n"

      mytitle= ggdraw() + draw_label(risklabel, x = 0.5, y = 0.8, size = 22, colour= "black") +
        draw_label(risklabel2, x = 0.5, y = .24, size = 20, hjust= 0.5, colour= mycolor) 
        # draw_label(risklabel3, x = 0.76, y = .24, size = 20, colour= mycolor)

      dualplot= ggdraw() +
      draw_plot(p1, 0, .02, 0.46, 0.98) +
      draw_plot(pphappy, 0.48, 0.048, .52, 0.952) +
      draw_plot_label(paste0(foot1, foot2),
                      .08, .1, size= 12, hjust= 0, colour= "darkgreen")

      triplot= plot_grid(mytitle, dualplot, ncol=1, rel_heights = c(0.1, 1.2))

      }, message = "Preparing graphs... Please wait")
    return(triplot)
  }, height= 760, width= "auto")

###############################  OUTPUTS RR SIM_NEG TABLE ====
  output$mytable_neg= renderTable({
    rr_neg= as.data.frame(read_excel("./input_data/relative risks.xlsx"))
    rr_neg= filter(rr_neg, SIM_Status==0 & use_me==1)
    rr_neg= select(rr_neg, Risk_Factor_level, rr, lowCI, highCI, Notes)
  })

###############################  OUTPUTS RR SIM_POS TABLE ====
  output$mytable_pos= renderTable({
    rr_pos= as.data.frame(read_excel("./input_data/relative risks.xlsx"))
    rr_pos= filter(rr_pos, SIM_Status==1 & use_me==1)
    rr_pos= select(rr_pos, Risk_Factor_level, rr, lowCI, highCI, Notes)
  })

###############################  BMI Calculator ====
output$mybmi= renderText({
  if(input$eng_met == 0) {
  me_inches= input$heightin + 12* input$heightft
  me_bmi= (input$weightlb* 703 / me_inches^2)
  } else if(input$eng_met == 1) {
  me_bmi= 10000* (input$weightkg / input$heightcm^2)
}
  me_bmi_final= c("BMI = ", as.character(round(me_bmi)))
  return(me_bmi_final)
})

###############################  PHYSICAL ACTIVITY CALCULATOR ====
output$myPA= renderUI({
    tot_modmin= input$mod_min + 60*input$mod_hour
    tot_vigmin= input$vig_min + 60*input$vig_hour
    tot_mets= 3.5* tot_modmin + 7.0* tot_vigmin
    if(tot_mets<200) {
      my_quartile= 1
      } else if(tot_mets<525) {
        my_quartile= 2
        } else if(tot_mets<750) {
            my_quartile= 3
            } else if(tot_mets<9000) {
                my_quartile= 4
              } else my_quartile= NA
    me_mets= HTML("Level = ", as.character(round(my_quartile,0)), "<br/>", "<br/>",
                    as.character(round(tot_mets)), " METs per week", sep="<br/>")
})

###############################  Output forest plots ====
output$rainforest = renderPlot({

get_rf_plot= function(sim) {
  maxCI= 30
  minCI= 0.18
  rforest0= as.data.frame(read_excel("./input_data/relative risks.xlsx"))
  rforest0= mutate(rforest0, rflevel2= ifelse(rforest0$rflevel_cat==0, # indents levels for output
                                            as.character(rforest0$Risk_Factor_level),
                                            paste("     ",rforest0$Risk_Factor_level)))
  rforest0= filter(rforest0, (SIM_Status== sim & use_me== 1))

  rf_plot = ggplot(rforest0,aes(rr, y= factor(rforest0$rflevel2, levels= rev(rforest0$rflevel2)))) +
    geom_point(size=3, shape=18, colour= ifelse(rforest0$SIM_Status, "blue", "firebrick")) +
    # geom_errorbarh(aes(xmax = highCI, xmin = lowCI), height = 0.0) +
    # geom_errorbarh(aes(xmax = ifelse(highCI>maxCI, maxCI, highCI), xmin = lowCI), height = 0.0) +
    geom_errorbarh(aes(xmax = ifelse(highCI>maxCI, maxCI, highCI), xmin = ifelse(lowCI<minCI, minCI, lowCI)), height = 0.0) +
    geom_vline(xintercept = 1, linetype = "longdash", size= 0.8, colour= "blue") +
    scale_x_continuous(trans= "log",
                       breaks= c(.3, .5, 0.7, 1, 1.5, 2.0, 3.0, 5.0, 10, 20, maxCI),
                       limits= c(0.05, maxCI)) +
    labs(title= paste("Relative Risks (95% CI):  ",
                      ifelse(sim==1, "BE positive", "BE negative")),
         x="", y="") +
    geom_text(aes(x=0.0,label=rflevel2),size=4, hjust=0) +
    theme_minimal() +
    theme(plot.title = element_text(size=16, margin = margin(10, 0, 20, 0),hjust = 0.5),
          axis.title.x = element_text(size = rel(1.1), colour = "black"),
          panel.grid.major.x = element_line(colour = "gray", size=0.50),
          panel.grid.major.y = element_blank(),
          panel.grid.minor.x = element_blank(),
          axis.text.y = element_blank(),
          axis.ticks.y = element_blank())
}

p_rf_neg= get_rf_plot(0)
p_rf_pos= get_rf_plot(1)
rf_dual= plot_grid(p_rf_neg, p_rf_pos, ncol=2)
return(rf_dual)
},
height= 660, width= "auto")

###############################  Output incidence plots ====
output$myincidence= renderPlot({
  source("incidence_mortality_models.R")
  return(p1)
},
height= 660, width= "auto")

})    # End of shinyServer

