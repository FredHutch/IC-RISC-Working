#ui.R

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

library(shiny)
library(shinyBS)
library(shinythemes)
library(ggthemes)
library(rsconnect)

shinyUI(navbarPage("IC-RISC", theme = shinytheme("spacelab"),inverse= TRUE,

# about tab ====
tabPanel("About",
        #shinythemes::themeSelector(),

        fluidRow(
        column(1),
        column(10, wellPanel(h3("Personalized Risk Calculator for Esophageal Adenocarcinoma"), align="center"))),

        fluidRow(
        column(1),
        column(10, includeMarkdown("markdown_files/about_text.md"))
         )),

#risk calculator tab ====
    tabPanel("Risk Calculator",
            fluidRow(
              tags$head(
      tags$style(
        HTML(".shiny-notification {
             position:fixed;
             opacity: 0.95;
             background: #d3d3d3;
             top: calc(7.8%);;
             left: calc(50%);;
             }
             "
            ))),

              column(2,

                wellPanel(
                bsButton("bg_help", label= p("Background"),
                         block = TRUE,
                         style = "primary",
                         size = "medium",
                         type = "action"),
                # bsPopover("bg_help", "Background", includeText("markdown_files/bg_help.md")),
                
                br(),
                radioButtons("sex_race", label= p("Sex/Race", style="color:green"),
                             choices = c("Male/White"= 0, "Male/Black"= 1, "Female/White"= 2), selected= 0),

                sliderInput("age", label = p("Age", style="color:green"), value = 62, min= 40, max= 79),

                br(),
                bsButton("pf_help", label=p("Preventive", br(), "factors"),
                         block = TRUE,
                         style = "primary",
                         size = "medium",
                         type = "action"),
                # bsPopover("pf_help", "blbl", includeText("markdown_files/cf_help.md")),

                br(),
                
                
                radioButtons("nsaid", label= p("Aspirin/NSAID use", style="color:green"),
                             choices = c("No"= 0, "Yes"= 1), selected= 0),
                
                radioButtons("statin", label= p("Statin use", style="color:green"),
                             choices = c("No"= 0, "Yes"= 1), selected= 0),
                
                conditionalPanel(condition= "input.simstatus != 1",
                                 sliderInput("exercise", label= p("Physical activity", style="color:green"), min=1, max=4, value= 2)
                ),
                br()
                
              )),

              column(2, p(icon("question-circle-o", "fa-2x",  lib = "font-awesome"),
                                 br(),
                                 "Click blue buttons for more information",
                                 br(),
                                 style = "color:#cc0000", align= "center", size= "large"),
                br(),
                wellPanel(
                  bsButton("rf_help", label=p("Risk", br(), "factors"),
                           block = TRUE,
                           style = "primary",
                           size = "medium",
                           type = "action"),

                
                  br(),
                  
                  radioButtons("refluxfreq", label = p("Reflux symptoms", br(), "(off meds)", style="color:green"),
                              choices= list("Rarely"= 0, "< Weekly"= 1, "Weekly - Daily"= 2, "> Daily" = 3), selected= 1),
                  sliderInput("bmic", label = p("Body Mass Index", style="color:green"), min = 20, max = 45, value = 28),
                  radioButtons("cig2", label = p("Cigarette use", style="color:green"),
                              choices = c("Never"= 0, "Ever"= 1), selected= 0),
                  
                  conditionalPanel(condition= "input.simstatus != 1",
                                   
                                   radioButtons("famhx", label= p("Family history", style="color:green"),
                                                choices = list("No"= 0, "1"= 1, "2+"= 2), selected= 0))
              )),

              column(2,

                wellPanel(
                br(),
                
                bsButton("cf_help", label=p("Screening", br(), "results"),
                         block = TRUE,
                         style = "primary",
                         size = "medium",
                         type = "action"),
                br(),

                radioButtons("simstatus", label= p("Barrett's status", style="color:green"),
                             choices = list("Unknown"= 9, "Negative"= 0, "Positive"= 1), selected = 9),

                conditionalPanel(
                condition= "input.simstatus == 1",
                radioButtons("segment", label = p("Segment length", style="color:green"),
                           choices= list("<1"= 0, "1 - 3"= 1, "4 - 6"= 2, "7 +" = 3), selected= 1),

                radioButtons("biopsy", label= p("Biopsy results", style="color:green"),
                             choices = c("No dysplasia"= 0, "LG dysp only"= 1, "HG dysp or abnorm DNA or p53"= 2), selected= 0))),

                br(), br()

              ),

            column(6, plotOutput("myrisk"))
            )),
            #
            # fluidRow(
            #   column(6),
            #   column(6, includeMarkdown("markdown_files/myfootnote.md"))
            #),

#BMI tab ====
tabPanel("Body Mass Index",
         fluidRow(
           column(1),
           column(10, wellPanel(h3("Calculate Body Mass Index (BMI)"), align="center"))),

         fluidRow(
           column(1),
           column(2, wellPanel(
             radioButtons("eng_met", label= h4("Scale"),
                          choices = list("English"= 0, "Metric"= 1), selected = 0))),

           conditionalPanel(
             condition= "input.eng_met == 0",
             column(3, wellPanel(
               sliderInput("heightft", label = h4("Height (feet)"), value = 5, min= 4, max= 7),
               sliderInput("heightin", label = h4("Height (inches)"), value = 8, min= 0, max= 11),
               sliderInput("weightlb", label = h4("Weight (lbs)"), value = 178, min= 80, max= 300)))),

           conditionalPanel(
             condition= "input.eng_met == 1",
             column(3, wellPanel(
               sliderInput("heightcm", label = h4("Height (cm)"), value = 168, min= 135, max= 215),
               sliderInput("weightkg", label = h4("Weight (kg)"), value = 76, min= 38, max= 140)))),

           column(5, h3(textOutput("mybmi")), hr(), br(), br(),
                  h4("More information on BMI and health can be found at", br(), br(),
                     a(href= "http://www.cdc.gov/healthyweight/assessing/bmi/adult_bmi/index.html",
                       "Centers for Disease Control", target="_blank"),
                     br(), br(), "and",
                     br(), br(),
                     a(href= "http://www.nhlbi.nih.gov/health/educational/lose_wt/index.htm",
                       "National Institutes of Health", target="_blank")))
         )),

#phys activity tab ====
    tabPanel("Physical Activity",
         fluidRow(
           column(1),
           column(10, wellPanel(h3("Calculate Physical Activity Category"), align="center"))),

         fluidRow(
           column(1),
           column(3, wellPanel(
             h4("Moderate physical activity per week"),
             sliderInput("mod_hour", label = p("Hours"), value = 2, min= 0, max= 14),
             sliderInput("mod_min", label = p("Minutes"), value = 0, min= 0, max= 90))),
           column(3, wellPanel(
             h4("Vigorous physical activity per week"),
             sliderInput("vig_hour", label = p("Hours"), value = 0, min= 0, max= 14),
             sliderInput("vig_min", label = p("Minutes"), value = 0, min= 0, max= 90))),
           column(4, h3(htmlOutput("myPA")), hr(), br(), br(),
                  h4("More information on physical activity and health can be found at", br(), br(),
                     a(href= "http://www.cdc.gov/physicalactivity/index.html",
                       "Centers for Disease Control", target="_blank")))
         )),


#Risk factor tab ====
navbarMenu("Risk factors",

           tabPanel("Risk factor summary",
                    fluidRow(column(1),
                             column(10, wellPanel(h3("Risk factors by Barrett's status", align= "center")))),
                    fluidRow(column(1),
                             column(10, plotOutput("rainforest")))
           ),

           tabPanel("Input table - Barrett's negative",

                    fluidRow(
                      column(1),
                      column(10, wellPanel(h3("Parameter Values - BE negative", align= "center")))),
                    fluidRow(
                      column(1),
                      column(10, tableOutput("mytable_neg")))
           ),

           tabPanel("Input table - Barrett's positive",

                    fluidRow(
                      column(1),
                      column(10, wellPanel(h3("Parameter Values - BE positive", align= "center")))),
                    fluidRow(
                      column(1),
                      column(10, tableOutput("mytable_pos")))
           )),

#more information tab ====
    navbarMenu("More Information...",

    tabPanel("What's New",
         fluidRow(
           column(1),
            column(10, wellPanel(h3("Change Log"), align="center"))
          ),

         fluidRow(
            column(1),
            column(10, includeMarkdown("markdown_files/whats_new.md")))
          ),

    tabPanel("EA incidence and overall mortality",
         fluidRow(
           column(1),
           column(10, wellPanel(h3("EAC incidence and overall mortality by age", align= "center")))
           ),

         fluidRow(
           column(1),
           column(10, plotOutput("myincidence")))
    ),

    tabPanel("License/Contact",
             fluidRow(
               column(1),
               column(10, wellPanel(h3("License"), align="center"))
             ),
             
             fluidRow(
               column(1),
               column(10, includeMarkdown("markdown_files/license.md")))
    ),
    
    tabPanel("Acknowledgements",
             fluidRow(
               column(1),
               column(10, wellPanel(h3("Acknowledgements"), align="center"))
             ),
             
             fluidRow(
               column(1),
               column(10, includeMarkdown("markdown_files/acknow.md")))
    ),
    
    tabPanel("Further Information",
        fluidRow(
          column(1),
          column(10, wellPanel(h3("References"), align="center"))
          ),

        fluidRow(
          column(1),
          column(10, img(src="contextnatgen.png", height = 300, width = 600), align="center")
          ),

        fluidRow(
          column(1),
          column(10, includeMarkdown("markdown_files/further_info.md"))
          )
    ))
))
