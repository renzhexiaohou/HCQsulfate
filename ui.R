##################################################################
# Author: Yubo Xiao                                              #
# Phone: 13971656760                                             #
# email: xiaoyubocpuS@hotmail.com                                #
# script created: Feb,24,2020                                    #
# Software version: R 3.5.3                                      #
##################################################################
#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)


shinyUI(
    
    fluidPage(
        theme = shinytheme("flatly"),
        tags$head(tags$link(rel = "stylesheet", type = "text/css", href = "mystyle.css")),
        navbarPage(
            title = strong("Hydroxychloroquine sulfate (HCQ sulfate)"),
            
            
# simulation -------------------------------------------------

            # tabPanel("模拟预测",
            tabPanel(
                strong("HCQ concentration prediction"),
                #左栏
                fluidRow(
                    column(4,
                           style = "background: #ffffff",
                           
                           br(),
                           br(),
                           br(),
                           # 给药方案的设定 ------------------------------------------
                           # titlePanel(h3(strong("给药方案"))),
                           titlePanel(h3(strong(""))),
                           wellPanel(
                               style = "background: #ededed",
                               fluidRow(
                                   column(3, numericInput("timepoint0", "Time (h)", value = 0, min = 0, step = 1)),
                                   column(3, numericInput("dosage0", "Dose (mg)", value = 200, min = 0, step = 10)),
                                   column(3, numericInput("interval0", "Interval (h)", value = 12, min = 0, step = 2)),
                                   column(3, numericInput("times0", "N", value = 2, min = 1, step = 1)),
                                   tags$div(id = 'placeholder')
                               ),
                               wellPanel(style='background: #ededed; padding:0px;',
                                         fluidRow(style='background: #ededed; padding:0px;',
                                                  column(9,style='background: #ededed; padding:0px;',
                                                         wellPanel(style='background: #ededed; padding:0px;',
                                                                   column(4, style='background: #ededed; padding:14px;',
                                                                          # h5(strong("观测时间"))
                                                                          h5(strong("ObsTime"))
                                                                   ),
                                                                   column(8,style='background: #ededed; padding:4px;',
                                                                          # tags$style(type = "text/css", ".irs-slider {width: 20px; height: 20px; top: 15px;}"),
                                                                          uiOutput("sliderOBSTIME", inline = T)
                                                                   )
                                                         )
                                                  ),
                                                  column(3,style='background: #ededed; padding:0px;',
                                                         wellPanel(style='background: #ededed;',
                                                                   column(6,style='background: #ededed; padding:1px;', 
                                                                          actionButton('insertBtn', NULL, width = "85%",
                                                                                       icon = icon(":O", class = "glyphicon glyphicon-plus"),
                                                                                       style='padding:5px;
                                                                                  color: white; background-color: #ec4c3c;
                                                                                  border-width: 0px;
                                                                                  font-size:70%')), 
                                                                   column(6, style='background: #ededed; padding:1px;',
                                                                          actionButton('removeBtn', NULL, width = "85%",
                                                                                       icon = icon(":X", class = "glyphicon glyphicon-minus"),
                                                                                       style='padding:5px;
                                                                                  color: white; background-color: #3498db;
                                                                                  border-width: 0px;
                                                                                  font-size:70%'))
                                                         )
                                                  )
                                         )
                               ),
                               titlePanel(h5(strong("HCQ plasma concentration range (ng/mL)"))),
                               sliderInput("concrange", label = NULL, min = 0, max = 5000, step = 0.1, value = c(20, 20)),
                               h6("* Dose regimen is the dosage of hydroxychloroquine sulfate.")
                           )
                    ),
                    column(8,
                           # 药时曲线呈现  ----------------------------------------
                           # titlePanel(h3(strong("药时曲线"))),
                           titlePanel(h3(strong("Conc-Time Curve"))),
                           br(),
                           plotOutput(outputId = "pkconcplot1")
                    )
                ),
                fluidRow(
                    column(2,
                           sliderInput("cl_pk", label = "CL/F (L/h)", min = 1, max = 100, step = 0.5, value = c(10.9))),
                    column(2,
                           sliderInput("vc_pk", label = "Vc/F (L)", min = 10, max = 2000, step = 10, value = c(437))),
                    column(2,
                           sliderInput("vp_pk", label = "Vp/F (L)", min = 10, max = 2000, step = 10, value = c(1390))),
                    column(2,
                           sliderInput("q_pk", label = "Q/F (L)", min = 10, max = 100, step = 10, value = c(45.1))),
                    column(2,
                           sliderInput("ka_pk", label = "Ka (1/h)", min = 0.1, max = 2, step = 0.05, value = c(1.15))),
                    column(2,
                           sliderInput("tlag_pk", label = "Tlag (h)", min = 0, max = 1, step = 0.01, value = c(0.39)))
                ),
                absolutePanel(
                    top = 105, right = 335, width = 160, height = 10, draggable = FALSE,
                    titlePanel(h5(strong("AUC Range")))
                ),
                absolutePanel(
                    top = 105, right = 248, width = 160, height = 10, draggable = FALSE,
                    uiOutput("sliderAUC", inline = T)
                ),
                absolutePanel(
                    top = 123, right = 8, width = 220, height = 10, draggable = FALSE,
                    HTML(paste0("<strong>AUC<sub>", textOutput(outputId = "auclower", inline = T), "-",
                                textOutput(outputId = "aucupper", inline = T), "hr</sub> = <code style='color:#ec4c3c;background-color:#F8F9F9'>",
                                textOutput(outputId = "pkauc1", inline = T), "</code> hr*ng/mL</strong>"))
                ),
                absolutePanel(
                    top = 100, left = 55, width = 100, height = 10, draggable = TRUE,
                    img(src="LOGOdMed.png", height = 50)
                )
            ),
            
            
            
# Who we are ? ------------------------------------------------------------
            
            tabPanel("Who we are ?",
                     fluidRow(
                         column(5,
                                h3(HTML("<strong style='color:#ec4c3c;background-color:#F8F9F9'> EDCP </strong> Capability")),
                                hr(),
                                h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Innovative trial </body></strong>")),
                                h5(HTML("<strong style='color:#000000'> - </strong> opimal early phase study design: FIH, POM, POC ")),
                                h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Clinical pharmacology </body></strong>")),
                                h5(HTML("<strong style='color:#000000'> - </strong> BA/BE, FE, DDI, mass balance, special populations, QTc")),
                                h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> PK/PD calculation and analysis </body></strong>")),
                                h5(HTML("<strong style='color:#000000'> - </strong> NCA, exploratory PK/PD relationship analysis")),
                                h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> PK/PD modeling and simulation </body></strong>")),
                                h5(HTML("<strong style='color:#000000'> - </strong> translational PKPD for safety and efficacy dose")),
                                h5(HTML("<strong style='color:#000000'> - </strong> clinical PKPD for dose rationale on RP2D, RP3D")),
                                h5(HTML("<strong style='color:#000000'> - </strong> covariates analysis, optimal sampling, pediatric extrapolation")),
                                h3("  "),
                                h4(HTML("<strong><body  style='color:#000000;background-color:#ffffff'> Strategic consultation </body></strong>")),
                                h5(HTML("<strong style='color:#000000'> - </strong> CDP/TPP, bridging strategy, ethnic sensitivity assessment"))
                                
                         ),
                         column(7,
                                img(src="EDCP.png",height = 440)
                         )
                         
                     )
            ),
            

# footer ------------------------------------------------------------------

            footer = h5(HTML("dMed Copyright 2020 : 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> E </strong>
                       arly <strong style='color:#ec4c3c;background-color:#F8F9F9'> D </strong> evelepment and 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> C </strong> linical 
                       <strong style='color:#ec4c3c;background-color:#F8F9F9'> P </strong>harmacology"), align = "right")
        )
    )
)
