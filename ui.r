

library(AMR)
library(data.table)
library(DT)
library(ggridges)
library(lubridate)
library(plotly)
library(qicharts2)
library(rintrojs)
library(shiny)
library(shinyBS)
library(shinydashboard)
library(shinycssloaders)
library(shinyjs)
library(shinyWidgets)
library(survival)
library(ggpubr)
library(survminer)
library(tidyverse)
library(dplyr)
library(viridis)
library(zoo)
library(gapminder)
library(gganimate)
library(transformr)
library(readr)
library(gifski)
#library(tseries) # time series analysis
library(forecast) # time series analysis
#library(prophet) # time series analysis
#library(timetk)
#library(fpp2)




fluid_design <- function(id, w, x, y, z, t1, t2, t3, t4) {
  fluidRow(div(
    id = id,
    column(width = 6,
           plotlyOutput(w, width = "500px")),
    column(width = 6,
           plotlyOutput(y, width = "500px")),
    column(width = 6,
           plotlyOutput(x, width = "500px")),
    column(width = 6,
           plotlyOutput(z, width = "500px"))
  ))
}

fluid_design3 <- function(id, w, x, y, z, t1, t2, t3, t4) {
  fluidRow(div(
    id = id,
    column(width = 6,
           plotlyOutput(y, width = "500px")),
    column(width = 6,
           imageOutput(w, width = "800px")),
    column(width = 6,
           plotlyOutput(x, width = "500px")),
    column(width = 6,
           plotlyOutput(z, width = "500px"))
  ))
}

fluid_design2 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    box(width = 12, column(width = 12,
           plotlyOutput(x), align = "center"),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           plotlyOutput(y, width = "500px")),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           plotlyOutput(z, width = "500px")),
        collapsible = TRUE,
        status = "primary")
  ))
}
fluid_design5 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    box(width = 12, column(width = 12,
           div(plotlyOutput(x), align = "center")),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           imageOutput(y)),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           plotlyOutput(z, width = "500px")),
        collapsible = TRUE,
        status = "primary")
  ))
}
fluid_design4 <- function(id, x, y, z, t1, t2, t3) {
  fluidRow(div(
    id = id,
    box(width = 12, column(width = 12,
           div(imageOutput(x), align = "center")),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           plotlyOutput(y)),
        collapsible = TRUE,
        status = "primary"),
    box(width = 6, column(width = 12,
           plotlyOutput(z)),
        collapsible = TRUE,
        status = "primary")
  ))
}

fluid_design6 <- function(id, x) {
  fluidRow(div(id = id,
               box(width = 12,
               column(
                 width = 12,
                 div(plotOutput(x), align = "center")
               ),
               collapsible = TRUE,
               status = "primary")))
}


ui <- dashboardPage(
  skin = "black",
  title = "World",
  dashboardHeader(title = span(
    icon("globe", class = "fa-solid"),
    "GLOBAL SNAPSHOT"
  )),
  dashboardSidebar(
      textOutput(" "),
      fluidRow(width = 12, plotlyOutput("globe")),
      fluidRow(width = 12,
        valueBox(
          width = 6,
          value = tags$p("7.734B", style = "font-size: 50%; color:black; text-align: center"),
          subtitle = tags$p("World Population", style = "font-size: 100%; color:black; text-align: center"),
        ),
        valueBox(
          width = 6,
          value = tags$p("93.9%", style = "font-size: 50%; color:black; text-align: center"),
          subtitle = tags$p("Trade (% of GDP)", style = "font-size: 100%; color:black; text-align: center"),
        )
      ),
      fluidRow(width = 12,
        valueBox(
                 width = 6,
                 value = tags$p( "1064 KWH", style = "font-size: 50%; color:black; text-align: center"),
                 subtitle = tags$p("Electric Consumption (per Capita)", style = "font-size: 100%; color:black; text-align: center"),
        ),
        valueBox(
                 width = 6,
                 value = tags$p("4.59 GT", style = "font-size: 50%; color:black; text-align: center"),
                 subtitle = tags$p("Green House Emission (Giga Tonne)", style = "font-size: 100%; color:black; text-align: center"),
        )
      )
  ),
    dashboardBody(
      tags$head(
        tags$link(rel = "stylesheet",
                  type = "text/css",
                  href = "mystyle.css"),
        tags$style(
          HTML(
            "
            code {
                display:block;
                padding:0px;
                margin:0 10 0 0px;
                margin-top:10px;
                font-size:12px;
                line-height:25px;

                white-space:pre-wrap;
                background-color:#c9e3cc;
                border:2px solid rgba(0,0,0,0.15);
                border-radius:4px;
                font-family:monospace;
                font-weight:bold;
                color:black;
                width:100px;
                text-align:center;
            }"
          )
        )
      ),
      
      useShinyjs(),
      introjsUI(),
      
      # MAIN BODY ---------------------------------------------------------------
      
      
      fluidRow(column(
        width = 12,
        introBox(
          bsButton(
            "social",
            label = "SOCIAL",
            icon = icon("globe", class = "fa-solid"),
            style = "success"
          ),
          bsButton(
            "economic",
            label = "ECONOMIC",
            icon = icon("wallet"),
            style = "success"
          ),
          bsButton(
            "environmental",
            label = "ISSUES",
            icon = icon("flask", class = "flask-box"),
            style = "success"
          ),
          bsButton(
            "modeling",
            label = "MODELING",
            icon = icon("signal"),
            style = "success"
          )
        )
      )),
      
      fluid_design5(
        "economic_panel",
        "box1",
        "box2",
        "box3",
        "Title 1",
        "Title 2",
        "Title 3"
      ),
      fluid_design2(
        "environmental_panel",
        "box5",
        "box6",
        "box7",
        "Title 1",
        "Title 2",
        "Title 3"
      ),
      fluid_design6("modeling_panel",
                    "box_los1"),
      fluid_design4(
        "social_panel",
        "box_pat",
        "box_pat2",
        "box_year",
        "Title 1",
        "Title 2",
        "Title 3"
      )
      
    )
  )

