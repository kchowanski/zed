library(shiny)
library(ggplot2)
library(plotly)


shinyUI(fluidPage(
  titlePanel("MovieApp"),
  sidebarLayout(
    sidebarPanel(

      
      sliderInput("bins",
                  "Liczba filmów na wykresie:",
                  min = 5,
                  max = 50,
                  value = 5),
      h4('Lista linków'),
      div('Linki generowane są po zaznaczeniu filmu na liście. Czas odpowiedzi zależy od API IMDB, więc może to chwilę potrwać.', class='alert alert-warning'),
      uiOutput("moreControls")
      
    ),
    mainPanel(
      plotlyOutput("distPlot", height = "600px"),
      fluidRow(
        column(9, DT::dataTableOutput('x3')),
        column(3, verbatimTextOutput('x4'))
      ),
      tags$head(tags$script(src="my.js"))
      
    )
  )
)
)
