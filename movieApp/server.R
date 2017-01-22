#
# This is the server logic of a Shiny web application. You can run the 
# application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
# 
#    http://shiny.rstudio.com/
#

#install.packages('shiny')
#install.packages('DT')
#install.packages('RCurl')
#install.packages('rjson')
#install.packages('ggplot2')
#install.packages('dplyr')
#install.packages('plotly')

library(shiny)
library(DT)
library(RCurl)
library(rjson)
library(ggplot2)
library(dplyr)
library(plotly)

data <- read.csv('filmdeathcounts.csv')
someenv <- new.env()

shinyServer(function(input, output) {
  
  output$distPlot <- renderPlotly({
    
    topXByKill <- data %>% top_n(input$bins, Body_Count)
    
    topXByKill$Film <- factor(topXByKill$Film, levels = topXByKill$Film[order(-topXByKill$Body_Count)])

    ggplotly(ggplot(topXByKill, aes(y=Body_Count, x=Film)) 
             + geom_bar(stat = 'identity', fill="orange") 
             + theme(axis.text.x = element_text(angle = -90, hjust = 0))
             + ggtitle(paste('Top ', input$bins, ' filmów o największej liczbie śmierci'))
             + labs(y='Liczba śmierci', x='')
             )
  })
  
  
  
  
  output$x3 = DT::renderDataTable(data, server = FALSE)
  
  output$moreControls <- renderUI({
    s = input$x3_rows_selected
    if (length(s)) {

      htmlRes <- '';
      
      for(i in c(s)) {
        row <- data[i,]
        # do stuff with row
        url <- paste('http://www.omdbapi.com/?t=', curlEscape(as.character(row$Film)), '&y=', row$Year, '&r=json', sep = '')
        cat(url)
        cat('\n')

        if(is.null(someenv[[as.character(row$Film)]])) {
          json <- fromJSON(getURL(url))
          someenv[[as.character(row$Film)]] <- json

        } else {
          cat('Url already stored. Use API is pointless.\n')
          json <- someenv[[as.character(row$Film)]]
        }
        
        
        if(json$Response == "True") {
          htmlRes <- paste(htmlRes, '<img style="margin: 10px; width: 50px;height: 50px;border-radius: 50px;" src="', 
                           json$Poster,
                           '"/>')
          htmlRes <- paste(htmlRes,'<a target="_blank" href="http://www.imdb.com/title/', json$imdbID, '/">', json$Title, '</a><br/>', sep='')
        } else {
          htmlRes <- paste(htmlRes, '<p>Movie ', as.character(row$Film), ' not found.</p><br/>', sep='')
        }
        
      }
      

      HTML(htmlRes)
      
    }

  })

  
})
