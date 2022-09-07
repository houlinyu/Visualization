#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
library(shiny)
library(tidyverse)
library(pheatmap) 
library(readxl)

#ui.R
ui <- fluidPage(
    titlePanel("Fusarium oxysporum infected Arabidopsis: RNA-seq"), sidebarLayout(
        sidebarPanel(
            helpText("This app shows how a user can upload a .xlsx file with Arabidopsis genes of interests to plot the expression pattern; Download below for a sample file you can upload, modify accoringly (Do not change the header)."),
            a("sample file",  href="https://www.dropbox.com/s/t3q2eayogbe0bgl/shiny_data.csv?dl=0"),
            fileInput("file","Upload the file"), 
        ),
        mainPanel(
            plotOutput("heatmap")             
        )
    )
)


#server.R
server <- function(input,output){
    data <- reactive({
        file1 <- input$file
        if(is.null(file1)){return()} 
        list <- read_xlsx(path=file1$datapath)})

    output$heatmap <- renderPlot({
        if (is.null(data())) { return() }
        list <- data()
        trans <- read_csv('normalized_counts_mean.csv')
        combine <- left_join(list, trans, by = c('Gene' = 'X1'))
        combine %>% select(Fo47_96hpi,Fo47_48hpi,Fo47_24hpi,Fo47_12hpi,Mock_12hpi,Fo5176_12hpi,Fo5176_24hpi,Fo5176_48hpi,Fo5176_96hpi)-> mat1
        mat2 <- as.matrix(mat1)
        mat3 <- log2(mat2 + 1)
        mat4 <- t(scale(t(mat3)))
        row.names(mat4) <- combine$Symbol
        print(mat4)
        print(pheatmap(mat4, cluster_cols = F, cluster_rows = T, show_rownames = T,  angle_col = 45, display_numbers = F, cellwidth = 20, cellheight = 9))
    })
}


shinyApp(ui = ui, server = server)