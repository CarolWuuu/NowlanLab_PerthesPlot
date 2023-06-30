library(shiny)
library(DT)
library(ggplot2)
library(dplyr)
library(magrittr)
library(data.table)
library(plotly)
library(forcats)
library(tidyverse)
library(scales)
library(ggrepel)
library(shinythemes)


# Define UI
ui <- shinyUI(fluidPage(
  
 
  theme = shinytheme("flatly"),
  titlePanel("Plot Generator for Perthes Patients"),
  
  navlistPanel(
    "File Input",
    tabPanel( 
      "Example Datasets",
      h3("Example csv file format for all segments"),
      DT::dataTableOutput("Example1"),
      h3("Example csv file format for malperfused"),
      DT::dataTableOutput("Example2")
    ),
    tabPanel(
      "Upload Files",
      textInput("id", "Patient number:", value = "Patient 1", width = NULL, placeholder = "Patient 1"),
      fileInput('allsegments', 'All segments: Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                )),
      fileInput('malperfused', 'Malperfused: Choose file to upload',
                accept = c(
                  'text/csv',
                  'text/comma-separated-values',
                  '.csv'
                ))
    ),
    tabPanel( 
      "Dataset for all segments",
      DT::dataTableOutput("table1")
    ),
    tabPanel( 
      "Dataset for malperfused",
      DT::dataTableOutput("table2")
    ),
    "Charts",
    tabPanel("Bar Chart",
             sliderInput("ylim", "Max Volume",
                         min = 0, max = 20000, value = 15000
             ),
             plotlyOutput("barchart",height = "600px")),
    tabPanel("Pie Chart: Hip Volumes on Both Sides",
             radioButtons("perthesSide", "The patient has Perthes on :",
                          c("the right side" = "right",
                            "the left side" = "left")),
             plotlyOutput("pie1",height = "600px"),
             plotOutput("pie3")),
    tabPanel("Pie Chart: Perfused VS Malperfused",
             sliderInput("angle", "Pie Chart Rotation",
                         min = 0, max = 360, value = 90
             ),
             checkboxInput("visible", "Show legend", value = FALSE, width = NULL),
             plotlyOutput("pie2",height = "600px")),
  )
)
)

# Define server logic
server <- shinyServer(function(input, output) {
  
  # Display example datasets: Patient 1
  output$Example1<- DT::renderDataTable({
    eg1<- read.csv('patient1.csv')
    DT::datatable(eg1)
  })
  
  output$Example2<- DT::renderDataTable({
    eg2<- read.csv('patient1_malperfused.csv')
    DT::datatable(eg2)
  })
  
  # User input datasets
  # If not user input use the example datasets to generate plots
  df1 <- reactive({
    inFile <- input$allsegments
    if (is.null(inFile)){
      eg1<- read.csv('patient1.csv')
      return(eg1)
    }
    df1 <- read.csv(inFile$datapath, header = TRUE)
    return(df1)
  })
  
  df2 <- reactive({
    inFile <- input$malperfused
    if (is.null(inFile)){
      eg2<- read.csv('patient1_malperfused.csv')
      return(eg2)
    }
    df2 <- read.csv(inFile$datapath, header = TRUE)
    return(df2)
  })
  
  
  # Display user input datasets
  output$table1<- DT::renderDataTable({
    df1 <- df1()
    DT::datatable(df1)
  })
  
  output$table2<- DT::renderDataTable({
    df2 <- df2()
    DT::datatable(df2)
  })
  
  # Generate bar chart
  output$barchart<- renderPlotly({
    
    df1 <- df1() %>% arrange(factor(Segment,levels=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") ))
    Segment=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post
","Malperfusion - Dark on post")
    
    ## seperate tissue 2 from malperfusion 
    tissue2=df1[7,]
    tissue2_perthes=c(0,0,0,0,0,tissue2$VolumePerthes)
    
    ddf=data.table(Segment,
                   Volume=df1$VolumePerthes[c(1:6)],
                   Tissue2=tissue2_perthes[1:6])
    mm <- melt(ddf, id.vars = "Segment", measure.vars = c("Volume", "Tissue2"))
    g2 <- ggplot(mm %>% dplyr::filter(variable == "Volume")) +
      geom_col(aes(x = fct_inorder(Segment), y = value, fill = Segment,text = paste("Segment: ", Segment, "\nVolume: ", value," mm^3")))+
      scale_x_discrete(labels=c("Bone", "Epiphyseal Cartilage", "Physeal Cartilage", "Epiphyseal Tissue 1","Physeal Tissue 1","Malperfusion","Tissue 2"))+
      scale_fill_manual("Segment",values = c("#fdbf6f", "#1f78b4", "#a6cee3", "#33a02c", "#b2df8a", "#e31a1c","#cab2d6"),breaks = c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post
","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") )+ 
      ggtitle(paste("Perthes hip volumes in",input$id)) +
      xlab("Segmentation") +
      ylab("Volume (mm^3)") +
      ylim(0,input$ylim) + 
      geom_col(data = mm %>% dplyr::filter(variable != "Volume"), 
               aes(x = Segment, y = value, fill = "Tissue 2 - Dark T1, Dark T1 fs, Dark post",text = paste("Segment: ", variable, "\nVolume: ", value, " mm^3")), inherit.aes = F)+
      theme(axis.text.x=element_text(size=7,angle=90))
    
    ggplotly(g2,tooltip=c("text"))
    
  })
  
  # Generate pie chart of hip volumes for right and left side
  output$pie1<- renderPlotly({
    
    # Clean dataset
    df1 <- df1() %>% arrange(factor(Segment,levels=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") ))
    Segment=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post
","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post")
    
    left_side <- if(input$perthesSide == "left"){df1[c(1:5,7),c(1,3)]}else{df1[c(1:5,7),c(1,2)]}
    right_side <- if(input$perthesSide == "right"){df1[c(1:5,7),c(1,3)]}else{df1[c(1:5,7),c(1,2)]}
    subtitleL <-if(input$perthesSide == "left"){"Perthes Side"}else{"Healthy Side"}
    subtitleR <-if(input$perthesSide == "right"){"Perthes Side"}else{"Healthy Side"}
    sumP=sum(df1$VolumePerthes)-df1$VolumePerthes[6]
    sumH=sum(df1$VolumeHealthy)
    sumL<-if(input$perthesSide == "left"){sumP}else{sumH}
    sumR<-if(input$perthesSide == "right"){sumP}else{sumH}
    
    
    pie_chart_margins <- list(
      l = 40,
      r = 40,
      b = 30,
      t = 100,
      pad = 4
    )
    # Pie chart for perthes side
    pie <- plot_ly()
    pie <- pie %>% add_pie(left_side, labels = ~fct_inorder(left_side$Segment), values = ~left_side[,2], 
                           domain = list(row = 0, column = 0),
                           title = list(text=paste(subtitleL, "\nTotal Volume:", sumL,"mm^3"),font=list(size=20)), 
                           name = if(input$perthesSide == "left"){"Perthes Side"}else{"Healthy Side"},
                           textposition = 'inside', insidetextfont = list(color = '#FFFFFF',size=20), 
                           marker = list(colors = c("#fdbf6f", "#1f78b4", "#a6cee3", "#33a02c", "#b2df8a","#cab2d6"),
                                         line = list(color = '#FFFFFF', width = 1)),
                           sort=F,
                           direction='clockwise')
    
    # Pie chart for healthy side
    pie <- pie %>% add_pie(right_side, labels = ~fct_inorder(right_side$Segment), values = ~right_side[,2], 
                           domain = list(row = 0, column = 1), 
                           title =  list(text=paste(subtitleR, "\nTotal Volume:", sumR,"mm^3"),font=list(size=20)), 
                           name = if(input$perthesSide == "right"){"Perthes Side"}else{"Healthy Side"},
                           textposition = 'inside', insidetextfont = list(color = '#FFFFFF',size=20), 
                           marker = list(colors = c("#fdbf6f", "#1f78b4", "#a6cee3", "#33a02c", "#b2df8a","#cab2d6"),
                                         line = list(color = '#FFFFFF', width = 1)),
                           sort=F,
                           direction='clockwise')
    
    
    pie <- pie %>% layout(title = list(text=paste("Hip Volumes in", input$id),font=list(size=25)), 
                          showlegend = T,
                          legend = list(orientation = "h",   # show entries horizontally
                                        xanchor = "center",  # use center of legend as anchor
                                        x = 0.5),
                          grid=list(rows=1, columns=2),
                          xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                          margin = pie_chart_margins)
    
    pie
    
  })
  
  # Generate pie chart of malperfused areas
  output$pie2<- renderPlotly({
    
    # Clean dataset
    segment2<-df2()
    segment1 <- df1() %>% arrange(factor(Segment,levels=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") ))
    Segment=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post
","Malperfusion - Dark on post")
    seg_perthes <- segment1[c(1:5,7),c(1,3)]
    malperfusion = segment1$VolumePerthes[6]
    #perfused = sum(seg_perthes$VolumePerthes)-malperfusion
    malperfusedTissue <- data.table(Segment=c('Tissue 2','Malperfused Bone','Epiphyseal Cartilage Overlapped with Malperfusion','Physeal Cartilage Overlapped with Malperfusion'), Volume=c(segment2$VolumePerthes))
    malperfusedTissue = malperfusedTissue[order(factor(malperfusedTissue$Segment,levels=c('Tissue 2','Malperfused Bone','Epiphyseal Cartilage Overlapped with Malperfusion','Physeal Cartilage Overlapped with Malperfusion')))]
    
    # Generate pie charts
    pie_chart_margins <- list(
      l = 40,
      r = 40,
      b = 30,
      t = 100,
      pad = 4
    )
    pie2 <- plot_ly(malperfusedTissue,labels=~fct_inorder(malperfusedTissue$Segment),values=~malperfusedTissue$Volume,
                    type = 'pie',
                    textfont = list(size = 30),
                    textinfo = 'percent',
                    insidetextfont = list(color = '#444'),
                    hoverinfo = 'text',
                    text = ~paste(malperfusedTissue$Volume, ' mm^3'),
                    marker = list(colors = c("#cab2d6","#B88101","#14527B","#758F9F"),
                                  line = list(color = '#444', width = 2)),
                    rotation=input$angle,              
                    showlegend = input$visible,
                    sort=F)
    pie2 <- pie2 %>% layout(title = list(text=paste('Malperfused Area in', input$id),font=list(size=25)),
                            xaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            yaxis = list(showgrid = FALSE, zeroline = FALSE, showticklabels = FALSE),
                            margin = pie_chart_margins)
    
    pie2
  })
  
  # Generate a pie chart showing relative size of hip volumes
  output$pie3<- renderPlot({
    
    segment1 <- df1() %>% arrange(factor(Segment,levels=c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post","Malperfusion - Dark on post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") ))
    segment <- segment1[c(1:5,7),]
    segment <- segment %>% pivot_longer(
      cols = c('VolumeHealthy','VolumePerthes'),
      names_to = 'Condition',
      values_to = 'Volume'
    )
    
    sumP=sum(segment1$VolumePerthes)-segment1$VolumePerthes[6]
    sumH=sum(segment1$VolumeHealthy)
    segment$Total <- 0
    segment$Total[segment$Condition=="VolumeHealthy"] <- sumH
    segment$Total[segment$Condition=="VolumePerthes"] <- sumP
    
    
    segment %>% ggplot(aes(x=Total/2, y=Volume, fill=Segment, width = Total))+
      geom_bar(stat = "identity", position = "fill", color = "white") +
      scale_fill_manual("Segment",values = c("#fdbf6f", "#1f78b4", "#a6cee3", "#33a02c", "#b2df8a","#cab2d6"),breaks = c("Bone - Bright on T1", "Epiphyseal Cartilage - Bright on T1 fs", "Physeal Cartilage - Bright on T1 fs", "Epiphyseal Tissue 1 - Grey T1, Grey T1 fs, White post","Physeal Tissue 1 - Grey T1, Grey T1 fs, White post","Tissue 2 - Dark T1, Dark T1 fs, Dark post") )+ 
      facet_wrap(~ Condition + Total, strip.position = "bottom") +
      coord_polar("y", start = 0, direction = -1)+
      theme_bw(base_size = 20) +
      theme(axis.title = element_blank(),
            axis.ticks = element_blank(),
            axis.text = element_blank(),
            panel.grid = element_blank(),
            panel.border = element_blank(),
            legend.title = element_text(size = 8),
            legend.position = "none",
            strip.background = element_rect(fill = NA, colour = NA),
            strip.text = element_text(size = 20)) +
      ggtitle(paste("Hip volumes in relative size in", input$id))
  })
  
}
)

# Run the application 
shinyApp(ui = ui, server = server)