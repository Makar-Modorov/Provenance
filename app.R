library(shiny)
library(stringr)
library(dplyr)



find_one_parent <- function (parent_base, offspring_base, nloci)
{
  nloci <- nloci
  
  for (k in 1:nrow(offspring_base))
  {
    find_father_rez <- data.frame(offspring_name = rep(NA, nrow(parent_base)),
                                  offspring_number = rep(NA, nrow(parent_base)),          
                                  sir_name = parent_base[,1],
                                  sir_number = parent_base[,2],
                                  good = rep(0, nrow(parent_base)),
                                  bad = rep(0, nrow(parent_base)),
                                  no_data = rep(0, nrow(parent_base)))
    
    
    for (j in 1:nrow(parent_base))
    { rrr <- c(rep(NA, nloci))
    
    for (i in 1:nloci)
    {
      ccc <- c(as.numeric(parent_base[j, (2*i+1):(2*i+2)]), as.numeric(offspring_base[k, (2*i+1):(2*i+2)]))
      
      if ((class(ccc)!=c("numeric") | sum(is.na(ccc))>0)==TRUE)
      {find_father_rez$no_data[j] <- find_father_rez$no_data[j]+1}
      else {
        if ((ccc[1]==ccc[3] | ccc[2]==ccc[3] | ccc[1]==ccc[4] | ccc[2]==ccc[4])==TRUE)
        {find_father_rez$good[j] <- find_father_rez$good[j]+1}
        if ((ccc[1]==ccc[3] | ccc[2]==ccc[3] | ccc[1]==ccc[4] | ccc[2]==ccc[4])==FALSE)
        {find_father_rez$bad[j] <- find_father_rez$bad[j]+1}
      }}}
    
    find_father_rez_2 <- cbind(find_father_rez, parent_base[,3:32])
    
    find_father_rez_3 <- find_father_rez_2[order(find_father_rez_2$bad, decreasing = FALSE), ]
    
    find_father_rez_final <- rbind(c(offspring_base[k,1], offspring_base[k,2],
                                     c(offspring_base[k,1], offspring_base[k,2]),
                                     rep(NA,3), as.numeric(offspring_base[k,c(3:32)])),
                                   find_father_rez_3)
    find_father_rez_final_2 <- find_father_rez_final %>% replace(is.na(.), c(""))
    
    if (k==1)
    {super_final <- find_father_rez_final_2}
    else
    {super_final <- rbind(super_final, find_father_rez_final_2)}
  }
  return(super_final)
}  


sort_find_one_parent <- function(data, bad)
{data_rez <- data[-which(data$bad>bad),]
return(data_rez)}


ui_upload <- fluidPage(
  titlePanel("Проба пера"),
  sidebarLayout(
    sidebarPanel(
      fileInput("parents", "aaa"),
      fileInput("offspring", "aaa"),
      numericInput("nloci", "aaa", value = 12, min = 1),
      downloadButton("download1", "aaa"),
      numericInput("penalti", "aaa", value = 0, min = 0),
      downloadButton("download2", "aaa")
                ),
    mainPanel(
      tabsetPanel(
        tabPanel("aaa", tableOutput("parents_rez")),
        tabPanel("aaa", tableOutput("offspring_rez")),
        tabPanel("aaa", tableOutput("test_rez")),
        tabPanel("aaa", tableOutput("test_rez_sort"))
      )                               
    )
  )
)

server <- function(input, output, session)
{
   output$parents_rez <- renderTable({read.csv(req(input$parents$datapath), sep=';')})
  
  output$offspring_rez <- renderTable({read.csv(req(input$offspring$datapath), sep=';')})
  
  mmm <- reactive({find_one_parent(read.csv(req(input$parents$datapath), sep=';'),
                                   read.csv(req(input$offspring$datapath), sep=';'), input$nloci)})
  
  output$test_rez <- renderTable({ mmm() })

  mmm2 <- reactive({sort_find_one_parent(mmm(), input$penalti)})
  output$test_rez_sort <- renderTable({ mmm2() })  
  
  output$download1 <- downloadHandler(
    filename = function() {"result_full.csv"},
    content = function(file) {write.csv2 (mmm(), row.names = FALSE, file)})

  output$download2 <- downloadHandler(
    filename = function() {"result_filtr.csv"},
    content = function(file) {write.csv2 (mmm2(), row.names = FALSE, file)})
  }

shinyApp(ui_upload, server)