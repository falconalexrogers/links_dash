


signalData <- reactiveValues(data = NULL)

signalData$linksTable <- links_output2

signalData$numLinks <- nrow(links_output2)

linksTableForOut <- reactive({
  # invalidateLater(60000)
  if(nrow(signalData$linksTable) > 0){
    linksTable <- signalData$linksTable
    
    return(linksTable)
  } else {
    return('No links')
  }
})


output$linksTable_datatable <- DT::renderDataTable({datatable(
  linksTableForOut(),
  escape = FALSE,
  options = list(
    dom = 't',
    # pageLength = 200,
    paging = FALSE,
    columnDefs = list(
      list(className = 'dt-center', targets = "_all")
    )
  ),
  container = tableHeaderLinksTables,
  rownames=TRUE) %>%
    formatStyle( 0, target= 'row', lineHeight='70%')}, escape = FALSE)


output$linksTableOutput <- renderUI({
  column(width = 8,
         h3(paste0("Links: ",signalData$numLinks), align = 'center'),
         br(),
         box(width = 12,
             DT::dataTableOutput('linksTable_datatable'),
             style = "height:500px; overflow-y: scroll;"),
         br(),
         hr(),
         br()
         )
})







