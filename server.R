function(session, input, output) {
  observeEvent(
    input$page,
    updateSelectInput(session, "questions", "question",
                      choices =  c(setNames(recup_questions_formation2(input$page)$id, recup_questions_formation2(input$page)$libelle))
    ),
    ignoreNULL = F
  )
  output$hist2 <- renderPlot({
    tentative_graph(input$page, input$questions, input$bins, input$color)
  })
  #Dataframe
  output$df <- renderDataTable({
    datatable(recup_(input$name2), extensions="Buttons", options=list(
      dom='Bfrtip', pageLength = 100 ,buttons =c('copy','csv','excel','pdf','print')))
  })
  #outputText if a text question is selected
  output$commentaires <- renderDataTable({
    get_data(input$page, input$questions) %>%
      select(texte)
  })
  # conditionne l'affichage des panels sur Data
  output$graph_or_text <- reactive({
    if(input$questions %in% list_of_text_question_id()$id){
      return("texte")
    }else if (input$questions == "vide"){
      return("nothing")
    }else {
      return("graph")
    }
  })
  
  outputOptions(output, "graph_or_text", suspendWhenHidden = FALSE)

  onStop(function(){
    dbDisconnect(con)
  })
}
