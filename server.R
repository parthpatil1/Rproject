function(input,output) {
  
  final_data <- reactive({
    reg_qt<-subset(imdb, 
                   genres==input$genres & 
                     language==input$language)
    
    return(reg_qt)
  })
  
  
 
  
  output$share<-renderDataTable({
    
    DT::datatable(final_data(),filter = 'none', selection = "none", rownames= TRUE,
                  options = list(scrollX = TRUE,
                                 scrollY = TRUE,
                                 paging = FALSE,
                                 dom="t",
                                 columnDefs = list(
                                   list(className = 'dt-center',
                                        targets = "_all")),
                                 scrollCollapse = TRUE,
                                 tabIndex = 1,
                                 ordering = FALSE,
                                 searching = FALSE,
                                 dom = 'Bfrtip'
                  ))
    
    
  })
  
  output$bubble<-renderPlot({ 
    data<-final_data()
    graph <- ggplot(data,aes(x=title_year,y=language,size=country,color=imdb_score))+
      geom_point(alpha=6)+
      ggtitle(" Year V/S Language (By Country)")+
      theme(plot.title = element_text(hjust = 0.5))
   
   
    
    return(graph)
    
    
  }) 
  output$bar=renderPlot({
    data<-final_data()
    print(data)
    data %>% 
      arrange(desc(imdb_score)) %>% 
      slice(1:10) %>% 
      ggplot(aes(x = imdb_score, y = reorder(movie_title, imdb_score), fill = country)) +
      geom_bar(stat = "identity", width = 0.8) +
      labs(title = "Top 10 Movies by IMDB Score and Country",
           x = "IMDB Score",
           y = "Movie Title",
           fill = "Country") +
      theme(plot.title = element_text(hjust = 0.5))
  })
  output$sum<-renderPrint({
    imdb %>%
      summary()
  })

}


  
