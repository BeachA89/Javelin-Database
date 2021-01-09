server <- function(input, output) {
  
  tab_Collateddata_1 <-  reactive({
    
    Collateddatadf%>%
      dplyr::filter(Name == input$Name) %>% 
      dplyr::filter(Competition == input$Competition)
    
  })  
  
  output$select_Name <-  renderUI({
    selectizeInput('Name', 'Select Name', choices = c("select" = "", unique(Collateddatadf$Name)))  
  }) 
  
  output$select_Competition <-  renderUI({
    inputName = as.character(input$Name)
    choice_Competition <- reactive({
      Collateddatadf %>% 
        dplyr::filter(Name == inputName) %>% 
        pull(Competition) %>% 
        as.character()
      
      
    })
    
    
    
    if (input$Report_Type == "Single Comp"){
    selectizeInput('Competition', 'Select Competition', choices = c("select" = "", choice_Competition())) 
    }
  })
  
  Distance_filter_1 <-  reactive({
    if (input$checkbox1 == TRUE){
      a <- Collateddatadf%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >70)
    }else{
      a=NULL
    }
    
    if (input$checkbox2 == TRUE){
      b <- Collateddatadf%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >=66&Distance<=70)
    }else{
      b=NULL
    } 
    if (input$checkbox3 == TRUE){
      c <- Collateddatadf%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance >=61&Distance<=65)
    }else{
      c=NULL
    } 
    if (input$checkbox4 == TRUE){
      d <- Collateddatadf%>%
        dplyr::filter(Name == input$Name) %>%
        dplyr::filter(!Distance %in% NA) %>%
        dplyr::filter(Distance<60) 
    }else{
      d=NULL
    }
    
    combined = bind_rows(a,b,c,d)
    return(combined)
  })
  
  # Distance_filter_2 <-  reactive({
  #   if (input$Distance2 == ">70"){
  #     Collateddatadf%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(!Distance %in% NA) %>%
  #       dplyr::filter(Distance >70)
  #     
  #   }else if (input$Distance2 == "66-70"){
  #     Collateddatadf%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(!Distance %in% NA) %>%
  #       dplyr::filter(Distance >=66&Distance<=70)
  #     
  #   }else if (input$Distance2 == "61-65"){
  #     Collateddatadf%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(!Distance %in% NA) %>%
  #       dplyr::filter(Distance >=61&Distance<=65)
  #     
  #   }else if (input$Distance2 == "<60"){
  #     Collateddatadf%>%
  #       dplyr::filter(Name == input$Name) %>%
  #       dplyr::filter(!Distance %in% NA) %>%
  #       dplyr::filter(Distance<60) 
  #   }
  #   
  #   
  # })
  
  
  # "ImpulseContact", "ImpulsePhase", "DeliveryPhase", "ReleasePhase",
  # "AttitudeAngle","ReleaseAngle", "AttackAngle","YawAngle", "PKAc", "PKAm", "Difference", "ElbowAngle", 
  # "JavForearmAngle", "JavForearmAngle", "ISL", "BSL", "ISLBSL", "NADrawLength", "PullLength", "DrawDistance",
  # "ReleaseDistance"
  
  ##### Tables ########
  output$datatablePhases <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable1 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      # if (is.null(dim(Distance_filter_2()))){
      #   datatable_Distance2 = NULL
      # } else {
      #   datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      # }
      # datatable1 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "ImpulseContact", "ImpulsePhase", "DeliveryPhase", "ReleasePhase"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
    } else {
      
      datatable1 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      datatable1[is.na(datatable1)] <- ""
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "ImpulseContact", "ImpulsePhase", "DeliveryPhase", "ReleasePhase"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  dom='t',ordering=F, scrollX = TRUE))})
    }
  })
  
  output$datatableRelease <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable1 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, AttitudeAngle,ReleaseAngle, AttackAngle,YawAngle)
      # if (is.null(dim(Distance_filter_2()))){
      #   datatable_Distance2 = NULL
      # } else {
      #   datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      # }
      # datatable1 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "AttitudeAngle","ReleaseAngle", "AttackAngle","YawAngle"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
    } else {
      
      datatable1 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, AttitudeAngle,ReleaseAngle, AttackAngle,YawAngle)
      datatable1[is.na(datatable1)] <- ""
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "AttitudeAngle","ReleaseAngle", "AttackAngle","YawAngle"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),   dom='t',ordering=F, scrollX = TRUE))})
    }
  })
  
  output$datatableKneeElbow <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable1 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, PKAc, PKAm, Difference, ElbowAngle)
      # if (is.null(dim(Distance_filter_2()))){
      #   datatable_Distance2 = NULL
      # } else {
      #   datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      # }
      # datatable1 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "PKAc", "PKAm", "Difference", "ElbowAngle"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
    } else {
      
      datatable1 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, PKAc, PKAm, Difference, ElbowAngle)
      datatable1[is.na(datatable1)] <- ""
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "PKAc", "PKAm", "Difference", "ElbowAngle"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    }
  })
  
  output$datatableDistances <-  DT::renderDataTable({
    
    if (input$Report_Type == "Distance Comparison"){
      datatable1 <- Distance_filter_1() %>% dplyr::select(Name, Competition, Round, Distance, ISL, BSL, ISLBSL, NADrawLength, PullLength, DrawDistance,ReleaseDistance)
      # if (is.null(dim(Distance_filter_2()))){
      #   datatable_Distance2 = NULL
      # } else {
      #   datatable_Distance2 <- Distance_filter_2() %>% dplyr::select(Name, Competition, Round, Distance, ImpulseContact, ImpulsePhase, DeliveryPhase, ReleasePhase)
      # }
      # datatable1 = bind_rows(datatable_Distance1,datatable_Distance2)
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "ISL", "BSL", "ISLBSL", "NADrawLength", "PullLength", "DrawDistance","ReleaseDistance"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")),  order=list(list(3, 'desc')), scrollX = TRUE))})
    } else {
      
      datatable1 <- tab_Collateddata_1() %>% dplyr::select(Name, Competition, Round, Distance, ISL, BSL, ISLBSL, NADrawLength, PullLength, DrawDistance,ReleaseDistance)
      datatable1[is.na(datatable1)] <- ""
      ({datatable(datatable1, rownames= FALSE, colnames=c("Name", "Competition", "Round", "Distance", "ISL", "BSL", "ISLBSL", "NADrawLength", "PullLength", "DrawDistance","ReleaseDistance"), 
                  options = list(columnDefs = list(list(className = 'dt-center', targets = "_all")), dom='t', ordering=F, scrollX = TRUE))})
    }
  })
  
  
  
  
  
  
  
  
  
  
  
 
 

  
  
  
  
  
  
  ##### GGPlots #####
  # 
  # output$ggplot1 <-  renderPlot({
  #   if (input$Report_Type == "Distance Comparison"){
  #     
  #     Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `Distance`, `Turn1Single`, `Turn1Double`, 
  #                                                              `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                              `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
  #     Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`,`Distance`, `Turn1Single`, `Turn1Double`, 
  #                                                              `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                              `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
  #     
  #     Plot1 <- bind_rows(Plot_Distance1, Plot_Distance2)
  #     
  #     nb.cols <- n_distinct(Plot1$`Round&Distance`)
  #     mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
  #     
  #     ggplot(data=Plot1, aes(x=variable, y=value, fill=reorder(`Round&Distance`, `Distance`))) + geom_bar(stat="identity",position=position_dodge()) + 
  #       scale_fill_manual(values = mycolors) + scale_x_discrete(labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double")) +
  #       scale_y_continuous("time (s)") + theme(axis.title.x = element_blank())+ labs(fill = "Round & Distance (m)")
  #   }else {
  #     
  #     Plot1 <-  tab_Collateddata_1() %>% dplyr::select(`Round&Distance`, `Turn1Single`, `Turn1Double`, 
  #                                                      `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                      `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round&Distance"))
  #     
  #     ggplot(data=Plot1, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens") +
  #       scale_y_continuous("time (s)") + scale_x_discrete(labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double")) + 
  #       theme(axis.title.x = element_blank())+ labs(fill = "Round & Distance (m)")
  #   }
  # })
  # 
  # output$ggplot2 <-  renderPlot({
  #   if (input$Report_Type == "Distance Comparison"){
  #     Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `Turn1Single`, `Turn1Double`, 
  #                                                              `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                              `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
  #     
  #     Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`, `Turn1Single`, `Turn1Double`, 
  #                                                              `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                              `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Distance", "Round&Distance"))
  #     
  #     
  #     Plot2 <- bind_rows(Plot_Distance1, Plot_Distance2) 
  #     
  #     
  #     ggplot(data=Plot2, aes(x=reorder(`Round&Distance`,-`Distance`), y=value, fill=`variable`)) + geom_bar(stat="identity")  + 
  #       scale_fill_manual(name = "Turn", labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double"),values=c("orange","green4", "orange","green4", "orange", "green4", "orange", "green4")) +
  #       scale_x_discrete("Round & Distance (m)") +
  #       scale_y_continuous("Time (s)", breaks = seq(0, 2.4, 0.1)) + coord_flip()
  #     
  #     
  #   }else {
  #     
  #     Plot2 <-tab_Collateddata_1() %>% dplyr::select(`Round`, `Turn1Single`, `Turn1Double`, 
  #                                                    `Turn2Single`, `Turn2Double`,  `Turn3Single`, `Turn3Double`,
  #                                                    `Turn4Single`, `Turn4Double`) %>% reshape2::melt(id = c("Round"))
  #     
  #     ggplot(data=Plot2, aes(x=`Round`, y=value, fill=`variable`)) + geom_bar(stat="identity")  + 
  #       scale_fill_manual(name = "Turn", labels = c("Turn 1 Single", "Turn 1 Double", "Turn 2 Single", "Turn 2 Double","Turn 3 Single", "Turn 3 Double","Turn 4 Single", "Turn 4 Double"),values=c("orange","green4", "orange","green4", "orange", "green4", "orange", "green4")) +
  #       scale_x_continuous(breaks = c(1:6)) + scale_y_continuous("Time (s)", breaks = seq(0, 2.4, 0.1)) + coord_flip()
  #     
  #   }
  # })
  # 
  # output$ggplot3 <-  renderPlot({
  #   if (input$Report_Type == "Distance Comparison"){
  #     Plot_Distance1 <-  Distance_filter_1() %>% dplyr::select(`Distance`, `Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% 
  #       reshape2::melt(id = c("Distance","Round&Distance"))
  #     Plot_Distance2 <-  Distance_filter_2() %>% dplyr::select(`Distance`, `Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% 
  #       reshape2::melt(id = c("Distance","Round&Distance"))
  #     
  #     Plot3 <- bind_rows(Plot_Distance1, Plot_Distance2)
  #     nb.cols <- n_distinct(Plot3$`Round&Distance`)
  #     mycolors <- colorRampPalette(brewer.pal(8, "Greens"))(nb.cols)
  #     
  #     ggplot(data=Plot3, aes(x=variable, y=value, fill=reorder(`Round&Distance`, `Distance`))) + geom_bar(stat="identity",position=position_dodge()) + 
  #       scale_fill_manual(values = mycolors) + scale_x_discrete("Round", labels = c("Turn 1", "Turn 2", "Turn 3", "Turn 4")) +
  #       scale_y_continuous("time (s)")+ labs(fill = "Round & Distance (m)")
  #     
  #   }else {
  #     
  #     
  #     
  #     Plot3 <- tab_Collateddata_1() %>% dplyr::select(`Round&Distance`, `DUTurn1`, `DUTurn2`, `DUTurn3`, `DUTurn4`) %>% reshape2::melt(id = c("Round&Distance"))
  #     ggplot(data=Plot3, aes(x=variable, y=value, fill=`Round&Distance`)) + geom_bar(stat="identity",position=position_dodge()) + scale_fill_brewer(palette = "Greens") + 
  #       scale_x_discrete("Round", labels = c("Turn 1", "Turn 2", "Turn 3", "Turn 4")) +
  #       scale_y_continuous("time (s)")+ labs(fill = "Round & Distance (m)")
  #   }
  # })
  # 
  # 
  
  # Table2 <- tab_Collateddata() %>% dplyr::select(Name, Competition, Round, Distance, Turn1, Turn2, Turn3, Turn4, TotalTime)
  # 
  # 
  # 
  # Table3 <- tab_Collateddata() %>% dplyr::select(Name, Competition, Round, Distance, DUTurn1, DUTurn2, DUTurn3, DUTurn4)
  # 
  # 
  # 
  # 
  # 
  # 
  # Table1 <-  t(Table1)
  # Table1[is.na(Table1)] <- "X"
  # 
  # datatable(Table1)
  # 
  # 
  # Table2 <-  t(Table2)
  # Table2[is.na(Table2)] <- "X"
  # datatable(Table2)
  # 
  # Table3 <-  t(Table3)
  # Table3[is.na(Table3)] <- "X"
  # 
  # datatable(Table3)
  # 
  ##### GGPLOTS ######
  
}
