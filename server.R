
server <- function(input, output, session) {
 
  
                       
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    #dplyr::filter(inds_dahu,ano==input$ano)
    shp_sfil <-left_join(shp_sf,inds_dahu)

    pal <- colorNumeric(palette = "Reds", domain = shp_sfil[[input$indicador]])
    leaflet(shp_sf,
      options = leafletOptions(zoomControl = T)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
       addAwesomeMarkers(data=geocnesf,icon = ~icones_mapa[ícone],
                         label =~NO_RAZAO_SOCIAL)%>%
      addPolygons(data=regioes_saude_mapa,fillOpacity = 0.2,smoothFactor = 0.7,
                  color = paleta5[4],weight=0.7)#%>%
      # addPolygons(data=shp_sfil,
      #             smoothFactor = 0.5,
      #             fillOpacity = 0.5,
      #             weight = 0.5,
      #             color = ~pal(get(input$indicador)),
      #             opacity = 0.8,
      #             highlightOptions = highlightOptions(color = "gray",
      #                                                 weight = 2,
      #                                                 bringToFront = T),
      #             popup = ~paste0(sep = " ",
      #                             "<b>Cidade: </b>",Município,"<br>",
      #                             "<b>Proporção de partos cesáreos: </b>",round(prop_cesario*100,2),"% <br>",
      #                             "<b>Proporção de gestações com acompanhamento pré-natal adequado:</b>",round(prenatal_adequado*100,2),"% <br>"),
      #             label = ~`Município`) %>%
      #             addLegend("bottomright",
      #                           title = "Escala",
      #                           pal = pal,
      #                           values = as.formula(paste0("~",input$indicador)),
      #                           opacity = 0.8)    %>%
 #                   addTiles() 
  })
  
   esconde <- reactiveVal(1)
#   
#   
   observeEvent(input$xis,
                esconde(0)
                ,ignoreInit = T)
   
   
   output$mesel <- renderUI({
     monthsRangeInput("ano","Período",min = as.Date("2018-01-01","%Y-%m-%d"),
                      format = "M/yyyy",startview = "decade",
                      max = as.Date("2022-04-01","%Y-%m-%d"),
                      start = as.Date("2022-04-01","%Y-%m-%d"),
                      end = as.Date("2021-04-30","%Y-%m-%d"))
     
   })
   
}