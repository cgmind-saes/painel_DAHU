
server <- function(input, output, session) {
 
  
                       
  
  output$map <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    shp_sfil <-left_join(shp_sf,filter(ecd_saude,ano==input$ano))

    pal <- colorNumeric(palette = "Reds", domain = shp_sfil[[input$indicador]])
    leaflet(shp_sfil,
      options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shp_sfil,
                  smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color = ~pal(get(input$indicador)),
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = T),
                  popup = ~paste0(sep = " ",
                                  "<b>Cidade: </b>",Município,"<br>",
                                  "<b>Proporção de partos cesáreos: </b>",round(prop_cesario*100,2),"% <br>",
                                  "<b>Proporção de gestações com acompanhamento pré-natal adequado:</b>",round(prenatal_adequado*100,2),"% <br>"),
                  label = ~`Município`) %>%
                  addLegend("bottomright",
                                title = "Escala",
                                pal = pal,
                                values = as.formula(paste0("~",input$indicador)),
                                opacity = 0.8)    %>%
                    addTiles() 
  })
  
  output$maped <- renderLeaflet({
    # Use leaflet() here, and only include aspects of the map that
    # won't need to change dynamically (at least, not unless the
    # entire map is being torn down and recreated).
    shp_sfil <-left_join(shp_sf,filter(ecd_ed,ano==input$anoed))
    
    pal <- colorNumeric(palette = "Greens", domain = shp_sfil[[input$indicadored]])
    leaflet(shp_sfil,
            options = leafletOptions(zoomControl = FALSE)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addPolygons(data=shp_sfil,
                  smoothFactor = 0.5,
                  fillOpacity = 0.5,
                  weight = 0.5,
                  color = ~pal(get(input$indicadored)),
                  opacity = 0.8,
                  highlightOptions = highlightOptions(color = "gray",
                                                      weight = 2,
                                                      bringToFront = T),
                  popup = ~paste0(sep = " ",
                                  "<b>Cidade: </b>",Município,"<br>",
                                  "<b>Taxa de Cobertura Efetiva de Creche: </b>",round(tx_cobertura_creche_efetiva*100,2),"% <br>",
                                  "<b>Proporção de pré-escolas públicas:</b>",round(`prop. de pre-escolas pub`*100,2),"% <br>"),
                  label = ~`Município`) %>%
      addLegend("bottomright",
                title = "Escala",
                pal = pal,
                values = as.formula(paste0("~",input$indicadored)),
                opacity = 0.8)    %>%
      addTiles() 
  })
  
#}
#   
#   
#   ####
#   ##Reactive values to use in more than one place (titles, popups)
#   titspais <- reactive({ 
#     return(paste(varst[varst$var==input$indicador,"pt"]))
#   })
#   
#   subtspais <- reactive({
#     return(paste0(paises[paises$Legenda==input$pais,1],
#                   ", ",
#                   as.character(ano_min),
#                   " - ",
#                   as.character(ano_max)))
#   })
#   
#   ####
#   
#   
#   
   esconde <- reactiveVal(1)
#   
#   
   observeEvent(input$xis,
                esconde(0)
                ,ignoreInit = T)
#   
#   
#   ####
#   
#   observeEvent(input$indicador,{
#     tiranpaises <- paises[!(paises$Legenda %in% c("WWW","ROW")),"Legenda"]
#     basecam <- sea_paises["WIOD13",as.character(input$ano),input$indicador,tiranpaises]
#     
#     camadas <- joinCountryData2Map(enframe(basecam),nameJoinColumn = "name")
#     camadas <- camadas[camadas$ISO3 %in% tiranpaises,]
#     labels <- sprintf("<strong>%s</strong><br/>%s : %g %s",
#                       camadas$ADMIN, input$indicador, camadas$value, varst[varst$var == input$indicador,"type"]
#     ) %>% lapply(htmltools::HTML)
#     binas <- c(0,min(basecam, na.rm = T),median(basecam, na.rm = T),max(basecam, na.rm = T))
#     palas <- colorBin("YlOrRd" , 
#                       domain = camadas$value,
#                       bins = binas)
#     
#     proxy <- leafletProxy("map")
#     
#     proxy   %>%  
#       clearShapes() %>%
#       clearControls()%>%
#       addPolygons(data = camadas,
#                   fillColor = ~palas(camadas$value),
#                   color="white",
#                   weight = 0.5,
#                   opacity = 0.9,
#                   dashArray = "3",
#                   fillOpacity = 0.6,
#                   label = labels,
#                   labelOptions = labelOptions(textsize = "9px",direction="auto",style=list("font-weight" = "normal", padding = "3px 8px")),
#                   highlight = highlightOptions(
#                     weight = 1,
#                     color = "#666",
#                     dashArray = "",
#                     fillOpacity = 0.7,
#                     bringToFront = TRUE)
#       )%>%
#       addLegend("bottomright",
#                 values = camadas$value,
#                 title = titspais(),
#                 pal = palas
#       )
#   }
#   )
#   
#   observeEvent(input$pais,ignoreInit = T,{
#     esconde(1)
#   }
#   )
#   
#   output$esconde <- reactive(
#     return(esconde())
#   )
#   outputOptions(output, 'esconde', suspendWhenHidden=FALSE)
#   
#   
#   observeEvent(input$map_click, {
#     click <- input$map_click
#     piso3 <- coords2country(data.frame(lng = click$lng, lat = click$lat))
#     pafil <- (paises%>%filter(Legenda ==  piso3))$Legenda
#     paisvei <- input$pais
#     updateSelectInput(inputId = "pais",
#                       selected = pafil)
#     if (piso3 %in% paises$Legenda) {
#       esconde(1)
#     }
#   })
#   
#   
#   observeEvent(input$map_hover, {
#     hover <- input$map_hover
#     piso3 <- coords2country(data.frame(lng = hover$lng, lat = hover$lat))
#     paises%>%filter(Legenda = piso3)$Legenda
#     valorpontopais <- sea_paises[,as.character(max(input$ano)),,pafil]
#     text <- paste("Country:",pafil,"<br>",
#                   titspais(),"-",subtspais(),":",
#                   valorpontopais)
#     proxy <-leafletProxy("map")
#     proxy %>% clearPopups() %>%
#       addLabelOnlyMarkers(hover$lng,hover$lat,label = text)
#   })
#   
#   linhas_13 <- reactive({
#     encontrar_pais(m_io_13, input$pais, rownames)
#   })
#   
#   colunas_13 <- reactive({
#     encontrar_pais(m_io_13, input$pais_transacoes, colnames)
#   })
#   
#   linhas_16 <- reactive({
#     encontrar_pais(m_io_16, input$pais_transacoes, rownames)
#   })
#   
#   colunas_16 <- reactive({
#     encontrar_pais(m_io_16, input$pais_transacoes, colnames)
#   })
# 
#   comerciantes_p <-  function(bd=13,pais = input$paistrade,ano = input$anotrade, elemento = "exportacoes_pm",qtde = 15) {
#       mat <- get(paste0("m_paises_",bd))
#       mat <- mat[as.character(ano),elemento,pais,]
#       paises <- names(sort(mat,T)[2:(qtde+1)])
#     }
# 
# 
#   
# 
#   fazer_selecao <- reactive({
#     switch(paste0(input$transacoes_versao,input$transacoes_agregacao),
#            "WIOD13Aggr." = 1,
#            "WIOD13Sector" = 2,
#            "WIOD16Aggr." = 3,
#            "WIOD16Sector" = 4
#     )
#   })
# 
#   
#   dados <- reactive({
#     paste0(input$transacoes_versao)
#   })
#   
#   output$debuga <- renderText({
#     #glimpse(dados())
#     paste(input$pais)
#   })
#   
  # output$estado <- renderDataTable(
  # 
  #  ecd_saude%>%filter(),
  #   # rownames = TRUE,
  #   #    spacing = "xs",
  #   #    striped = TRUE,
  #   #    hover = TRUE,
  #   #    width = "100%",
  #   options = list(
  #     ordering = FALSE,
  #     searching = FALSE,
  #     paging = FALSE,
  #     #      scrollY = "200",
  #     # pageLength = 10,
  #     info = FALSE,
  #     lengthChange = FALSE
  #   )
  # )

  output$serie_estado <- renderPlotly({
    dados <- ecd_saude%>%filter(ano == input$ano %>% grepl("^32",cod_mun))%>%select_(input$indicador)
    plotaserie(dados)
  })


  output$setores_pais_13 <- renderDataTable(
    tabmil(sea_setores_13[as.character(input$ano),
                          input$indicador,,
                          input$pais])%>%
      left_join(setorest,by=c("var" = "Code"))%>%
      select(sector=pt,value=x),
    options = list(
      ordering = TRUE,
      searching = FALSE,
      paging = FALSE,
      #pageLength = 8,
      scrollY= "340",
      info = FALSE,
      lengthChange = FALSE
    )
  )
#   
#   output$setores_pais_16 <- renderDataTable(
#     tabmil(sea_setores_16[as.character(input$ano),
#                           input$indicador,,
#                           input$pais])%>%
#       left_join(setorest,by=c("var" = "Code"))%>%
#       select(sector=pt,value=x),
#     options = list(
#       ordering = TRUE,
#       searching = FALSE,
#       paging = FALSE,
#       #pageLength = 8,
#       scrollY= "340",
#       info = FALSE,
#       lengthChange = FALSE
#     )
#   )
#   
#   
#   output$titulo_detalhamento_pais <-
#     renderText(
#       paste(varst[varst$var==input$indicador,"pt"])
#     )
#   
#   
#   output$subtitulo_detalhamento_pais <-
#     renderText(
#       paste(paises[paises$Legenda==input$pais,1],
#             "-",
#             input$ano)
#     )
#   
#   output$titulo_serie_pais <-
#     renderText(titspais())
#   
#   output$subtitulo_serie_pais <-
#     renderText(
#       subtspais())
#   
#   output$titulo_painel <-
#     renderText(
#       paste("Country Profile:",paises[paises$Legenda==input$pais,1],
#             "-",
#             input$ano
#       )
#       
#     )
#   output$indicadores <- renderDT(
#     {
#       dados <- t(rbind(
#         paises[match(names(sea_paises[1,1,1,]),paises[,3]),1],
#         sea_paises[,as.character(input$anoind),input$indicadorind,]))
#       dados <- datatable(dados,
#                 rownames = 1,
#                 options = list(
#                   ordering = TRUE,
#                   searching = TRUE,
#                   paging = FALSE,
#                   info = TRUE,
#                   #pageLength = 8,
#                   scrollY= 220,
#                   lengthChange = FALSE
# 
#                 )
#       ) %>%
#         formatPercentage("WIOD13", 2)
#       formatStyle(dados, names(dados$x$data),`line-height` = '10px')
#     },
#   )
#   output$serie <- renderPlotly({
#     dados <- sea_paises[input$versao,,
#                         input$indicadorind,
#                         input$paises]
#     plotaserie(dados)
#   })
#   
#   
#   ### sobre esses outputs que se seguem: é preciso melhorar. Seria possível ter
#   ### uma única função que fosse chamada conforme a seleção de (exportação,
#   ### importação e saldo), e chamada 3 vezes (monetária, valor e transferência)?
#   # Problema: os dados de exportacoes, importacoes e saldo são distintos.
#   # Mas são os mesmos dados conforme o tipo de variável (monetário, valor transf)
#   # No entanto, os gráficos conforme tipo de variábel são concomitantes.
# 
# ### Sim certamente possível
#   
#   prep_treemap <- function(bd=input$transacoes_versao,
#                            pais = input$paistrade,
#                            ano = input$anotrade,
#                            agr = input$transacoes_agregacao,
#                            el = "exportacoes_pm",
#                            qcorte = T,
#                            qtde = 15,
#                            agru = agrupamento,
#                            pod = 1) {
#     bd <- ifelse(grepl("13",bd),13,16)
#     p <- comerciantes_p(bd,pais,ano,el,10)
#     dados <- get(paste0("m_io_",bd)) %>%
#       agregado(ano, el, get(paste0("linhas_",bd))()) %>%
#       as.data.table(keep.rownames = "paisect")%>%
#       separate(paisect,c("pais_origen","sector_origen"),sep="\\.")%>%
#       select(-pais_origen)%>%
#       pivot_longer(-c(sector_origen),names_to="paisect_d",values_to="valor")%>%
#       separate(paisect_d,c("pais_d","sect_d"),sep="\\.")%>%
#       mutate(pais_d = ifelse(pais_d %in% p,pais_d,"ROW"))%>%
#       dplyr::group_by(across(all_of(agru)))%>%
#       summarize(valor=sum(valor))%>%
#       left_join(paises, by = c("pais_d" = "Legenda"))%>%
#       left_join(setorest,by = c("sect_d" = "Code"))%>%
#       transmute(pais_d = `Países`,sect_d = pt, valor)%>%
#       mutate(across(-valor,as.factor))%>% ungroup()
#     dados
#   }
#   
#   
#   output$exportacoes_monetarias <- renderD3tree3({
#     selecao <- fazer_selecao()
#     
#     agrupamento <- case_when(
#       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
#       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
#     )
#     
#     agrupamento <- unlist(agrupamento)
#     dados <- prep_treemap(agru = agrupamento)%>%filter(pais_d != "Resto do mundo")
#     
#     d3tree3(treemap(dados, index = agrupamento, vSize = "valor",
#                     type = "index", palette = "Set1"),
#             "Monetary Exports")
#   })
#   # 
# #   
# #   
#    output$exportacoes_valores <- renderD3tree3({
#      selecao <- fazer_selecao()
#      
#      agrupamento <- case_when(
#        selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
#        selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
#      )
#  
#      agrupamento <- unlist(agrupamento)
#      dados <- prep_treemap(agru = agrupamento,el = "exportacoes_valores")%>%filter(pais_d != "Resto do mundo")
#      
#      d3tree3(treemap(dados,  index = agrupamento, vSize = "valor",
#                      type = "index", palette = "Set1"),
#              rootname = "Exports in Value Terms")
#      })
#  
#    output$exportacoes_transferencias <- renderD3tree3({
#      selecao <- fazer_selecao()
#      
#      agrupamento <- case_when(
#        selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
#        selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
#      )
#      
#      agrupamento <- unlist(agrupamento)
#      dados <- prep_treemap(agru = agrupamento,el="transferencias_valores")%>%filter(pais_d != "Resto do mundo")%>%
#        mutate(tam=abs(valor),
#               pais_d=as.factor(pais_d),
#               colorido=as.numeric(cut(valor,20,labels=F)))
#      print(head(dados))
#      print(class(dados$colorido))
#      print(head(dados$colorido))
#      
#      d3tree3(treemap(dados,  index=agrupamento,vSize = "tam", vColor="colorido",
#                      type = "index", algorithm = "pivotSize",
#                      sortId = "color", palette = "Set1"),
#              rootname = "Value Transfers(Unequal Exchange)")
#      
#  }
#  )
# 
# #   
# #   output$importacoes_monetarias <- renderD3tree3({
# #     selecao <- fazer_selecao()
# #     
# #     agrupamento <- case_when(
# #       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
# #       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
# #     )
# #     agrupamento <- unlist(agrupamento)
# #     dados <- prep_treemap(agru = agrupamento,el="importacoes_monetarias")
# #     
# #     d3tree3(treemap(dados, index = agrupamento, vSize = "valor",
# #                     type = "index", palette = "Set1"),)
# #     
# # })
# #   
# #   output$importacoes_valores <- renderD3tree3({
# #     selecao <- fazer_selecao()
# #     
# #     agrupamento <- case_when(
# #       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
# #       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
# #     )
# #     agrupamento <- unlist(agrupamento)
# #     dados <- prep_treemap(agru = agrupamento,el="importacoes_valores")
# #     
# #     d3tree3(treemap(dados, title = "exportações",  index = agrupamento, vSize = "valor",
# #                     type = "index", palette = "Set1"))
# #     
# # })
# #   
# #   output$saldo_monetarias <- renderD3tree3({
# #     selecao <- fazer_selecao()
# #     
# #     agrupamento <- case_when(
# #       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
# #       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
# #     )
# #     agrupamento <- unlist(agrupamento)
# #     dados <- prep_treemap(agru = agrupamento,el="saldo")
# #     
# #     d3tree3(treemap(dados, title = "exportações",  index = agrupamento, vSize = "valor",
# #                     type = "index", palette = "Set1"))
# #     
# #     
# #     })
# #   
# #   
# #   output$saldo_valores <- renderD3tree3({
# #     selecao <- fazer_selecao()
# #     
# #     agrupamento <- case_when(
# #       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
# #       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
# #     )
# #     agrupamento <- unlist(agrupamento)
# #     dados <- prep_treemap(agru = agrupamento,el="saldo_valores")
# #     
# #     d3tree3(treemap(dados, title = "exportações",  index = agrupamento, vSize = "valor",
# #                     type = "index", palette = "Set1"))
# #     
# #     
# # })
# #   
# #   output$saldo_transferencias <- renderD3tree3({
# #     selecao <- fazer_selecao()
# #     
# #     agrupamento <- case_when(
# #       selecao %% 2 == 1 ~  list(c("pais_d","sect_d")),
# #       selecao %% 2 == 0 ~ list(c("sector_origen","pais_d","sect_d"))
# #     )
# #     agrupamento <- unlist(agrupamento)
# #     dados <- prep_treemap(agru = agrupamento,el="saldo_transferencias")
# #     
# #     d3tree3(treemap(dados, title = "exportações",  index = agrupamento, vSize = "valor",
# #                     type = "index", palette = "Set1"))
# #     
# #     
# #     })
# # 
# # ### Análise das transferências: tabelas sobre troca desigual e trocas nos setores
# # ### improdutivos
# # 
# #   ## Juntar tanto as exportacoes quanto as importacoes
# #   output$td_envios_recebimentos <- renderTable({
# #     selecao <- fazer_selecao()
# #     if (selecao == 1) {
# #       temp1 <- m_paises_13[as.character(input$ano_transacoes),
# #                            "transferências_produtivas.valores",
# #                            input$pais_transacoes,
# #                            ]
# #       temp2 <-  -m_paises_13[as.character(input$ano_transacoes),
# #                             "transferências_produtivas.valores",
# #                             ,
# #                             input$pais_transacoes]
# #       names(temp1) <- paste0("X.",names(temp1))
# #       names(temp2) <- paste0("M.",names(temp2))
# #       c(temp1, temp2)
# #     } else if (selecao == 2) {
# #     } else if (selecao == 3) {
# #     } else if (selecao == 4) {
# #     } else if (selecao == 5) {
# #     } else if (selecao == 6) {
# #     }
# #   }, rownames = TRUE)
# # 
# #   output$td_envios_recebimentos_saldo <- renderTable({
# #     selecao <- fazer_selecao()
# #     if (selecao == 1){
# #       m_paises_13[as.character(input$ano_transacoes),
# #                            "transferências_produtivas.valores",
# #                            input$pais_transacoes,] -
# #       m_paises_13[as.character(input$ano_transacoes),
# #                           "transferências_produtivas.valores",
# #                           ,input$pais_transacoes]
# #     } else if (selecao == 2) {
# #     } else if (selecao == 3) {
# #     } else if (selecao == 4) {
# #     } else if (selecao == 5) {
# #     } else if (selecao == 6) {
# #     }
# #   }, rownames = TRUE)
# # 
# # 
# #   output$improdutivos_envios_recebimentos <- renderTable({
# #     selecao <- fazer_selecao()
# #     if (selecao == 1){
# #       temp1 <- m_paises_13[as.character(input$ano_transacoes),
# #                            "transferencias_valores",
# #                            input$pais_transacoes,] -
# #         m_paises_13[as.character(input$ano_transacoes),
# #                            "transferências_produtivas.valores",
# #                            input$pais_transacoes,]
# #       temp2 <-  -(m_paises_13[as.character(input$ano_transacoes),
# #                             "transferencias_valores",
# #                             ,input$pais_transacoes] -
# #         m_paises_13[as.character(input$ano_transacoes),
# #                     "transferências_produtivas.valores",
# #                     ,input$pais_transacoes])
# #       names(temp1) <- paste0("X.",names(temp1))
# #       names(temp2) <- paste0("M.",names(temp2))
# #       c(temp1, temp2)
# #     } else if (selecao == 2) {
# #     } else if (selecao == 3) {
# #     } else if (selecao == 4) {
# #     } else if (selecao == 5) {
# #     } else if (selecao == 6) {
# #     }
# #   }, rownames = TRUE)
# # 
# # 
# #   output$improdutivos_envios_recebimentos_saldo <- renderTable({
# #     selecao <- fazer_selecao()
# #     if (selecao == 1){
# #       temp1 <- m_paises_13[as.character(input$ano_transacoes),
# #                            "transferencias_valores",
# #                            input$pais_transacoes,] -
# #         m_paises_13[as.character(input$ano_transacoes),
# #                     "transferências_produtivas.valores",
# #                     input$pais_transacoes,] -
# #         (m_paises_13[as.character(input$ano_transacoes),
# #                               "transferencias_valores",
# #                               ,input$pais_transacoes] -
# #            m_paises_13[as.character(input$ano_transacoes),
# #                                 "transferências_produtivas.valores",
# #                                 ,input$pais_transacoes])
# #     } else if (selecao == 2) {
# #     } else if (selecao == 3) {
# #     } else if (selecao == 4) {
# #     } else if (selecao == 5) {
# #     } else if (selecao == 6) {
# #     }
# #   }, rownames = TRUE)
# # 
# #   output$proporcao_td_transferencias <- renderText({
# #     as.character(sum(m_paises_13[as.character(input$ano_transacoes),
# #                                  "transferências_produtivas.valores",
# #                                  input$pais_transacoes,] +
# #                        m_paises_13[as.character(input$ano_transacoes),
# #                                    "transferências_produtivas.valores",
# #                                    ,input$pais_transacoes])/
# #                    sum(m_paises_13[as.character(input$ano_transacoes),
# #                                    "transferencias_valores",
# #                                    input$pais_transacoes,] +
# #                          m_paises_13[as.character(input$ano_transacoes),
# #                                      "transferencias_valores",
# #                                      ,input$pais_transacoes]))
# #   })
# #   textOutput("proporcao_td_transferencias_saldo")
# 
 }
# 
# 
