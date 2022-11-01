
server <- function(input, output, session) {
 
  
                       
  
  output$map <- renderLeaflet({

    leaflet(shp_sf,
      options = leafletOptions(zoomControl = T)) %>%
      addProviderTiles(providers$CartoDB.Positron) %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 5),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),
                                                tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE == 5,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Hospitais") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 7),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE ==7,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Hospitais") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 15),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE ==15,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Hospitais") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 20),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE==20,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Hospitais") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 21),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE==21,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Hospitais") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 36),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE==36,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Clínicas/Unidades de apoio diagnose e terapia\nSADT ISOLADO") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 39),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE==39,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Clínicas/Unidades de apoio diagnose e terapia\nSADT ISOLADO") %>%
          addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == 42),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[tp_estabs$TP_UNIDADE == 42,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Unidades Móveis") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == tp_estabs[14,]$TP_UNIDADE),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[14,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Outros") %>%
      
      
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == tp_estabs[1,]$TP_UNIDADE),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[1,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        
                        group = "Unidades Atenção Básica") %>%
      addAwesomeMarkers(data=geocnesf%>%dplyr::filter(TP_UNIDADE == tp_estabs[2,]$TP_UNIDADE),icon = ~icones_mapa[ícone],
                        label =~paste(str_to_title(NO_FANTASIA),"com",habs_incs,"habilitações e incentivos\n de interesse da AES"),
                        options = markerOptions(habs = ~ifelse(is.na(habs_incs),0,habs_incs),tp = pluraltp(str_to_title(tp_estabs[2,]$NO_TP_UNIDADE))),
                        clusterOptions = markerClusterOptions(
                          iconCreateFunction =   
                            ~JS(paste0("function (cluster) {    
                          var conta = cluster.getAllChildMarkers();
                          var contaa = conta.length;
                          var c = ' marker-cluster';
                          /*if (contaa < 10) {
                                c+= 'small';
                          } else if (contaa <100) {
                                c+= 'medium';
                          } else {
                                c+= 'large';
                          }*/
                          var habs = 0;
                          for (i =0; i<conta.length;i++) {
                          habs +=Number(conta[i].options.habs)
                          }
                          
                          var tp = conta[0].options.tp;
                          

                          return L.divIcon({ 
                            html: '<div style = \"background-color: ",pegacor(moda(paleta),moda(cor)),";\"><center title=\"'+contaa+' '+tp+' com '+habs+' habilitações\"><i class = \"fa-solid fa-",
                                       moda(ícone),"\"></i><br>'+contaa+'</center></div>', 
                            className: 'marker-cluster' + c, 
                            iconSize: new L.Point(38, 38) });
                          }"
                            ))),
                        group = "Unidades Atenção Básica") %>%
      
      addPolygons(data= regioes_saude_mapa%>%dplyr::filter(code_health_region %in% c(52001,52009,52010,52012)),fillOpacity = 0.2,smoothFactor = 0.7,
                  color = paleta5[4],weight=0.7,
                   group = "Regiões de saúde") %>%

      addLayersControl(
        baseGroups = c("Regiões de saúde","outros"),
        overlayGroups = c("Hospitais", "Unidades Atenção Básica", "Unidades Móveis","Clínicas/Unidades de apoio diagnose e terapia\nSADT ISOLADO","Outros"),
        options = layersControlOptions(collapsed = FALSE),
        position = "bottomright"
      )
    
    
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