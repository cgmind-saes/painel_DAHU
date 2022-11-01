ui <- navbarPage(
  
  theme = bs_theme(version = 4, bootswatch = "minty"),
  collapsible = TRUE,
  windowTitle = "Painel da Rede de Atenção à Saúde Especializada",
  title = "RAS/SAES",
  
  tabPanel(
    "A Rede",
    # Mapa (estilos para eliminar borda)
    tags$style(type = "text/css", "#map {height: calc(100vh - 48px)  !important;
               z-index: 500;}"),
    tags$style(type="text/css",".awesome-marker {background: rgba(230,230,230,0.5) !important;}"),
    tags$style(type="text/css",".awesome-marker i{font-size: 20px !important;}"),
    leafletOutput("map", width = "100%"),
    tags$style("@import url(https://cdnjs.cloudflare.com/ajax/libs/font-awesome/6.2.0/css/all.min.css);"),
    tags$style("@import url(styles.css);"),
    tags$style(type = "text/css", ".container-fluid {padding-left:0px;padding-right:0px;}"),
    tags$style(type = "text/css", ".navbar {margin-bottom: 0px;}"),
    tags$style(type = "text/css", ".container-fluid .navbar-header .navbar-brand {margin-left: 0px;}"),
    tags$style(type = "text/css", ".js-plotly-plot .plotly .main-svg:first-of-type {background: rgba(255,255,255,0.4) !important;}"),
    tags$style(type = "text/css", "tr.odd {background-color: rgba(249,249,249,0.7) !important};"),
    tags$style(type = "text/css", "tr.even {background-color: rgba(255,255,255,0.7) !important};"),
    tags$style(type = "text/css", "tr.even.selected {background-color: rgba(176, 190, 217,0.6) !important};"),
#  )
#)
     absolutePanel(
       id="controles",
       top = 50,
       right = "1.5%",
       width = "22%",
       height = "28%",
       class = "panel panel-default",
       style =
         "background-color: rgba(255,255,255,0.2);
         z-index: 504;
         padding: 0;
         box-shadow: 0 0 10px rgba(0,0,0,0.2);
         border-radius: 2px;
         font-size: 10px",
#       selectInput("estado","Estado",lista_estados, selected="ES"),
       selectInput("indicador","indicador",lista_ind_dahu, selected="prop_cesario"),
       uiOutput("mesel"),
#sliderInput("ano","Ano",min = 2012, max = 2020, value = 2014, ticks = F, animate=F, sep = "")
     ),




    conditionalPanel("output.esconde",
                     style =
                       "background-color: rgba(255,255,255,0.3);
            z-index: 500;
            padding: 0;
            box-shadow: 0 0 10px rgba(0,0,0,0.2)",
            #          absolutePanel(
            #            id = "header",
            #            top = 50,
            #            left = "1.5%",
            #            width = "72%",
            #            height = "32%",
            #            style =
            #              "background-color: rgba(255,255,255,0.01);
            # z-index: 500;
            # padding: 0;
            # box-shadow: 0 0 0px rgba(0,0,0,0);
            # border-radius= none",
            #            #      draggable = TRUE,
            #            textOutput("titulo_painel"),
            #            dataTableOutput("estado")
            #          ),

                     absolutePanel(
                       id = "fechabaixa",
                       top = 50,
                       left = "40%",
                       style =
                         "z-index: 501;
    padding: 0;
    border-line: none;
    font-size: 8px",
                       tags$div(tags$table(tags$tr(tags$td(actionButton("xis","X",
                                                                        style ="border-radius: 20px;
                                                          border-color: transparent;
                                                          background-color: rgba(80,30,30,0.4)")),
                                                   tags$td(width = "20px",""),
                                                   tags$td(a(href="infancia.grupo.pro.br","Página do Grupo de Pesquisa")),

                       )))
                     ),
                     absolutePanel(
                       style =
                         "background-color: rgba(255,255,255,0);
        z-index: 500;
        padding: 0;
        box-shadow: 0 0 0px rgba(0,0,0,0);
        border-radius: none",
                       top = "44%",
                       left = "1%",
                       width = "34%",
                       height = "23%",
#                       div(textOutput("titulo_serie_estado"), align = "center" ,
#                           style = "font-size:18px; font-weight: bold"),
#                       div(textOutput("subtitulo_serie_pais"), align = "center"),
                       plotlyOutput("serie_estado")
                     )
    ),
    absolutePanel(
      id = "creditos",
      class = "panel panel-default",
      style =
        "background-color: rgba(255,255,255,0.2);
        z-index: 500;
        padding: 0;
        box-shadow: 0 0 10px rgba(0,0,0,0.3);
        border-radius: 5px;
        ",
      bottom = 20,
      left = "2%",
      width = "130px",
      height = "100px",
      tags$i(tags$table(tags$tr(tags$td(img(src = "img/logo_sintese.png",
                                            height = "64px",
                                            #style = "-webkit-filter: grayscale(100%); filter: grayscale(80%)"
                                            )),
                                tags$td(tags$p("SAES/MS",align="center"),colspan=3,
                                        style = "font-size: 12px;")),
                        tags$tr(tags$td(tags$p("🄯 CC-BY-NC SA 4.0", align="center"),colspan=4,
                                        style = "font-size: 10px;"))))

    )
),
tabPanel(
  "Gráficos",
  absolutePanel(
     id= "assistencia-controles",
     top = 50,
     right = "1.5%",
     width = "22%",
     height = "68%",
     class = "panel panel-default",
     style =
       "background-color: rgba(255,255,255,0.2);
        z-index: 504;
        padding: 0;
        box-shadow: 0 0 10px rgba(0,0,0,0.2);
        border-radius: 2px;
        font-size: 10px",
  ),
  )
)

