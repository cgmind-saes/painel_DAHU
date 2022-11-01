coordsestados = function(dadosmun) {  
  # Primeiro precisamos dos dados de latitude e longitude das cidades analisadas. 
  
  urlfile <- "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv"
  cities_lat_lng <- read.csv(urlfile,encoding = "UTF-8", col.names = c("COD_IBGE", "Cidade","lat","lng","Capital","Codigo_UF"))
  # é necessário se certificar que o código de cada cidade estará em formato de texto, para o que a função left_join funcione. 
  cities_lat_lng$COD_IBGE <- as.character(cities_lat_lng$COD_IBGE)
  dadosmun <- left_join(dadosmun, cities_lat_lng, by = c("city_ibge_code" = "COD_IBGE"))  
}





portes <- c(20,50,100,250,500,1e5)
names(portes) <- c("Pequeno Porte I","Pequeno Porte II","Médio Porte I","Médio Porte II",
                   "Grande Porte I","Grande Porte II")
c_portes <- c(0,portes)

#estr_pad_leitos <- read_fst("dados/estr_pad_leitos.fst")

tipos_leitos <- read_csv("dados/sigtap_leitos.csv", col_names = F)[[1]]
names(tipos_leitos) <- janitor::make_clean_names(read_csv("dados/sigtap_leitos.csv", col_names = F)[[2]])


lista_capcid_base <- read_csv2("dados/tabela_cid.csv")



get_pg <- "https://sage.saude.gov.br/paineis/regiaoSaude/lista.php?output=jsonbt&=&order=asc&_=1659576808022"


lista_regioes <- fromJSON(GET(get_pg) %>% content(as = "text", encoding = "utf-8"))

lista_regioes <- rbindlist(lista_regioes)

regioes_de_saude <- lista_regioes$co_colegiado

names(regioes_de_saude) <- lista_regioes$no_colegiado

vma <- read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/main/csv/estados.csv")

lista_regioes <- lista_regioes%>%left_join(vma%>%select(uf,regiao))

names(lista_regioes)[4:6] <- c("CO_REGIAO_SAUDE","NO_REGIAO_SAUDE","NO_REGIAO")

lista_regioes$regiao <- paste("Região",lista_regioes$regiao)
lista_regioes <- lista_regioes%>%mutate(across(c("CO_REGIAO_SAUDE","ibge"),as.numeric))


capitulos <- unique(lista_capcid_base$capitulo)
names(capitulos) <- make_clean_names(unique(lista_capcid_base$cap_nm))
diagnosticos <- as.factor(unique(lista_capcid_base)$subcat_nm)
names(diagnosticos) <- make_clean_names(unique(lista_capcid_base$subcat_nm))


# regioes <- as.factor(sort(unique(dados_combinados$NO_REGIAO)))
# levels(regioes) <- str_to_title(levels(regioes))
# cidades <- as.factor(unique(dados_combinados$DS_MUN))
# cidades <- cidades[order(cidades)]
# names(cidades) <- levels(cidades)[levels(cidades)%in%as.character(cidades)] 
# estabelecimentos <- as.factor(unique(dados_combinados$NO_FANTASIA))
# estabelecimentos <- estabelecimentos[order(estabelecimentos)]
# names(estabelecimentos) <- levels(estabelecimentos)[levels(estabelecimentos)%in%as.character(estabelecimentos)]

geo_estabs <- readRDS("dados/geo_estabs_2022_10_21.rds")


#ras_mapa <- geobr::read_health_facilities()

regioes_saude_mapa <-  geobr::read_health_region()
#estados< read.csv2...
inds_dahu <-read.csv2("dados/2012-2017-indicadores_saude_brutos.csv")
lista_ind_dahu <- read_csv2("dados/lista_dahu.csv")
names(lista_ind_dahu$indicador) <- lista_ind_dahu$desc

lista_ind_dahu <- lista_ind_dahu$indicador

lista_estados <- read_csv("https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/estados.csv")


ecd_ed <- read_csv2("dados/2012-2017-tab_ecd_ed.csv")[-1]

lista_educacao <- unique(ecd_ed$indicador)
names(lista_educacao) <- gsub("_"," ",lista_educacao)

ecd_ed <- ecd_ed %>% pivot_wider(names_from=indicador, values_from= value)
ecd_ed$cod_mun <- as.character(ecd_ed$cod_mun)
#Mapa municipios
shp <- get_brmap("City")
shp$cod_mun <- as.character(trunc(shp$City/10))

shp_sf <- st_as_sf(shp)%>%st_transform(4326)

cnes_abr_geo <- readRDS("dados/2022-10-23-cnes_abr22_validos.rds")


cnes_abr_geo %<>%dplyr::filter(!is.na(NU_LATITUDE) & !is.na(NU_LONGITUDE))

geocnes <- sf::st_as_sf(cnes_abr_geo%>%dplyr::filter(!is.na(NU_LONGITUDE)),
                        coords = c("NU_LONGITUDE","NU_LATITUDE"),
                        crs = sf::st_crs(3857))



tp_estabs <- read_csv2("dados/tipos_unidades_todos.csv")

tp_estabs%<>%mutate(aes_destaque = case_when(
  TP_UNIDADE %in% c(1,2,4,5,7,15,20,21,36,39,42,62,71,73) ~ 1,
  T ~ 0
))

tp_icones <- read_csv("dados/estabelecimentos_icones.csv")

tp_estabs%<>%left_join(tp_icones[c("código","ícone","paleta","cor")],by = c("TP_UNIDADE" = "código"))


tp_estabs[is.na(tp_estabs$ícone),]$ícone <- "star-of-life"
tp_estabs[is.na(tp_estabs$paleta),]$paleta <- 2
tp_estabs[is.na(tp_estabs$cor),]$cor <- 4
#tp_estabs$ícone <- paste0("fa-solid fa-",tp_estabs$ícone)

geocnes$TP_UNIDADE <- as.numeric(geocnes$TP_UNIDADE)

geocnes%<>%left_join(tp_estabs)

geocnesf <- geocnes%>%dplyr::filter(grepl("^52",CO_MUNICIPIO_GESTOR),
                                    aes_destaque == 1)

#geocnesf <- geocnes

###habilitações e incentivos por CNES
hi_cnes <- readRDS("dados/habilitacoes_por_cnes.rds")

geocnesf%<>%left_join(hi_cnes%>%select(CNES,habs_incs)%>%
                        mutate(CNES=as.character(CNES)),
                      by = c("CO_CNES" = "CNES"))

geocnesf[is.na(geocnesf$habs_incs),]$habs_incs <- 0

# geocnesf$ícone <- lapply(paste0("fa-",geocnesf$ícone),makeAwesomeIcon,library = "fa",
#                          iconColor = "orange")


# marcacor <- c("red", "darkred", "lightred", "orange", "beige", "green", "darkgreen", "lightgreen", "blue", "darkblue", "lightblue", "purple", "darkpurple", "pink", "cadetblue", "white", "gray", "lightgray", "black")
# ##Selecionar 15 - feito com sample(1:19,15)
# cor15 <- c(6, 12, 17, 9, 13, 19, 2, 5, 8, 14, 10,1, 16,  3, 15)

icones_mapa <- mapply(makeAwesomeIcon,paste0("fa-",c(tp_icones$ícone,"star-of-life")),library = "fa",
                            iconColor = mapply(pegacor,c(tp_icones$paleta,2),c(tp_icones$cor,4)),
                      markerColor = marcacor[cor15],squareMarker = T,
                      SIMPLIFY=F)

class(icones_mapa) <- "leaflet_awesome_icon_set"

names(icones_mapa) <- unique(tp_estabs$ícone)



#rl_estab_complementar <- dbGetQuery(con_pd,"select * from cnes.rl_estab_complementar")
rl_estab_complementar <- readRDS("dados/2022-10-31-rl_estab_complementar.rds")

##sumário unidades leitos
unid_leitos <- rl_estab_complementar%>%group_by(CO_UNIDADE)%>%summarize(across(contains("QT_"),sum,na.rm=T))%>%mutate(QT_N_SUS = QT_EXIST-QT_SUS,
                                                                                                                      HPP = ifelse(QT_EXIST>4 & QT_EXIST<31,T,F))
unid_leitos$CO_CNES <- substr(unid_leitos$CO_UNIDADE,7,13)

geocnesf%<>%left_join(unid_leitos%>%select(-CO_UNIDADE))

geocnesf[is.na(geocnesf$QT_EXIST),]$QT_EXIST <- 0

geocnesf[is.na(geocnesf$QT_SUS),]$QT_SUS <- 0

geocnesf[is.na(geocnesf$QT_N_SUS),]$QT_N_SUS <- 0

geocnesf[is.na(geocnesf$HPP),]$HPP <- F




##Serviços referenciados


tb_servico_referenciado <- dbGetQuery(con_pd,"select * from cnes.tb_servico_referenciado")

tb_servico_referenciado$CO_CNES <- substr(tb_servico_referenciado$CO_UNIDADE,7,13)


geocnesf%<>%left_join(tb_servico_referenciado%>%select(CO_CNES,TP_SERVICO_REFERENCIADO,CO_SERVICO_REFERENCIADO))

##Serviços cf documentação
servicos_cnes <- c(120,121,122,129,142,145,146,151)
