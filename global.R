#####
## global.R:
## Painel para exibição dos indicadores a partir de microdados de Política Social
## para o Brasil 
## - Leitura dos dados
## - Preparação das variáveis para exibição
#####

## Carrega pacotes
source("requ.R")

## Carrega os dados
######
#estados< read.csv2...
ecd_saude <-read.csv2("dados/2012-2017-indicadores_saude_brutos.csv")
lista_ind_saude <- read_csv2("dados/lista_saude.csv")
names(lista_ind_saude$indicador) <- lista_ind_saude$desc

lista_ind_saude <- lista_ind_saude$indicador

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




## Funções a reutilizar
plotaserie <- function(dados,perc=F) {
  ##produz data.frame com cada versão para juntar
  
  if(length(dim(dados))>2){
    dados <- as.data.table(dados)
    print(head(dados))
    ifelse(ncol(dados)==5,
           names(dados) <- c("bd","ano","indicador","pais","valor"),
           names(dados) <- c("bd","ano","pais","valor")
    )
    print(dados$ano)
    dados <- dados %>% mutate(ano = as.Date(paste0("1/1/",ano),
                                            tryFormats="%d/%m/%Y"),
                              across(c(-ano,-valor),as.factor))
  }else{
    bds <- names(dados[,1])
    anos <- names(dados[1,])
    dados <- as.data.table(t(dados))
    dados$ano <- as.Date(paste0("01/01/",anos),
                         tryFormats="%d/%m/%Y")
    dados <- dados%>%pivot_longer(-ano,names_to = "bd",values_to="valor")%>%
      mutate(bd=as.factor(bd))
  }
  
  ifelse(ncol(dados)==3,
         p <- ggplot(dados,aes(x=ano,y=valor,col=bd)),
         p <- ggplot(dados,aes(x=ano,y=valor,col=pais,linetype=bd)))
  p <- p+geom_line(size = 1) +
    scale_y_continuous(labels = comma_format(big.mark = ".", decimal.mark = ","))+
    theme_minimal()+
    theme(rect = element_rect(fill = "transparent"),panel.background = element_rect(fill = "transparent", colour = NA),  plot.background = element_rect(fill = "transparent", colour = NA))
  
  ps <- ggplotly(p)

}
milhares <- function(x){prettyNum(x,big.mark = ".",decimal.mark = ",")}
tabmil <- function(x) {
  x <- as.data.table(x,keep.rownames="var")%>%mutate(across(where(is.numeric),milhares))
}

# The single argument to this function, points, is a data.frame in which:
#   - column 1 contains the longitude in degrees
#   - column 2 contains the latitude in degrees
coordsestados = function(dadosmun) {  
  
# Primeiro precisamos dos dados de latitude e longitude das cidades analisadas. 
urlfile <- "https://raw.githubusercontent.com/kelvins/Municipios-Brasileiros/master/csv/municipios.csv"
cities_lat_lng <- read.csv(urlfile,encoding = "UTF-8", col.names = c("COD_IBGE", "Cidade","lat","lng","Capital","Codigo_UF"))
# é necessário se certificar que o código de cada cidade estará em formato de texto, para o que a função left_join funcione. 
cities_lat_lng$COD_IBGE <- as.character(cities_lat_lng$COD_IBGE)
dadosmun <- left_join(dadosmun, cities_lat_lng, by = c("city_ibge_code" = "COD_IBGE"))  
}



