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
source("R/paletas.R")
source("R/estaticos.R")



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



