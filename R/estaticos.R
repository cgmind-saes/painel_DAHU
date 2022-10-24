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
lista_ind_dahu <- read_csv2("dados/lista_saude.csv")
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

