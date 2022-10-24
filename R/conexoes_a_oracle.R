jdbcDriver = JDBC("oracle.jdbc.OracleDriver", classPath = "C:/app/client/rodrigo.emmanuel/product/12.2.0/client_1/jdbc/lib/ojdbc8.jar")

###Exemplo para RJPO1DR
esquemapd <- "RJPO1DR.saude.gov"
servidorpd <- "exaccdfdr-scan.saude.gov"
#CNES.TB_ESTABELECIMENTO

###Exemplo para DFPO1
esquemaodbc <- "DFPO1.saude.gov"
servidorodbc <- "exaccdfprd-scan.saude.gov"

baseodbc <- "jdbc:oracle:thin:@//"

odbcender <- paste0(baseodbc,servidorodbc,":1521/",esquemaodbc)

con_cnes  <-  dbConnect(jdbcDriver, odbcender,Sys.getenv("usuarioodbc"),Sys.getenv("senhaodbc"))


#testecnes <- dbGetQuery(con_cnes,'select * from DBCNESRJ.TB_ESTABELECIMENTO')


pd_ender <- paste0(baseodbc,servidorpd,":1521/",esquemapd)
con_pd  <-  dbConnect(jdbcDriver, pd_ender,Sys.getenv("usrjp"),Sys.getenv("senharjp"))


# geo_estabs <- dbGetQuery(con_pd,'select co_unidade,co_cnes,cg_longitude,cg_latitude,tp_sus_nao_sus,tp_icone_amb,tp_icone_esf,tp_icone_hosp from cnes_snp.tb_geo_coord_estab')
# 
# saveRDS(geo_estabs,"dados/geo_estabs_2022_10_21.rds")




# ####Lista CNES abril
# tb_estabs <- dbGetQuery(con_cnes,'select * from DBCNESRJ.TB_ESTABELECIMENTO')
# 
# 
# tb_estabs%<>%mutate(across(contains("CO_"),as.numeric),across(contains("NU_"),as.numeric))
# 
# 
# cnesvalidos <- dbGetQuery(con_pd,'select * from CNES.TB_ESTABELECIMENTO WHERE (co_motivo_desab is null)')
# 
# cnes_todos <- dbGetQuery(con_pd,'select * from CNES.TB_ESTABELECIMENTO')
# 
# cnes_val_abril <- cnes_todos%>%dplyr::filter(DT_CMTP_INICIO<as.Date("2022-05-01"), 
#                                                is.na(CO_MOTIVO_DESAB))
# 
# 
# cnes_val_abril%<>%mutate(across(contains("NU_"),as.numeric))
# 
# cnes_val_abril%<>%mutate(CO_CNES = as.numeric(CO_CNES))
# 
# 
# 
# cnes_abr_geo <- cnes_val_abril%>%dplyr::filter(!is.na(NU_LATITUDE) & !is.na(NU_LONGITUDE))

