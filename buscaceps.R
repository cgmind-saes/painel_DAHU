geo_falta2 <- readRDS("dados/geofalta2.rds")

latefalta2 <- cepR::busca_multi(lista_ceps = geo_falta2$CO_CEP,token = Sys.getenv("tokencepaberto"))