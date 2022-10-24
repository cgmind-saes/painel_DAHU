#Funções auxiliares


##Leitura e gravação - carga
###Leitura e gravação de array em fst
read_fst_array <- function(file_name) {
  
  ft <- fst::read_fst(file_name)  # single column data.frame
  metaf <- paste0(file_name, ".meta")
  if(file.exists(metaf)) {
    meta_data <- readRDS(metaf)  # retrieve dim
    
    m <- ft[[1]]  
    attr(m, "dim") <- meta_data$dim
    dimensiones <- length(meta_data$dim)
    meta_data <- meta_data[2:(dimensiones+1)]
    # lapply(1:dimensiones,function(d,f) {
    #   dimnames(f)[[d]] <- meta_data[[d]]
    # })
    dimnames(m) <- meta_data
    
    m} else {
      ft
    }
}

write_fst_array <- function(m, file_name) {
  
  # store and remove dims attribute
  dim <- attr(m, "dim")
  
  meta_data <- list(
    dim = dim
  )
  for (i in 1:length(dim)){
    meta_data[[i+1]] <- dimnames(m)[[i]]
  }
  
  # serialize tale and meta data
  attr(m, "dim") <- NULL
  fst::write_fst(data.frame(Data = m), file_name)
  saveRDS(meta_data, paste0(file_name, ".meta"))
}


###Filtrar e formatar pedaços de tb_aih
lepedaco_filtra <- function(narq) {
  a <- ifelse(narq == 0,1,0)
  pedaco2 <- read_csv2(
    paste0("dados/sepTB_AIH/TB_AIH-",sprintf("%03d",narq),".csv"),
    quote="\"",
    skip = a,
    col_names = nom_cols,
    col_types = tipodados,
    num_threads = 8,
    progress = T,
    col_select = all_of(c(colunas_neces,"CO_IDENT","CO_MODALIDADE_INTERNACAO","ST_SITUACAO"))
  )
  pedaco2 %<>% mutate(across(c(CO_IDENT,CO_MODALIDADE_INTERNACAO),as.numeric))
  pedaco2 %<>% filter(ST_SITUACAO == 0,CO_IDENT != 5, CO_MODALIDADE_INTERNACAO == 2)
  pedaco2 %<>% select(all_of(colunas_neces))
  pedaco2
}



#Formatos e adequação de colunas e listas de filtros
### Formatar para data um número YYYYmmdd
paradata <- function(num) {
  if (!is.null(dim(num))) {
    num <- apply(num,1, function(x){
      if(x < 1e7){x <- x*100+1} 
      x})
  }else if (length(num)>1){
    num <- sapply(t(num), function(x){if(x < 1e7)
    {x <- x*100+1}
      x
    })
  }
}

#comtodos <- function(x) {x <- c(x,"Todas/os")}

todesadd <- function(x) {c(subdims[[x]],"todos")}
todesaddl <- function(x) {
  if(is.numeric(x)){
    d <- c(x,9e6)
    names(d)[length(d)] <- "todos"
  } else {
    d <- c(subdims[[x]],"todos")
    names(d)[length(d)] <- "todos"
  }
  d
}

lu <- function(x) {length(unique(x))}

damos <- function(x,tam = 6e6) {
  sample(x,tam, replace = T)
}


recortes <- c("NO_REGIAO","CO_UF_SIGLA","CO_REGIAO_SAUDE","DS_MUN","CO_CNES")
names(recortes) <- c("Região","UF","Região Saúde","Município","Estabelecimento")

listar_loc <- function(recorte) {unlist(case_when(
  recorte == "DS_MUN" ~ list(cidades),
  recorte == "CO_UF_SIGLA" ~ list(ufsa),
  recorte == "NO_REGIAO" ~ list(regioes),
  recorte == "CO_REGIAO_SAUDE" ~ list(regioes_de_saude),
  recorte == "CO_CNES" ~ list(estabelecimentos)
))
}

numa_loc <- function(filatr){
  unlist(case_when(
    filatr[length(filatr)] == "DS_MUN" ~ 0,
    filatr[length(filatr)] == "CO_UF_SIGLA" ~ 1e8,
    filatr[length(filatr)] == "NO_REGIAO" ~ 1e9,
    filatr[length(filatr)] == "CO_REGIAO_SAUDE" ~ 1e6,
    filatr[length(filatr)] == "CO_CNES" ~ 1e10,
    T ~ 1e9
  ))}

##Funções de cálculo dos indicadore
calcs_tmp <- function(reco = "DS_MUN",array_base = dados_combinados) {
  print(paste("calcs_tmp reco é",reco))
  arrj <- array_base%>%rename(periodo = DT_CMPT)%>%
    group_by(periodo,!!(sym(reco))) %>% summarize(pacientes = n(),
                                                  diarias = sum(QT_DIARIAS),
                                                  TMP = diarias/pacientes,
                                                  param_tmp = cut(TMP,c(0,2,5,30)))%>%arrange(2,1)
  arrj <- array(unlist(arrj[,-1:-2]),
                dim = c(lu(arrj[[1]]),lu(arrj[[2]]),ncol(arrj)-2),
                dimnames = list(unique(arrj[[1]]),unique(arrj[[2]]),colnames(arrj)[-1:-2]))
  arrj %<>% aperm(c(1,2,3))
  
  print(dim(arrj))
  
  print(paste("OK tmp e param_tmp",reco))
  arrj
}

##Filtra tb dim  <--- VERSAO PARA QUANDO PEGA DO BD
# filtra_dim <- function(rega,ab = tb) {
#   print(paste("filtra_dim vai de",names(rega),collapse = "\n"))
#   rega <- rega[!grepl("todos",sapply(rega,as.character))]
#   abab <- rep(character(),7)
#   if(length(rega)>1) {
#     for (i in 1:(length(rega)-1)) {
#       abab <- abab[1:(length(rega)-1)]
#       abab[i] <- sprintf('!!sym(names(rega)[%d]) %s rega[[%d]]',i,"%in%",i)
#     }
#     xpre <- paste("filter(ab,",paste(abab,collapse = " & "),")")
#     d <- eval(parse(text = xpre))
#   } else {
#     ab
#   }
#   
# }
filtra_dim <- function(rega,ab = tb) {
  rega <- rega[!grepl("todos",sapply(rega,as.character))]
  
  names(rega) <- filtr_a_cdc[filtr_a_cdc$filtro %in% names(rega),]$coldc
  
  if (length(rega)<2) {
    ab
  } else {
    abab <- rep(character(),length(rega))
    for (i in 1:(length(rega)-1)){
      abab[i] <- sprintf('!!sym(names(rega)[[%d]]) %s rega[[%d]]',i,"%in%",i)
    }
    
    
    xpre <- paste("filter(ab,",paste(abab,collapse = ", "),")")
    
    abdimi <- dim(ab)
    b <- eval(parse(text = xpre))
    print(paste("saiu filtrando filtra_dim e ficou de",paste(abdimi,collapse="x"), 
                "para",paste(dim(b),collapse="x")))
    b
  }
}



# estr_pad_leitos <- dados_combinados%>%select(QT_DIARIAS,NU_ESPECIALIDADE)%>%
#   group_by(NU_ESPECIALIDADE)%>%summarize(internacoes = n())%>%
#   mutate(partic_leitos = prop.table(internacoes))

tmpp <- function(corte = "DS_MUN",array_base = dados_combinados) {
  
  arrj <- dados_combinados%>%mutate(periodo = format.Date(DT_CMPT,"%Y%m"))%>%
    group_by(periodo,!!(sym(corte)),NU_ESPECIALIDADE)%>%
    summarize(tmpl = sum(QT_DIARIAS)/n())%>%arrange(3,2,1)
  arrj <- array(unlist(arrj[,-1:-3]),dim = c(lu(arrj[[1]]),lu(arrj[[2]]),lu(arrj[[3]]),1),dimnames = list(unique(arrj[[1]]),unique(arrj[[2]]),unique(arrj[[3]]),"tmpl"))
  arrj <- aperm(arrj,c(3,1,4,2))
  arrj <- arrj*estr_pad_leitos[c(2, 3,1,7,5,4, 13, 9,8, 6, 10, 12, 11),]$partic_leitos
  
  arrj <- apply(arrj,c(2,3,4),sum,na.rm = T,simplify = F)
  arrj <- aperm(arrj,c(1,3,2))
  print(dim(arrj))
  print(paste("OK TMPP",corte))
  arrj
}

cindica <- function(corte = "DS_MUN",array_base = dados_combinados) {
  dim_local <- listar_loc(corte)
  a <- calcs_tmp(corte,array_base)
  print(paste("cindica tmp e todos menos tmpp"))
  b <- tmpp(corte,array_base)
  if(dim(a)[1] == 0) {
    if (corte=="CO_UF_SIGLA"){
      ccc <- array(NA,c(lu(periodos),lu(dim_local),5),
                   dimnames=list(as.numeric(format.Date(periodos,"%Y%m")),vma[order(vma$uf),]$uf,indicadores))  
    } else {
      ccc <- array(NA,c(lu(periodos),lu(dim_local),5),
                   dimnames=list(as.numeric(format.Date(periodos,"%Y%m")),dim_local,indicadores))
    }
    
    
    print(paste("a zerado, nomes de dim de c",paste(head(dimnames(ccc)),collapse="\n")))
  } else if (dim(a)[1]<lu(periodos) || dim(a)[2]<lu(dim_local)){
    ajuntaper <- setdiff(periodos,dimnames(a)[[1]])
    ajuntaper <- array(NA,dim = c(length(ajuntaper),dim(a)[-1]), dimnames = list(ajuntaper,dimnames(a)[[2]],dimnames(a)[[3]]))
    a <- abind(a,ajuntaper,along = 1)
    print("adicionados periodos faltantes")
    if(length(dimnames(a)[[2]])<length(dim_local)) {
      if(corte == "CO_UF_SIGLA") {
        ajuntaloc <- setdiff(vma$uf,dimnames(a)[[2]])
      } else {
        ajuntaloc <- setdiff(dim_local,dimnames(a)[[2]])
      }
      print(length(ajuntaloc))
      print("a adicionar a")
      print(dim(a)[2])
      ajuntaloc <- array(NA,dim = c(dim(a)[1],length(ajuntaloc),dim(a)[3]), dimnames = list(dimnames(a)[[1]],ajuntaloc,dimnames(a)[[3]]))
      a <- abind(a,ajuntaloc, along = 2)
      print("adicionados locais faltantes")
    }
    
  } else if (is.null(dim(b)) | dim(b)[1]==0){
    b <- array(1,dim=c(dim(a)[1:2],1),dimnames = c(dimnames(a)[1:2],"TMPP"))
  }
  #      subbum <- dimnames(b)[[1]][dimnames(b)[[1]] %in% dimnames(a)[[1]]]
  #      subdois <- dimnames(b)[[2]][dimnames(b)[[2]] %in% dimnames(a)[[2]]]
  
  #      b <- b[subbum,subdois,]
  print(paste("cindica tmp e todos menos tmpp... tmpp",paste(dim(b),collapse = "X")))
  aba <- try({
    ccc <- abind(a,b,along=3)
    dimnames(ccc)[[3]][5] <- "TMPP"
  })
  if(class(aba)== 'try-error') {
    a
  } else {
    ccc
  }
  
}

ccalcs_tmp <- function(reg = filtros,tb = dados_combinados) {
  reg <- sapply(reg,as.character)
  print(dim(tb))
  tb <- filtra_dim(reg,tb)
  print("dim ficou em")
  print(dim(tb))
  print(paste("ccalcs_temp",paste(reg,collapse = " e ")))
  coho <- unlist(reg[9])
  
  if (is.data.frame(coho) == T) {
    coho <- sapply(coho,as.character)
  }
  dim_local <- listar_loc(coho)
  
  sbrc <- cindica(coho,tb)
  
  if (0 %in% dim(sbrc)) {
    sbrc <- array(1,dim = c(lu(periodos),lu(dim_local),lu(indicadores),1,1,1,1,1,1,1),
                  dimnames(list(periodos = unique(periodos),local = unique(dim_local), 
                                indicador = indicadores,
                                porte = reg[[1]],sexo = reg[[2]],cor = reg[[3]],
                                fx_et = reg[[4]],fx_et_det = reg[[5]],capitulo = reg[[6]],
                                capitulo2 = reg[[7]],especialidade = reg[[8]]))
    )
  } else  {
    perpres <- dimnames(sbrc)[[1]]
    adic <- numa_loc(coho)
    
    d <- adic+as.numeric(as.factor(dimnames(sbrc)[[2]]))
    
    e <- dimnames(sbrc)[[3]]
    
    dim(sbrc) <- c(dim(sbrc)[1:3],1,1,1,1,1,1,1,1)
    
    dimnames(sbrc) <- list(perpres,d,e,reg[[1]],reg[[2]],reg[[3]],reg[[4]],reg[[5]],
                           reg[[6]],reg[[7]],reg[[8]])
    print(paste("ja com tudo ccal",paste(dim(sbrc),collapse="x")))
    
  }
  sbrc
}
#Fonte:https://stackoverflow.com/questions/31152960/display-only-months-in-daterangeinput-or-dateinput-for-a-shiny-app-r-programmin
monthsRangeInput <- function(inputId, label, start = as.Date("2018-01-01","%Y-%m-%d"), end = as.Date("2021-01-01","%Y-%m-%d"),
                             min = NULL, max = NULL, format = "yyyy-mm-dd", startview = "month",
                             minviewmode="months", # added manually
                             weekstart = 0, language = "pt-br", separator = " a ", width = NULL) {
  
  # If start and end are date objects, convert to a string with yyyy-mm-dd format
  # Same for min and max
  if (inherits(start, "Date"))  start <- format(start, "%b/%Y")
  if (inherits(end,   "Date"))  end   <- format(end,   "%b/%Y")
  if (inherits(min,   "Date"))  min   <- format(min,   "%b/%Y")
  if (inherits(max,   "Date"))  max   <- format(max,   "%b/%Y")
  print(class(start))
  htmltools::attachDependencies(
    div(id = inputId,
        class = "shiny-date-range-input form-group shiny-input-container",
        style = if (!is.null(width)) paste0("width: ", validateCssUnit(width), ";"),
        
        controlLabel(inputId, label),
        # input-daterange class is needed for dropdown behavior
        div(class = "input-daterange input-group",
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = start
            ),
            span(class = "input-group-addon", separator),
            tags$input(
              class = "input-sm form-control",
              type = "text",
              `data-date-language` = language,
              `data-date-weekstart` = weekstart,
              `data-date-format` = format,
              `data-date-start-view` = startview,
              `data-date-min-view-mode` = minviewmode, # added manually
              `data-min-date` = min,
              `data-max-date` = max,
              `data-initial-date` = end
            )
        )
    ),
    datePickerDependency
  )
}

`%AND%` <- function(x, y) {
  if (!is.null(x) && !is.na(x))
    if (!is.null(y) && !is.na(y))
      return(y)
  return(NULL)
}

controlLabel <- function(controlName, label) {
  label %AND% tags$label(class = "control-label", `for` = controlName, label)
}

# the datePickerDependency is taken from https://github.com/rstudio/shiny/blob/master/R/input-date.R
datePickerDependency <- htmltools::htmlDependency(
  "bootstrap-datepicker", "1.6.4", c(href = "shared/datepicker"),
  script = "js/bootstrap-datepicker.min.js",
  stylesheet = "css/bootstrap-datepicker3.min.css",
  # Need to enable noConflict mode. See #1346.
  head = "<script>
 (function() {
 var datepicker = $.fn.datepicker.noConflict();
 $.fn.bsDatepicker = datepicker;
 })();
 </script>")

cria_geom <- function(x,cores = paleta7) {
  geom_line(aes(y = !!sym(as.character(x))),alpha = 1, color = sample(1:12,1), size = 1)
}

plotabanda_destaque <- function(recorte = "NO_REGIAO",destaque_reg = 3,
                                indicador = "TMP",
                                paramin = 3,
                                paramax = 5,
                                filatr = filtros(),
                                btab = dados_combinados,
                                datas = as.Date(c("2018-01-31","2022-03-31"))){
  
  #filtros <- gsub("todos","",sapply(filatr,as.character))
  btab <- as.data.frame(btab[,,indicador,,,,,,,,])
  
  limfiltro <- numa_loc(filatr)
  
  if (is.null(datas) || 0 %in% dim(datas)||length(datas)==0||is.na(datas)) {
    datas = as.Date(c("2018-01-31","2022-03-31"))
  }
  
  if (as.numeric(row.names(btab)[1]) > 201200){
    btab$periodos <- as.Date(paste0(as.numeric(row.names(btab)),01), "%Y%m%d")
  } else {
    btab$periodos <- as.Date(as.numeric(row.names(btab)), "1970-01-01")
  }
  
  btab <- btab%>%pivot_longer(-periodos,names_to = "local",values_to="valor")
  btab$local <- as.numeric(btab$local)
  contextos <- btab%>%group_by(periodos)%>%summarize(mini=min(valor,na.rm = T),
                                                     mediana=median(valor,na.rm = T),
                                                     maxi=max(valor,na.rm = T),
                                                     media= mean(valor,na.rm = T))
  contextos <- as.data.table(contextos)
  
  destaque_reg <- limfiltro+as.numeric(destaque_reg)
  destaques <- btab%>%filter(local %in% destaque_reg, periodos>datas[1],periodos<datas[2])%>%select(periodos,local,valor)
  destaques <- as.data.frame(destaques)
  
  dim_local <- listar_loc(recorte)
  
  if(0 %in% dim(destaques)){
    cd <- contextos
    print("sem destaques")
  } else {
    njun <- data.frame(local = as.numeric(dim_local),nom_local = str_to_title(as.character(dim_local)))
    njun$local <- njun$local+limfiltro
    destaques <- destaques[!duplicated(destaques[1:2]),] #%>%mutate(local = local-limfiltro)
    destaques <- destaques%>%left_join(njun)
    destaques$local <- destaques$nom_local
    destaques%<>%select(-nom_local)%>%pivot_wider(names_from = local,values_from = valor)
    
    cd <- destaques%>%left_join(contextos)
    cd <- as_tibble(cd)
  }
  print("Em plota banda destaque")
  
  ###PRODUZ A PLOTAGEM  
  d <- ggplot(cd,aes(x = periodos))+
    geom_line(aes(y = media),color = paleta3[1], size = 0.3, linetype = "longdash")+
    geom_line(aes(y = paramax),color = paleta4[5],size= 0.2, linetype = "dashed")+
    geom_line(aes(y = paramin),color = paleta3[5],size= 0.2, linetype = "dashed")+
    geom_line(aes(y = maxi), color = paleta3[4],alpha=0.5,linetype = "dotted")+
    geom_line(aes(y = mini),alpha=0.5,color = paleta3[5], size = 0.7,linetype = "dotted")+
    geom_line(aes(y = mediana),alpha=0.5,color = paleta6[2] ,linetype = "dashed",size = 0.5)+
    geom_ribbon(aes(ymin = mini, ymax = maxi,fill = maxi>mini),alpha = 0.1, show.legend = F)+
    scale_fill_manual(values = paleta6[c(10,11,1:9)])+
    ylab("TMP")+
    scale_x_date(breaks = "1 year",date_labels = "%b %Y")+
    xlab("Período de competência")+
    ggtitle(paste("Tempo médio de permanência","por ",names(recortes[recortes == recorte]),
                  "\n",format.Date(first(cd$periodos),"%B/%Y"),"a",format.Date(last(cd$periodos),"%B/%Y")))+
    #    expand_limits(y=0)+
    theme_minimal()
  if(nrow(destaques)==0){d <- d} else{
    for (i in 1:length(destaque_reg)) d <- d+cria_geom(names(cd)[2:(ncol(cd)-4)][i],i)}
  
  hide_guides(ggplotly(d))
}

plota_lolly_agrupadas <- function(recorte = "NO_REGIAO", 
                                  destaque_reg = "REGIÃO NORDESTE",
                                  indicador = "TMP",
                                  periodo_de_tempo = "ultimos 12 meses",
                                  paramin = 3, 
                                  paramax = 5,
                                  btab = dados_combinados,
                                  datas = as.Date(c("2018-01-31","2022-03-31"))){
  indi <- indicador
  
  btab <- btab[,,indicador,,,,,,,,]
  
  loc_nums <- dimnames(btab)[[2]]
  datas_nums <- as.Date(paste0(dimnames(btab)[[1]],"01"),"%Y%m%d")
  
  btab <-  rbindlist(
    lapply(dimnames(btab)[[3]],
           function(x) {
             data.frame(matrix(btab[,,x],nrow=dim(btab)[1],byrow = T),fx_et=x,
                        periodos = datas_nums)
           }))
  
  if (is.null(datas) || 0 %in% dim(datas)||length(datas)==0||is.na(datas)){datas <- as.Date(c("2018-01-01","2021-01-01"),"%Y-%m-%d")}
  names(btab)[1:length(loc_nums)] <- loc_nums
  
  btab <- btab%>%filter(periodos == datas[1] | periodos == datas[2])
  # print(names(btab))
  
  nom_idades <- data.frame("fx_et"=as.character(lista_grp_id), "faixa_etaria"=names(lista_grp_id))
  
  btab <- btab%>%left_join(nom_idades)
  btab[btab$fx_et == "todos",]$faixa_etaria <- "todas as idades"
  # print(head(btab))
  btab <- btab%>%pivot_longer(-c(periodos,fx_et,faixa_etaria),names_to = "local",values_to="tmp")%>%group_by(faixa_etaria,periodos)%>%
    summarize(tmp=mean(tmp))%>%ungroup()
  # print(str(btab))
  
  # print("vejamos tabela idades")
  # print(tail(btab))
  #color = cut(btab[,periodos], c(-Inf, 25, 55, 70, 90, Inf))), )
  d <- ggplot(btab,aes(faixa_etaria,y=tmp,groups=periodos))+
    geom_bar(ggplot2::aes(fill = faixa_etaria),
             position = position_dodge2(preserve ="total"),
             stat = "identity",
             width=5,lwd=0)+
    ggtitle("Tempo médio de permanência por faixa etária")+
    scale_fill_manual(values=paleta7[-1:-2])+
    #scale_fill_manual(values=tableau_color_pal(palette = "Classic Purple-Gray 12","regular")(12))+
    # geom_segment(aes(x = periodos, xend = periodos, y = 0, yend = tmp,
    #                  fill = fx_et),
    #               lwd = 1) +
    # geom_point(size = 2, pch = 21, bg = scales::seq_gradient_pal("grey20", "red")(seq(0,1,length.out=nrow(btab))), col = 1)+
    #scale_color_manual()+
    # geom_text(aes(label = tmp), color = "white", size = 1,angle=90) +
    
    #    scale_x_discrete(labels = paste0("Fx_", 1:10)) +
    #    coord_flip() +
    theme_minimal()+
    scale_y_continuous(limits=c(max(0,min(btab$tmp, na.rm = T))-0.25,min(5.5,max(btab$tmp, na.rm = T))),oob = rescale_none)+
    theme(legend.position = "bottom",axis.text.x=element_blank(),axis.text.y.right=element_blank())+
    #scale_x_date(breaks = "1 year",date_labels = "%b %Y")+
    xlab(paste0(format.Date(datas[1],"%b/%Y"),"                                   ",
                format.Date(datas[2],"%b/%Y"),"\n\n\n"))+
    facet_wrap(~faixa_etaria, scales="free_x")
  
  # scale_y_continuous(breaks = c(2,3,4,5,6),limits = c(2,6))
  ggplotly(d)%>%plotly::layout(legend = list(orientation = 'h'))
}


pizza_params <- function(recorte = "NO_REGIAO",
                         destaque_reg = "Região Nordeste",
                         indicador = "TMP",
                         periodo_de_tempo = "ultimos 12 meses",
                         paramin = 3,
                         paramax = 5,
                         btab = dados_combinados,
                         datas = as.Date(c("2018-01-01","2021-12-01"),"%Y-%m%-d")) {
  
  print(paste0(paste0(destaque_reg,collapse=","),"cara pizza ",paste(dim(btab),collapse="x")))
  
  aga <- as.data.frame(btab[,,indicador,,,,,,,,])
  aganam <- row.names(aga)
  
  aga$periodos <- as.Date(paste0(aganam,"01"),"%Y%m%d")
  
  
  aga <- aga%>%pivot_longer(-periodos,names_to = "local",values_to = "valor"  )
  
  aga$local <- as.numeric(aga$local)
  print(dim(btab))
  print(dimnames(btab)[[3]])
  pesos <- as.data.frame(btab[,,'pacientes',,,,,,,,])
  pesos$periodos <- as.Date(paste0(row.names(pesos),"01"),"%Y%m%d")
  pesos %<>% pivot_longer(-periodos,names_to = "local",values_to = "pesos")
  pesos$local <- as.numeric(pesos$local)
  #aga <- aga[c(1:2,ncol(aga))]
  
  # aga%<>%mutate(grupo = case_when(valor>=5 ~ "critico",
  #                          valor>=2 & valor<5 ~ "dentro da faixa esperada",
  #                          valor < 2 ~ "subutilizado"))
  #aga%<>% filter(as.numeric(periodo)> max(as.numeric(periodo))-99)
  limfiltro <- numa_loc(recorte)
  
  dim_local <- listar_loc(recorte)
  print(dim_local)
  aga <- as_tibble(aga)%>% left_join(pesos)%>%group_by(local)%>%summarise(valor = mean(valor,na.rm = T),
                                                                          pesos = sum(pesos,na.rm = T),
                                                                          grupo = case_when(valor>= 4.3 ~ "critico",
                                                                                            valor>2.2 & valor<4.3 ~ "entre 2 e 5<br>dias",
                                                                                            valor <= 2.2 ~ "tempo médio baixo",))
  aga <- aga %>% mutate(local = as.numeric(local)-limfiltro)
  print(paste0("mudado local tirado limfiltro",paste0(head(aga$local),collapse = "\n")))
  
  njun <- data.frame(local = as.numeric(dim_local),nom_local = as.character(dim_local))
  
  aga %<>% left_join(njun)
  aga$local <- aga$nom_local
  
  
  percg <- aga%>%group_by(grupo)%>%summarize(pesos = sum(pesos),
                                             valor = sum(valor*pesos)/sum(pesos))
  
  grupos <- sort(unique(aga$grupo))
  
  percg$local <- percg$grupo
  
  percg$grupo <- NA
  
  aga %<>%bind_rows(percg)
  
  
  aga$labels <- paste0(aga$local,"<br>",paste0(aga$valor," dias"))
  deno <- aga[is.na(aga$grupo),]$valor
  
  aga[is.na(aga$grupo),]$labels  <-  paste0(aga[is.na(aga$grupo),]$local,"<br>",paste0(aga[is.na(aga$grupo),]$valor/sum(deno),"%"))
  
  aga$colores <-  c(paleta3,paleta6,paleta4,paleta6)[1:nrow(aga)]  
  
  print(aga,n=30)
  # d <- plot_ly(aga,ids =c(aga$local,grupos),
  #              labels =c(aga$local,labelsg),
  #              parents = c(aga$grupo,rep("",lu(aga$grupo))),type = 'sunburst',branchvalues = 'relative',
  #              maxdepth = 3, domain = list(column = 1),colors = ~colores)%>%
  d <- plot_ly(aga,ids=~local,labels = ~labels,parents = ~grupo, type = 'sunburst',branchvalues = 'relative',
               maxdepth = 3, domain = list(column = 1),colors = ~colores)%>%
    plotly::layout(sunburstcolorway = ~colores, colorway = ~colores, extendsunburstcolors = T)
  d
}

plota_barras_agrupadas <- function(recorte = "NO_REGIAO",
                                   destaque_reg = "REGIÃO NORDESTE",
                                   indicador = "TMP",
                                   periodo_de_tempo = "ultimos 12 meses",
                                   paramin = 3,
                                   paramax = 5,
                                   btab = dados_combinados,
                                   datas = as.Date(c("2018-01-01","2021-12-01"),"%Y-%m%-d")){
  indi <- indicador
  print("entrou por porte")
  
  btab <- btab[,,indicador,,,,,,,,]
  btabnam <- as.Date(paste0(dimnames(btab)[[1]],"01"),"%Y%m%d")
  if(dimnames(btab)[[3]][7] =="") {dimnames(btab)[[3]][7] <- "Grande Porte II"}
  btab <-  rbindlist(
    lapply(dimnames(btab)[[3]],
           function(x) {
             data.frame(matrix(btab[,,x],nrow=dim(btab)[1],byrow = T),porte=x,
                        periodos = btabnam)
           }))
  
  
  if(!length(recorte)) {
    names(btab)[1:length(listar_loc("CO_UF_SIGLA"))] <- ufsa
  } else {
    names(btab)[1:length(listar_loc(recorte))] <- listar_loc(recorte)
  }
  btab%<>% pivot_longer(-c(periodos,porte),names_to="local",values_to="valor")
  
  btabg <- btab %>% group_by(porte)%>% summarize(tmp=mean(valor,na.rm =T))%>%
    mutate(`porte agregado` = case_when(
      grepl("eno",porte) ~ "Pequeno",
      grepl("éd",porte) ~ "Médio",
      grepl("and",porte) ~ "Grande",
      grepl("odos",porte) ~ "Todos",
      T ~ "Todos"), periodos = Sys.Date())
  # btab$periodos <- as.Date(paste0(btabnam,"01"),"%Y%m%d")
  btab <- btab %>% 
    group_by(periodos,porte)%>%summarize(tmp=mean(valor,na.rm =T))%>%
    mutate(`porte agregado` = case_when(
      grepl("eno",porte) ~ "Pequeno",
      grepl("éd",porte) ~ "Médio",
      grepl("and",porte) ~ "Grande",
      grepl("odos",porte) ~ "Todos",
      T ~ "Todos"))
  btab[btab$porte == "Grande Porte II",]$tmp <-  NA
  if (is.null(datas) || 0 %in% dim(datas)||length(datas)==0||is.na(datas)){
    print("acerta datas")
    datas <- c(as.Date("2018-01-01","%Y-%m-%d"),as.Date("2021-12-01","%Y-%m-%d"))
  }
  
  
  btab <- btab%>%filter(periodos %in% datas)
  btab <- btab%>%bind_rows(btabg)
  
  btab$periodos <- format.Date(btab$periodos,"%B/%Y")
  btab[btab$periodos == format.Date(Sys.Date(),"%B/%Y"),]$periodos <- "Todo o período"
  d <- ggplot(btab,aes(periodos,y=tmp,colour = `porte agregado`))+
    geom_bar(ggplot2::aes(fill = porte),
             position = position_dodge2(6,preserve = "total"),
             stat = "identity",lwd=0,width=3)+
    geom_line(aes(y=5), linetype = "dotted",lwd=0.5,color = paleta3[1])+
    geom_line(aes(y=2), linetype = "dotted",lwd=0.5,color = paleta4[1])+
    scale_y_continuous(limits=c(max(0,min(btab$tmp, na.rm = T))-0.35,min(5.5,max(btab$tmp, na.rm = T))),oob = rescale_none)+
    #scale_x_date(breaks = "1 year",date_labels = "%b %Y")+
    coord_flip()+
    ggtitle(paste("Tempo médio de permanência por porte, de",datas[1],"a",datas[2]))+
    #    xlab("Grupos de Especialidades de Leito")+
    scale_fill_manual(values=c(paleta5[c(1,2,4,6,8,10,13)],paleta7[1:4]))+
    # geom_segment(aes(x = periodos, xend = periodos, y = 0, yend = tmp,
    #                  fill = fx_et),
    #               lwd = 1) +
    #geom_point(size = 6.5, pch = 21, bg = scales::seq_gradient_pal("grey20", "red")(seq(0,1,length.out=nrow(btab))), col = 1)+
    # scale_color_manual()+
    # geom_text(aes(label = tmp), color = "white", size = 1,angle=90) +
    
    #    scale_x_discrete(labels = paste0("Fx_", 1:10)) +
    theme_minimal()+
    theme(legend.position = "left")+
    facet_wrap(vars(`porte agregado`,porte),scales="free_y", dir = 'h',ncol = 2,
               strip.position = "right")
  
  # scale_y_continuous(breaks = c(2,3,4,5,6),limits = c(2,6))
  ggplotly(d)%>%plotly::layout(xaxis = list(showticklabels=FALSE))
}


