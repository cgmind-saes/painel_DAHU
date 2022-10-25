## Funções a reutilizar

milhares <- function(x){prettyNum(x,big.mark = ".",decimal.mark = ",")}
tabmil <- function(x) {
  x <- as.data.table(x,keep.rownames="var")%>%mutate(across(where(is.numeric),milhares))
}




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

