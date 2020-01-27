
### 06/06/2019 1.01.0: Harmonisation de la selection de variables
### 15/07/2019 1.02.1: Corrections (na.rm, noms en clairs), ajout des dénombrements
### 31/07/2019 1.03.1: AJout du comptage d'une modalité
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 19/12/2019 1.05.1: Correction min max sur données pondérées
### 09/01/2020 1.05.2: Correction quantiles sur données non pondérées
###                    Ajout d'un message en cas de moyenne sur données de type caractère
### 14/01/2020 1.05.3: Simplification des comptages sans pondération
### 23/01/2020 1.05.4: Correction prise en compte des valeurs manquantes avec min, max pondérés

.IGoR$page$summarise$ui <- function() .IGoR$ui(page="summarise", control=TRUE)


.IGoR$page$summarise$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"summarise")
  
  statsv <- c("n","v","sum","mean","median","q1","q3","p10","p90","sd","var","min","max","first","last") %>%
    {names(.) <- .; .} %>% .IGoR$Zrename()
  
  statsf  <- function(i,w,value)
    if (w=='')
      c(n="n()",
        v=glue("sum({value})"),
        sum=glue("sum_NA2_"),
        mean=glue("mean_NA2_"),
        median=glue("median_NA2_"),
        q1=glue("quantile(.,p=.25_NA1_)"),
        q3=glue("quantile(.,p=.75_NA1_)"),
        p10=glue("quantile(.,p=.10_NA1_)"),
        p90=glue("quantile(.,p=.90_NA1_)"),
        sd=glue("sd_NA2_}"),
        var=glue("var_NA2_"),
        min=glue("min_NA2_"),
        max=glue("max_NA2_"),
        first="first",
        last="last")[i]
    else 
      c(n=glue("sum({w})"),
        v=glue("sum({w}*{value})"),
        sum=glue("sum({w})*wtd.mean(.,w={w}_NA1_)"),
        mean=glue("wtd.mean(.,w={w}_NA1_)"),
        median=glue("wtd.quantile(.,p=.5,w={w}_NA1_)"),
        q1=glue("wtd.quantile(.,p=.25,w={w}_NA1_)"),
        q3=glue("wtd.quantile(.,p=.75,w={w}_NA1_)"),
        p10=glue("wtd.quantile(.,p=.10,w={w}_NA1_)"),
        p90=glue("wtd.quantile(.,p=.90,w={w}_NA1_)"),
        sd=glue("sqrt(wtd.var(.,w={w}_NA1_))"),
        var=glue("wtd.var(.,w={w}_NA1_)"),
        min=glue("min_NA2_"),
        max=glue("max_NA2_"),
        first="first",
      last="last")[i]
  
  statna <- function(i,w,na.rm) {
    j <- case_when(i %in% c('n','v','first','last') ~ 0, # na.rm doesn't make sense
                   i %in% c('min','max') ~ 1,            # na.rm=FALSE by default
                   (w=='') ~ 1,                          # na.rm=FALSE by default
                   TRUE ~ 2)                             # na.rm=TRUE by default
    v <- case_when(((j==1)&na.rm)  ~ 'na.rm=TRUE',
                   ((j==2)&!na.rm) ~ 'na.rm=FALSE',
                              TRUE ~ '')
    u <- unique(v)
    if (length(u)==1) u else v
  }
  
  stat <- function (i,w,na) {
    value <- 
      if (.isNotEmpty(input$summarise.value)) 
        if (input$summarise.value=="TRUE") "."
        else 
        if (input$summarise.value=="FALSE") "!."
        else
          if (w=='') paste0(".==",input$summarise.value)
          else      paste0("(.==",input$summarise.value,")")
      else "is.na(.)"
    if (length(na)>1)
      statsf(i,w,value) %>%
        str_replace("_NA1_",ifelse(na!='',paste0(',',na),'')) %>%
        str_replace("_NA2_",ifelse(na!='',paste0('(.,',na,')'),''))
    else
      statsf(i,w,value) %>% str_replace("_NA._",'')
  }
  
  suffix <- function() if (.isNotEmpty(input$summarise.value)) paste0("_",input$summarise.value) else "_NA"
  
  output$summarise.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          .IGoR$select.ui("summarise", buttons.title=.IGoR$s2(.IGoR$Z$summarise$summarise)),
          box(width='100%', collapsible=TRUE,
            column(width=6, .IGoR$group.ui(input,"summarise", box=FALSE)),
            column(width=6, selectizeInput("summarise.W", .IGoR$s3(.IGoR$Z$any$weight), .numeric(input)))
        ) ),
        column(width=6,
          .IGoR$load.ui("summarise"),
          box(width='100%',
            fluidRow(
              column(width=6,
                selectizeInput("summarise.funs", .IGoR$s1(.IGoR$Z$summarise$funs),
                           multiple=TRUE, options=list(placeholder = .IGoR$Z$any$funs),
                           choices=statsv)),
              column(width=6, uiOutput("summarise.value"))
            ),
            fluidRow(
              column(width=6, checkboxInput("summarise.names",.IGoR$s5(.IGoR$Z$summarise$names),TRUE)),
              column(width=6, checkboxInput("summarise.na.rm",.IGoR$s5(.IGoR$Z$any$na.rm),TRUE))
      ) ) ) )
  )

  output$summarise.value <- renderUI(
    if ("v" %in% input$summarise.funs) .IGoR$label.ui("summarise","value",'', title=.IGoR$Z$summarise$value, suffix='')
  )
  
  output$summarise.command2 <- renderUI(
    .IGoR$textarea("summarise", "summarise...(...)", 3,
      if ((length(input$main.data)>0)&&(length(input$summarise.funs)>0)) {
        na <- statna(input$summarise.funs,input$summarise.W,input$summarise.na.rm)
        f1 <- stat(input$summarise.funs,input$summarise.W,na)
        nm <- (if (.isTRUE(input$summarise.names))
                    names(statsv)[Vectorize(function (x) which(x==statsv))(input$summarise.funs)] %>%
                      ifelse(.=="Comptage",paste0("Comptage",suffix()),.)
               else input$summarise.funs %>%
                      ifelse(.=="v",paste0("v",suffix()),.)) %>%
                      make.names()
        fn <- .collapse(
                if (.isTRUE(input$summarise.names))
                     paste0('\"',nm,'\"=',f1)
                else ifelse(str_detect(f1,"[()]"),paste0('\"',nm,'\"=',f1),f1))
        if ((length(f1)>1)||.isTRUE(input$summarise.names)||str_detect(f1,"[()]")) fn <- glue("funs({fn})")
        .IGoR$command2(
          .IGoR$group_by(input,"summarise"),
          "summarise",
          if (input$summarise.type==2)
            if (input$summarise.drop)
                 glue("_if(Negate(is.{input$summarise.class}), ")
            else glue("_if(is.{input$summarise.class}, ")
          else
          if (input$summarise.type==3) 
            if (input$summarise.drop) 
                 "_at(c(), "
            else "_all("
          else glue("_at({.IGoR$select(input,'summarise',vars=TRUE)}, "),
          fn,
          if ((length(na)==1)&&(nchar(na)>0)) paste0(', ',na),
          ")",
          .IGoR$ungroup(input,"summarise",1)
        )
      }
  ) )
  
  observeEvent(input$summarise.command2, 
    .IGoR$try(input,output,"summarise",
      .fn=function(x) isolate({
        d <- get(input$main.data,envir=.GlobalEnv)[,.IGoR$select.columns(input,output,"summarise")]
        l <- setdiff(colnames(x),input$summarise.group)
        n <- length(l)/length(input$summarise.funs)
        paste(
         if (("character" %in% Map(class,d))&&("mean" %in% input$summarise.funs)) .IGoR$Z$summarise$msg.error1,
         sprintf(.IGoR$Z$summarise$msg.result,n),
         sep='\n'
        )
      })
  ))
  
}

