
### 06/06/2019 v1.01.0: Harmonisation de la selection de variables
### 15/07/2019 v1.02.1: Corrections (na.rm, noms en clairs), ajout des dénombrements
### 31/07/2019 v1.03.1: AJout du comptage d'une modalité

.IGoR$page$summarise$ui <- function()
  div(id = "bloc_summarise",
    fluidRow(
      column(width=4, 
        img(src="images/summarise.png", height = "48px"),
        h3(span("Récapituler des variables quantitatives", style="color: blue"))
      ),
      column(width=8, 
        p("Les fonctions de la famille ", code("summarise"), " du package ", strong("dplyr"),
          " prennent un ensemble d'observations et construisent une statistique synthétique ou un ensemble de statistiques synthétiques, ",
          "sur une variable ou une famille de variables quantitatives.", br(),
          "Le résultat est une table comportant une seule ligne, sauf si on traite la table courante par groupe d'observations, ",
          "auquel cas le résultat aura autant de lignes que de groupes."
     ) ) ),
    uiOutput("summarise.control"),
    .IGoR$commandBox("summarise")
  )


.IGoR$page$summarise$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"summarise")
  
  statsv <- c("Nombre"="n","Comptage"="v",.IGoR$STATSV)
  
  stat <- function (i,w,has.na,na.rm,wtd) {
    value <- 
      if (.isNotEmpty(input$summarise.value)) 
         if (input$summarise.value=="TRUE") "."
         else 
         if (input$summarise.value=="FALSE") "!."
         else
           if (w=='') paste0(".==",input$summarise.value)
           else      paste0("(.==",input$summarise.value,")")
       else "is.na(.)"
    na0 <- if (wtd)
         if (na.rm) "" else ", na.rm=FALSE"
    else if (na.rm) ", na.rm=TRUE" else ""
    na1 <- if (has.na) na0 else ""
    na2 <- if (has.na) glue("(.{na0})") else ""
    if (w=='')
      c(n="{n()}",
        v=glue("sum({value})"),
        sum=glue("sum{na2}"),
        mean=glue("mean{na2}"),
        median=glue("median{na2}"),
        q1="quantile(.,p=.25{na1})",
        q3="quantile(.,p=.75{na1})",
        p10="quantile(.,p=.10{na1})",
        p90="quantile(.,p=.90{na1})",
        sd=glue("sd{na2}"),
        var=glue("var{na2}"),
        min=glue("min{na2}"),
        max=glue("max{na2}"),
        first="first",
        last="last")[i]
    else 
      c(n=glue("sum({w})"),
        v=glue("sum({w}*{value})"),
        sum=glue("sum({w})*wtd.mean(.,w={w}{na1})"),
        mean=glue("wtd.mean(.,w={w}{na1})"),
        median=glue("wtd.quantile(.,p=.5,w={w}{na1})"),
        q1=glue("wtd.quantile(.,p=.25,w={w}{na1})"),
        q3=glue("wtd.quantile(.,p=.75,w={w}{na1})"),
        p10=glue("wtd.quantile(.,p=.10,w={w}{na1})"),
        p90=glue("wtd.quantile(.,p=.90,w={w}{na1})"),
        sd=glue("sqrt(wtd.var(.,w={w}{na1}))"),
        var=glue("wtd.var(.,w={w}{na1})"),
        min="min{na2}",
        max="max{na2}",
        first="first",
        last="last")[i]
  }
  
  suffix <- function() if (.isNotEmpty(input$summarise.value)) paste0("_",input$summarise.value) else "_NA"
  
  output$summarise.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          .IGoR$select.ui("summarise", "Cumuler les variables..."),
          box(width='100%', collapsible=TRUE,
            column(width=6, selectizeInput("summarise.group", label=.IGoR$GROUPS,
                                           multiple = TRUE, options = list(placeholder = .IGoR$DISCOLS),
                                           choices = .columns(input$main.data,"discrete"))),
            column(width=6, selectizeInput("summarise.W", label=.IGoR$WEIGHT, 
                                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric"))))
        ) ),
        column(width=6,
          .IGoR$loadBox("summarise","summarise.out"),
          box(width='100%', title="Calculer :",
            fluidRow(
              column(width=6,
                selectizeInput("summarise.funs", label=NULL,
                           multiple=TRUE, options = list(placeholder = .IGoR$STATS),
                           choices=statsv)),
              column(width=6, uiOutput("summarise.value"))
            ),
            fluidRow(
              column(width=6, checkboxInput("summarise.names","Intitulés en clair",TRUE)),
              column(width=6, checkboxInput("summarise.na.rm","Ignorer les valeurs manquantes",TRUE))
      ) ) ) )
  )

  output$summarise.value <- renderUI(
    if ("v" %in% input$summarise.funs)
      tagList(
        tags$head(
          tags$style(type="text/css",
                    "#summarise_value label{ display: table-cell; text-align: center; vertical-align: middle; } 
                     #summarise_value .form-group { display: table-row;}")
          ),
        tags$div(id = "summarise_value", textInput("summarise.value","Valeur",""))
  )   )
  
  output$summarise.command2 <- renderUI(
    .IGoR$textarea("summarise", "summarise...(...)", 3,
      if ((length(input$main.data)>0)&&(length(input$summarise.funs)>0)) {          g <- if (length(input$summarise.group)>0) glue("\n  group_by({paste(input$summarise.group,collapse=',')})") else ""
        na <- if (length(intersect(c("n","v","first","last"),input$summarise.funs))>0) "" ## na.rm n'a pas de sens
          else if (.isNotEmpty(input$summarise.W))
                    if (input$summarise.na.rm) "" else ", na.rm=FALSE"  # Dans HMisc le défaut est à TRUE
               else if (input$summarise.na.rm) ", na.rm=TRUE" else ""   # En R de base le défaut est à FALSE
        f1 <- stat(input$summarise.funs,input$summarise.W,nchar(na)==0,input$summarise.na.rm,.isNotEmpty(input$summarise.W))
        nm <- (if (.isTRUE(input$summarise.names))
                    names(statsv)[Vectorize(function (x) which(x==statsv))(input$summarise.funs)] %>%
                      ifelse(.=="Comptage",paste0("Comptage",suffix()),.)
               else input$summarise.funs %>%
                      ifelse(.=="v",paste0("v",suffix()),.)) %>%
               str_replace_all(" ","_") %>%
               str_replace_all("[^a-zA-Z0-9_]",".")
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
          na,
          ")",
          .IGoR$ungroup(input,"summarise",1)
        )
      }
  ) )
  
  observeEvent(input$summarise.command2, 
    .IGoR$try(input,output,"summarise",
      .fn=function(x) {
        l <- setdiff(colnames(x),isolate(input$summarise.group))
        n <- length(l)/length(isolate(input$summarise.funs))
        paste0("NOTE : ",
          if (n>1)      sprintf("%d variables vont être cumulées.",n)
          else if(n==1)         "Une variable va être cumulée."
          else                  "Aucune variable ne va être cumulée."
        )
      }
  ))
  
}

