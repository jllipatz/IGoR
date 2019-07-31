### TODO Offrir la possibilité de convertir le résultat en data.frame
### TODO Protéger contre les modalités de facteur sous forme de chaine vide 'attempt to use zero-length variable name'

### 03/06/2019 1.01.0: Formattage des pourcentages
### 14/06/2019 1.01.1: Justification des comptages avec séparateurs des milliers

.IGoR$page$tabular$ui <- function()
  div(id = "bloc_tabular",
    fluidRow(
      column(width=4, 
        img(src="images/tabular.png", height = "46px"),
        h3(span("Tableaux de statistiques multivariées", style="color: blue"))
      ),
      column(width=8, 
        p("A partir de variables qualitatives qui doivent impérativement être du type 'énumération', la fonction ", code("tabular"), " du package ", strong("tables"), 
          " produit des comptages ou des statistiques ventilées selon les modalités croisées de ces variables. ",
          "Les chiffres obtenus sont formattés et mis en page de manière à minimiser le travail nécessaire à leur intégration à un produit de diffusion.",br(),
          "Le résultat n'est pas une table mais un objet d'un type particulier qui peut être exporté sous format HTML pour être relu sous un tableur."
    ) ) ),
    uiOutput("tabular.control"),
    .IGoR$commandBox("tabular"),
    tableOutput("tabular.output")
  )


.IGoR$page$tabular$sv <- function(input, output, session) {
  
  .IGoR$bServer(input,output,"tabular")
  
  wtd.percent <- function (x,y) 100*sum(x)/sum(y)
  
  stat <- function(.i,.w) glue(
    if (nchar(.w)==0) 
      c(count="",
        all="Percent('all')",
        row="Percent('row')",
        col="Percent('col')",
        mean="mean",
        median="median",
        q1="Heading('q1')*quantile*Arguments(p=.25)",
        q3="Heading('q3')*quantile*Arguments(p=.75)",
        p10="Heading('p10')*quantile*Arguments(p=.1)",
        p90="Heading('p90')*quantile*Arguments(p=.9)")[.i]
    else 
      c(count="sum*{.w}",
        all="Percent('all',fn=wtd.percent)*{.w}",
        row="Percent('row',fn=wtd.percent)*{.w}",
        col="Percent('col',fn=wtd.percent)*{.w}",
        mean="Heading('mean')*wtd.mean*Arguments(w={.w})",
        median="Heading('median')*wtd.quantile*Arguments(p=.5,w={.w})",
        q1="Heading('q1')*wtd.quantile*Arguments(p=.25,w={.w})",
        q3="Heading('q3')*wtd.quantile*Arguments(p=.75,w={.w})",
        p10="Heading('p10')*wtd.quantile*Arguments(p=.1,w={.w})",
        p90="Heading('p90')*wtd.quantile*Arguments(p=.9,w={.w})")[.i]
  )

  output$tabular.control <- renderUI(
    if ((length(input$main.data>0))&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            fluidRow(
              column(width=6, selectizeInput("tabular.X", label="Variables qualitatives en colonne", 
                           multiple = TRUE, options = list(placeholder = .IGoR$FCTCOLS),
                           choices = .columns(input$main.data,"factor"))),
              column(width=6, uiOutput("tabular.mixX"))
            ),
            fluidRow(
              column(width=6, selectizeInput("tabular.Y", label="Variables qualitatives en ligne", 
                           multiple = TRUE, options = list(placeholder = .IGoR$FCTCOLS),
                           choices = .columns(input$main.data,"factor"))),
              column(width=6, uiOutput("tabular.mixY"))
            ),
            fluidRow(
              column(width=6, selectizeInput("tabular.W", label="Pondération (optionnelle)", 
                            choices=c(.IGoR$NUMCOLV,.columns(input$main.data,c("numeric","integer"))))
        )))),
        column(width=6,
          box(width='100%',
            column(width=6,radioButtons("tabular.type","Type de tabulation",
                            c("Comptage"="count",
                              "Pourcentage"="all",
                              "Pourcentage ligne"="row",
                              "Pourcentage colonne"="col",
                              "Statistiques sur une autre variable"="var"))),
            column(width=6,uiOutput("tabular.args"))
          ),
          uiOutput("tabular.save.control")
        )
  ))

  output$tabular.mixX <- renderUI(
    if (length(input$tabular.X)>1)
      checkboxInput("tabular.mixX","Croiser les modalités des variables",TRUE)
  )
  
  output$tabular.mixY <- renderUI(
    if (length(input$tabular.Y)>1)
      checkboxInput("tabular.mixY","Croiser les modalités des variables",TRUE)
  )
  
  output$tabular.args <- renderUI(
    if (.isEQ(input$tabular.type,'var')&&(length(input$tabular.W)>0))
      tagList(
        selectizeInput("tabular.Z", label="Statistiques sur",
                  choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric"))),
        selectizeInput("tabular.funZ", label="", 
                  multiple=TRUE, options = list(placeholder = 'Fonctions statistiques'),
                  choices=c("Moyenne"=stat("mean",input$tabular.W),
                            "Médiane"=stat("median",input$tabular.W),
                   "Premier quartile"=stat("q1",input$tabular.W),
                   "Dernier quartile"=stat("q3",input$tabular.W),
                     "Premier décile"=stat("p10",input$tabular.W),
                     "Dernier décile"=stat("p90",input$tabular.W),
                            "Minimum"="min",
                            "Maximum"="max"))
        )
    else
    if (.isIn(input$tabular.type,c("col","row","all")))
      radioButtons("tabular.digits","Décimales :",choices=c("deux"=2,"une"=1,"aucune"=0))
    else
    if (.isEQ(input$tabular.type,"count"))
      checkboxInput("tabular.sep","Séparer les milliers",FALSE)
  )
  
  output$tabular.save.control <- renderUI(if (.isNotEmpty(input$tabular.X)) .IGoR$save.ui("tabular",.title=.IGoR$TSAVE0))
  
  output$tabular.save <- renderUI(
    if (.isTRUE(input$tabular.save))
      shinySaveButton("tabular", .IGoR$BROWSE, "Sauvegarder le tableau sous :", filetype=list(html="html"))
  )
  
  shinyFileSave(input, "tabular", roots=.IGoR$volumes, defaultPath='', defaultRoot='home')

  output$tabular.command2 <- renderUI(
    .IGoR$textarea("tabular", "tabular(...)", 2, 
      if ((length(input$tabular.X)>0)
        ||(length(input$tabular.Y)>0)
        ||(.isNotEmpty(input$tabular.Z)&&(length(input$tabular.funZ)>0))){
        z <- if (input$tabular.type=='var')
               if (.isNotEmpty(input$tabular.Z)&&(length(input$tabular.funZ)>0))
                 paste0(input$tabular.Z,'*',
                        if (length(input$tabular.funZ)>1)
                             paste0('(',paste(input$tabular.funZ, collapse='+'),')')
                        else input$tabular.funZ
                 )
               else ""
             else 
              stat(input$tabular.type,input$tabular.W)
        if ((input$tabular.type %in% c('col','row','all'))&&(length(input$tabular.digits)>0))
          z <- paste0(z,glue("*Format(partial(sprintf,\"%.{input$tabular.digits}f\")())*Justify(r)"))
        y <- if (length(input$tabular.Y)==0) ""
             else paste0(
               if (length(input$tabular.Y)>1)
                 paste(input$tabular.Y,
                       collapse=if (!.isTRUE(input$tabular.mixY)) '+' else '*')
               else input$tabular.Y,
               '+1')
        x <- if (length(input$tabular.X)==0) ""
             else paste0(
               if (length(input$tabular.X)>1)
                 paste(input$tabular.X,
                       collapse=if (!.isTRUE(input$tabular.mixX)) '+' else '*')
               else input$tabular.X,
               '+1')
        if ((nchar(z)==0)&&(nchar(x)==0)) z <- "1"
        else if ((nchar(z)>0)&&(nchar(x)>0)) x <- glue("*({x})")
        x <- paste0(z,x)
        if ((input$tabular.type=='count')&&.isTRUE(input$tabular.sep))
          x <- paste0(if (x=="1") x else glue("({x})"),"*Format(partial(format,big.mark=' ')())*Justify(r)")
        .IGoR$command2(
          glue("tabular({y} ~ {x}, . )"),
          if (.isTRUE(input$tabular.save)) {
            f  <- parseSavePath(.IGoR$volumes,input$tabular)$datapath
            if (.isNotEmpty(f)) paste0(NL,"{",glue("Hmisc::html(.,\"{f}\")"),";.}")
          }
        )
      }
) )
                  
observeEvent(input$tabular.command2,
    isolate(
      output$tabular.output <- renderPrint(
        if (nchar(input$tabular.command2)>0) {
          x <- tryCatch(eval(parse(text=paste0(input$main.data,' %>% ',input$tabular.command2))),
                        error=function(e) {
                          output$tabular.comment <- renderText(e$message)
                          NULL
                })
          if (!is.null(x)) {
           output$tabular.comment <- renderText("")
           html(x,options=htmloptions(pad=TRUE, HTMLleftpad=TRUE))
          }
})))
  
}