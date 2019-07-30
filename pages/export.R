### 18/06/2019 1.01.1: Ajout du type 'feather'
### 19/07/2019 1.03.0: Ajout du type 'json'
### 27/07/2019 1.02.0: Ajout du paramètre 'compress' pour le type 'fst'


.IGoR$page$export$ui <- function()
  div(id = "bloc_export",
    fluidRow(
      column(width=4, 
        img(src="images/export.png", height = "46px"),
        h3(span("Ecriture de tables dans un fichier", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction", code("export"), " du package ", strong("rio")," fournit une interface unifiée pour le transfert dans un fichier, de données présentes en mémoire.",
          "Elle reconnaît de nombreux types de fichiers : ", em("Excel"),
          ", mais aussi des formats universels : ", em("CSV"), ", ", em("DBF"), ", ", em("json"), 
          ", ou des formats spécifiques à R :", em("fst"), ", ", em("feather"),". ",
          "Elle permet de sauvegarder des tables dans les formats ", em("RData"), " et ", em("RDS"), 
          " qui garantissent qu'une relecture ultérieure reconstituera les données dans leur état exact du moment de la sauvegarde.",br(),
          "Selon le type du fichier, des paramètres de contrôle complémentaires pourront être précisés.",br(),
          "Les formats ", em("Excel"), " et ", em("RData"), "permettent de transférer plusieurs tables dans un même fichier."
    ) ) ),
    fluidRow(
      column(width=6,
        box(width='100%',
          radioButtons("export.type","",
                       c("Exporter la table courante"=1,
                         "Exporter une liste de tables (Excel xlsx)"=2,
                         "Sauvegarder une liste de tables (RData)"=3)),
          uiOutput("export.control")
        ),
        uiOutput("export.parms")
      ),
      column(width=6,
        box(width='100%',
          fluidRow(
            column(width=8, uiOutput("export.file")),
            column(width=4,
              uiOutput("export.files"),
              uiOutput("export.load"))
        ) )
    ) ),
    .IGoR$commandBox("export") 
)


.IGoR$page$export$sv <- function(input, output, session) {

  .IGoR$rLogo(input,output,"export")
  
  output$export.control <- renderUI(
    if (input$export.type>1)
     tagList(
       selectizeInput("export.tables",
                     if (input$export.type==2) "Tables à exporter" else "Tables à sauvegarder",
                     multiple=TRUE,  options = list(placeholder = '<toutes>'),
                     choices=.IGoR$tables),
       if (input$export.type==2) checkboxInput("export.names","Préciser le nom des feuilles",FALSE)
  ))
    
  output$export.files <- renderUI(
    if (length(input$main.data)>0)
      if (input$export.type==1)
        shinySaveButton("export", .IGoR$BROWSE, "Exporter sous :",
          filename=input$main.data,
          filetype=list("R Data Serialization"="RDS",
                          "Fast serialization"="fst",
                                     "feather"="feather",
                              "Excel Open XML"="xlsx",
                                       "DBase"="dbf",
                      "Comma Separated Values"="csv",
                                        "JSon"="json"))
      else
      if (input$export.type==2)
        shinySaveButton("export", .IGoR$BROWSE, "Exporter sous :", filetype=list("Excel Open XML"="xlsx"))
      else
      if (input$export.type==3)
        shinySaveButton("export", .IGoR$BROWSE, "Sauvegarder sous :", filetype=list("R Data"="RData"))
  )
  
  output$export.parms <- renderUI(
    if (.isFile(input$export.file)) 
      box(width='100%',
        switch(get_ext(input$export.file),
          "fst" = sliderInput("export.fst.compress", "Niveau de compression", 0, 100, 50)
  )   ) )
  
  observe({
    volumes <- .IGoR$volumes
    shinyFileSave(input, "export", roots = volumes, defaultPath='', defaultRoot='home')
    fileinfo <- parseSavePath(volumes, input$export)
  
    output$export.file <- renderUI(
      textInput("export.file","Chemin d'accès au fichier :",fileinfo$datapath))
  })
  
  output$export.command1 <- renderText(
    if (length(input$main.data)>0)
      .IGoR$export.command1 <<-
        if (input$export.type==1)
          glue("{input$main.data} %>%")
        else {
          l <- if (length(input$export.tables)==0) .IGoR$tables else input$export.tables
          glue("c({collapse1(l)}) %>%")
        }
  )
  
  output$export.command2 <- renderUI(
    .IGoR$textarea("export", "export(path,parms)", 3,
      if ((length(input$export.type)>0)&&.isNotEmpty(input$export.file)) 
        .IGoR$command2( 
          if (input$export.type==1)   # - save using rio -----------------------------------------------------------
            paste0(
              glue("export(file=\"{input$export.file}\""),
              switch(get_ext(input$export.file),
                "fst" = if (.isNE(input$export.fst.compress,50)) glue(", compress={input$export.fst.compress}")
              ),
              ")"
            )
          else
          if (input$export.type==2) { # - save as Excel sheets -----------------------------------------------------
            l <- if (length(input$export.tables)==0) .IGoR$tables else input$export.tables
            # When generated code is manually changed, an invalid file type cause a strange message:
            # 'x' is not a data.frame or matrix
            t <- if (!str_detect(input$export.file,"\\.xlsx$")) ".xlsx" else ""
            paste0(
              "Map(get,.)",NL,       # -- make sheet names explicit so user change them
              if (.isTRUE(input$export.names)) paste0("{",glue("names(.)<- c({.collapse1(l)})"),"; .}",NL),
              glue("export(file=\"{input$export.file}{t}\")")
            )
          }
          else
          if (input$export.type==3) { # - save as RData ------------------------------------------------------------
            t <- if (!str_detect(input$export.file,"\\.RData$")) ".RData" else ""
            glue("save(list=., file=\"{input$export.file}{t}\")")
          }
  )   ) )
  
  observeEvent(input$export.command2, {
    if (.isNotEmpty(input$export.file))
      output$export.load <- renderUI(
        actionButton("export.load",
          if (input$export.type==1) "Exporter la table"
          else
          if (input$export.type==2) "Exporter les tables" 
          else                      "Sauvegarder"
      ))
    else output$export.comment <- renderText("")
  })             
  
  observeEvent(input$export.load,
    isolate({
      .IGoR$do1(input,output,"export",paste0(.IGoR$export.command1,input$export.command2),
        function (x) {
          # Protection contre une modification manuelle du nom de fichier dans la commande
          f <- str_extract(isolate(input$export.command2),"(?<=file=([\"'])).*?(?=\\1)")
          sprintf("NOTE : La taille du fichier '%s' est de %d octets.", f, file.size(f))
        }
      )
      shinyjs::disable("export.load")
  }))
  
}
      