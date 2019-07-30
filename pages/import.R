###
### Les fondamentaux : 
###       dictionnaire
###       %>%
###       fonctions

### 18/06/2019 1.01.1: Ajout du type 'feather'
### 19/06/2019 1.01.1: Ajout du parametre 'from' pour le type 'fst'
### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 12/07/2019 1.02.0: Ajout du parametre 'encoding' pour le type 'csv'
### 19/07/2019 1.03.0: Ajout du type 'json'

.IGoR$page$import$ui <- function()
  div(id = "bloc_import",
    fluidRow(
      column(width=4, 
        img(src="images/import.png", height = "46px"),
        h3(span("Lecture d'une table depuis un fichier", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction", code("import"), " du package ", strong("rio")," fournit une interface unifiée pour le transfert en mémoire de données présentes dans un fichier.",
          "Elle reconnaît de nombreux types de fichiers : ",em("Excel"), ", ", em("Calc"), ", ", em("SAS"),
          ", mais aussi des formats universels : ", em("CSV"), ", ", em("DBF"), ", ", em("json"), 
          ", ou des formats spécifiques à R :", em("RData"), ", ", em("RDS"), ", ", em("fst"), ", ", em("feather"),".",br(),
          "Selon le type du fichier, des paramètres de contrôle complémentaires pourront être précisés.",br(),
          "Le résultat deviendra la table courante, sauf si une table de même nom éxistait déjà."
    ) ) ),
    fluidRow(
     column(width=6,
        box(width='100%',
          column(width=9, uiOutput("import.file")),
          column(width=3, shinyFilesButton("import", label='Parcourir...', title='Sélectionnez un fichier', multiple=FALSE))
      )),
      column(width=6, uiOutput("import.out"))
    ),
    uiOutput("import.parms"),
    .IGoR$commandBox("import")
  )

  
.IGoR$import.command1 <- "{make.names(input$import.out)} <-"

.IGoR$page$import$sv <- function(input, output, session) {
  
  .IGoR$rLogo(input,output,"import") 

  expr <- function(.data, fst=TRUE) {
    e <- tryCatch(parse(text=input$import.expr),error=identity)
    if (is(e,"condition")) "*** ERREUR ***"
    else {
      n <- if (fst) metadata_fst(input$import.file)$columnNames
           else attr(feather_metadata(input$import.file)$types,'names')
      l <- as.list(parse(text=paste0(.data,"$",n)))
      names(l)<- n
      paste(deparse(do.call(substitute,list(e[[1]],l)),width.cutoff = 130L),
            collapse='\n')
  }}
  
  observe({
    shinyFileChoose(input, "import",
                    roots = .IGoR$volumes, defaultPath='', defaultRoot='wd',
                    filetypes=c('csv',
                                "Excel open XML"='xlsx','xls','ods','dbf','json',
                                "SAS"='sas7bdat',
                                'fst','feather',
                                'funcamp',
                                'rds','rData'))
    fileinfo <- parseFilePaths(.IGoR$volumes, input$import)
    
    output$import.file <- renderUI(
      textInput("import.file","Chemin d'accès au fichier :",fileinfo$datapath))
  })

  output$import.out <- renderUI(
    if (.isFile(input$import.file)) 
    box(width='100%',
      column(width=8, {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if (type=="rdata") {
          attach(input$import.file)
          l <- ls(pos=2)
          detach(2)
          selectizeInput("import.out","Restaurer la table :",choices=l)
        }
        else  
          textInput("import.out", label=.IGoR$OUT,
                    str_extract(input$import.file,"(?<=/)[^/]*(?=\\.[^.]+$)"))
      }),
      column(width=4, uiOutput("import.load"))
    ))
  
  output$import.parms <- renderUI(
    if (.isFile(input$import.file)) 
      box(width='100%', {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if (type=="csv")
          fluidRow(
            column(width=4,
                   selectizeInput("import.encoding",label = "Encodage :", choices=c("","UTF-8"))
          ) )
        else 
        if (type=="dbf")
          checkboxInput("import.dbf","Ne pas convertir les chaînes de caractères en facteurs",TRUE)
        else 
        if (type %in% c("xls","xlsx"))
          fluidRow(
            column(width=6, numericInput("import.xls.sheet","Feuille :", 1)),
            column(width=4, radioButtons("import.xls.type","",
                                        c("Sauter les premières lignes..."=1,
                                          "En tête format insee.fr (5 lignes)"=2)),
                            uiOutput("import.xls.names")),
            column(width=2, uiOutput("import.xls.skip"))
         )
        else
        if (type=="ods")
          fluidRow(
            column(width=6, numericInput("import.xls.sheet","Feuille :", 1)),
            column(width=4, checkboxInput("import.xls.names","Importer le nom des colonnes", TRUE)),
            column(width=2, numericInput("import.xls.skip","Sauter les premières lignes :", NA))
          )
        else
        if (type=="fst")
          fluidRow(
            column(width=6,
               selectizeInput("import.columns",label = "Variables à conserver :",
                             multiple = TRUE, options = list(placeholder = '<toutes>'),
                             choices = metadata_fst(input$import.file)$columnNames)
            ),
            column(width=6,
              uiOutput("import.fst.parms"),
              radioButtons("import.fst.filter","",
                           c("Tout lire"=1,
                             "Lire une plage de lignes"=2,
                             "Filtrer les lignes sur une condition"=3))
          ) )
        else
        if (type=="feather")
          fluidRow(
            column(width=6,
              selectizeInput("import.columns",label = "Variables à conserver :",
                             multiple = TRUE, options = list(placeholder = '<toutes>'),
                             choices = attr(feather_metadata(input$import.file)$types,'names'))
            ),
            column(width=6,
               checkboxInput("import.feather.filter","Filtrer les lignes sur une condition",FALSE),
               uiOutput("import.feather.parms")
            ) )
		    else
		    if (type=="sas7bdat")
		      fluidRow(
		        column(width=4,
		          selectizeInput("import.encoding",label = "Encodage :", choices=c("","UTF-8"))
		      ) )
        else
        if (type=="funcamp") # --- This is a non standard extension for funcamp use only!
          fluidRow(          # funcamp files are just fst files with another file extension
            column(width=4,  # to disable any access not using the 'id' variable as record key
              textInput("import.funcamp.id",label = "Clé :",0)
          ) )
  }))
  
  output$import.xls.names <- renderUI(
    if (.isEQ(input$import.xls.type,1))
      checkboxInput("import.xls.names","Importer le nom des colonnes", TRUE)
  )
  
  output$import.xls.skip <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if ((type %in% c("xlsx","xls","ods"))&&.isEQ(input$import.xls.type,1))
        numericInput("import.xls.skip","",NA)
    }
  )
  
  output$import.fst.parms <- renderUI(
    if (length(input$import.fst.filter)>0)
      if (input$import.fst.filter==2) 
        fluidRow(
          column(width=6, numericInput("import.fst.from","Première ligne à conserver :",1)),
          column(width=6, numericInput("import.fst.to","Dernière ligne à conserver :",metadata_fst(input$import.file)$nrOfRows))
        )
      else
      if (input$import.fst.filter==3)
        textInput("import.expr","Lignes à conserver :")
  )
  
  output$import.feather.parms <- renderUI(
    if (.isTRUE(input$import.feather.filter))
      textInput("import.expr","Lignes à conserver :")
  )
  
  output$import.command1 <- renderText(if (.isFile(input$import.file)) glue(.IGoR$import.command1))
  
  output$import.command2 <- renderUI(
    .IGoR$textarea("import", "import(parms)", 6,
      if (.isFile(input$import.file)) {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if ((type=="fst")&&.isNotEmpty(input$import.expr))            # --- Use 'fst' connection -------------------
          .IGoR$command2(
            glue("fst(\"{input$import.file}\")"),NL,
            glue(".[{expr('.',TRUE)},{.collapse2(input$import.columns)}]"), .IGoR$look(input$import.expr),
            if (length(input$import.fst.columns)==1) # PB fst 0.8.8 drop=FALSE ne marche pas
              paste0(NL,glue("data.frame({input$import.columns}=., stringsAsFactors=FALSE)"))
          )
        else
        if ((type=="funcamp")&&.isNotEmpty(input$import.funcamp.id))
          .IGoR$command2(
            glue("fst(\"{input$import.file}\")"),NL,
            glue(".[.$id=={input$import.funcamp.id},grepl('^[^.]',names(.))]")
          )
        else
        if ((type=="feather")&&.isNotEmpty(input$import.expr))         # --- Use 'feather' connection --------------
          .IGoR$command2(
            glue("feather(\"{input$import.file}\")"),NL,
            glue(".[{expr('.',FALSE)},{.collapse2(input$import.columns)}]")
          )
        else
        if ((type %in% c("xlsx","xls"))&&.isEQ(input$import.xls.type,2)) # --- from insee.fr : get column labels ----
          paste0("   (function (file, sheet=1) {\n",
                 "     df <- import(file,  sheet=sheet, skip=5)\n",
                 "     Map(function(x) attr(df[[x[2]]],'label')<<- x[1],\n",
                 "         import(file, sheet=sheet, skip=4, n_max=2, col_names=FALSE))\n",
                 "     df\n",
                 "   })",
                 glue("(\"{input$import.file}\""),
                 if (.isNE(input$import.xls.sheet,1)) glue(", {input$import.xls.sheet}"),
                 ")"
          )
        else # --- Calls to 'rio::import' --------------------------------------------------------------------------
          .IGoR$command2(
            glue("import(\"{input$import.file}\""),
            switch(type,
              csv      = 
                if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
              dbf      = glue(", as.is={.isTRUE(input$import.dbf)}"), # PB rio 0.5.16 default should be TRUE
              sas7bdat = 
				        if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
              xls      =,
              xlsx     = 
                paste0(
                  if (.isNE(input$import.xls.sheet,1)) glue(", sheet={input$import.xls.sheet}"),
                  if (.isFALSE(input$import.xls.names)) ", col_names=FALSE",
                  if (.isNotNA(input$import.xls.skip)) glue(", skip={input$import.xls.skip}")
                ),
              ods      =
                paste0(
                  if (.isNE(input$import.xls.sheet,1)) glue(", sheet={input$import.xls.sheet}"),
                  if (.isFALSE(input$import.xls.names)) ", header=FALSE", # PB rio 0.5.16 col_names ne marche pas
                  if (.isNotNA(input$import.xls.skip)) glue(", skip={input$import.xls.skip}")
                ),
              fst      = 
                paste0(
                  if (length(input$import.columns)>0) glue(", columns={.collapse2(input$import.columns)}"),
                  if (.isEQ(input$import.fst.filter,2)&&(length(input$import.fst.from)>0)) glue(", from={as.character(input$import.fst.from)}"),
                  if (.isEQ(input$import.fst.filter,2)&&(length(input$import.fst.to)>0))   glue(", to={as.character(input$import.fst.to)}")
                ),
				      feather  = 
				        if (length(input$import.columns)>0) glue(", columns={.collapse2(input$import.columns)}"),
				      json     = "",
				      rds      = "",
              rdata    = glue(", which=\"{input$import.out}\"")),
            ")"
          )
        } 
  )   )
 
  observeEvent({input$import.command2;input$import.out},
    if (nchar(input$import.command2)>0)
      output$import.load <- renderUI(actionButton("import.load",.IGoR$buttonName(input,"import")))
    else {
      output$import.comment <- renderText("")
      output$import.preview <- renderText("")
    }
  )
  
  observeEvent(input$import.load, 
    .IGoR$do(input,output,"import",
             paste0(glue(.IGoR$import.command1),' ',input$import.command2)
  ))
  
  
}