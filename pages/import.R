
### 18/06/2019 1.01.1: Ajout du type 'feather'
### 19/06/2019 1.01.1: Ajout du parametre 'from' pour le type 'fst'
### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 12/07/2019 1.02.0: Ajout du parametre 'encoding' pour le type 'csv'
### 19/07/2019 1.03.0: Ajout du type 'json'
### 09/08/2019 1.04.2: Externalisation des libellés en français
### 28/10/2019 1.04.4: Sélection des feuilles des fichiers Excel par leur nom
### 15/11/2019 1.04.5: Séparateur de décimales pour le type 'csv'
### 19/11/2019 1.05.1: Tri des colonnes des fichiers fst et feather
### 24/01/2020 1.05.4: Correction d'un bug de rafraichissement avec fst
### 30/01/2020 1.06.0: Rétablissement du contournement pour les extractions fst d'une seule colonne
### 20/02/2020 1.06.2: Ajout du type 'rda'
### 27/02/2020 1.06.3: Ajout de paramètre à la lecture des CSV
### 16/06/2020 1.07.1: Ajout de paramètres à la lecture des CSV; Gestion des noms de fichiers manuels
### 19/06/2020 1.07.2: Ajout de paramètres à la lecture des SAS7BDAT
### 03/08/2020 1.10.0: Protection contre les noms de colonnes non normalisés
### 30/09/2020 1.10.5: Correction de bug dans la lecture de fichiers FST et FEATHER
### 09/03/2021 1.12.0: Ajout de paramètres à la lecture des ODS; ajout de la lecture d'un classeur entier
###                    NB: list_ods_sheet n'existe qu'en 1.7, mais ods_sheet est marqué comme deprecated.


.IGoR$page$import$ui <- function()
  .IGoR$ui(page="import",
    fluidRow(
     column(width=6,
        box(width='100%',
          column(width=9, uiOutput("import.file")),
          column(width=3, shinyFilesButton("import", label=.IGoR$Z$any$browse, title=.IGoR$Z$import$choose, multiple=FALSE))
      )),
      column(width=6, uiOutput("import.out"))
    ),
    uiOutput("import.parms")
  )


.IGoR$import.command1 <- "{make.names(input$import.out)} <-"

.IGoR$page$import$sv <- function(input, output, session) {

  .IGoR$rLogo(input,output,"import")

  expr <- function(.data, fst=TRUE) {
    x <- if (fst) input$import.fst.expr else input$import.feather.expr
    e <- tryCatch(parse(text=x),error=identity)
    if (is(e,"condition")) "*** ERREUR ***"
    else {
      n <- if (fst) metadata_fst(input$import.file)$columnNames
           else attr(feather_metadata(input$import.file)$types,'names')
      l <- as.list(parse(text=paste0(.data,"$",.name(n))))
      names(l)<- n
      paste(deparse(do.call(substitute,list(e[[1]],l)),width.cutoff = 130L),
            collapse='\n')
  }}

  observe({
    shinyFileChoose(input, "import",
                    roots = .IGoR$volumes, defaultPath='', defaultRoot='wd',
                    filetypes=c('csv',
                    "Excel open XML"='xlsx','xls','dbf','json',
                              "Calc"="ods",
                               "SAS"='sas7bdat',
                                'fst','feather',
                                'funcamp',
                                'rds','rData','rda'))
    fileinfo <- parseFilePaths(.IGoR$volumes, input$import)

    output$import.file <- renderUI(
      textInput("import.file",.IGoR$s1(.IGoR$Z$any$path),fileinfo$datapath))
  })

  output$import.out <- renderUI(
    if (.isFile(input$import.file))
    box(width='100%',
      column(width=8, {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if (is.na(type)) NULL  # Occurs at dot when typing in the file name
        else
        if (type=="rdata") {
          attach(input$import.file)
          l <- ls(pos=2)
          detach(2)
          selectizeInput("import.out",.IGoR$s2(.IGoR$Z$import$rdata.load),choices=l)
        }
        else
          textInput("import.out", label=.IGoR$s2(.IGoR$Z$any$out),
                    str_extract(input$import.file,"(?<=/)[^/]*(?=\\.[^.]+$)"))
      }),
      column(width=4, uiOutput("import.load"))
    ))

  encoding.ui <- function () selectizeInput("import.encoding",.IGoR$s2(.IGoR$Z$import$encoding), choices=c("","UTF-8"))

  output$import.parms <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if (!is.na(type)) # Occurs at dot when typing in the file name
        box(width='100%',
            if (type=="sas7bdat")
              if (packageVersion("haven")>="2.2.0")
                fluidRow(                
                  column(width=6,
                         selectizeInput("import.sas.columns", .IGoR$s2(.IGoR$Z$import$vars),
                                        multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                                        choices = sort(colnames(haven::read_sas(input$import.file,n_max=0))))),
                  column(width=3, numericInput("import.sas.nrows",.IGoR$s2(.IGoR$Z$import$nrows),Inf)),
                  column(width=3, encoding.ui())
                )
              else
                fluidRow(
                  column(width=9),
                  column(width=3, encoding.ui())
                )
          else
          if (type=="csv")
            list(
              fluidRow(
                column(width=6,
                       selectizeInput("import.csv.columns", .IGoR$s2(.IGoR$Z$import$vars),
                                      multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                                      choices = sort(colnames(data.table::fread(input$import.file,nrows=0))))),
                column(width=3, numericInput("import.csv.nrows",.IGoR$s2(.IGoR$Z$import$nrows),Inf)),
                column(width=3, encoding.ui())
              ),
              fluidRow(
                column(width=3, checkboxInput("import.csv.chars",.IGoR$s5(.IGoR$Z$import$csv.chars),FALSE)),
                column(width=3, uiOutput("import.csv.dec")),
                column(width=6)
              ))
          else
          if (type=="dbf")
            checkboxInput("import.dbf",.IGoR$s5(.IGoR$Z$any$stringsAsFactors),FALSE)
          else
          if (type %in% c("xls","xlsx"))
            fluidRow(
              column(width=6, selectizeInput("import.xls.sheet",.IGoR$s2(.IGoR$Z$import$xls.sheet),
                                             readxl::excel_sheets(input$import.file))),
              column(width=4, radioButtons("import.xls.type","",.IGoR$Znames("import","xls.type",c("skip","insee"))),
                              uiOutput("import.xls.names")),
              column(width=2, uiOutput("import.xls.skip"))
            )
          else
          if (type=="ods")
            fluidRow(
              column(width=2, radioButtons("import.ods.type",.IGoR$Z$import$ods.type, .IGoR$Znames("import","ods.type",c("one","all")))),
              column(width=3, uiOutput("import.ods.sheet")),
              column(width=2, checkboxInput("import.ods.names",.IGoR$s5(.IGoR$Z$import$header), TRUE)),
              column(width=2, radioButtons("import.ods.filter",.IGoR$Z$import$ods.filter, .IGoR$Znames("import","ods.filter",c("skip","range")))),
              column(width=3, uiOutput("import.ods.parms"))
            )
          else
          if (type=="fst")
            fluidRow(
              column(width=6,
                 selectizeInput("import.fst.columns", .IGoR$s2(.IGoR$Z$import$vars),
                               multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                               choices = sort(metadata_fst(input$import.file)$columnNames))
              ),
              column(width=6,
                uiOutput("import.fst.parms"),
                radioButtons("import.fst.filter",.IGoR$Z$import$fst.filter, .IGoR$Znames("import","fst.filter",c("all","range","where")), inline=TRUE)
            ) )
          else
          if (type=="feather")
            fluidRow(
              column(width=6,
                selectizeInput("import.feather.columns", .IGoR$s2(.IGoR$Z$import$vars),
                               multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                               choices = sort(attr(feather_metadata(input$import.file)$types,'names')))
              ),
              column(width=6,
                 uiOutput("import.feather.parms"),
                 checkboxInput("import.feather.filter",.IGoR$s3(.IGoR$Z$import$feather.where),FALSE)
              ) )
		      else
          if (type=="funcamp") # --- This is a non standard extension for funcamp use only!
            fluidRow(          # funcamp files are just fst files with another file extension
              column(width=4,  # to disable any access not using the 'id' variable as record key
                textInput("import.funcamp.id",.IGoR$s2(.IGoR$Z$import$funcamp.key),0)
            ) )
    )})
  
  output$import.csv.dec <- renderUI(
    if (.isFALSE(input$import.csv.chars))
      checkboxInput("import.csv.dec",.IGoR$s5(.IGoR$Z$import$csv.dec),FALSE)
  )

  output$import.xls.names <- renderUI(
    if (.isEQ(input$import.xls.type,"skip"))
      checkboxInput("import.xls.names",.IGoR$s5(.IGoR$Z$import$header), TRUE)
  )

  output$import.xls.skip <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if ((type %in% c("xlsx","xls"))&&.isEQ(input$import.xls.type,"skip"))
        numericInput("import.xls.skip","",NA)
    }
  )
  
  output$import.ods.sheet <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if (type=="ods")
        if (.isEQ(input$import.ods.type,"one"))
          selectizeInput("import.ods.sheet","",readODS::ods_sheets(input$import.file))
        else
        if (.isEQ(input$import.ods.type,"all"))
          list(
            checkboxInput("import.ods.all.skip",.IGoR$s5(.IGoR$Z$import$ods.all.skip), FALSE),
            textInput("import.ods.all.name",.IGoR$s5(.IGoR$Z$import$ods.all.name),"import.sheet")
          )
    }
  )
  
  output$import.ods.parms <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if (type=="ods")
        if (.isEQ(input$import.ods.filter,"skip"))
          numericInput("import.ods.skip","",NA)
        else
        if (.isEQ(input$import.ods.filter,"range"))
          textInput("import.ods.range","","")
    }
  )

  output$import.fst.parms <- renderUI(
    if (length(input$import.fst.filter)>0)
      if (input$import.fst.filter=="range")
        fluidRow(
          column(width=6, numericInput("import.fst.from",.IGoR$s2(.IGoR$Z$import$fst.from),1)),
          column(width=6, numericInput("import.fst.to",  .IGoR$s2(.IGoR$Z$import$fst.to),metadata_fst(input$import.file)$nrOfRows))
        )
      else
      if (input$import.fst.filter=="where")
        textInput("import.fst.expr",.IGoR$s2(.IGoR$Z$import$expr))
  )

  output$import.feather.parms <- renderUI(
    if (.isTRUE(input$import.feather.filter))
      textInput("import.feather.expr",.IGoR$s3(.IGoR$Z$import$expr))
  )

  output$import.command1 <- renderText(if (.isFile(input$import.file)) glue(.IGoR$import.command1))

  output$import.command2 <- renderUI(
    .IGoR$textarea("import", "import(parms)", 6,
      if (.isFile(input$import.file)) {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if (is.na(type)) NULL  # Occurs at dot when typing in the file name
        else
        if ((type=="funcamp")&&.isNotEmpty(input$import.funcamp.id))
          .IGoR$command2(
            glue("fst(\"{input$import.file}\")"),NL,
            glue(".[.$id=={input$import.funcamp.id},grepl('^[^.]',names(.))]")
          )
        else
        if ((type=="fst")&&.isEQ(input$import.fst.filter,'where')&&.isNotEmpty(input$import.fst.expr))
          .IGoR$command2(      # --- Use 'fst' connection -------------------
            glue("fst(\"{input$import.file}\")"),NL,
            glue(".[{expr('.',TRUE)},{.collapse2(input$import.fst.columns)}]"), .IGoR$look(input$import.fst.expr),
            if (length(input$import.columns)==1) # PB fst 0.8.8 drop=FALSE ne marche pas
              paste0(NL,glue("data.frame({input$import.fst.columns}=., stringsAsFactors=FALSE)"))
          )
        else
        if ((type=="feather")&&.isNotEmpty(input$import.feather.expr))                
          .IGoR$command2(      # --- Use 'feather' connection --------------
            glue("feather(\"{input$import.file}\")"),NL,
            glue(".[{expr('.',FALSE)},{.collapse2(input$import.feather.columns)}]")
          )
        else
        if ((type %in% c("xlsx","xls"))&&.isEQ(input$import.xls.type,"insee")) 
          .IGoR$command2(      # --- from insee.fr : get column labels ----
            paste0("(function (file, sheet) {\n",
                   "     df <- import(file,  sheet=sheet, skip=5)\n",
                   "     Map(function(x) attr(df[[x[2]]],'label')<<- x[1],\n",
                   "         import(file, sheet=sheet, skip=4, n_max=2, col_names=FALSE))\n",
                   "     df\n",
                   "   })",
                   glue("(\"{input$import.file}\""),
                   if (.isNE(input$import.xls.sheet,1)) glue(", \"{input$import.xls.sheet}\""),
                   ")"
          ))
        else
        if (type=="ods") 
          .IGoR$command2(
            if (.isEQ(input$import.ods.type,"one"))
              paste0(
                glue("read_ods(\"{input$import.file}\""),  # 'sheet' parameter doesn't work with 'import'                
                if (.isNE(input$import.ods.sheet,1))     glue(", sheet=\"{input$import.ods.sheet}\""),
                if (.isFALSE(input$import.ods.names))    ", col_names=FALSE",
                if (.isNotNA(input$import.ods.skip))     glue(", skip={input$import.ods.skip}"),
                if (.isNotEmpty(input$import.ods.range)) glue(", range=\"{input$import.ods.range}\""),
                ")"
              )
            else
            if (.isEQ(input$import.ods.type,"all"))
              paste0(
                "(function (file)\n",
                "      ods_sheets(file)",
                if (.isTRUE(input$import.ods.all.skip)) "[-1]", 
                " %>%\n",
                "      Map(function(x)\n",
                "        read_ods(file, sheet=x",
                if (.isFALSE(input$import.ods.names))    ", col_names=FALSE",
                if (.isNotNA(input$import.ods.skip))     glue(", skip={input$import.ods.skip}"),
                if (.isNotEmpty(input$import.ods.range)) glue(", range=\"{input$import.ods.range}\""),
                ") %>%\n",
                "        mutate(",.name(input$import.ods.all.name),"=x) %>%\n",
                "        select(",.name(input$import.ods.all.name),",everything()), .) %>%\n",
                "      Reduce(bind_rows, .)\n",
                "   )(\"",input$import.file,"\")"
              )
          )
        else # --- Calls to 'rio::import' --------------------------------------------------------------------------
          .IGoR$command2(
            glue("import(\"{input$import.file}\""),
            switch(type,
              sas7bdat =
                paste0(
                  if (.isNotEmpty(input$import.sas.columns))  glue(", col_select={.collapse2(input$import.sas.columns)}"),
                  if (.isNE(input$import.sas.nrows,Inf))      glue(", n_max={input$import.sas.nrows}"),
                  if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\"")
                ),
              csv      =
                paste0(
                  if (.isNotEmpty(input$import.csv.columns))  glue(", select={.collapse2(input$import.csv.columns)}"),
                  if (.isNE(input$import.csv.nrows,Inf))      glue(", nrows={input$import.csv.nrows}"),
                  if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
                  if (.isFALSE(input$import.csv.chars)&&.isTRUE(input$import.csv.dec)) ", dec=','",
                  if (.isTRUE(input$import.csv.chars)) ", colClasses=\"character\""
                ),
              dbf      = glue(", as.is={.isFALSE(input$import.dbf)}"), # PB rio 0.5.16 default should be TRUE
              xls      =,
              xlsx     =
                paste0(
                  if (.isNE(input$import.xls.sheet,1)) glue(", sheet=\"{input$import.xls.sheet}\""),
                  if (.isFALSE(input$import.xls.names)) ", col_names=FALSE",
                  if (.isNotNA(input$import.xls.skip)) glue(", skip={input$import.xls.skip}")
                ),
              fst      =
                paste0(
                  if (length(input$import.fst.columns)>0) glue(", columns={.collapse2(input$import.fst.columns)}"),
                  if (.isEQ(input$import.fst.filter,"range")&&(length(input$import.fst.from)>0)) glue(", from={as.character(input$import.fst.from)}"),
                  if (.isEQ(input$import.fst.filter,"range")&&(length(input$import.fst.to)>0))   glue(", to={as.character(input$import.fst.to)}")
                ),
				      feather  =
				        if (length(input$import.feather.columns)>0) glue(", columns={.collapse2(input$import.feather.columns)}"),
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
