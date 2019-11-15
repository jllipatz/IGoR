
### 18/06/2019 1.01.1: Ajout du type 'feather'
### 19/06/2019 1.01.1: Ajout du parametre 'from' pour le type 'fst'
### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 12/07/2019 1.02.0: Ajout du parametre 'encoding' pour le type 'csv'
### 19/07/2019 1.03.0: Ajout du type 'json'
### 09/08/2019 1.04.2: Externalisation des libellés en français
### 28/10/2019 1.04.4: Selection des feuilles des fichiers Excel par leur nom
### 15/11/2019 1.04.5: Séparateur de décimales pour letype 'csv'

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
                    "Excel open XML"='xlsx','xls','dbf','json',
                              "Calc"="ods",
                               "SAS"='sas7bdat',
                                'fst','feather',
                                'funcamp',
                                'rds','rData'))
    fileinfo <- parseFilePaths(.IGoR$volumes, input$import)
    
    output$import.file <- renderUI(
      textInput("import.file",.IGoR$s1(.IGoR$Z$any$path),fileinfo$datapath))
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
          selectizeInput("import.out",.IGoR$s2(.IGoR$Z$import$rdata.load),choices=l)
        }
        else  
          textInput("import.out", label=.IGoR$s2(.IGoR$Z$any$out),
                    str_extract(input$import.file,"(?<=/)[^/]*(?=\\.[^.]+$)"))
      }),
      column(width=4, uiOutput("import.load"))
    ))
  
  encoding.ui <- function () selectizeInput("import.encoding",.IGoR$s3(.IGoR$Z$import$encoding), choices=c("","UTF-8"))
  
  output$import.parms <- renderUI(
    if (.isFile(input$import.file)) 
      box(width='100%', {
        type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
        if (type=="csv")
          fluidRow(
            column(width=4, encoding.ui()),
            column(width=4, checkboxInput("import.csv.dec",.IGoR$Z$import$csv.dec,FALSE))
          )
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
            column(width=6, numericInput("import.xls.sheet", .IGoR$s2(.IGoR$Z$import$xls.sheet), 1)),
            column(width=4, checkboxInput("import.xls.names",.IGoR$s5(.IGoR$Z$import$header), TRUE)),
            column(width=2, numericInput("import.xls.skip",  .IGoR$s3(.IGoR$Z$import$xls.type.skip), NA))
          )
        else
        if (type=="fst")
          fluidRow(
            column(width=6,
               selectizeInput("import.columns", .IGoR$s3(.IGoR$Z$import$vars),
                             multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                             choices = metadata_fst(input$import.file)$columnNames),
               uiOutput("import.fst.parms")
            ),
            column(width=6,
              radioButtons("import.fst.filter",.IGoR$Z$import$fst.filter, .IGoR$Znames("import","fst.filter",c("all","range","where")), inline=TRUE)
          ) )
        else
        if (type=="feather")
          fluidRow(
            column(width=6,
              selectizeInput("import.columns", .IGoR$s3(.IGoR$Z$import$vars),
                             multiple = TRUE, options = list(placeholder = .IGoR$Z$any$all),
                             choices = attr(feather_metadata(input$import.file)$types,'names'))
            ),
            column(width=6,
               uiOutput("import.feather.parms"),
               checkboxInput("import.feather.filter",.IGoR$s3(.IGoR$Z$import$feather.where),FALSE)
            ) )
		    else
		    if (type=="sas7bdat")
		      fluidRow(
		        column(width=4, encoding.ui())
		      )
        else
        if (type=="funcamp") # --- This is a non standard extension for funcamp use only!
          fluidRow(          # funcamp files are just fst files with another file extension
            column(width=4,  # to disable any access not using the 'id' variable as record key
              textInput("import.funcamp.id",.IGoR$s2(.IGoR$Z$import$funcamp.key),0)
          ) )
  }))
  
  output$import.xls.names <- renderUI(
    if (.isEQ(input$import.xls.type,"skip"))
      checkboxInput("import.xls.names",.IGoR$s5(.IGoR$Z$import$header), TRUE)
  )
  
  output$import.xls.skip <- renderUI(
    if (.isFile(input$import.file)) {
      type <- input$import.file %>% str_extract("(?<=\\.)[^.]+$") %>% str_to_lower()
      if ((type %in% c("xlsx","xls","ods"))&&.isEQ(input$import.xls.type,"skip"))
        numericInput("import.xls.skip","",NA)
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
        textInput("import.expr",.IGoR$s3(.IGoR$Z$import$expr))
  )
  
  output$import.feather.parms <- renderUI(
    if (.isTRUE(input$import.feather.filter))
      textInput("import.expr",.IGoR$s3(.IGoR$Z$import$expr))
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
        if ((type=="feather")&&.isNotEmpty(input$import.expr))                # --- Use 'feather' connection --------------
          .IGoR$command2(
            glue("feather(\"{input$import.file}\")"),NL,
            glue(".[{expr('.',FALSE)},{.collapse2(input$import.columns)}]")
          )
        else
        if ((type %in% c("xlsx","xls"))&&.isEQ(input$import.xls.type,"insee")) # --- from insee.fr : get column labels ----
          paste0("   (function (file, sheet) {\n",
                 "     df <- import(file,  sheet=sheet, skip=5)\n",
                 "     Map(function(x) attr(df[[x[2]]],'label')<<- x[1],\n",
                 "         import(file, sheet=sheet, skip=4, n_max=2, col_names=FALSE))\n",
                 "     df\n",
                 "   })",
                 glue("(\"{input$import.file}\""),
                 if (.isNE(input$import.xls.sheet,1)) glue(", \"{input$import.xls.sheet}\""),
                 ")"
          )
        else # --- Calls to 'rio::import' --------------------------------------------------------------------------
          .IGoR$command2(
            glue("import(\"{input$import.file}\""),
            switch(type,
              csv      = 
                paste0(
                  if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
                  if (.isTRUE(input$import.csv.dec)) ", dec=','"
                ),
              dbf      = glue(", as.is={.isFALSE(input$import.dbf)}"), # PB rio 0.5.16 default should be TRUE
              sas7bdat = 
				        if (.isNotEmpty(input$import.encoding)) glue(", encoding=\"{input$import.encoding}\""),
              xls      =,
              xlsx     = 
                paste0(
                  if (.isNE(input$import.xls.sheet,1)) glue(", sheet=\"{input$import.xls.sheet}\""),
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
                  if (.isEQ(input$import.fst.filter,"range")&&(length(input$import.fst.from)>0)) glue(", from={as.character(input$import.fst.from)}"),
                  if (.isEQ(input$import.fst.filter,"range")&&(length(input$import.fst.to)>0))   glue(", to={as.character(input$import.fst.to)}")
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