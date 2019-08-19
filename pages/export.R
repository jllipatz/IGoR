
### 18/06/2019 1.01.1: Ajout du type 'feather'
### 19/07/2019 1.03.0: Ajout du type 'json'
### 27/07/2019 1.02.0: Ajout du paramètre 'compress' pour le type 'fst'
### 09/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$export$ui <- function()
  .IGoR$ui(page="export",
    fluidRow(
      column(width=6,
        box(width='100%',
          radioButtons("export.type",NULL,.IGoR$Znames("export","type",c("export","excel","rdata"))),
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
)   ) ) ) )


.IGoR$page$export$sv <- function(input, output, session) {

  .IGoR$rLogo(input,output,"export")
  
  output$export.control <- renderUI(
    if (input$export.type!="export")
     tagList(
       selectizeInput("export.tables",
                     .IGoR$s1(if (input$export.type==2) .IGoR$Z$export$excel.tables else .IGoR$Z$export$rdata.tables),
                     multiple=TRUE,  options = list(placeholder = .IGoR$Z$any$all),
                     choices=.IGoR$tables),
       if (input$export.type=="excel") checkboxInput("export.names",.IGoR$s4(.IGoR$Z$export$excel.names),FALSE)
  ))
    
  output$export.files <- renderUI(
    if (length(input$main.data)>0)
      if (input$export.type=="export")
        shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$choose,
          filename=input$main.data,
          filetype=list("R Data Serialization"="RDS",
                          "Fast serialization"="fst",
                                     "feather"="feather",
                              "Excel Open XML"="xlsx",
                                       "DBase"="dbf",
                      "Comma Separated Values"="csv",
                                        "JSON"="json"))
      else
      if (input$export.type=="excel")
        shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$choose, filetype=list("Excel Open XML"="xlsx"))
      else
      if (input$export.type=="rdata")
        shinySaveButton("export", .IGoR$Z$any$browse, .IGoR$Z$export$rdata.choose, filetype=list("R Data"="RData"))
  )
  
  output$export.parms <- renderUI(
    if (.isFile(input$export.file)) 
      switch(get_ext(input$export.file),
        "fst" = box(width='100%', sliderInput("export.fst.compress", .IGoR$s2(.IGoR$Z$export$fst.compress), 0, 100, 50))
  )   )
  
  observe({
    volumes <- .IGoR$volumes
    shinyFileSave(input, "export", roots = volumes, defaultPath='', defaultRoot='home')
    fileinfo <- parseSavePath(volumes, input$export)
  
    output$export.file <- renderUI(
      textInput("export.file",.IGoR$s1(.IGoR$Z$any$path),fileinfo$datapath))
  })
  
  output$export.command1 <- renderText(
    if (length(input$main.data)>0)
      .IGoR$export.command1 <<-
        if (input$export.type=="export")
          glue("{input$main.data} %>%")
        else {
          l <- if (length(input$export.tables)==0) .IGoR$tables else input$export.tables
          glue("c({.collapse1(l)}) %>%")
        }
  )
  
  output$export.command2 <- renderUI(
    .IGoR$textarea("export", "export(path,parms)", 3,
      if ((length(input$export.type)>0)&&.isNotEmpty(input$export.file)) 
        .IGoR$command2( 
          if (input$export.type=="export")  # - save using rio -----------------------------------------------------------
            paste0(
              glue("export(file=\"{input$export.file}\""),
              switch(get_ext(input$export.file),
                "fst" = if (.isNE(input$export.fst.compress,50)) glue(", compress={input$export.fst.compress}")
              ),
              ")"
            )
          else
          if (input$export.type=="excel") { # - save as Excel sheets -----------------------------------------------------
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
          if (input$export.type=="rdata") { # - save as RData ------------------------------------------------------------
            t <- if (!str_detect(input$export.file,"\\.RData$")) ".RData" else ""
            glue("save(list=., file=\"{input$export.file}{t}\")")
          }
  )   ) )
  
  observeEvent(input$export.command2, {
    if (.isNotEmpty(input$export.file))
      output$export.load <- renderUI(
        actionButton("export.load",
          if (input$export.type=="export") .IGoR$Z$export$export
          else
          if (input$export.type=="excel")  .IGoR$Z$export$excel 
          else                             .IGoR$Z$export$rdata
      ))
    else output$export.comment <- renderText("")
  })             
  
  observeEvent(input$export.load,
    isolate({
      .IGoR$do1(input,output,"export",paste0(.IGoR$export.command1,input$export.command2),
        function (x) {
          # Protection contre une modification manuelle du nom de fichier dans la commande
          f <- str_extract(isolate(input$export.command2),"(?<=file=([\"'])).*?(?=\\1)")
          sprintf(.IGoR$Z$export$msg.result, f, file.size(f))
        }
      )
      shinyjs::disable("export.load")
  }))
  
}
      