
### 28/06/2019 1.01.2: Protection contre les noms de table incorrects
### 09/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$create$ui <- function()
  .IGoR$ui(page="create",
    fluidRow(
      column(width=6,
        box(width='100%',
          textInput("create.columns",    .IGoR$s1(.IGoR$Z$create$names)),
          checkboxInput("create.factors",.IGoR$s4(.IGoR$Z$any$stringsAsFactors),FALSE),
          textInput("create.na.strings", .IGoR$s2(.IGoR$Z$create$na.strings),"NA")
      )),
      column(width=6,.IGoR$load.ui("create"))
  ) )


.IGoR$page$create$sv <- function(input, output, session) {
  
  .IGoR$rLogo(input,output,"create")
  
  na.strings <- function () if (input$create.na.strings=="NA") "" else glue(", na.strings=\"{input$create.na.strings}\"")
  
  command1 <- "read.table(header=TRUE, text=\"{input$create.columns}"
  
  output$create.command1 <- renderText(
    .IGoR$create.command1 <<- paste0(make.names(input$create.out),' <- ',glue(command1)))
  
  output$create.command2 <- renderUI(
    .IGoR$textarea("create", .IGoR$Z$create$data, 5, ''))
  
  output$create.command3 <- renderText(
    .IGoR$create.command3 <<- glue("\", stringsAsFactors={input$create.factors}{na.strings()})"))
  
  observeEvent({input$create.command2;input$create.out;input$create.columns;input$create.factors},
    output$create.comment <- renderText({
      t <- make.names(input$create.out)
      b <- "create.load"
      s <- paste0(glue(command1),'\n',input$create.command2,glue(.IGoR$create.command3))
      if (nchar(s)>0) {
        x <- tryCatch(eval(parse(text=s),envir=.GlobalEnv),
                      error=function(e) e)
        if (is.data.frame(x)) { 
          output$create.load <- renderUI(actionButton(b,.IGoR$buttonName(input,"create")))
          shinyjs::enable(b)
          shinyjs::show(b)
          sprintf(.IGoR$Z$create$msg.result,t,nrow(x),ncol(x))
        }
        else {
          shinyjs::hide(b)
          x$message
      } }
      else {
        shinyjs::hide(b)
        ""
  }}))
  
  observeEvent(input$create.load, 
   .IGoR$do(input,output,"create",
      paste0(.IGoR$create.command1,'\n',input$create.command2,.IGoR$create.command3)
  ))
 
}
          
  