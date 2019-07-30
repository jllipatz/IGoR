
.IGoR$page$cut$ui <- function()
  div( id = "bloc_cut", h3("'cut' : Transformation d'une variable numérique en variable qualitative"),
    uiOutput("cut.control"),
    .IGoR$commandBox("cut")
  )


.IGoR$page$cut$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"cut")
  
  output$cut.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          box(width='100%',
            selectizeInput("cut.column", label=.IGoR$OLDVAR,
                           choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric"))),
            uiOutput("cut.args"),
            uiOutput("cut.method")
          )),
          column(width=6,
            .IGoR$loadBox("cut",input$main.data),    
            box(width='100%',
              textInput("cut.new",.IGoR$NEWCOL,"cut.new"),
              plotOutput("cut.plot",height='200px')
        )))
  )

  output$cut.method <- renderUI(
    if (.isNotEmpty(input$cut.column))
      radioButtons("cut.method", "Méthode :",
                   c("Transformation en facteur"=1,
                            "Tranches manuelles"=2,
                           "Tranches régulières"=3))
  )
  
  output$cut.args <- renderUI(
    if (.isNotEmpty(input$cut.column))
      tagList(        
        verbatimTextOutput("cut.min"),
        verbatimTextOutput("cut.max"),
        if (length(input$cut.method)>0)
          if (input$cut.method==2)
            tagList(
              textInput("cut.breaks","Breaks (séparés par des virgules) :"),
              checkboxInput("cut.breaks.right","Borne supérieure comprise",TRUE)
            )
          else
          if (input$cut.method==3)
            numericInput("cut.step","Taille des tranches",NA)
  ))
  
  output$cut.min <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste("Valeur minimum :",min(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )
  
  output$cut.max <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste("Valeur maximum :",max(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )

  output$cut.command2 <- renderUI(
    .IGoR$textarea("cut", "mutate(column=cut(column,breaks))", 4,
      if ((length(input$cut.method)>0)&&.isNotEmpty(input$cut.column)&&.isNotEmpty(input$cut.new))
        .IGoR$command2(
          if (input$cut.method==1) 
            glue("mutate({input$cut.new} = factor({input$cut.column}))")
          else 
          if ((input$cut.method==2)&&(length(input$cut.breaks.right)>0)) {
            b <- if (length(input$cut.breaks)>0) str_replace_all(input$cut.breaks," ","") else ""
            if (str_length(b)>0) b <- paste0(.collapse(as.numeric(str_split(b,",")[[1]])),", ")
            if (input$cut.breaks.right)
                 glue("mutate({input$cut.new} = cut({input$cut.column}, c(min({input$cut.column})-1, {b}max({input$cut.column}))))")
            else glue("mutate({input$cut.new} = cut({input$cut.column}, c(min({input$cut.column}), {b}max({input$cut.column})+1), right=FALSE))")
          }
          else 
          if ((input$cut.method==3)&&.isNotNA(input$cut.step))
            glue("mutate({input$cut.new} = cut({input$cut.column}, seq(min({input$cut.column})-1,max({input$cut.column})+{input$cut.step},{input$cut.step})))")
  ) )   )

  observeEvent(input$cut.command2, {
    .IGoR$try(input,output,"cut",.subset=glue("select({input$cut.column})"))
    output$cut.plot <- renderPlot(
      tryCatch(
        eval(parse(text=glue("{input$main.data} %>% {input$cut.command2} %>% gf_bar( ~ {input$cut.new})")),
          envir=.GlobalEnv),
        error=function(e) NULL))
  })

}
