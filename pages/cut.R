
### 12/08/2019 1.04.2: Externalisation des libellés en français

### BUG La methode des tracnhes régulières ne semble pas marcher avec des pas <1

.IGoR$page$cut$ui <- function() .IGoR$ui(page="cut", control=TRUE)


.IGoR$page$cut$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"cut")
  
  output$cut.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        fluidRow(
          column(width=3,
            box(width='100%',
              selectizeInput("cut.column", label=.IGoR$s1(.IGoR$Z$any$old.var), .numeric(input))
          ) ),
          column(width=9,
            box(width='100%',
              column(width=3, textInput("cut.new",.IGoR$s2(.IGoR$Z$any$new.col),"cut.new")),
              column(width=6, textInput("cut.out",.IGoR$s2(.IGoR$Z$any$out),input$main.data)),
              column(width=3, uiOutput("cut.load"))
        ) ) ),
        fluidRow(
          column(width=6, uiOutput("cut.method")),
          column(width=6, uiOutput("cut.plot1"))
  )   ) )

  output$cut.method <- renderUI(
    if (.isNotEmpty(input$cut.column))
      box(width='100%',
        column(width=6, radioButtons("cut.method", .IGoR$s2(.IGoR$Z$cut$method), .IGoR$Znames("cut","method",c("value","breaks","step")))),
        column(width=6, uiOutput("cut.args"))
  )   )
  
  output$cut.args <- renderUI(
    if (.isNotEmpty(input$cut.column))
      tagList(
        verbatimTextOutput("cut.min"),
        verbatimTextOutput("cut.max"),
        if (length(input$cut.method)>0)
          if (input$cut.method=="breaks")
            tagList(
              textInput("cut.breaks",.IGoR$s1(.IGoR$Z$cut$breaks)),
              checkboxInput("cut.breaks.right",.IGoR$s5(.IGoR$Z$cut$breaks.right),TRUE)
            )
          else
          if (input$cut.method=="step")
            numericInput("cut.step",.IGoR$s1(.IGoR$Z$cut$step),NA)
  )   )

  output$cut.min <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste(.IGoR$Z$cut$min, min(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )

  output$cut.max <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste(.IGoR$Z$cut$max, max(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )
  
  output$cut.plot1 <- renderUI(
    if (.isNotEmpty(input$cut.column)) plotOutput("cut.plot",height='200px')
  )

  output$cut.command2 <- renderUI(
    .IGoR$textarea("cut", "mutate(column=cut(column,breaks))", 4,
      if ((length(input$cut.method)>0)&&.isNotEmpty(input$cut.column)&&.isNotEmpty(input$cut.new))
        .IGoR$command2(
          if (input$cut.method=="value")
            glue("mutate({input$cut.new} = factor({input$cut.column}))")
          else
          if ((input$cut.method=="breaks")&&(length(input$cut.breaks.right)>0)) {
            b <- if (length(input$cut.breaks)>0) str_replace_all(input$cut.breaks," ","") else ""
            if (str_length(b)>0) b <- paste0(.collapse(as.numeric(str_split(b,",")[[1]])),", ")
            if (input$cut.breaks.right)
                 glue("mutate({input$cut.new} = cut({input$cut.column}, c(min({input$cut.column})-1, {b}max({input$cut.column}))))")
            else glue("mutate({input$cut.new} = cut({input$cut.column}, c(min({input$cut.column}), {b}max({input$cut.column})+1), right=FALSE))")
          }
          else
          if ((input$cut.method=="step")&&.isNotNA(input$cut.step))
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
