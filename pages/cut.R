
.IGoR$page$cut$ui <- function()
  div( id = "bloc_cut",
    fluidRow(
      column(width=4, 
        img(src="images/cut.png", height = "48px"),
        h3(span( "Transformation d'une variable de quantitative à qualitative", style="color: blue"))
      ),
      column(width=8, 
         p("La fonction ", code("cut"), 
           span("transforme une variable quantitative en variable qualitative", style='color:blue'), "sous forme de colonne de type 'facteur' ('énumération').",
           "L'ensemble des valeurs prise par la variable quantitative est découpé en tranches régulières ou non, ",
           "le numéro de chaque tranche devenant une modialité de la nouvelle variable qualitative.", br(),
           em("ATTENTION : l'opération n'est pas réversible."), br(),
           em("NOTE : si le nom de la table résultat est celui de la table courante, la page se réinitialise dès que la modification a fonctionné.")
    ) ) ),
    uiOutput("cut.control"),
    .IGoR$commandBox("cut")
  )


.IGoR$page$cut$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"cut")
  
  output$cut.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        fluidRow(
          column(width=3,
            box(width='100%',
              selectizeInput("cut.column", label=.IGoR$s1(.IGoR$OLDVAR),
                             choices=c(.IGoR$NUMCOLV,.columns(input$main.data,"numeric")))
          ) ),
          column(width=9,
            box(width='100%',
              column(width=3, textInput("cut.new",.IGoR$s2(.IGoR$NEWCOL),"cut.new")),
              column(width=6, textInput("cut.out",.IGoR$s2(.IGoR$OUT),input$main.data)),
              column(width=3, uiOutput("cut.load"))
        ) ) ),
        fluidRow(
          column(width=6, uiOutput("cut.method")),
          column(width=6, uiOutput("cut.plot1"))
  )   ) )

  output$cut.method <- renderUI(
    if (.isNotEmpty(input$cut.column))
      box(width='100%',
        column(width=6, radioButtons("cut.method", .IGoR$s2("Méthode :"),
                        c("Valeurs indivudelles"=1,
                            "Tranches manuelles"=2,
                           "Tranches régulières"=3))),
        column(width=6, uiOutput("cut.args"))
  )   )
  
  output$cut.args <- renderUI(
    if (.isNotEmpty(input$cut.column))
      tagList(
        verbatimTextOutput("cut.min"),
        verbatimTextOutput("cut.max"),
        if (length(input$cut.method)>0)
          if (input$cut.method==2)
            tagList(
              textInput("cut.breaks",.IGoR$s1("Breaks (séparés par des virgules) :")),
              checkboxInput("cut.breaks.right",.IGoR$s5("Borne supérieure comprise"),TRUE)
            )
          else
          if (input$cut.method==3)
            numericInput("cut.step",.IGoR$s1("Taille des tranches"),NA)
  )   )

  output$cut.min <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste("Valeur minimum :",min(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )

  output$cut.max <- renderText(
    if (.isNotEmpty(input$cut.column))
      paste("Valeur maximum :",max(get(input$main.data,envir=.GlobalEnv)[[input$cut.column]]))
  )
  
  output$cut.plot1 <- renderUI(
    if (.isNotEmpty(input$cut.column)) plotOutput("cut.plot",height='200px')
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
