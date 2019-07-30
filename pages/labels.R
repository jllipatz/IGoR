
.IGoR$page$labels$ui <- function()
  div(id = "bloc_labels",
    fluidRow(
      column(width=4, 
        img(src="images/labels.png", height = "46px"),
        h3(span("Importer les libellés d'une variable qualitative", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction", code("factor"), "permet de",
          span("mettre une variable qualitative dans le type 'énumération'", style='color:blue'),
          ", tout en précisant les libellés qui doivent apparaître pour chacune des modalités de la variable", br(),
          "Cette page permet d'exploiter une", em("table de passage"), "définie ailleurs sous forme de table contenant deux colonnes : modalités de la variable et libellés associés. "
    ) ) ),
    uiOutput("labels.control"),
    .IGoR$commandBox("labels")
  )


.IGoR$page$labels$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"labels")
  
  output$labels.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6,
          fluidRow(
            column(width=6, 
              box(width='100%',
                selectizeInput("labels.old", .IGoR$OLDVAR,
                             choices=c(.IGoR$DISCOLV,.columns(input$main.data,c("character","integer","logical"))))
            ) ),
            column(width=6, uiOutput("labels.new"))
          ),
          box(width='100%',
            selectizeInput("labels.data", "Table de correspondance", choices=.tables()),
            uiOutput("labels.data.columns")
        ) ),
        column(width=6, uiOutput("labels.out"))
  )   )
  
  output$labels.new <- renderUI(
    if (.isNotEmpty(input$labels.old))
      box(width='100%', textInput("labels.new",.IGoR$NEWCOL,input$labels.old))
  )
  
  output$labels.out <- renderUI(if (length(input$main.data)>0) .IGoR$loadBox("labels", input$main.data))
  
  output$labels.data.columns <- renderUI(
    if ((length(input$labels.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, selectizeInput("labels.data.levels","Variable donnant les codes (levels)",
                       choices=c(.IGoR$DISCOLV,.columns(input$labels.data,c("character","integer","logical"))))),
        column(width=6, selectizeInput("labels.data.labels","Variable donnant les libellés (labels)",
                       choices=c(.IGoR$CHRCOLV,.columns(input$labels.data,"character"))))
      )
  )

  output$labels.command2 <- renderUI(
    .IGoR$textarea("labels", "mutate(new=factor(old,levels=...,labels=...))", 3,
      if (.isNotEmpty(input$labels.old)
        &&(length(input$labels.data)>0)&&.isNotEmpty(input$labels.data.levels)&&.isNotEmpty(input$labels.data.labels))
        .IGoR$command2(
          glue("mutate({input$labels.new}=factor({input$labels.old},"),'\n     ',
          glue("levels={input$labels.data}${input$labels.data.levels},"),'\n     ',
          glue("labels={input$labels.data}${input$labels.data.labels}))")
  ) )   )
                             
  observeEvent(input$labels.command2, .IGoR$try(input,output,"labels"))

}