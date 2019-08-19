
### 12/08/2019 1.04.2: Externalisation des libellés en français

.IGoR$page$labels$ui <- function() .IGoR$ui(page="labels", control=TRUE)


.IGoR$page$labels$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"labels")
  
  output$labels.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      tagList(
        fluidRow(
          column(width=3, 
            box(width='100%',
              selectizeInput("labels.old", .IGoR$s1(.IGoR$Z$any$old.var),
                             choices=c(.IGoR$COLV,.columns(input$main.data,c("character","integer","logical"))))
          ) ),
          column(width=9, uiOutput("labels.new"))
        ),
        fluidRow(
          column(width=6,
            box(width='100%',
              selectizeInput("labels.data", .IGoR$s1(.IGoR$Z$labels$data), choices=c(.IGoR$TABLE,.tables())),
              uiOutput("labels.data.columns")
  )   ) ) ) )
  
  output$labels.new <- renderUI(
    if ((length(input$main.data)>0)&&.isNotEmpty(input$labels.old))
      box(width='100%',
        column(width=3, textInput("labels.new",.IGoR$s2(.IGoR$Z$any$new.col),input$labels.old)),
        column(width=6, textInput("labels.out",.IGoR$s2(.IGoR$Z$any$out),"labels.out")),
        column(width=3, uiOutput("labels.load"))
  )   )
  
  output$labels.data.columns <- renderUI(
    if ((length(input$labels.data)>0)&&.IGoR$test$meta
      &&.isNotEmpty(input$labels.data))
      fluidRow(
        column(width=6, selectizeInput("labels.data.levels",.IGoR$s1(.IGoR$Z$labels$data.levels),
                       choices=c(.IGoR$COLV,.columns(input$labels.data,c("character","integer","logical"))))),
        column(width=6, selectizeInput("labels.data.labels",.IGoR$s1(.IGoR$Z$labels$data.labels),
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