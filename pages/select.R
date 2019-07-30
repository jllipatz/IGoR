
### 07/06/2019 v1.01.0 Correction : inversion de la sélection
### 12/07/2019 v1.02.0 Ajout : everything()

.IGoR$page$select$ui <- function()
  div(id = "bloc_select",
    fluidRow(
      column(width=4, 
        img(src="images/select.png", height = "48px"),
        h3(span("Sélectionner des variables", style="color: blue"))
      ),
      column(width=8, 
        p("La fonction ", code("select"), " du package ", strong("dplyr"), " construit une nouvelle table limitée aux variables choisies.", br(),
          "La sélection des variables peut se faire sur la forme du nom de la variable ou sur son type.", br(), 
          "La sélection de variables peut également permettre de réordonner les variables."
    ) ) ),
    uiOutput("select.control"),
    .IGoR$commandBox("select")
  )      


.IGoR$page$select$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"select")
  
  output$select.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("select","Conserver les variables...")),
        column(width=6, .IGoR$loadBox("select"))
  ))
  
  output$select.columns.more <- renderUI(
    if ((length(input$select.type)>0)
      &&(((input$select.type==1)&&(length(input$select.columns)>0))
       ||(input$select.type>3)))
      checkboxInput("select.everything","Compléter avec toutes les autres",FALSE)
  )

  output$select.command2 <- renderUI(
    .IGoR$textarea("select", "select(columns)", 3,
      if ((length(input$select.type)>0)&&(length(input$select.drop)>0))
        .IGoR$command2(
          "select",
          if ((input$select.type==2)&&.isNotEmpty(input$select.class))
             if (input$select.drop)
                  glue("_if(Negate(is.{input$select.class})")
             else glue("_if(is.{input$select.class}")
            else paste0("(",.IGoR$select(input,"select")),
          if ((((input$select.type==1)&&(length(input$select.columns)>0))
             ||(input$select.type>3))
            &&.isTRUE(input$select.everything)) ", everything()",
          ")"
  ) )   ) 
  
  observeEvent(input$select.command2,
    .IGoR$try(input,output,"select",
      function(x) sprintf("NOTE : Le résultat aura %d colonne(s) :\n  %s.",ncol(x),.collapse(colnames(x)))
  ))
               
}