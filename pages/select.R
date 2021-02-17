
### 07/06/2019 1.01.0: Correction : inversion de la sélection
### 12/07/2019 1.02.0: Ajout : everything()
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 16/02/2021 1.11.4: Correction : inversion d'une selection vide 

.IGoR$page$select$ui <- function() .IGoR$ui(page="select", control=TRUE)


.IGoR$page$select$sv <- function(input, output, session) {
  
  .IGoR$vServer(input,output,"select")
  
  output$select.control <- renderUI(
    if ((length(input$main.data)>0)&&.IGoR$test$meta)
      fluidRow(
        column(width=6, .IGoR$select.ui("select", buttons.title=.IGoR$s2(.IGoR$Z$select$select), buttons.all=FALSE)),
        column(width=6, .IGoR$load.ui("select"))
  ))
  
  .IGoR$select.drop(input,output,"select")
  
  output$select.columns.more <- renderUI(
    if ((length(input$select.type)>0)
      &&(((input$select.type==1)&&(length(input$select.columns)>0))
       ||(input$select.type>3)))
      checkboxInput("select.everything",.IGoR$s4(.IGoR$Z$select$everything),FALSE)
  )

  output$select.command2 <- renderUI(
    .IGoR$textarea("select", "select(columns)", 3,
      if (length(input$select.type)>0)
        if ((input$select.type==1)&&.isTRUE(input$select.drop)&&(length(input$select.columns)==0))
          .IGoR$command2("identity()")
        else
          .IGoR$command2(
            "select",
            if ((input$select.type==2)&&.isNotEmpty(input$select.class))
              if (.isTRUE(input$select.drop))
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
      function(x) sprintf(.IGoR$Z$select$msg.result,ncol(x),.collapse(colnames(x)))
  ))
               
}