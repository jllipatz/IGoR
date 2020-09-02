
### 28/01/2020 1.06.0
### 03/08/2020 1.10.0: Prise en compte de la modification de .collapse

.IGoR$page$rbind$ui <- function()
  .IGoR$ui(page="rbind",
    fluidRow(
      column(width=6,
        box(width='100%', uiOutput("rbind.data")
      ) ),
      column(width=6, .IGoR$load.ui("rbind"))
 ) )


.IGoR$page$rbind$sv <- function(input, output, session) {
  
  .IGoR$aServer(input,output,"rbind")
  
  output$rbind.data<- renderUI({
    .IGoR$test$list
    fluidRow(
      column(width=6, 
        selectizeInput("rbind.data", .IGoR$s1(.IGoR$Z$rbind$data), 
                       multiple=TRUE, choices=c(.IGoR$TABLE,.tables())
    ) ) )
  })
  
  output$rbind.command2 <- renderUI(
    .IGoR$textarea("rbind", "bind_rows(tables)", 2,
      if (.isNotEmpty(input$rbind.data))
        .IGoR$command2(
          paste0("bind_rows(",.collapse0(input$rbind.data),")")
  )))
  
  observeEvent(input$rbind.command2,
    .IGoR$try(input,output,"rbind",
       .fn=function (x)
         sprintf(.IGoR$Z$rbind$msg.result,
           ncol(get(input$main.data,envir=.GlobalEnv)),
           do.call(partial(paste,sep='+'),
                   Map(function(x) ncol(get(x,envir=.GlobalEnv)),
                       input$rbind.data)),
           ncol(x)
          )
  ))
                           
  
}