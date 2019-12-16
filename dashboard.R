### 05/07/2019 1.01.4: Initialisation correcte du "home", sauvegarde automatique en mode batch
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 13/11/2019 1.04.5: Test préalable de la version de R
### 04/12/2019 1.04.6: Externalisation du menu dans init.R

.ui <- dashboardPage(
  skin = "blue", 
  
  header = dashboardHeader(
    title = "I Go R"
    ,tags$li(class = "dropdown", p(em(.IGoR$Z$version)))
    ,tags$li(a(href = 'http://www.insee.fr',
               img(src = 'images/logo_insee.png',
                   title = "insee.fr", height = "46px"),
               style = "padding-top:2px; padding-bottom:2px;"),
             class = "dropdown")
  )
  ,
  sidebar = dashboardSidebar(
    tags$head(
      tags$style(HTML("
                      .main-sidebar {
                      font-family : Arial;
                      }
                      .content-wrapper {
                      background-color: linen !important;
                      font-family : Arial;
                      }
                      "))),
    imageOutput("main.igor",height='128px'),  # Permet la surimpression du champ suivant
    tags$div(id = "loading", tags$script('$("#loading").hide()')),
    uiOutput("main.data"),
    do.call(sidebarMenu,
            append(
              list(id = "menu",
                width = "400"),
              append(
                map2(unname(.IGoR$menus), names(.IGoR$menus),
                     function(l,n) do.call(menuItem,
                                           append(.IGoR$Z$dashboard[[n]],
                                                  map(l, function(x) menuSubItem(.IGoR$Z[[x]]$menu.title,tabName=x))
                ))),
                list(
                  menuItem(
                    actionButton("main.quit", .IGoR$Z$dashboard$quit,
                                 style = "background-color:rgb(34, 45, 50);border-color:rgb(34, 45, 50);color:white;width:32%;text-align: center;"
                    )),
                  useShinyjs(),
                  extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", functions = c("closeWindow"))
              )))
    )) # dashboardSidebar
  ,
  body = dashboardBody(
    if (paste0(version$major,'.',version$minor)<"3.4.0")
      tags$script(paste0('window.onload = alert("',.IGoR$Z$dashboard$msg.version,'");')),
    div(id = "splash_screen", img(src = "images/R_logo.png"),
        style = "text-align:center; padding-top:250px;  padding-bottom:300px;"),
    div(id = "form",
        tags$script(
          'function checkifrunning() {
          var is_running = $("html").attr("class").includes("shiny-busy");
          if (is_running){
          $("#loading").show()
          } else {
          $("#loading").hide()
          }
          }; 
          setInterval(checkifrunning, 1000)'
        ), 
        tags$style(
          "#loading {
          display: inline-block;
          border: 3px solid #f3f3f3; 
          border-top: 3px solid #3498db; 
          border-radius: 50%;
          width: 50px;
          height: 50px;
          animation: spin 1s ease-in-out infinite;
          }
          
          @keyframes spin {
          0% { transform: rotate(0deg); }
          100% { transform: rotate(360deg); }
          }"
      ),
      do.call(tabItems,
              map(names(.IGoR$page), function(x) tabItem(x,.IGoR$page[[x]]$ui()))
      )))
  
  )


.server <- function(input, output, session){
  
  hide("splash_screen", anim = TRUE, animType = "fade", time = 3)
  
  .IGoR$volumes <<- c(home=fs::path_expand("~"), wd=getwd(), .IGoR$volumes)

  output$main.igor <- renderImage(list(src="images/igor.jpg"),deleteFile = FALSE)
  
  output$main.data <- .IGoR$renderTables(input,output)
  
  observeEvent(input$main.quit,
               showModal(modalDialog(
                 title = "Quitter IGoR?",
                 if (!.IGoR$save)
                      .IGoR$Z$dashboard$msg.drop
                 else .IGoR$Z$dashboard$msg.save,
                 footer = tagList(
                   modalButton(.IGoR$Z$dashboard$quit.cancel),
                   actionButton("quit.ok",.IGoR$Z$dashboard$quit.ok)
                 )))
  )
  
  observeEvent(input$quit.ok,{
    js$closeWindow()              # Ne marche pas: interdit si la fenetre n'a pas été ouverte par window.open
    stopApp()
  })
  
  ## En cas de fermeture de la page du navigateur
  ## NOTE : A l interieur de la fonction '~' ferait référence à D:/h2izgk/Mes documents
  ##        alors que l'initialisation de home' avec '~' contient C:/Users/h2izgk
  ##        et que shinyFiles interprete le '~' de cette manière
  session$onSessionEnded(
    function() {
      if (isTRUE(.IGoR$save)) save(list=.tables(), file=paste0(.IGoR$volumes["home"],"/IGoR.RData"))
      stopApp()
  }) 
  
  walk(names(.IGoR$page),function(x) .IGoR$page[[x]]$sv(input,output,session))
  
}