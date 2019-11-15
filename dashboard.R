### 05/07/2019 1.01.4: Initialisation correcte du "home", sauvegarde automatique en mode batch
### 12/08/2019 1.04.2: Externalisation des libellés en français
### 13/11/2019 1.04.5: Test préalable de la version de R

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
    sidebarMenu(id = "menu",
                width = "400",
                menuItem(.IGoR$Z$dashboard$manage,
                         menuSubItem(.IGoR$Z$contents$menu.title,  tabName="contents"),
                         menuSubItem(.IGoR$Z$view$menu.title,      tabName="view"),
                         menuSubItem(.IGoR$Z$browse$menu.title,    tabName="browse"),
                         menuSubItem(.IGoR$Z$distinct$menu.title,  tabName="distinct"),
                         menuSubItem(.IGoR$Z$create$menu.title,    tabName="create"),
                         menuSubItem(.IGoR$Z$import$menu.title,    tabName="import"),
                         menuSubItem(.IGoR$Z$export$menu.title,    tabName="export"),
                         menuSubItem(.IGoR$Z$tables$menu.title,    tabName="tables")
                ),
                menuItem(.IGoR$Z$dashboard$update,
                         menuSubItem(.IGoR$Z$rename$menu.title,    tabName="rename"),
                         menuSubItem(.IGoR$Z$factor$menu.title,  tabName="factor"),
                         menuSubItem(.IGoR$Z$cut$menu.title, 	     tabName="cut"),
                         menuSubItem(.IGoR$Z$mutate$menu.title,    tabName="mutate"),
                         menuSubItem(.IGoR$Z$mutate2$menu.title,   tabName="mutate2")
                ),
                menuItem(.IGoR$Z$dashboard$extract,
                         menuSubItem(.IGoR$Z$slice$menu.title,     tabName="slice"),
                         menuSubItem(.IGoR$Z$filter$menu.title,    tabName="filter"),
                         menuSubItem(.IGoR$Z$select$menu.title,    tabName="select")
                ),
                menuItem(.IGoR$Z$dashboard$reshape,
                         menuSubItem(.IGoR$Z$summarise$menu.title, tabName="summarise"),
                         menuSubItem(.IGoR$Z$gather$menu.title,    tabName="gather"),
                         menuSubItem(.IGoR$Z$spread$menu.title,    tabName="spread"),
                         menuSubItem(.IGoR$Z$arrange$menu.title,   tabName="arrange")
                ),
                menuItem(.IGoR$Z$dashboard$merge,
                         menuSubItem(.IGoR$Z$join$menu.title,      tabName="join"),
                         menuSubItem(.IGoR$Z$fuzzyjoin$menu.title, tabName="fuzzyjoin"),
                         menuSubItem(.IGoR$Z$labels$menu.title,    tabName="labels")
                ),
                menuItem(.IGoR$Z$dashboard$statistics,
                         menuSubItem(.IGoR$Z$skim$menu.title,      tabName="skim"),
                         menuSubItem(.IGoR$Z$tabular$menu.title,   tabName="tabular")
                ),
                menuItem(.IGoR$Z$dashboard$graphics,
                         menuSubItem(.IGoR$Z$bar$menu.title,       tabName="bar"),
                         menuSubItem(.IGoR$Z$col$menu.title,       tabName="col"),
                         menuSubItem(.IGoR$Z$histogram$menu.title, tabName="histogram"),
                         menuSubItem(.IGoR$Z$boxplot$menu.title, 	 tabName="boxplot"),
                         menuSubItem(.IGoR$Z$pie$menu.title,       tabName="pie"),
                         menuSubItem(.IGoR$Z$points$menu.title,    tabName="points"),
                         menuSubItem(.IGoR$Z$bin2d$menu.title,     tabName="bin2d"),
                         menuSubItem(.IGoR$Z$lorenz$menu.title,    tabName="lorenz"),
                         menuSubItem(.IGoR$Z$spplot$menu.title,    tabName="spplot")
                ),
                menuItem(
                  actionButton("main.quit", .IGoR$Z$dashboard$quit,
                               style = "background-color:rgb(34, 45, 50);border-color:rgb(34, 45, 50);color:white;width:32%;text-align: center;"
                  )),
                useShinyjs(),
                extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", functions = c("closeWindow"))
    )) # dashboardSidebar
  ,
  body = dashboardBody(
    if (paste0(version$major,'.',version$minor)<"3.4.0")
      tags$script(paste0('window.onload = alert("',.IGoR$Z$dashboard$msg.version,'");')),
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