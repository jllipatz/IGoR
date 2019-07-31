### 05/07/2019 1.01.4: Initialisation correcte du "home", sauvegarde automatique en mode batch

.ui <- dashboardPage(
  skin = "blue", 
  
  header = dashboardHeader(
    title = "I Go R"
    ,tags$li(class = "dropdown", p(em("version 1.03.1")))
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
                menuItem("Gérer les tables",
                         menuSubItem("Visualiser la structure",        tabName="contents"),
                         menuSubItem("Visualiser les données",         tabName="view"),
                         menuSubItem("Visualiser une observation",     tabName="browse"),
                         menuSubItem("Modalités des variables",        tabName="distinct"),
                         menuSubItem("Créer une table ex nihilo",      tabName="create"),
                         menuSubItem("Importer", 		                   tabName="import"),
                         menuSubItem("Exporter",                       tabName="export"),
                         menuSubItem("Lister les tables",              tabName="tables")
                ),
                menuItem("Recoder",
                         menuSubItem("Renommer des variables",         tabName="rename"),
                         menuSubItem("Changer le type des variables",  tabName="factor"),
                         menuSubItem("Discrétiser une variable", 	     tabName="cut"),
                         menuSubItem("Créer/modifier une variable",    tabName="mutate"),
                         menuSubItem("Modifier des variables",         tabName="mutate2")
                ),
                menuItem("Extraire",
                         menuSubItem("une plage d'observations",       tabName="slice"),
                         menuSubItem("des observations",               tabName="filter"),
                         menuSubItem("des variables",                  tabName="select")
                ),
                menuItem("Restructurer",
                         menuSubItem("Cumuler", 	                     tabName="summarise"),
                         menuSubItem("Transposer en format long",      tabName="gather"),
                         menuSubItem("Transposer en format large",     tabName="spread"),
                         menuSubItem("Trier",                          tabName="arrange")
                ),
                menuItem("Enrichir",
                         menuSubItem("Jointure sur égalité",           tabName="join"),
                         menuSubItem("Jointure floue", 	               tabName="fuzzyjoin"),
                         menuSubItem("Libellés d'un facteur",          tabName="labels")
                ),
                menuItem("Statistiques",
                         menuSubItem("Analyse rapide",                 tabName="skim"),
                         menuSubItem("Tableaux",                       tabName="tabular")
                ),
                menuItem("Graphiques",
                         menuSubItem("Barres (données individuelles)", tabName="bar"),
                         menuSubItem("Barres (données cumulées)",      tabName="col"),
                         menuSubItem("Histogramme", 		               tabName="histogram"),
                         menuSubItem("Boite à moustache", 	           tabName="boxplot"),
                         menuSubItem("Diagramme en camembert",         tabName="pie"),
                         menuSubItem("Nuage de points",	               tabName="points"),
                         menuSubItem("Carte de densité",               tabName="bin2d"),
                         menuSubItem("Courbe de Lorenz",               tabName="lorenz")
                ),
                menuItem(
                  actionButton("main.quit", label = "Quitter",
                               style = "background-color:rgb(34, 45, 50);border-color:rgb(34, 45, 50);color:white;width:32%;text-align: center;"
                  )),
                useShinyjs(),
                extendShinyjs(text = "shinyjs.closeWindow = function() { window.close(); }", functions = c("closeWindow"))
    )) # dashboardSidebar
  ,
  body = dashboardBody(
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
                      "ATTENTION : Le contenu de la mémoire ne sera pas sauvegardé!"
                 else "L'intégralite des tables en mémoire va être sauvegardé.",
                 footer = tagList(
                   modalButton("Annuler"),
                   actionButton("quit.ok", "Confirmer")
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