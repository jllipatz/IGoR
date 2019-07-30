### Lanceur de l'application
###   Faire tourner sous navigateur web pour avoir l'indicateur "shiny busy" pendant les longues operations
###
  source("init.R", echo=FALSE, encoding="UTF-8")
  walk(
    list.files("pages/",full.names=TRUE),
    function (x) source(x, echo=FALSE, encoding="UTF-8"))
  source("all.R", echo=FALSE, encoding="UTF-8")
  source("dashboard.R", echo=FALSE, encoding="UTF-8")
shinyApp(.ui, .server, options=list(launch.browser=TRUE))