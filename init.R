library(shiny)
library(shinydashboard)
library(shinyjs)
library(shinyFiles)

library(purrr)
library(tidyr)
library(dplyr)
library(fuzzyjoin)
library(stringr)
library(glue)

library(ggformula)
library(gglorenz)
library(tables)
library(skimr)

library(rio)
library(fst)
library(feather)

library(lubridate)
library(zoo)

`%not in%` <- Negate(`%in%`)

## Définition utile à la page 'Tableaux'
wtd.percent <- function (x,y) 100*sum(x)/sum(y) 

## Définition nécessaire à la page 'courbe de Lorenz'
gf_lorenz <- layer_factory(geom = "line",       # Le nouveau graphique sera une courbe
                           stat = "lorenz",     # et utilisera la fonction stat_lorenz du package 
                           aes_form = ~ x)      # avec une formule à une seule colonne

## Jeux de test
library(datasets)
mtcars1 <- mtcars %>% 
  mutate(id=rownames(mtcars)) %>% 
  select(id,everything()) %>%
  mutate_at(c("cyl","gear","vs"),factor) %>%
  mutate_at("carb",as.integer) %>%
  mutate_at("am",as.logical)
rownames(mtcars1) <- rownames(mtcars)
attr(mtcars1$mpg, 'label')<- "Miles/(US) gallon"
attr(mtcars1$cyl, 'label')<- "Number of cylinders"
attr(mtcars1$disp,'label')<- "Displacement (cu.in.)"
attr(mtcars1$hp,  'label')<- "Gross horsepower"
attr(mtcars1$drat,'label')<- "Rear axle ratio"
attr(mtcars1$wt,  'label')<- "Weight (1000 lbs)"
attr(mtcars1$qsec,'label')<- "1/4 mile time"
attr(mtcars1$vs,  'label')<- "Engine (0 = V-shaped, 1 = straight)"
attr(mtcars1$am,  'label')<- "Transmission (TRUE = automatic, FALSE = manual)"
attr(mtcars1$gear,'label')<- "Number of forward gears"
attr(mtcars1$carb,'label')<- "Number of carburetors"

nanopop <- data.frame(age=c( 20, 30, 40, 50, 60, 20, 30, 40, 50, 60, NA),
                      sexe=c("M","M","M","M","M","F","F","F","F","F","F"),
                      poids=c(  1,  1,  1,  1,  3,  6,  2,  2,  2,  2,  1))

## Volumes visibles sous les navigateurs de fichiers
.IGoR <- list(
  volumes = c(D='D:/',U='U:/',V='V:/',W='W:/',Z='Z:/'),
  page=list())

## désactivation des histogrammes de skimr qui ne s'affichent pas correctement
skim_with(numeric=list(hist=NULL), integer=list(hist=NULL))