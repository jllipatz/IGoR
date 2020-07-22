library(shiny)          # 1.2.0
library(shinydashboard) # 0.7.1
library(shinyjs)        # 1.0
library(shinyFiles)     # >=0.7.2, problème avec les versions antérieures
library(shinyWidgets)   # 0.4.8

library(lubridate)      # 1.7.4  Usage marginal, 
                        # A charger avant dplyr : conflit sur union, intersect,setdiff
                        # conflit réglé avec la 1.7.9 et 1.0.0 de dplyr

library(purrr)          # 0.2.5
library(tidyr)          # 0.8.2
library(dplyr)          # >=0.7.8, problème avec la 0.7.4
library(stringr)        # 1.3.1
library(glue)           # 1.3.0

library(rio)            # 0.5.16
library(ggformula)      # >=0.9.0, pour 'layer_factory'

library(zoo)            # 1.8-4  page 'mutate', pour 'na.locf'
library(fst)            # 0.8.8  pages 'import', 'export'
library(feather)        # 0.3.2  pages 'import', 'export'
library(fuzzyjoin)      # 0.1.4  page 'fuzzyjoin'
library(tables)         # 0.8.7  page 'tabular'
library(skimr)          # 1.0.3  page 'skim'
library(gglorenz)       # 0.1.0  page 'lorenz'
library(sp)             # 1.3-1  page 'spplot'
library(latticeExtra)   # 0.6-28 page 'spplot'

.IGoR <- list(
  volumes = c(D='D:/',U='U:/',V='V:/',W='W:/',X='X:/',Z='Z:/'),  # Volumes visibles sous les navigateurs de fichiers
  menus= list(
#      manage=c("import","contents","view","browse","distinct","create","tables","export"),
      manage=c("contents","view","browse","distinct","create","import","export","tables"),
      update=c("rename","factor","cut","mutate","mutate2"),
     extract=c("slice","filter","select"),
     reshape=c("summarise","gather","spread","arrange"),
       merge=c("join","fuzzyjoin","labels","rbind","union"),
  statistics=c("skim","tabular"),
    graphics=c("bar","col","histogram","boxplot","pie","line","points","bin2d","lorenz","spplot")
))

## Fonctions utiles

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
attr(mtcars1$am,  'label')<- "Transmission (FALSE = automatic, TRUE = manual)"
attr(mtcars1$gear,'label')<- "Number of forward gears"
attr(mtcars1$carb,'label')<- "Number of carburetors"

nanopop <- data.frame(age=c( 20, 30, 40, 50, 60, 20, 30, 40, 50, 60, NA),
                      sexe=c("M","M","M","M","M","F","F","F","F","F","F"),
                      poids=c(  1,  1,  1,  1,  3,  6,  2,  2,  2,  2,  1))

## désactivation des histogrammes de skimr qui ne s'affichent pas correctement
skim_with(numeric=list(hist=NULL), integer=list(hist=NULL))

## largeur de l'écran pour les aperçus de table
options(width=200)

## Import des fonds de carte (https://gadm.org/data.html, version juin 2018)
## ces fonds ne contiennent que la métropole
## ATTENTION : le package raster masque 'select'!
# library(raster)
# com2018.sp <- getData(name="GADM", country="FRA", level=5)
# can2018.sp <- getData(name="GADM", country="FRA", level=4) # anciens cantons
# dep2018.sp <- getData(name="GADM", country="FRA", level=2)
# reg2018.sp <- getData(name="GADM", country="FRA", level=1) # nouvelles régions
# fra2018.sp <- getData(name="GADM", country="FRA", level=0)
# save(com2018.sp,can2018.sp,dep2018.sp,reg2018.sp,fra2018.sp,file="data/geo_GADM2018.RData")
# detach("package:raster")

.IGoR$page=list() # Sera rempli plus tard avec le contenu du répertoire 'pages'

## Personnalisations
##   La variable système IGoR peut être positionnée
##   depuis le .bat par : set IGoR=D:/h2izgk/PALETTES/IGoR/startup.R
##   depuis RStudio par : Sys.setenv(IGoR="D:/h2izgk/PALETTES/IGoR/startup.R")
## 1.07
startup <- Sys.getenv("IGoR")
if (startup!="") source(startup)


