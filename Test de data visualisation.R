library(leaflet)

######################
### Initialisation ###
######################

apiKey <- "e6f6cfad-a662-4589-b060-1d7eed6d88a3"

# Lien des tuiles Stadia Maps pour les fonds de carte (Autres URL disponibles ici : https://stadiamaps.com/themes/)
ALIDADE_SMOOTH_TILES_URL <- "https://tiles.stadiamaps.com/tiles/alidade_smooth/{z}/{x}/{y}{r}.png?api_key="
OUTDOORS_TILES_URL <- "https://tiles.stadiamaps.com/tiles/outdoors/{z}/{x}/{y}{r}.png?api_key="
ALIDADE_SATELLITE_TILES_URL <- "https://tiles.stadiamaps.com/tiles/alidade_satellite/{z}/{x}/{y}{r}.png?api_key="

TILES_URL <- paste0(ALIDADE_SMOOTH_TILES_URL,apiKey)

# On importe les données
coordinatesRegions <- read.csv("regions_coordinates.csv", sep=";", dec=",", row.names = 1)
populationRegions <- read.csv("populations_regions_france_1900_2023.csv", sep=";", dec=",", row.names = 1)

# On fait une colonne avec les noms des régions
coordinatesRegions$region <- rownames(coordinatesRegions)
rownames(coordinatesRegions) <- NULL

# On transpose le dataframe et on fait une colonne avec les noms des régions
populationRegions <- data.frame(t(populationRegions))
populationRegions$region <- coordinatesRegions$region
rownames(populationRegions) <- NULL

# On choisit une année à afficher
year = 1990
populationRegionsYear <- populationRegions[,c(as.character(paste0("X",year)),"region"), drop=FALSE]

# On merge les deux dataframes
populationData <- merge(populationRegionsYear, coordinatesRegions, by = "region")
colnames(populationData) <- c("region","population","lng","lat")

##########################
### Programme statique ###
##########################


circleMarkersRadius = 0.0025

# Création de la carte avec leaflet
leafletMap <- leaflet(populationData) %>%
  
  # On ajoute le fond de carte
  addTiles(urlTemplate = TILES_URL) %>%
  
  # On ajoute des cercles bleus liés à la population
  addCircleMarkers(lng = ~lng,
                   lat = ~lat,
                   radius = ~sqrt(population) * circleMarkersRadius,
                   color = "#a3dca1", fillColor = "#3ae334", fillOpacity = 0.8, 
                   popup = ~paste(region, " - pop :", population)) %>%
  
  # On ajoute une ligne entre Île-de-France et l'Occitanie    
  addPolylines(lng = c(populationData[populationData$region=="Île-de-France","lng"], populationData[populationData$region=="Occitanie","lng"]),
             lat = c(populationData[populationData$region=="Île-de-France","lat"], populationData[populationData$region=="Occitanie","lat"]),
             color = "red", opacity = 0.5,
             weight = 2) %>%

  # On ajoute un marqueur sur Bretagne
  addMarkers(lng = populationData[populationData$region=="Bretagne","lng"],
             lat = populationData[populationData$region=="Bretagne","lat"],
             popup = "Bretagne")

# On affiche la carte
leafletMap

############################
### Programme interactif ###
############################

library(shiny)

# Assurez-vous que vos données sont préparées correctement pour cette application.

# Frontend
ui <- fluidPage(
  tags$header(
    tags$h1("Population des régions françaises par année", style = "text-align: center;")
  ),
  # On met une Disposition de la page avec un panneau latéral et un panneau principal
  sidebarLayout(
    # Panneau latéral
    sidebarPanel(
      # On ajoute un slider
      yearSlider <- sliderInput("yearSlider", "Année:", min = 1900, max = 2023, value = 1990)
    ),
    # Panneau principal
    mainPanel(
      # On ajoute la carte
      leafletOutput("map")
    )
  )
)

# Backend
server <- function(input, output) {
  # Fonction de rendu en fonction des inputs pour la carte
  output$map <- renderLeaflet({
    # On choisit une année à afficher (identique à la partie statique sauf input$yearSlider)
    year <- input$yearSlider
    populationRegionsYear <- populationRegions[,c(as.character(paste0("X",year)),"region"), drop=FALSE]
    
    # On merge les deux dataframes (identique à la partie statique)
    populationData <- merge(populationRegionsYear, coordinatesRegions, by = "region")
    colnames(populationData) <- c("region","population","lng","lat")
    
    # Création de la carte avec leaflet (identique à la partie statique)
    leaflet(populationData) %>%
      
      # On ajoute le fond de carte
      addTiles(urlTemplate = TILES_URL) %>%
      
      # On ajoute des cercles bleus liés à la population
      addCircleMarkers(lng = ~lng,
                       lat = ~lat,
                       radius = ~sqrt(population) * circleMarkersRadius,
                       color = "#a3dca1", fillColor = "#3ae334", fillOpacity = 0.8, 
                       popup = ~paste(region, " - pop :", population)) %>%
      
      # On ajoute une ligne entre Île-de-France et l'Occitanie    
      addPolylines(lng = c(populationData[populationData$region=="Île-de-France","lng"], populationData[populationData$region=="Occitanie","lng"]),
                   lat = c(populationData[populationData$region=="Île-de-France","lat"], populationData[populationData$region=="Occitanie","lat"]),
                   color = "red", opacity = 0.5,
                   weight = 2) %>%
      
      # On ajoute un marqueur sur Bretagne
      addMarkers(lng = populationData[populationData$region=="Bretagne","lng"],
                 lat = populationData[populationData$region=="Bretagne","lat"],
                 popup = "Bretagne")
  })
}

shinyApp(ui = ui, server = server)
