# ========
# PACKAGES
# ========

# --- Installation des packages ---
packages = c("shiny", "ggplot2", "dplyr", "sf", "leaflet", "jsonlite", "httr", "shinyauthr", "shinyjs")
to_install = setdiff(packages, rownames(installed.packages()))
if (length(to_install)) install.packages(to_install, dependencies = TRUE)
invisible(lapply(packages, require, character.only = TRUE))

# --- Packages nécessaires ---
library(shiny)
library(ggplot2)
library(dplyr)
library(sf)
library(leaflet)
library(jsonlite)
library(httr)
library(shinyauthr)
library(shinyjs)

# =======
# MODULES
# =======

# --- Téléchargement PNG ---
downloadPlotUI = function(id, label = "Télécharger en PNG") {
  ns = NS(id)
  div(style="display:flex;justify-content:flex-end;margin-top:8px;",
      downloadButton(ns("download"), label, class="btn btn-primary"))
}

downloadPlotServer = function(id, plot_expr, filename="graph.png", width=1200, height=900, dpi=120) {
  moduleServer(id, function(input, output, session) {
    output$download = downloadHandler(
      filename = function() filename,
      content  = function(file) {
        p = plot_expr(); if (is.null(p)) return()
        ggplot2::ggsave(filename=file, plot=p, device="png", width=width/dpi, height=height/dpi, dpi=dpi, units="in")
      })
  })
}

# --- Export CSV ---
downloadDataUI = function(id, label = "Exporter en CSV") {
  ns = NS(id)
  div(style = "display:flex; justify-content:flex-end; margin-top:6px;",
      downloadButton(ns("download"), label, class = "btn btn-secondary"))
}

downloadDataServer = function(id, data_expr, filename = "data.csv") {
  moduleServer(id, function(input, output, session) {
    output$download = downloadHandler(
      filename = function() filename,
      content = function(file) {
        df = data_expr()
        if (is.null(df)) return()
        if (!is.data.frame(df)) df = as.data.frame(df)
        write.csv(df, file, row.names = FALSE)
      })
  })
}

# ====
# DATA
# ====

# --- Chargement des données ---
data = read.csv(
  "https://raw.githubusercontent.com/bymatzo/SAE-R/refs/heads/main/app/data/data.csv",
  sep = ",", dec = "."
)

# --- Préparation des données --
# Convertit des colonnes en numérique
to_num = function(v) {
  if (is.factor(v)) v = as.character(v)
  v = gsub(",", ".", v)
  suppressWarnings(as.numeric(v))
}

# X/Y brutes pour la carte
x_raw = to_num(data$coordonnee_cartographique_x_ban)
y_raw = to_num(data$coordonnee_cartographique_y_ban)

# Filtrage des lignes valides
ok0 = is.finite(x_raw) & is.finite(y_raw)
data_pts = data[ok0, , drop = FALSE]

# Convertit les coordonnées en degrés (WGS84) si elles sont en Lambert 93
if (nrow(data_pts) > 0) {
  lon_like = all(abs(x_raw[ok0]) <= 180, na.rm = TRUE)
  lat_like = all(abs(y_raw[ok0]) <= 90,  na.rm = TRUE)
  if (lon_like && lat_like) {
    data_pts$lon = x_raw[ok0]
    data_pts$lat = y_raw[ok0]
  } else {
    pts_sf = sf::st_as_sf(
      data.frame(x = x_raw[ok0], y = y_raw[ok0]),
      coords = c("x","y"), crs = 2154
    )
    pts_wgs = sf::st_transform(pts_sf, 4326)
    xy = sf::st_coordinates(pts_wgs)
    data_pts$lon = xy[,1]
    data_pts$lat = xy[,2]
  }
}

# Convertit lettres DPE en chiffres (A=1 ... G=7)
niv = c("A","B","C","D","E","F","G")
data_pts$dpe_num = match(data_pts$etiquette_dpe, niv)

# Initialisation du centre de la vue
if (nrow(data_pts) > 0) {
  lng0 = mean(data_pts$lon, na.rm = TRUE)
  lat0 = mean(data_pts$lat, na.rm = TRUE)
} else {
  lng0 = 4.85
  lat0 = 45.75  
}

# Initialise ordre d'apparition des périodes
periodes = sort(unique(data$periode_construction))
periodes = c(periodes[grepl("^avant", periodes, ignore.case = TRUE)],
              periodes[!grepl("^avant", periodes, ignore.case = TRUE)])

# Table des utilisateurs
user_base <- data.frame(
  user = "Moi",
  password = "MotDePasse123",
  name = "Utilisateur"
)

# ==
# UI
# ==

ui = fluidPage(
  useShinyjs(),
  extendShinyjs(script = "switch.js", functions = c("addCSS","removeCSS")),
  tags$head(tags$link(rel="stylesheet", type="text/css", href="light.css")),
  div(
    id = "custom-login",
    class = "login-container",
    textInput("login_user", "Nom d'utilisateur :", placeholder = "Entrez votre identifiant"),
    passwordInput("login_password", "Mot de passe :", placeholder = "Entrez votre mot de passe"),
    actionButton("login_button", "Se connecter", class = "btn btn-primary"),
    div(id = "login_error", style = "color:red; margin-top:10px;")
  ),
  uiOutput("appUI")
)

# ======
# SERVER
# ======

# Initialisation des themes côté ggplot

theme_dark_custom = theme_minimal() +
  theme(
    plot.background = element_rect(fill="#0d1117", color=NA),
    panel.background = element_rect(fill="#0d1117", color=NA),
    panel.grid.major = element_line(color="#30363d"),
    panel.grid.minor = element_line(color="#30363d"),
    axis.text = element_text(color="white", size=12),
    axis.title = element_text(color="white", face="bold", size=14),
    plot.title = element_text(color="white", face="bold", size=18, hjust=0.5),
    legend.background = element_rect(fill="#0d1117"),
    legend.text = element_text(color="white"),
    legend.title = element_text(color="white")
  )

theme_light_custom = theme_minimal() +
  theme(
    plot.background = element_rect(fill="white", color=NA),
    panel.background = element_rect(fill="white", color=NA),
    axis.text = element_text(color="black", size=12),
    axis.title = element_text(color="black", face="bold", size=14),
    plot.title = element_text(color="black", face="bold", size=18, hjust=0.5)
  )

server = function(input, output, session) {
  
  #=============
  #"FAUX" SERVER
  #=============
  
  credentials = reactiveValues(user_auth = FALSE, user = NULL)
  
  observeEvent(input$login_button, {
    req(input$login_user, input$login_password)
    
    user_row = user_base[user_base$user == input$login_user & 
                           user_base$password == input$login_password, ]
    
    if (nrow(user_row) == 1) {
      credentials$user_auth = TRUE
      credentials$user = user_row$name
      shinyjs::hide("custom-login")
    } else {
      output$login_error <- renderText("Identifiant ou mot de passe incorrect.")
    }
  })
  
  # Theme
  current_theme = reactiveVal("light")
  ggtheme = ggtheme = reactive({
    if (current_theme() == "dark") theme_dark_custom else theme_light_custom
  })
  
  observeEvent(input$switch_theme, {
    if (current_theme() == "light") {
      js$addCSS("dark.css")
      current_theme("dark")
    } else {
      js$removeCSS()
      js$addCSS("light.css")
      current_theme("light")
    }
  })
  
  output$appUI = renderUI({
    req(credentials$user_auth)
    navbarPage(
      "Consommation énergetique dans le Rhône",
      
      tabPanel(
        "Répartition DPE",
        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-card",
              h4("Options de filtrage"),
              selectInput(
                inputId = "energie",
                label = "Type d’énergie principale :",
                choices = sort(unique(data$type_energie_principale_chauffage)),
                selected = unique(data$type_energie_principale_chauffage)[1]
              ),
              selectInput(
                inputId = "type_batiment",
                label = "Type de bâtiment :",
                choices = c("Tous", sort(unique(data$type_batiment))),
                selected = "Tous"
              ),
              selectInput(
                inputId = "periode_construction",
                label = "Période de construction :",
                choices = c("Tous", periodes),
                selected = "Tous"
              ),
              checkboxGroupInput(
                inputId = "dpe_filtre",
                label = "Étiquettes DPE à afficher :",
                choices = c("A","B","C","D","E","F","G"),
                selected = c("A","B","C","D","E","F","G")
              ),
              hr(),
              h5(tags$strong("Description :")),
              p("Ce graphique montre la répartition en pourcentage des classes DPE, 
              pour le type d’énergie, le type de bâtiment, 
              la période de construction et les étiquettes DPE sélectionnées.")
            )
          ),
          mainPanel(
            plotOutput("graphique_dpe", height = "500px"),
            downloadPlotUI("dl_dpe"),
            downloadDataUI("dl_dpe_data")
          )
        )
      ),
      
      tabPanel(
        "Émissions de CO2",
        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-card",
              selectInput(
                inputId = "type_batiment_boxplot",
                label = "Type de bâtiment :",
                choices = c("Tous", sort(unique(data$type_batiment))),
                selected = "Tous"
              ),
              selectInput(
                inputId = "periode_construction_boxplot",
                label = "Période de construction :",
                choices = c("Tous", periodes),
                selected = "Tous"
              ),
              hr(),
              uiOutput("kpi_moy_co2"),
              uiOutput("kpi_med_co2"),
              hr(),
              h5(tags$strong("Description :")),
              p("Ce graphique montre la distribution des émissions de CO2, 
               pour le type de bâtiment et la période de construction 
               sélectionnés, avec une comparaison entre les différents 
               types d’énergie.")
            )
          ),
          mainPanel(
            plotOutput("graphique_boxplot", height = "550px"),
            downloadPlotUI("dl_boxplot"),
            downloadDataUI("dl_boxplot_data")
          )
        )
      ),
      
      tabPanel(
        "Coût du chauffage",
        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-card",
              selectInput(
                inputId = "energie_cout",
                label = "Type d’énergie principale :",
                choices = sort(unique(data$type_energie_principale_chauffage)),
                selected = unique(data$type_energie_principale_chauffage)[1]
              ),
              selectInput(
                inputId = "type_batiment_cout",
                label = "Type de bâtiment :",
                choices = c("Tous", sort(unique(data$type_batiment))),
                selected = "Tous"
              ),
              selectInput(
                inputId = "periode_construction_cout",
                label = "Période de construction :",
                choices = c("Tous", periodes),
                selected = "Tous"
              ),
              sliderInput(
                "filtre_cout_max",
                "Limiter le coût du chauffage :",
                min = 0,
                max = 10000,
                value = 10000
              ),
              hr(),
              uiOutput("kpi_moy"),
              uiOutput("kpi_med"),
              hr(),
              h5(tags$strong("Description :")),
              p("Ce graphique montre la répartition du coût annuel de 
               chauffage, pour le type d’énergie, le type de bâtiment 
               et la période de construction sélectionnés, avec 
               suppression des valeurs extrêmes.")
            )
          ),
          mainPanel(
            plotOutput("graphique_histogramme", height = "550px"),
            downloadPlotUI("dl_histogramme"),
            downloadDataUI("dl_histogramme_data")
          )
        )
      ),
      
      tabPanel(
        "Conso vs Émission",
        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-card",
              selectInput(
                "var_x",
                "Variable X :",
                choices = c(
                  "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                  "Émissions CO2 (kgCO2/m²/an)" = "emission_ges_5_usages_par_m2",
                  "Surface habitable" = "surface_habitable_logement"
                ),
                selected = "conso_5_usages_par_m2_ep"
              ),
              selectInput(
                "var_y",
                "Variable Y :",
                choices = c(
                  "Émissions CO2 (kgCO2/m²/an)" = "emission_ges_5_usages_par_m2",
                  "Consommation (kWh/m²/an)" = "conso_5_usages_par_m2_ep",
                  "Surface habitable" = "surface_habitable_logement"
                ),
                selected = "emission_ges_5_usages_par_m2"
              ),
              hr(),
              selectInput(
                inputId = "energie_scatter",
                label = "Type d’énergie principale :",
                choices = sort(unique(data$type_energie_principale_chauffage)),
                selected = unique(data$type_energie_principale_chauffage)[1]
              ),
              selectInput(
                inputId = "type_batiment_scatter",
                label = "Type de bâtiment :",
                choices = c("Tous", sort(unique(data$type_batiment))),
                selected = "Tous"
              ),
              selectInput(
                inputId = "periode_construction_scatter",
                label = "Période de construction :",
                choices = c("Tous", periodes),
                selected = "Tous"
              ),
              hr(),
              uiOutput("kpi_corr"),
              hr(),
              h5(tags$strong("Description :")),
              p("Ce graphique montre la relation entre les deux variables 
               sélectionnées, pour le type d’énergie, le type de bâtiment 
               et la période de construction sélectionnés, avec suppression
               des valeurs extrêmes et affichage de la corrélation entre les variables.")
            )
          ),
          mainPanel(
            plotOutput("graphique_scatter", height = "550px"),
            downloadPlotUI("dl_scatter"),
            downloadDataUI("dl_scatter_data")
          )
        )
      ),
      
      tabPanel(
        "Carte",
        sidebarLayout(
          sidebarPanel(
            div(
              class = "sidebar-card",
              checkboxGroupInput(
                inputId = "energie_carte",
                label   = "Type d’énergie principale :",
                choices = sort(unique(data$type_energie_principale_chauffage)),
                selected = sort(unique(data$type_energie_principale_chauffage))
              ),
              selectInput(
                inputId = "cp_carte",
                label   = "Code postal :",
                choices = sort(unique(data_pts$code_postal_ban)),
                selected = sort(unique(data_pts$code_postal_ban))[1]
              ),
              hr(),
              h5(tags$strong("Description :")),
              p("Cette carte montre la localisation des logements, pour le type 
              d’énergie et le code postal sélectionnés."),
              hr(),
              tags$img(
                src = "logo_rhone.png",
                class = "sidebar-image"
              )
            )
          ),
          mainPanel(
            leafletOutput("map_rhone", height = "650px")
          )
        )
      ),
      
      tabPanel(
        "Contexte",
        fluidPage(
          div(
            class = "context-card",
            h2("Contexte du projet", class = "context-title"),
            p("Ce projet vise à analyser la performance énergétique des logements dans le département du Rhône à partir des données issues des Diagnostics de Performance Énergétique (DPE). L’objectif est de comprendre dans quelle mesure le type d’énergie utilisée pour le chauffage influence les performances énergétiques, les émissions de CO₂, la consommation énergétique et des indicateurs complémentaires comme le coût du chauffage ou la surface des logements."),
            p("Cette étude s’appuie sur un ensemble de données fournies par l’Ademe, contenant des informations telles que :"),
            tags$ul(
              tags$li("le type d’énergie principale de chauffage"),
              tags$li("la classe DPE attribuée (A à G)"),
              tags$li("les émissions de CO₂"),
              tags$li("la consommation énergétique"),
              tags$li("la période de construction"),
              tags$li("les caractéristiques du logement"),
              tags$li("les coordonnées géographiques"),
              tags$li("le coût annuel de chauffage")
            )
          ),
          br(),
          div(
            class = "context-card",
            h3("Aperçu des données", class = "context-title"),
            img(src = "data_preview.png",
                style = "width:95%; border-radius:10px; display:block; margin:auto;")
          )
        )
      ),
      
      tabPanel(
        "Paramètres",
        fluidPage(
          div(
            class = "param-panel",
            h3("Apparence", class = "param-title-left"),
            p("Personnalisez l'apparence générale de l'application :"),
            actionButton("switch_theme", "Basculer thème clair / sombre", class="btn btn-secondary")
          ),
          br(),
          div(
            class = "param-panel",
            h3("Données", class = "param-title-left"),
            p("Mettre à jour les données récupérées via l’API Ademe :"),
            actionButton("update_data_btn","Actualiser les données",class="btn btn-warning"),
            br(), br(),
            div(id="update_status", class="update-status",
                verbatimTextOutput("update_log")
            )
          )
        )
      )
    )
  })
  

  
  #=============
  #"VRAI" SERVER
  #=============
  
  # --- Actualisation des données ---
  observeEvent(input$update_data_btn, {
    
    shinyjs::html("update_status", "<div class='loading-msg'>Chargement des données...</div>")
    Sys.sleep(0.2)
    
    csv_url = "https://raw.githubusercontent.com/bymatzo/SAE-R/refs/heads/main/app/data/data.csv"
    old_data = read.csv(csv_url, colClasses = "character")
    old_data$date_etablissement_dpe = as.Date(old_data$date_etablissement_dpe)
    
    cp_list = unique(old_data$code_postal_ban)
    max_date = max(old_data$date_etablissement_dpe, na.rm = TRUE)
    
    base_url = "https://data.ademe.fr/data-fair/api/v1/datasets/dpe03existant/lines"
    new_data = data.frame()
    
    select_fields = paste(
      "type_energie_principale_chauffage,etiquette_dpe,cout_chauffage,periode_construction",
      "surface_habitable_logement,type_batiment,date_etablissement_dpe,conso_5_usages_ep",
      "conso_5_usages_par_m2_ep,emission_ges_5_usages_par_m2,code_postal_ban",
      "coordonnee_cartographique_x_ban,coordonnee_cartographique_y_ban",
      sep = ","
    )
    
    for (cp in cp_list) {
      params = list(
        page = 1,
        size = 10000,
        select = select_fields,
        qs = paste0("code_postal_ban:", cp, " AND date_etablissement_dpe:[", max_date, " TO ", Sys.Date(), "]")
      )
      
      url_encoded = httr::modify_url(base_url, query = params)
      response = httr::GET(url_encoded)
      
      if (status_code(response) == 200) {
        content = jsonlite::fromJSON(rawToChar(response$content))
        if (!is.null(content$result)) {
          new_data = dplyr::bind_rows(new_data, as.data.frame(content$result))
        }
      }
    }
    
    if (nrow(new_data) == 0) {
      shinyjs::html("update_status",
                    "<div class='error-msg'> Aucune nouvelle donnée trouvée.</div>"
      )
      return()
    }
    
    new_data$date_etablissement_dpe = as.Date(new_data$date_etablissement_dpe)
    if ("_score" %in% colnames(new_data))
      colnames(new_data)[colnames(new_data) == "_score"] <- "X_score"
    
    merged = rbind(old_data, new_data)
    merged = merged[!duplicated(merged), ]
    data <<- merged
    
    shinyjs::html("update_status",
                  paste0("<div class='success-msg'>Mise à jour réussie !<br>Nouvelles lignes ajoutées : ",
                         nrow(new_data), "</div>")
    )
  })
  
  
  
  # ---Fonctions de filtrage ---
  filter_data = function(df, energie_sel, batiment_sel, periode_sel) {
    df = df %>% filter(type_energie_principale_chauffage == energie_sel)
    if (batiment_sel != "Tous") df = df %>% filter(type_batiment == batiment_sel)
    if (periode_sel != "Tous") df = df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  filter_data_boxplot = function(df, batiment_sel, periode_sel) {
    if (batiment_sel != "Tous") df = df %>% filter(type_batiment == batiment_sel)
    if (periode_sel != "Tous") df = df %>% filter(periode_construction == periode_sel)
    return(df)
  }
  
  # --- Graphique 1 : Barplot répartition DPE ---
  plot_dpe = reactive({
    df_filtre = filter_data(data, input$energie, input$type_batiment, input$periode_construction) %>%
      filter(etiquette_dpe %in% input$dpe_filtre) %>% 
      count(etiquette_dpe) %>%
      mutate(proportion = n / sum(n) * 100)
    
    df_filtre$etiquette_dpe = factor(
      df_filtre$etiquette_dpe,
      levels = c("A","B","C","D","E","F","G")
    )
    
    p = ggplot(df_filtre, aes(x = etiquette_dpe, y = proportion, fill = etiquette_dpe)) +
      geom_col(width = 0.7, color = "white", linewidth = 0.5) +
      geom_text(aes(label = paste0(round(proportion, 1), "%")),
                vjust = -0.5, size = 5, color = if (current_theme() == "dark") "white" else "black", 
                fontface = "bold") +
      scale_fill_manual(
        name = "Etiquette DPE",
        values = c(
        "A" = "#009E3D","B" = "#6DBE45","C" = "#FFF200",
        "D" = "#F7A600","E" = "#E87511","F" = "#E30613","G" = "#B60000"
      )) +
      labs(
        title = paste("Répartition des classes DPE selon le type d’énergie"),
        x = "Classe DPE",
        y = "Proportion (%)"
      ) +
      ggtheme() +
      ylim(0, 100)
    
    return(p)
  })
  
  output$graphique_dpe = renderPlot(plot_dpe())
  downloadPlotServer("dl_dpe", plot_expr = plot_dpe, filename = "dpe.png")
  downloadDataServer("dl_dpe_data",
                     data_expr = function() {
                       df = filter_data(data, input$energie, input$type_batiment, input$periode_construction)
                       df = df[df$etiquette_dpe %in% input$dpe_filtre, ]
                       df[, c("type_energie_principale_chauffage", "type_batiment","periode_construction", "etiquette_dpe")]
                     },filename = "dpe_data.csv")
  
  # --- Graphique 2 : Boxplot emission CO2 ---
  moyenne_co2 = reactiveVal(NA)
  mediane_co2 = reactiveVal(NA)
  
  plot_boxplot = reactive({
    df_filtre = filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
    df_filtre$emission_num = suppressWarnings(as.numeric(gsub(",", ".", df_filtre$emission_ges_5_usages_par_m2)))
    df_filtre = df_filtre[is.finite(df_filtre$emission_num), ]
    moyenne_co2(round(mean(df_filtre$emission_num), 1))
    mediane_co2(round(median(df_filtre$emission_num), 1))
    ggplot(df_filtre, aes(x = type_energie_principale_chauffage, y = emission_num, fill = type_energie_principale_chauffage)) +
      geom_boxplot(outlier.colour = "red", alpha = 0.7) +
      ylim(0, 130) +
      labs(
        title = "Distribution des émissions de CO2 selon le type d’énergie",
        x = "Type d’énergie principale de chauffage",
        y = "Émissions de CO2 (kgCO2/m²/an)") +
      guides(fill = "none") +
      ggtheme()+
      theme(axis.text.x = element_text(angle = 45, hjust = 1))
  })
  
  output$graphique_boxplot = renderPlot(plot_boxplot())
  downloadPlotServer("dl_boxplot", plot_expr = plot_boxplot, filename = "boxplot_co2.png")
  downloadDataServer("dl_boxplot_data",
                     data_expr = function() {
                       df = filter_data_boxplot(data, input$type_batiment_boxplot, input$periode_construction_boxplot)
                       df$emission_num = suppressWarnings(as.numeric(gsub(",", ".", df$emission_ges_5_usages_par_m2)))
                       df = df[is.finite(df$emission_num), ]
                       df[, c("type_energie_principale_chauffage", "emission_num","type_batiment", "periode_construction")]
                     },filename = "co2_data.csv")
  
  output$kpi_moy_co2 = renderUI({HTML(paste("Moyenne : <b>", moyenne_co2(), " kgCO2/m²/an</b>"))})
  output$kpi_med_co2 = renderUI({HTML(paste("Médiane : <b>", mediane_co2(), " kgCO2/m²/an</b>"))})
  
  # --- Graphique 3 : Histogramme coût du chauffage ---
  moyenne_cout = reactiveVal(NA)
  mediane_cout = reactiveVal(NA)
  
  plot_histogramme = reactive({
    df_filtre = filter_data(
      data,
      input$energie_cout,
      input$type_batiment_cout,
      input$periode_construction_cout
    )
    
    x = df_filtre$cout_chauffage
    if (is.factor(x)) x = as.character(x)
    x = gsub(",", ".", x)
    x = suppressWarnings(as.numeric(x))
    
    ok = is.finite(x)
    df_filtre = df_filtre[ok, , drop = FALSE]
    x = x[ok]
    
    if (!is.null(input$filtre_cout_max))
      x = x[x <= input$filtre_cout_max]
    
    seuil_95 = quantile(x, 0.95, na.rm = TRUE)
    x2 = x[x <= seuil_95]
    moyenne_cout(round(mean(x2), 1))
    mediane_cout(round(median(x2), 1))
    
    df_filtre2 = df_filtre[df_filtre$cout_chauffage <= seuil_95, , drop = FALSE]
    df_filtre2$cout_num = x2
    
    ggplot(df_filtre2, aes(x = cout_num)) +
      geom_histogram(bins = 30, fill = "#2E86AB", color = "white", alpha = 0.8) +
      labs(title = paste("Répartition du coût annuel de chauffage"),
           x = "Coût du chauffage (€ / an)",
           y = "Nombre de logements") +
      ggtheme()
  })
  
  output$graphique_histogramme = renderPlot(plot_histogramme())
  downloadPlotServer("dl_histogramme", plot_expr = plot_histogramme, filename = "histogramme_cout.png")
  downloadDataServer("dl_histogramme_data",
                     data_expr = function() {
                       df = filter_data(data, input$energie_cout, input$type_batiment_cout, input$periode_construction_cout)
                       df$cout_num = suppressWarnings(as.numeric(gsub(",", ".", df$cout_chauffage)))
                       df = df[is.finite(df$cout_num) & df$cout_num <= input$filtre_cout_max, ]
                       seuil = quantile(df$cout_num, 0.95, na.rm = TRUE)
                       df = df[df$cout_num <= seuil, ]
                       df[, c("type_energie_principale_chauffage", "type_batiment", "periode_construction", "cout_num")]
                      },filename = "cout_chauffage_data.csv")
  
  output$kpi_moy = renderUI({HTML(paste("Moyenne : <b>", moyenne_cout(), "€</b>"))})
  output$kpi_med = renderUI({HTML(paste("Médiane : <b>", mediane_cout(), "€</b>"))})
  
  # --- Graphique 4 : Nuage de points consommation vs émission ---
  correlation_scatter = reactiveVal(NA)
  
  # Dictionnaire de noms propres
  pretty_names = list(
    "conso_5_usages_par_m2_ep"      = "Consommation (kWh/m²/an)",
    "emission_ges_5_usages_par_m2"  = "Émissions de CO2 (kgCO2/m²/an)",
    "surface_habitable_logement"    = "Surface habitable (m²)"
  )
  
  plot_scatter = reactive({
    df_filtre = filter_data(
      data,
      input$energie_scatter,
      input$type_batiment_scatter,
      input$periode_construction_scatter
    )
    
    cx = suppressWarnings(as.numeric(gsub(",", ".", df_filtre[[input$var_x]])))
    cy = suppressWarnings(as.numeric(gsub(",", ".", df_filtre[[input$var_y]])))
    
    ok = is.finite(cx) & is.finite(cy)
    cx = cx[ok]; cy = cy[ok]
    
    seuil_x = quantile(cx, 0.95, na.rm = TRUE)
    seuil_y = quantile(cy, 0.95, na.rm = TRUE)
    
    ok2 = cx <= seuil_x & cy <= seuil_y
    cx2 = cx[ok2]; cy2 = cy[ok2]
    
    # labels propres
    x_label = pretty_names[[input$var_x]]
    y_label = pretty_names[[input$var_y]]
    
    correlation_scatter(round(cor(cx2, cy2), 3))
    df_plot = data.frame(x = cx2, y = cy2)
    
    ggplot(df_plot, aes(x = x, y = y)) +
      geom_point(color = "#E74C3C", alpha = 0.7) +
      geom_smooth(method = "lm", se = FALSE, color = "blue") +
      labs(
        title = paste("Relation entre", x_label, "et", y_label),
        x = x_label,
        y = y_label
      ) +
      ggtheme()
  })
  
  
  
  output$graphique_scatter = renderPlot(plot_scatter())
  downloadPlotServer("dl_scatter", plot_expr = plot_scatter, filename = "scatter_conso_co2.png")
  downloadDataServer("dl_scatter_data",
                     data_expr = function() {
                       df = filter_data(data, input$energie_scatter, input$type_batiment_scatter, input$periode_construction_scatter)
                       df$x = suppressWarnings(as.numeric(gsub(",", ".", df[[input$var_x]])))
                       df$y = suppressWarnings(as.numeric(gsub(",", ".", df[[input$var_y]])))
                       df = df[is.finite(df$x) & is.finite(df$y), ]
                       seuil_x = quantile(df$x, 0.95, na.rm = TRUE)
                       seuil_y = quantile(df$y, 0.95, na.rm = TRUE)
                       df = df[df$x <= seuil_x & df$y <= seuil_y, ]
                       df[[input$var_x]] = df$x
                       df[[input$var_y]] = df$y
                       df[, c("type_energie_principale_chauffage","type_batiment","periode_construction", input$var_x, input$var_y)]
                     }, filename = "scatter_data.csv")
  
  output$kpi_corr = renderUI({HTML(paste("Corrélation : <b>", correlation_scatter(), "</b>"))})

  # --- Carte ---

  output$map_rhone = leaflet::renderLeaflet({
    m = leaflet::leaflet(options = leaflet::leafletOptions(zoomControl = TRUE))
    m = leaflet::addProviderTiles(m, providers$CartoDB.Positron)
    m = leaflet::setView(m, lng = lng0, lat = lat0, zoom = 9)
    m
  })
  
  observeEvent({
    input$map_rhone_bounds
    input$energie_carte
    input$cp_carte
  },{
    req(input$energie_carte, input$cp_carte)
    
    df = data_pts %>%
      mutate(energie_norm = tolower(trimws(type_energie_principale_chauffage))) %>%
      filter(energie_norm %in% tolower(trimws(input$energie_carte))) %>%
      filter(code_postal_ban == as.character(input$cp_carte))
    
    tmp = gsub(",", ".", df$conso_5_usages_par_m2_ep)
    df$val = suppressWarnings(as.numeric(tmp))
    
    ok = is.finite(df$lon) & is.finite(df$lat) & is.finite(df$val)
    df = df[ok,]
    
    seuil = quantile(df$val, 0.95, na.rm = TRUE)
    df = df[df$val <= seuil,]
    
    pal = colorNumeric(
      palette = c("#f7fbff", "#6baed6", "#2171b5", "#08306b"),
      domain = df$val
    )
    
    m = leafletProxy("map_rhone")
    m %>% clearGroup("pts") %>% clearControls()
    
    cols = pal(df$val)
    
    m %>% addCircleMarkers(
      lng=df$lon, lat=df$lat, radius=4,
      fillColor=cols, fillOpacity=0.9, stroke=FALSE,
      options=list(val=df$val, col=cols),
      group="pts",
      clusterOptions=markerClusterOptions(
        spiderfyOnMaxZoom=TRUE, maxClusterRadius=40,
        iconCreateFunction=JS("
      function(c){
        var m = c.getAllChildMarkers();
        var cols = m.map(e => e.options.col);
        var col = cols[0];
        return new L.DivIcon({
          html:'<div style=\"background:'+col+';width:40px;height:40px;border-radius:50%;line-height:40px;color:#fff;font-weight:bold;text-align:center;\">'+c.getChildCount()+'</div>',
          className:'cl', iconSize:[40,40]
        });
      }
    ")
      ),
      popup=paste0("<b>",df$type_energie_principale_chauffage,"</b>",
                   "<br>CP : ",df$code_postal_ban,
                   "<br>Conso : ",round(df$val,1)," kWh/m²/an")
    )
    
    m %>% addLegend(
      position="bottomright",
      pal=pal,
      values=df$val,
      title="Consommation (kWh/m²/an)"
    )
    
  })
}  


# =========
# LANCEMENT
# =========

shinyApp(ui = ui, server = server)
