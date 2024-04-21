library(shiny)
library(DT)
library(dplyr)
library(estadistica)
library(shinythemes)
library(tidyr)
library(tidyverse)
library(readxl)

ui <- navbarPage(
  theme = shinytheme("united"),  
  title = div(
    tags$img(src = "logobo.jpg", height = 25, width = 35,style = "margin-right: 10px;"),
    HTML("<b>La magia de... estadistica</b>")
  ),
  tabPanel("Inicio",
           h2("Bienvenido a tu aplicación de Estadística"),
           p(style="text-align: justify;","Esta aplicación ha sido diseñada para brindar una herramienta práctica y educativa en el mundo de la estadística descriptiva. Desarrollada con el objetivo de apoyar el proceso de enseñanza-aprendizaje, esta aplicación se basa en el paquete de R llamado",strong("'estadistica'"),", creado por el equipo liderado por el", strong("Dr. Vicente Coll-Serrano, la Dra. Rosario Martínez Verdú y la Dra. Cristina Pardo García"),"de la Universidad de Valencia (España)."),
           p(style="text-align: justify;","El paquete 'estadistica' abarca una amplia gama de conceptos básicos estudiados en cursos introductorios de estadística. Incluye tanto funciones de estadística descriptiva como de estadística inferencial, esta aplicación te permitirá explorar y comprender diversos aspectos de la estadística de manera interactiva y dinámica."),
           p(style="text-align: justify;","Esta aplicación permite a los usuarios aproximarse de una forma más práctica e intuitiva a las principales funciones de",strong("estadística descriptiva"), "que se encuentran en el paquete. Se incluyen medidas de centralización, medidas de dispersión, medidas de forma, relación entre variables, tablas de frecuencias y series temporales."),
           p(style="text-align: justify;","Te invitamos a sumergirte en el fascinante mundo de los datos, utilizando herramientas visuales y funciones analíticas que facilitarán tu comprensión y aplicación de conceptos estadísticos clave. ¡Comienza tu viaje ahora mismo y lleva tus habilidades estadísticas al siguiente nivel con nuestra aplicación de Estadística!"),
           p("A continuación, encontrarás algunos enlaces útiles relacionados con el paquete 'estadistica':"),
           tags$ul(
             # Enlace al sitio web
             tags$li(
               HTML("<a href='https://www.uv.es/estadistic/' target='_blank'>Sitio web</a>")
             ),
             # Enlace al canal de YouTube
             tags$li(
               HTML("<a href='https://www.youtube.com/channel/UCSE44FyVr87BEFshZAi4voQ' target='_blank'>Canal YouTube</a>")
             ),
             # Enlace a la documentación del paquete 'Estadistica'
             tags$li(
               HTML("<a href='https://cran.r-project.org/web/packages/estadistica/estadistica.pdf' target='_blank'>Documentación del paquete 'Estadistica'</a>")))),
  tabPanel("Análisis",
           tabsetPanel(
             tabPanel("Medidas de Posición",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file_centralizacion", "Cargar archivo (csv,xlsx,RData)",
                                    buttonLabel = "Buscar fichero",
                                    placeholder = "Ningún archivo seleccionado"),
                          selectInput("variable_centralizacion", "Variable:",
                                      choices = NULL),
                          checkboxGroupInput("centralizacion", "Medidas de tendencia central:",
                                             choices = c("Media" = "media",
                                                         "Mediana" = "mediana",
                                                         "Moda" = "moda")),
                          checkboxGroupInput("no_centralizacion", "Medidas de tendencia no central:",
                                             choices = c("Cuantiles" = "cuantiles")),
                          conditionalPanel(
                            condition = "input.no_centralizacion.includes('cuantiles')",
                            textInput("cuantiles_input", "Cortes para Cuantiles:", value = "0.25, 0.5, 0.75"),
                            tags$small("Los cortes deben estar en orden ascendente.")
                          ),conditionalPanel(
                            condition = "input.centralizacion.includes('media') || input.centralizacion.includes('mediana') || input.centralizacion.includes('moda')||input.no_centralizacion.includes('cuantiles')",
                            selectInput("pesos_var2", "Variable de pesos:",
                                        choices = NULL)
                          ),
                          actionButton("calcular_centralizacion", "Calcular")
                        ),
                        mainPanel(br(" "),
                                  tableOutput("primeras_filas_centralizacion"),
                                  conditionalPanel(condition = "input.calcular_centralizacion > 0",
                                                   h4(HTML("<b>Medidas de Posición</b>")),
                                                   br(" "),
                                                   conditionalPanel(condition = "input.centralizacion.includes('media') || input.centralizacion.includes('mediana') || input.centralizacion.includes('moda')",
                                                                    h4("Medidas de tendencia central"),
                                                                    tableOutput("data_centralizacion")),
                                                   conditionalPanel(condition = "input.no_centralizacion.includes('cuantiles')",
                                                                    h4("Medidas de tendencia no central"),
                                                                    tableOutput("data_cuantiles"))
                                  ))
                      )),tabPanel("Medidas de Dispersión",
                                  sidebarLayout(
                                    sidebarPanel(
                                      fileInput("file_dispersion", "Cargar archivo (csv,xlsx,RData)",
                                                buttonLabel = "Buscar fichero",
                                                placeholder = "Ningún archivo seleccionado"),
                                      selectInput("variable_dispersion", "Variable:",
                                                  choices = NULL),
                                      checkboxGroupInput("dispersion", "Medidas de dispersión:",
                                                         choices = c("Varianza" = "varianza",
                                                                     "Desviación Típica" = "desviacion",
                                                                     "Coeficiente de Variación" = "coeficiente.variacion",
                                                                     "Momento Central" = "momento.central"
                                                         )),
                                      conditionalPanel(
                                        condition = "input.dispersion.includes('varianza') || input.dispersion.includes('desviacion') || input.dispersion.includes('coeficiente.variacion')",
                                        selectInput("pesos_var", "Variable de pesos:",
                                                    choices = NULL)
                                      ),
                                      conditionalPanel(
                                        condition = "input.dispersion.includes('momento.central')",
                                        numericInput("orden_momento_central", "Orden del momento central:", value = NULL)
                                      ),
                                      conditionalPanel(
                                        condition = "input.dispersion.indexOf('varianza') !== -1 || input.dispersion.indexOf('desviacion') !== -1 || input.dispersion.indexOf('coeficiente.variacion') !== -1",
                                        selectInput("tipo_calculo",label = HTML("Tipo cálculo <small style='color: #888;'>(var., desv. tip., coef. var.)</small>"),
                                                    choices = c("Muestral" = "muestral", "Cuasi" = "cuasi"))
                                      ),
                                      actionButton("calcular_dispersion", "Calcular")
                                    ),
                                    mainPanel(
                                      br(" "),
                                      tableOutput("primeras_filas_dispersion"),
                                      conditionalPanel(
                                        condition = "input.calcular_dispersion > 0",
                                        h4("Medidas de Dispersión"),
                                        tableOutput("data_dispersion"))))),
             tabPanel("Medidas de forma",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file_forma", "Cargar archivo (csv,xlsx,RData)",
                                    buttonLabel = "Buscar fichero",
                                    placeholder = "Ningún archivo seleccionado"),
                          selectInput("variable_forma", "Variable:",
                                      choices = NULL),
                          checkboxGroupInput("forma", "Medidas de forma:",
                                             choices = c("Asimetria y Curtosis" = "medidas_forma")),
                          conditionalPanel(
                            condition = "input.forma.includes('medidas_forma')",
                            selectInput("pesos_var3", "Variable de pesos:",
                                        choices = NULL),
                            checkboxInput("alternativa",label = HTML("<div style='text-align: justify;'>Mostrar alternativa <small style='color: #888;'>(Muestra el coeficiente de asimetría y curtosis calculado según SPSS y EXCEL. Se facilita también los correspondientes errores típicos)</small></div>"), FALSE)
                          ),
                          actionButton("calcular_forma", "Calcular")
                        ),
                        mainPanel(br(" "),
                                  tableOutput("primeras_filas_forma"),  
                                  conditionalPanel(condition = "input.calcular_forma > 0",
                                                   h4("Medidas de forma"),
                                                   tableOutput("data_forma"),
                                                   tableOutput("data_forma_alternativa"))))),
             tabPanel("Relación entre variables",
                      sidebarLayout(
                        sidebarPanel(
                          fileInput("file_relacion", "Cargar archivo (csv,xlsx,RData)",
                                    buttonLabel = "Buscar fichero",
                                    placeholder = "Ningún archivo seleccionado"),
                          h4("Matrices de covarianza y correlación"),
                          actionButton("calcular_matriz_correlacion", "Calcular matriz de correlación"),
                          br(" "),
                          selectInput("tipo_covarianza2",label = HTML("Tipo cálculo <small style='color: #888;'>(matriz.covar)</small>")
                                      ,choices = c("Muestral" = "muestral", "Cuasi" = "cuasi")),
                          actionButton("calcular_matriz_covarianzas", "Calcular matriz de covarianza"),
                          br(" "),
                          h4("Covarianza y correlación"),
                          selectInput("var1_relacion", "Variable 1:", choices = NULL),
                          selectInput("var2_relacion", "Variable 2:", choices = NULL),
                          selectInput("tipo_covarianza1",label = HTML("Tipo cálculo <small style='color: #888;'>(covar)</small>"),
                                      choices = c("Muestral" = "muestral", "Cuasi" = "cuasi")),
                          selectInput("pesos_var4", "Variable de pesos:",
                                      choices = NULL),
                          actionButton("calcular_correlacion", "Calcular correlación"),
                          br(" "),
                          actionButton("calcular_covarianza", "Calcular covarianza"),
                          br(" "), 
                          h4("Regresión lineal simple"),
                          selectInput("var_independiente_regresion_simple", "Variable independiente:", choices = NULL),
                          selectInput("var_dependiente_regresion_simple", "Variable dependiente:", choices = NULL),
                          numericInput("confianza_regresion_simple", "Confianza:", value = 0.95, min = 0, max = 1, step = 0.01),
                          checkboxInput("inferencia_regresion_simple", "Resultados inferenciales", value = FALSE),
                          checkboxInput("grafico_regresion_simple", "Mostrar gráfico", value = FALSE),
                          actionButton("calcular_regresion_simple", "Regresión Simple")
                        ),
                        mainPanel(
                          br(" "),
                          tableOutput("primeras_filas_relacion"),
                          conditionalPanel(
                            condition = "input.calcular_matriz_correlacion > 0",
                            h4("Matriz de correlación"),  
                            DTOutput("matriz_correlacion_output")
                          ),
                          conditionalPanel(
                            condition = "input.calcular_matriz_covarianzas > 0",
                            br(" "),
                            h4("Matriz de covarianza"),  
                            DTOutput("matriz_covarianzas_output")
                          ),
                          conditionalPanel(
                            condition = "input.calcular_correlacion > 0",
                            br(" "),
                            h4("Correlación entre variables"),
                            verbatimTextOutput("correlacion_output")
                          ),
                          conditionalPanel(
                            condition = "input.calcular_covarianza > 0",
                            h4("Covarianza entre variables"),
                            verbatimTextOutput("covarianza_output")
                          ),
                          conditionalPanel(
                            condition = "input.calcular_regresion_simple > 0",
                            br(" "),
                            h4("Regresión Simple"),
                            DTOutput("regresion_simple_output"),
                            conditionalPanel(
                              condition = "input.inferencia_regresion_simple",
                              h4("Resumen"),
                              DTOutput("regresion_simple_output_parcial"),
                              br(" "),
                              h4("ANOVA"),
                              DTOutput("regresion_simple_output_ANOVA"),
                              br(" "),
                              h4("Modelo estimado"),
                              DTOutput("regresion_simple_output_Modelo")),
                            br(" "),
                            conditionalPanel(
                              condition= "input.grafico_regresion_simple",
                              plotOutput("regresion_simple_plot")))))
             ),tabPanel("Tablas de frecuencias",
                        sidebarLayout(
                          sidebarPanel( fileInput("file_tablas_frecuencias", "Cargar archivo (csv,xlsx,RData)",
                                                  buttonLabel = "Buscar fichero",
                                                  placeholder = "Ningún archivo seleccionado"),
                                        tags$h4(style = "font-size: 16px; color: black;",
                                                tags$strong("Tabla de frecuencias bidimensionales")),
                                        selectInput("var_filas", "Variable para filas:",
                                                    choices = NULL),
                                        selectInput("var_columnas", "Variable para columnas:",
                                                    choices = NULL),
                                        selectInput("distribucion", "Tipo de distribución:",
                                                    choices = c("Cruzada" = "cruzada",
                                                                "Condicionada" = "condicionada")),
                                        selectInput("frecuencias", "Tipo de frecuencias:",
                                                    choices = c("Absolutas" = "absolutas",
                                                                "Relativas" = "relativas")),
                                        conditionalPanel(
                                          condition = "input.distribucion == 'condicionada' && input.frecuencias == 'relativas'",
                                          numericInput("tipo", "Condicionada por columnas(1) o filas(2):", value = 1)
                                        ),
                                        actionButton("calcular_tablas_frecuencias", "Calcular"),
                                        tags$h4(style = "font-size: 16px; color: black;",
                                                tags$strong("Tabla de frecuencias unidimensional")),
                                        selectInput("var_tabla_unidimensional", "Variable para tabla unidimensional:", choices = NULL),
                                        checkboxInput("agrupar_checkbox", "No agrupar", value = FALSE),
                                        checkboxInput("grafico_checkbox", "Mostrar gráfico", value = FALSE),
                                        actionButton("calcular_tabla_unidimensional", "Calcular")
                          ),
                          mainPanel(
                            br(" "),
                            tableOutput("primeras_filas_tablas_frecuencias"),
                            conditionalPanel(
                              condition = "input.calcular_tablas_frecuencias > 0",
                              h4("Tabla de Frecuencias Bidimensional"),
                              tableOutput("tabla_frecuencias_bidimensional")
                            ),conditionalPanel(
                              condition = "input.calcular_tabla_unidimensional > 0",
                              h4("Tabla de Frecuencias Unidimensional"),
                              DTOutput("tabla_frecuencias_unidimensional"),
                              br(" "),
                              plotOutput("histograma")
                            )))),tabPanel("Series temporales",
                                          sidebarLayout(
                                            sidebarPanel(tags$h4(style = "font-size: 16px; color: black;",
                                                                 tags$strong("Serie temporal (Medias móviles centradas)")),
                                                         fileInput("file_series_temporales", "Cargar archivo (csv,xlsx,RData)",
                                                                   buttonLabel = "Buscar fichero",
                                                                   placeholder = "Ningún archivo seleccionado"),
                                                         selectInput("var_serie", "Variable:",
                                                                     choices = NULL),
                                                         numericInput("inicio_anual", "Año de inicio:", value = 1),
                                                         numericInput("periodo_inicio", "Periodo de inicio:", value = 1),
                                                         numericInput("frecuencia", label = HTML("<div style='text-align: justify;'>Frecuencia: <small style='color: #888; display: block;'>(Periodicidad de la serie)</small></div>"), value = 4),
                                                         numericInput("prediccion_tendencia", 
                                                                      label = HTML("<div style='text-align: justify;'>Valor para predicción de tendencia: <small style='color: #888; display: block;'>(t=0, origen de la serie)</small></div>"), 
                                                                      value=0),
                                                         checkboxInput("grafico_series", "Mostrar gráfico", value = FALSE),
                                                         actionButton("generar_series", "Analizar serie temporal",icon = icon("play"))),
                                            mainPanel(
                                              br(" "),
                                              tableOutput("primeras_observaciones_series"),
                                              conditionalPanel(
                                                condition = "input.generar_series > 0",
                                                h4("IVE (Índice Variación Estacional)"),
                                                tableOutput("ive_output"),
                                                h4("Modelo ajustado"),
                                                tableOutput("modelo_ajuste_output"),
                                                conditionalPanel(
                                                  condition= "input.prediccion_tendencia!=0",
                                                  h4("Pronósticos"),
                                                  tableOutput("pronosticos_output"),
                                                  p("( Para el cálculo de la predicción no se tiene en cuenta el efecto estacional)")),
                                                br(" "),
                                                conditionalPanel(
                                                  condition = "input.grafico_series > 0",
                                                  h4("Representación"),
                                                  tags$small(
                                                    tags$strong("negro"), ": valores observados de la serie",
                                                    tags$br(),
                                                    tags$span("rojo", style = "color: red;"), ": tendencia de la serie",
                                                    tags$br(),
                                                    tags$span("azul", style = "color: blue;"), ": ajuste lineal"
                                                  )),br(" "),
                                                plotOutput("grafico_output"))
                                            )
                                            
                                          )))),tabPanel("Resumen estadísticos descriptivos",
                                                        sidebarLayout(
                                                          sidebarPanel(
                                                            fileInput("file_data", "Cargar archivo (csv,xlsx,RData)",
                                                                      buttonLabel = "Buscar fichero",
                                                                      placeholder = "Ningún archivo seleccionado"),
                                                            actionButton("obtener_resumen", "Obtener resumen")
                                                          ),
                                                          mainPanel(
                                                            conditionalPanel(
                                                              condition = "input.obtener_resumen > 0",
                                                              br(" "),
                                                              DTOutput("tabla_resumen")
                                                            ),
                                                            conditionalPanel(
                                                              condition = "input.obtener_resumen == 0",
                                                              br(" "),
                                                              p(
                                                                style = "text-align: justify;",
                                                                "Esta función te permite obtener los principales estadísticos 
        descriptivos muestrales en un objeto de tipo data.frame.
        Los descriptivos que se obtienen son: media, mínimo, cuartil 1,
        mediana, cuartil 3, máximo, varianza muestral, desviación típica
        muestral, coeficiente de variación, recorrido intercuartílico,
        asimetría, curtosis y moda."))))),
  
  tabPanel("Video-tutoriales",
           h2("La magia de ... 'estadistica' (Youtube)"),
           p(style="text-align: justify;",strong("La magia de ... 'estadistica'"),"es un canal de Youtube de carácter didáctico puesto en marcha por los desarrolladores del paquete como parte del proyecto del paquete 'estadistica'."),
           p(style="text-align: justify;","En este canal, la", strong("Dra. Rosario Martínez Verdú"),"da voz y forma a un conjunto de video-tutoriales con el objetivo de enseñar todo lo necesario para dominar el paquete de R: 'estadistica'. Los video-tutoriales combinan un enfoque teorico-práctico el cual te permite no tan sólo conocer las funcionalidades del paquete sino también repasar contenidos estadísticos fundamentales."),
           p(style="text-align: justify;","Te invitamos a sumergirte en estos video-tutoriales para conocer el software y el paquete que se encuentran detrás de esta aplicación!"),
           p("A continuación, encontrarás los enlaces de los video-tutoriales de las funciones del paquete 'estadistica' que se incluyen en la aplicación:"),
           tags$ul(
             
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=AY-HxvaYbIk' target='_blank'>Media, mediana y moda</a>")
             ),
             
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=_CXzMoNcE_Q' target='_blank'>Cuantiles</a>")
             ),
             
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=A79byQNoeMc' target='_blank'>Varianza, desviación típica y coeficiente de variación</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=A79byQNoeMc' target='_blank'>Varianza, desviación típica y coeficiente de variación</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=J3YDXBfyJq8' target='_blank'>Coeficiente de asimetría y curtosis</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=RzWaJjnmD5Q' target='_blank'>Coeficiente de correlación lineal y matriz de correlación</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=91DtlUm7ve8' target='_blank'>Covarianza y matriz de varianzas-covarianzas</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=dwoh323v5M8' target='_blank'>Regresión lineal simple (ejemplo 1)</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=sQTPbUkAo6U' target='_blank'>Regresión lineal simple (ejemplo 2)</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=xLFUSLvAHoc' target='_blank'>Tabla de frecuencias</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=b1OgOQ8veKc&t=777s' target='_blank'>Series temporales (medias móviles)</a>")
             ),
             tags$li(
               HTML("<a href='https://www.youtube.com/watch?v=vCF4FzrBrEc' target='_blank'>Resumen de estadísticos descriptivos</a>")
             )
           )
  )
)

server <- function(input, output, session) {
  
  data_introducidos_centralizacion <- reactiveVal(NULL)
  data_introducidos_dispersion <- reactiveVal(NULL)
  data_introducidos_relacion <- reactiveVal(NULL)
  data_introducidos_forma <- reactiveVal(NULL)
  data_introducidos_tablas_frecuencias <- reactiveVal(NULL)
  data_introducidos_series <- reactiveVal(NULL)
  
  observeEvent(input$file_centralizacion, {
    if (tools::file_ext(input$file_centralizacion$datapath) == "csv") {
      data_introducidos_centralizacion(read.csv(input$file_centralizacion$datapath))
    } else if (tools::file_ext(input$file_centralizacion$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_centralizacion(read_excel(input$file_centralizacion$datapath))
    } else if (tools::file_ext(input$file_centralizacion$datapath) == "RData") {
      data_introducidos_centralizacion(readRDS(input$file_centralizacion$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "variable_centralizacion", choices = names(data_introducidos_centralizacion()))
    updateSelectInput(session, "pesos_var2", choices = c("NULL",names(data_introducidos_centralizacion())))
  })
  
  output$primeras_filas_centralizacion <- renderTable({
    if (!is.null(data_introducidos_centralizacion())) {
      return(head(data_introducidos_centralizacion(), 5))
    }
  })
  
  observeEvent(input$calcular_centralizacion, {
    req(input$file_centralizacion)
    req(input$variable_centralizacion)
    req(data_introducidos_centralizacion())
    
    if (!is.numeric(data_introducidos_centralizacion()[[input$variable_centralizacion]])) {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (!is.numeric(data_introducidos_centralizacion()[[input$pesos_var2]]) && input$pesos_var2 != "NULL") {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada como peso es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (input$variable_centralizacion==input$pesos_var2) {
      showModal(modalDialog(
        title = "Error",
        "La variable para el cálculo de la medida y la variable para los pesos no pueden coincidir."
      ))
      return(NULL)
    }
    
    data2 <- data_introducidos_centralizacion()
    
    output$primeras_filas_centralizacion <- renderTable(NULL)
    
    funciones_centralizacion <- list()
    nombres_centralizacion <- list()
    
    if ("media" %in% input$centralizacion) {
      funciones_centralizacion[["media"]] <- function(x) {
        if (input$pesos_var2=="NULL") {
          round(media(x,variable = which(names(x) == input$variable_centralizacion)), 2)
        } else {
          round(media(x, variable = which(names(x) == input$variable_centralizacion) , pesos = which(names(x) == input$pesos_var2)), 2)
        }
      }
      nombres_centralizacion[["media"]] <- "media"
    }
    
    if ("mediana" %in% input$centralizacion) {
      funciones_centralizacion[["mediana"]] <- function(x) {
        if (input$pesos_var2=="NULL") {
          round(mediana(x,variable = which(names(x) == input$variable_centralizacion)), 2)
        } else {
          round(mediana(x, variable = which(names(x) == input$variable_centralizacion) , pesos = which(names(x) == input$pesos_var2)), 2)
        }
      }
      nombres_centralizacion[["mediana"]] <- "mediana"
    }
    
    moda_form <- function(x,variable,pesos) {
      result <- moda(x,variable,pesos)
      modas <- strsplit(gsub("[c\\(\\)]", "", result), ", ")[[1]]
      resultado_final <- paste(modas, collapse = ", ")
      return(resultado_final)
    }
    
    if ("moda" %in% input$centralizacion) {
      if (input$pesos_var2 == "NULL") {
        funciones_centralizacion[["moda"]] <- function(x) moda_form(x, variable = which(names(x) == input$variable_centralizacion),pesos =NULL)
      } else {
        funciones_centralizacion[["moda"]] <- function(x) moda_form(x,variable = which(names(x) == input$variable_centralizacion), pesos = which(names(x) == input$pesos_var2))
      }
      nombres_centralizacion[["moda"]] <- "moda"
    }
    
    if ("cuantiles" %in% input$no_centralizacion) {
      cortes <- as.numeric(unlist(strsplit(input$cuantiles_input, ",\\s*")))
      output$data_cuantiles <- renderTable({
        if (input$pesos_var2 == "NULL") {
          res <- cuantiles(data2,variable = which(names(data2) == input$variable_centralizacion), cortes = cortes)
        } else {
          res <- cuantiles(data2,variable = which(names(data2) == input$variable_centralizacion), cortes = cortes, pesos = which(names(data2) == input$pesos_var2))
        }
        col_name <- paste("cuantiles", input$variable_centralizacion, sep = ".")
        df <- data.frame(Cuantil = paste0(cortes * 100, "%"), res)
        names(df)[2] <- col_name
        return(df)
      })
    } else {
      output$data_cuantiles <- renderTable(NULL)
    }
    
    "%notin%" <- function(x, table) {
      !(x %in% table)
    }
    
    if (length(funciones_centralizacion) == 0 && "cuantiles" %notin% input$no_centralizacion) {
      output$data_centralizacion <- renderTable(NULL)
      showModal(modalDialog(
        title = "Error",
        "Por favor, selecciona al menos una medida de Posición."
      ))
      
    } else {
      resultados_centralizacion <- lapply(names(funciones_centralizacion), function(nombre) {
        funcion2 <- funciones_centralizacion[[nombre]]
        nombre_funcion2 <- nombres_centralizacion[[nombre]]
        summarise(data2, 
                  resultado = funcion2(data2)) %>%
          mutate(Medida = nombre_funcion2)
      })
      
      data_centralizacion <- do.call(rbind, resultados_centralizacion)
      output$data_centralizacion <- renderTable(data_centralizacion)
    }
  })
  
  observeEvent(input$file_dispersion, {
    if (tools::file_ext(input$file_dispersion$datapath) == "csv") {
      data_introducidos_dispersion(read.csv(input$file_dispersion$datapath))
    } else if (tools::file_ext(input$file_dispersion$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_dispersion(read_excel(input$file_dispersion$datapath))
    } else if (tools::file_ext(input$file_dispersion$datapath) == "RData") {
      data_introducidos_dispersion(readRDS(input$file_dispersion$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "variable_dispersion", choices = names(data_introducidos_dispersion()))
    updateSelectInput(session, "pesos_var", choices = c("NULL", names(data_introducidos_dispersion())))
  })
  
  output$primeras_filas_dispersion <- renderTable({
    if (!is.null(data_introducidos_dispersion())) {
      return(head(data_introducidos_dispersion(), 5))
    }
  })
  
  observeEvent(input$calcular_dispersion, {
    req(input$file_dispersion)
    req(input$variable_dispersion)
    req(data_introducidos_dispersion())
    
    if (!is.numeric(data_introducidos_dispersion()[[input$variable_dispersion]])) {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (!is.numeric(data_introducidos_dispersion()[[input$pesos_var]]) && input$pesos_var != "NULL") {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada como peso es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (input$variable_dispersion==input$pesos_var) {
      showModal(modalDialog(
        title = "Error",
        "La variable para el cálculo de la medida y la variable para los pesos no pueden coincidir."
      ))
      return(NULL)
    }
    
    
    data <- data_introducidos_dispersion()
    
    output$primeras_filas_dispersion <- renderTable(NULL)
    
    funciones_dispersion <- list()
    nombres_dispersion <- list()
    
    
    if ("varianza" %in% input$dispersion) {
      tipo_calculo <- input$tipo_calculo
      funciones_dispersion[["varianza"]] <- function(x) {
        if (input$pesos_var=="NULL") {
          round(varianza(x,variable = which(names(x) == input$variable_dispersion), tipo = tipo_calculo), 2)
        } else {
          round(varianza(x, tipo = tipo_calculo, variable = which(names(x) == input$variable_dispersion) , pesos = which(names(x) == input$pesos_var)), 2)
        }
      }
      nombres_dispersion[["varianza"]] <- "Varianza"
    }
    
    if ("desviacion" %in% input$dispersion) {
      tipo_calculo <- input$tipo_calculo
      funciones_dispersion[["desviacion"]] <- function(x) {
        if (input$pesos_var=="NULL") {
          round(desviacion(x,variable = which(names(x) == input$variable_dispersion), tipo = tipo_calculo), 2)
        } else {
          round(desviacion(x, tipo = tipo_calculo, variable = which(names(x) == input$variable_dispersion) , pesos = which(names(x) == input$pesos_var)), 2)
        }
      }
      nombres_dispersion[["desviacion"]] <- "Desviación Típica"
    }
    
    if ("coeficiente.variacion" %in% input$dispersion) {
      tipo_calculo <- input$tipo_calculo
      funciones_dispersion[["coeficiente.variacion"]] <- function(x) {
        if (input$pesos_var=="NULL") {
          round(coeficiente.variacion(x,variable = which(names(x) == input$variable_dispersion), tipo = tipo_calculo), 2)
        } else {
          round(coeficiente.variacion(x, tipo = tipo_calculo, variable = which(names(x) == input$variable_dispersion) , pesos = which(names(x) == input$pesos_var)), 2)
        }
      }
      nombres_dispersion[["coeficiente.variacion"]] <- "Coeficiente de Variación"
    }
    
    if ("momento.central" %in% input$dispersion) {
      if (is.null(input$orden_momento_central)) {
        showModal(modalDialog(
          title = "Error",
          "Por favor, especifica el orden del momento central."
        ))
        return(NULL)
      }
      orden <- input$orden_momento_central
      funciones_dispersion[["momento.central"]] <- function(x) {
        round(momento.central(x[[input$variable_dispersion]], orden), 2)
      }
      nombres_dispersion[["momento.central"]] <- paste("Momento Central (Orden =", orden, ")")
    }
    
    if (length(funciones_dispersion) == 0) {
      output$data_dispersion <- renderTable(NULL)
      showModal(modalDialog(
        title = "Error",
        "Por favor, selecciona al menos una medida de Dispersión."
      ))
      
    } else {
      resultados_dispersion <- lapply(names(funciones_dispersion), function(nombre) {
        funcion <- funciones_dispersion[[nombre]]
        nombre_funcion <- nombres_dispersion[[nombre]]
        summarise(data, 
                  resultado = funcion(data)) %>%
          mutate(Medida = nombre_funcion)
      })
      
      data_dispersion <- do.call(rbind, resultados_dispersion)
      output$data_dispersion <- renderTable(data_dispersion)
    }
  })
  
  observeEvent(input$file_forma, {
    if (tools::file_ext(input$file_forma$datapath) == "csv") {
      data_introducidos_forma(read.csv(input$file_forma$datapath))
    } else if (tools::file_ext(input$file_forma$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_forma(read_excel(input$file_forma$datapath))
    } else if (tools::file_ext(input$file_forma$datapath) == "RData") {
      data_introducidos_forma(readRDS(input$file_forma$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "variable_forma", choices = names(data_introducidos_forma()))
    updateSelectInput(session, "pesos_var3", choices = c("NULL", names(data_introducidos_forma())))
    output$primeras_filas_forma <- renderTable({
      if (!is.null(data_introducidos_forma())) {
        return(head(data_introducidos_forma(), 5))
      }
    })
  })
  
  observeEvent(input$calcular_forma, {
    req(input$file_forma)
    req(input$variable_forma)
    req(data_introducidos_forma())
    
    if (!is.numeric(data_introducidos_forma()[[input$variable_forma]])) {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (!is.numeric(data_introducidos_forma()[[input$pesos_var3]]) && input$pesos_var3 != "NULL") {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada como peso es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (input$variable_forma == input$pesos_var3) {
      showModal(modalDialog(
        title = "Error",
        "La variable para el cálculo de la medida y la variable para los pesos no pueden coincidir."
      ))
      return(NULL)
    }
    
    data3 <- data_introducidos_forma()
    
    output$primeras_filas_forma <- renderTable(NULL)
    
    medidas_seleccionadas <- input$forma
    
    resultados <- list()
    
    if ("medidas_forma" %in% medidas_seleccionadas) {
      if (input$pesos_var3 == "NULL") {
        if (input$alternativa) {
          
          resultados <- medidas.forma(data3, variable = which(names(data3) == input$variable_forma),alternativa=TRUE)
        } else {
          medidas <- medidas.forma(data3, variable = which(names(data3) == input$variable_forma))
          asimetria <- medidas["asimetria",] 
          resultados[["Asimetria"]] <- asimetria 
          curtosis <- medidas["curtosis",]
          resultados[["Curtosis"]] <- curtosis
        }
      } else {
        if (input$alternativa) {
          
          medidas <- medidas.forma(data3, variable = which(names(data3) == input$variable_forma), pesos = which(names(data3) == input$pesos_var3))
          asimetria <- medidas["asimetria",] 
          resultados[["Asimetria"]] <- asimetria 
          curtosis <- medidas["curtosis",]
          resultados[["Curtosis"]] <- curtosis
          
        } else {
          medidas <- medidas.forma(data3, variable = which(names(data3) == input$variable_forma), pesos = which(names(data3) == input$pesos_var3))
          asimetria <- medidas["asimetria",] 
          resultados[["Asimetria"]] <- asimetria 
          curtosis <- medidas["curtosis",]
          resultados[["Curtosis"]] <- curtosis
        }
      }
    }
    
    if (length(resultados) == 0) {
      output$data_forma <- renderTable(NULL)
      output$data_forma_alternativa <- renderTable(NULL)
      showModal(modalDialog(
        title = "Error",
        "Por favor, selecciona al menos una medida de Forma."
      ))
    } else {
      if (input$alternativa && input$pesos_var3 == "NULL") {
        output$data_forma <- renderTable(NULL)
        output$data_forma_alternativa <- renderTable(resultados)
      } else{
        
        output$data_forma_alternativa <- renderTable(NULL)
        data_forma <- data.frame(
          Medida = names(resultados),
          Valor = unlist(resultados)
        )
        
        output$data_forma <- renderTable(data_forma)}}
    
  })
  
  observeEvent(input$file_relacion, {
    if (tools::file_ext(input$file_relacion$datapath) == "csv") {
      data_introducidos_relacion(read.csv(input$file_relacion$datapath))
    } else if (tools::file_ext(input$file_relacion$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_relacion(read_excel(input$file_relacion$datapath))
    } else if (tools::file_ext(input$file_relacion$datapath) == "RData") {
      data_introducidos_relacion(readRDS(input$file_relacion$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "var1_relacion", choices = names(data_introducidos_relacion()))
    updateSelectInput(session, "var2_relacion", choices = names(data_introducidos_relacion()))
    updateSelectInput(session, "pesos_var4", choices = c("NULL", names(data_introducidos_relacion())))
    updateSelectInput(session, "var_independiente_regresion_simple", choices = names(data_introducidos_relacion()))
    updateSelectInput(session, "var_dependiente_regresion_simple", choices = names(data_introducidos_relacion()))
  })
  
  output$primeras_filas_relacion <- renderTable({
    if (!is.null(data_introducidos_relacion())) {
      return(head(data_introducidos_relacion(), 5))
    }
  })
  
  output$correlacion_output <- renderPrint({
    req(input$var1_relacion, input$var2_relacion, input$calcular_correlacion, data_introducidos_relacion())
    data5<-data_introducidos_relacion()
    if(input$calcular_correlacion > 0) {
      if (input$pesos_var4=="NULL") {
        correlacion(data5,variable = c(which(names(data5) == input$var1_relacion),which(names(data5) == input$var2_relacion)))
      } else {
        correlacion(data5,variable = c(which(names(data5) == input$var1_relacion),which(names(data5) == input$var2_relacion)) , pesos = which(names(data5) == input$pesos_var4))
      }
    }
  })
  
  output$covarianza_output <- renderPrint({
    req(input$var1_relacion, input$var2_relacion, input$calcular_covarianza, data_introducidos_relacion())
    
    if(input$calcular_covarianza > 0) {
      data5<-data_introducidos_relacion()
      tipo <- input$tipo_covarianza1
      if (input$pesos_var4=="NULL") {
        covarianza(data5,variable = c(which(names(data5) == input$var1_relacion),which(names(data5) == input$var2_relacion)),tipo=tipo)
      } else {
        covarianza(data5,variable = c(which(names(data5) == input$var1_relacion),which(names(data5) == input$var2_relacion)) , pesos = which(names(data5) == input$pesos_var4),tipo=tipo)
      }
    }
  })
  
  observeEvent(input$calcular_correlacion, {
    output$primeras_filas_relacion <- renderTable(NULL)
  })
  
  observeEvent(input$calcular_correlacion, {
    req(input$var1_relacion, input$var2_relacion, data_introducidos_relacion())
    
    if (!is.numeric(data_introducidos_relacion()[[input$var1_relacion]]) ||
        !is.numeric(data_introducidos_relacion()[[input$var2_relacion]])) {
      showModal(modalDialog(
        title = "Error",
        "Las variables seleccionadas para correlación deben ser cuantitativas. Por favor, seleccione variables cuantitativas."
      ))
    }
    
    if (!is.numeric(data_introducidos_relacion()[[input$pesos_var4]]) && input$pesos_var4 != "NULL") {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada como peso es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (input$var1_relacion==input$pesos_var4 ||input$var2_relacion==input$pesos_var4) {
      showModal(modalDialog(
        title = "Error",
        "Las variables para el cálculo de la correlación y la variable para los pesos no pueden coincidir."
      ))
      return(NULL)
    }
  })
  
  observeEvent(input$calcular_covarianza, {
    output$primeras_filas_relacion <- renderTable(NULL)
  })
  
  observeEvent(input$calcular_covarianza, {
    req(input$var1_relacion, input$var2_relacion, data_introducidos_relacion())
    
    if (!is.numeric(data_introducidos_relacion()[[input$var1_relacion]]) ||
        !is.numeric(data_introducidos_relacion()[[input$var2_relacion]])) {
      showModal(modalDialog(
        title = "Error",
        "Las variables seleccionadas para covarianza deben ser cuantitativas. Por favor, seleccione variables cuantitativas."
      ))
    }
    
    if (!is.numeric(data_introducidos_relacion()[[input$pesos_var4]]) && input$pesos_var4 != "NULL") {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada como peso es cualitativa. Por favor, selecciona una variable cuantitativa."
      ))
      return(NULL)
    }
    
    if (input$var1_relacion==input$pesos_var4 ||input$var2_relacion==input$pesos_var4) {
      showModal(modalDialog(
        title = "Error",
        "Las variables para el cálculo de la covarianza y la variable para los pesos no pueden coincidir."
      ))
      return(NULL)
    }
    
  })
  
  calcular_matriz_correlacion <- function(data) {
    matriz_correlacion <- matriz.correlacion(data)
    return(matriz_correlacion)
  }
  
  observeEvent(input$calcular_matriz_correlacion, {
    output$primeras_filas_relacion <- renderTable(NULL)
  })
  
  
  observeEvent(input$calcular_matriz_correlacion, {
    req(input$file_relacion, data_introducidos_relacion())
    
    matriz_correlacion_resultado <- calcular_matriz_correlacion(data_introducidos_relacion())
    matriz_correlacion_resultado <- round(matriz_correlacion_resultado, 2)
    
    output$matriz_correlacion_output <- renderDT({
      datatable(matriz_correlacion_resultado,
                options = list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE,
                  autoWidth = TRUE),
                width = "50%"
      )
    })
  })
  
  calcular_matriz_covarianzas <- function(data) {
    tipo2 <- input$tipo_covarianza2
    matriz_covarianzas <- matriz.covar(data, tipo = tipo2)
    return(matriz_covarianzas)
  }
  
  observeEvent(input$calcular_matriz_covarianzas, {
    output$primeras_filas_relacion <- renderTable(NULL)
  })
  
  observeEvent(input$calcular_matriz_covarianzas, {
    req(input$file_relacion, data_introducidos_relacion())
    
    matriz_covarianzas_resultado <- calcular_matriz_covarianzas(data_introducidos_relacion())
    matriz_covarianzas_resultado <- round(matriz_covarianzas_resultado, 2)
    
    output$matriz_covarianzas_output <- renderDT({
      datatable(matriz_covarianzas_resultado,
                options = list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE,
                  autoWidth = TRUE),
                width = "50%"
      )
    })
  })
  
  calcular_regresion_simple <- function(data, var_depen, var_indepen, confianza, grafico,inferencia) {
    resultado <- regresion.simple(data, var_depen = var_depen, var_indepen = var_indepen, confianza = confianza, grafico = grafico,inferencia=inferencia)
    if (!grafico && !inferencia) {
      
      output$regresion_simple_output <- renderDT({
        datatable(resultado$Resumen.regresion,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
    } else if(grafico && !inferencia) {
      # Calcular límites del eje x
      x_min <- min(data[[var_indepen]], na.rm = TRUE)
      x_max <- max(data[[var_indepen]], na.rm = TRUE)
      
      # Calcular límites del eje y
      y_min <- min(data[[var_depen]], na.rm = TRUE)
      y_max <- max(data[[var_depen]], na.rm = TRUE)
      
      output$regresion_simple_plot <- renderPlot({
        plot(resultado$Graficos, xlim = c(x_min, x_max), ylim = c(y_min, y_max)) #FUNDAMENTAL EL $Graficos
      })
      
      output$regresion_simple_output <- renderDT({
        datatable(resultado$Resumen.regresion,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
    }
    
    else if(!grafico && inferencia) {
      
      output$regresion_simple_output_parcial <- renderDT({
        datatable(round(resultado$Resultados.parciales,3),
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
      output$regresion_simple_output_ANOVA <- renderDT({
        datatable(resultado$ANOVA,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
      output$regresion_simple_output_Modelo <- renderDT({
        datatable(round(resultado$Modelo.estimado,3),
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
      
    }
    else{
      # Calcular límites del eje x
      x_min <- min(data[[var_indepen]], na.rm = TRUE)
      x_max <- max(data[[var_indepen]], na.rm = TRUE)
      
      # Calcular límites del eje y
      y_min <- min(data[[var_depen]], na.rm = TRUE)
      y_max <- max(data[[var_depen]], na.rm = TRUE)
      
      output$regresion_simple_plot <- renderPlot({
        plot(resultado$Graficos, xlim = c(x_min, x_max), ylim = c(y_min, y_max)) 
      })
      
      
      output$regresion_simple_output_parcial <- renderDT({
        datatable(round(resultado$Resultados.parciales,3),
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
      output$regresion_simple_output_ANOVA <- renderDT({
        datatable(resultado$ANOVA,
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
      
      output$regresion_simple_output_Modelo <- renderDT({
        datatable(round(resultado$Modelo.estimado,3),
                  options = list(
                    dom = 't',
                    paging = FALSE,
                    ordering = FALSE,
                    autoWidth = TRUE),
                  width = "50%"
        )
      })
    }
    
  }
  
  observeEvent(input$calcular_regresion_simple, {
    output$primeras_filas_relacion <- renderTable(NULL)
  })
  
  observeEvent(input$calcular_regresion_simple, {
    req(input$file_relacion, input$var_independiente_regresion_simple, input$var_dependiente_regresion_simple, data_introducidos_relacion())
    
    if (!is.numeric(data_introducidos_relacion()[[input$var_independiente_regresion_simple]]) ||
        !is.numeric(data_introducidos_relacion()[[input$var_dependiente_regresion_simple]])) {
      showModal(modalDialog(
        title = "Error",
        "Las variables seleccionadas para la regresión simple deben ser cuantitativas. Por favor, selecciona variables cuantitativas."
      ))
      return(NULL)
    }
    
    confianza <- input$confianza_regresion_simple
    grafico <- input$grafico_regresion_simple
    inferencia<-input$inferencia_regresion_simple
    
    resultado_regresion_simple <- calcular_regresion_simple(
      data = data_introducidos_relacion(),
      var_depen = input$var_dependiente_regresion_simple,
      var_indepen = input$var_independiente_regresion_simple,
      confianza = confianza,
      grafico = grafico,
      inferencia=inferencia
    )
    
    observe({
      
      if (input$inferencia_regresion_simple) {
        output$regresion_simple_output<- renderDT(NULL)
      }
    })
    
  })  
  
  
  calcular_resumen <- function(data) {
    resumen <- resumen.descriptivos(data)
    return(resumen)
  }
  
  observeEvent(input$obtener_resumen, {
    req(input$file_data)
    if (tools::file_ext(input$file_data$datapath) == "csv") {
      datos<-read.csv(input$file_data$datapath)
    } else if (tools::file_ext(input$file_data$datapath) %in% c("xlsx", "xls")) {
      datos<-read_excel(input$file_data$datapath)
    } else if (tools::file_ext(input$file_data$datapath) == "RData") {
      datos<-readRDS(input$file_data$datapath)
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    
    
    resumen_estadistico <- calcular_resumen(datos)
    
    
    output$tabla_resumen <- renderDT({
      datatable(resumen_estadistico)
    })
  })
  
  observeEvent(input$file_tablas_frecuencias, {
    if (tools::file_ext(input$file_tablas_frecuencias$datapath) == "csv") {
      data_introducidos_tablas_frecuencias(read.csv(input$file_tablas_frecuencias$datapath))
    } else if (tools::file_ext(input$file_tablas_frecuencias$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_tablas_frecuencias(read_excel(input$file_tablas_frecuencias$datapath))
    } else if (tools::file_ext(input$file_tablas_frecuencias$datapath) == "RData") {
      data_introducidos_tablas_frecuencias(readRDS(input$file_tablas_frecuencias$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "var_filas", choices = names(data_introducidos_tablas_frecuencias()))
    updateSelectInput(session, "var_columnas", choices = names(data_introducidos_tablas_frecuencias()))
    updateSelectInput(session, "var_tabla_unidimensional", choices = names(data_introducidos_tablas_frecuencias()))
  })
  
  output$primeras_filas_tablas_frecuencias <- renderTable({
    if (!is.null(data_introducidos_tablas_frecuencias())) {
      return(head(data_introducidos_tablas_frecuencias(), 5))
    }
  })
  
  observeEvent(input$calcular_tablas_frecuencias, {
    req(input$file_tablas_frecuencias, input$var_filas, input$var_columnas, 
        input$distribucion, input$frecuencias,input$tipo, data_introducidos_tablas_frecuencias())
    
    output$primeras_filas_tablas_frecuencias <- renderTable(NULL)
    
    if ((is.numeric(data_introducidos_tablas_frecuencias()[[input$var_filas]]) && is.numeric(data_introducidos_tablas_frecuencias()[[input$var_columnas]])))
    {
      
    } else {
      showModal(modalDialog(
        title = "Error",
        HTML("El par de variables seleccionadas deben ser numéricas. En caso de que esté tratando de obtener una tabla de contingencia, le recomendamos que pase sus variables a <b>factor</b>."),
        easyClose = TRUE
      ))
      return()
    }
    
    if (input$var_filas == input$var_columnas) {
      showModal(modalDialog(
        title = "Error",
        "Las variables seleccionadas para la tabla de frecuencias bidimensional no pueden ser las mismas.", 
        easyClose = TRUE
      ))
      return()
    }
    
    if (!(input$tipo %in% c(1, 2))) {
      showModal(modalDialog(
        title = "Error",
        "El valor ingresado para 'Condicionada por columnas o filas' debe ser 1 o 2.", 
        easyClose = TRUE
      ))
      return()
    }
    
    data <- data_introducidos_tablas_frecuencias()
    var_filas <- input$var_filas
    var_columnas <- input$var_columnas
    distribucion <- input$distribucion
    frecuencias <- input$frecuencias
    tipo<-input$tipo
    
    if (distribucion == "condicionada" & frecuencias == "absolutas") {
      distribucion <- "cruzada"
      frecuencias <- "absolutas"
    }
    
    tabla.bidimensional<- function(x,
                                   var_filas = NULL,
                                   var_columnas = NULL,
                                   distribucion = c("cruzada","condicionada"),
                                   frecuencias = c("absolutas","relativas"),
                                   exportar = FALSE,
                                   tipo=1){
      
      distribucion <- tolower(distribucion)
      distribucion <- match.arg(distribucion)
      
      frecuencias <- tolower(frecuencias)
      frecuencias <- match.arg(frecuencias)
      
      x <- data.frame(x)
      varnames <- names(x)
      
      if(length(x)<2){
        stop("El conjunto de datos seleccionada solo tiene una variable.")
      }
      
      if(is.null(var_filas) | is.null(var_columnas)){
        stop("Debes seleccionar la variable fila y columna")
      }
      
      if(is.numeric(var_filas)){
        if(var_filas<=length(x)){
          var_filas <- var_filas}
        else{
          stop("Selecci\u00f3n err\u00f3nea de variable")
        }
      }
      
      if(is.character(var_filas)){
        if(var_filas %in% varnames){
          var_filas = match(var_filas,varnames)
        } else {
          stop("El nombre de la variable por filas no es v\u00e1lido")
        }
      }
      
      
      if(is.numeric(var_columnas)){
        if(var_columnas<=length(x)){
          var_columnas <- var_columnas}
        else{
          stop("Selecci\u00f3n err\u00f3nea de variable")
        }
      }
      
      if(is.character(var_columnas)){
        if(var_columnas %in% varnames){
          var_columnas = match(var_columnas,varnames)
        } else {
          stop("El nombre de la variable por columna no es v\u00e1lido")
        }
      }
      
      if(var_filas == var_columnas){
        stop("La variable por fila y columna es la misma variable")
      }
      
      variable <- c(var_filas,var_columnas)
      
      
      x <- x[,variable] %>% as.data.frame()
      names(x) <- varnames[variable]
      varnames <- names(x)
      
      clase <- sapply(x, class)
      
      if (!all(clase %in% c("numeric","integer","factor","logic"))){
        stop("No puede construirse la tabla de frecuencias, alguna variable seleccionada es car\u00e1cter")
      }
      
      
      if(frecuencias == "absolutas"){
        
        if(distribucion == "cruzada"){
          tabla <- x %>%
            table()
          tabla <- addmargins(tabla)
          
        } else{
          
          tipoelegido = as.numeric(tipo)
          
          tabla2 <- x %>%
            table()
          namesfilas <- row.names(tabla2)
          namescolumnas <- colnames(tabla2)
          
          if(tipoelegido == 1){
            tabla_aux <- x %>%
              select(tipoelegido) %>%
              group_by(filas) %>%
              count() %>%
              ungroup() %>%
              select(n)
            n = length(namescolumnas)
            tabla_aux <- cbind(tabla_aux, replicate(n-1,tabla_aux$n))
            
          } else{
            tabla_aux <- x %>%
              select(tipoelegido) %>%
              group_by(columnas) %>%
              count() %>%
              ungroup() %>%
              select(n)
            
            n = length(namesfilas)
            tabla_aux <- cbind(tabla_aux, replicate(n-1,tabla_aux$n)) %>%
              t()
          }
          
          tabla <-  tabla_aux * as.matrix(prop.table(tabla2,tipoelegido))
          row.names(tabla) <- namesfilas
          colnames(tabla) <- namescolumnas
          
        }
        
      } else{
        
        if(distribucion == "cruzada"){
          tabla <- x %>%
            table()
          tabla <- prop.table(tabla)
          tabla <- addmargins(tabla)
          
        } else{
          
          tipoelegido = as.numeric(tipo)
          
          if(tipoelegido == 1){
            tabla2 <- x %>%
              table()
            tabla <- prop.table(tabla2,2)
          } else{
            tabla2 <- x %>%
              table()
            tabla <- prop.table(tabla2,1)
          }
        }
      }
      
      tabla <- as.data.frame.matrix(tabla)
      
      
      if (exportar) {
        filename <- paste("Tabla cruzada de ", variable[1]," y ", variable[2], " (", Sys.time(), ").xlsx", sep = "")
        filename <- gsub(" ", "_", filename)
        filename <- gsub(":", ".", filename)
        rio::export(tabla, rowNames = TRUE, file = filename)
      }
      
      return(tabla)
      
    }
    
    tabla_frecuencias <- tabla.bidimensional(
      data,
      var_filas = var_filas,
      var_columnas = var_columnas,
      distribucion = distribucion,
      frecuencias = frecuencias,
      tipo=tipo
    )
    
    
    tabla_frecuencias <- cbind(" " = rownames(tabla_frecuencias), tabla_frecuencias)
    rownames(tabla_frecuencias) <- NULL  
    
    output$tabla_frecuencias_bidimensional<- renderTable(tabla_frecuencias)
  })
  
  
  tabla.frecuencias<- function(x,
                               eliminar.na = TRUE,
                               grafico = FALSE,
                               exportar = FALSE,
                               agrupar=TRUE){
    x <- as.data.frame(x)
    
    varnames <- colnames(x)
    
    if(length(x) > 1 ) {
      
      variable <- readline(prompt = "Intoduce el nombre de la variable: ")
      
    } else{
      
      variable <- varnames
      
    }
    
    if(is.character(variable)){
      if(variable %in% varnames){
        variable = which(varnames == variable)
      } else {
        stop("El nombre de la variable no es v\u00e1lido")
      }
    }
    
    x <- as.data.frame(x) %>%
      dplyr::select(all_of(variable))
    
    y <- varnames[variable] 
    
    clase <- sapply(x, class)
    
    if (!clase %in% c("numeric","integer","factor","logic")) {
      stop("No puede construirse la tabla de frecuencias, la variable que has\n
         seleccionado es car\u00e1cter")
    }
    
    if(length(x) > 1){
      stop("Esta funci\u00f3n solo puede contruir la tabla de frecuencias de una variable")
      print("Para obtener la tabla de frecuencias de mas de una variable utiliza la funci\u00f3n apply")
    }
    
    valores_distintos <- nrow(unique(x))
    valores_ordenados <- unique(x)
    valores_ordenados <- valores_ordenados[,1]
    
    if(valores_distintos >= 20){
      
      
      
      if(agrupar == TRUE){
        clase <- sapply(x, class)
        
        x <- na.omit(x)
        
        if(clase %in% c("factor","character","logic")){
          stop("La variable no es cuantitativa, no puede representarse el histograma")
        }
        
        if(nrow(x)<=100){
          intervalos <- ceiling(sqrt(nrow(x)))
        } else {
          intervalos <- ceiling(log(nrow(x))/log(2) + 1)
        }
        
        amplitud <- (max(x,na.rm=TRUE)-min(x,na.rm=TRUE))/intervalos
        
        x$cut <- cut(x[,1], seq(min(x[,1],na.rm=TRUE),max(x[,1],na.rm=TRUE),amplitud),
                     include.lowest = TRUE,
                     dig.lab = 8)
        x <- x[2]
        names(x) <- y
        
      } else{
        
        agrupar = FALSE
        
      }
      
    } else{
      agrupar = FALSE
    }
    
    tabla <- x %>% dplyr::arrange(x) %>%
      dplyr::group_by_at(y) %>%   
      dplyr::count() %>%
      dplyr::ungroup()
    
    names(tabla) <- c(y,"ni") 
    
    tabla <- tabla %>%
      dplyr::mutate(Ni = cumsum(ni),
                    fi = ni / sum(ni),
                    Fi = cumsum(fi))
    
    if(eliminar.na == TRUE){
      x <- drop_na(x)
      
      tabla <- x %>% dplyr::arrange(x) %>%
        dplyr::group_by_at(y) %>%
        dplyr::count() %>%
        dplyr::ungroup()
      
      names(tabla) <- c(y,"ni")
      
      tabla <- tabla %>%
        dplyr::mutate(Ni = cumsum(ni),
                      fi = ni / sum(ni),
                      Fi = cumsum(fi))
      
    } else {
      
      tabla <- x %>% dplyr::arrange(x) %>%
        dplyr::group_by_at(y) %>%
        dplyr::count() %>%
        dplyr::ungroup()
      
      names(tabla) <- c(y,"ni")
      
      tabla <- tabla %>%
        dplyr::mutate(Ni = cumsum(ni),
                      fi = ni / sum(ni),
                      Fi = cumsum(fi))
    }
    
    if (exportar) {
      filename <- paste("Tabla de frecuencias de ", y, " (", Sys.time(), ").xlsx", sep = "")
      filename <- gsub(" ", "_", filename)
      filename <- gsub(":", ".", filename)
      rio::export(tabla, file = filename)
    }
    
    if(grafico){
      
      df <- cbind(tabla[1],tabla[2],tabla[4],tabla[3],tabla[5])
      
      if(agrupar == 1){
        
        marca_clase <- function(cut_label) {
          mean(as.numeric(unlist(strsplit(gsub("\\(|\\)|\\[|\\]", "", as.character(cut_label)), ","))))
        }
        
        df$marca <- sapply(df[,1], marca_clase)
        variables <- names(df)
        
        print("Los valores han sido agrupados en intervalos y se representar\u00e1n mediante un histograma")
        
        plot <- ggplot(df, aes_string(x=variables[1], y = variables[2])) +
          geom_bar(stat = "identity", width = 1,
                   color = "white", fill = "orange") +
          geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
          geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.5, size = 2.5, color = "darkgreen" ) +
          labs(title = paste("Histograma de ",variables[1],sep=""),
               x = variables[1],
               y = "") +
          theme(
            panel.background = element_rect(fill = "transparent"), 
            plot.background = element_rect(fill = "transparent", color = NA), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            legend.background = element_blank(), 
            legend.box.background = element_blank(), 
            legend.key = element_blank(), 
            axis.ticks.x=element_blank(),
            axis.line.y = element_blank(),
            axis.ticks.y=element_blank(),
            axis.text.x=element_text(size=6,angle=30),
            axis.text.y=element_blank(),
            legend.title = element_blank()
          )
        
      } else {
        
        variables <- names(df)
        
        if(clase == "factor"){
          x_string <- df[,1]
          
        } else{
          x_string <- as.factor(round(df[,1],2))
        }
        
        if(valores_distintos > 10){
          
          print("El diagrama de barras puede no ser una buena representaci\u00f3n gr\u00e1fica si la variable presenta muchos distintos valores (aconsejable como m\u00e1ximo 10-15)")
          
          plot <- ggplot(df, aes_string(x=x_string,y=variables[2])) +
            geom_bar(stat = "identity",  fill = "orange") +
            geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
            geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.65, size = 2.5, color = "blue" ) +
            labs(title = paste("Diagrama de barras de ", variables[1], sep=""),
                 x = variables[1],
                 y = "") +
            theme(
              panel.background = element_rect(fill = "transparent"), 
              plot.background = element_rect(fill = "transparent", color = NA), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.background = element_blank(), 
              legend.box.background = element_blank(), 
              legend.key = element_blank(), 
              axis.ticks.x=element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.x=element_text(size=6,angle=90),
              axis.text.y=element_blank(),
              legend.title = element_blank()
            )
          
        } else {
          
          plot <- ggplot(df, aes_string(x=x_string,y=variables[2])) +
            geom_bar(stat = "identity", width = 0.5, fill = "orange") +
            geom_text(aes_string(label=variables[2]), vjust=1.5, size = 2.5) +
            geom_text(aes(label=paste("(",round(df[,3]*100,2),"%)",sep="")), vjust=2.65, size = 2.5, color = "blue" ) +
            labs(title = paste("Diagrama de barras de ", variables[1], sep=""),
                 x = variables[1],
                 y = "") +
            scale_x_discrete(breaks = valores_ordenados) +
            theme(
              panel.background = element_rect(fill = "transparent"), 
              plot.background = element_rect(fill = "transparent", color = NA), 
              panel.grid.major = element_blank(), 
              panel.grid.minor = element_blank(), 
              legend.background = element_blank(), 
              legend.box.background = element_blank(), 
              legend.key = element_blank(), 
              axis.ticks.x=element_blank(),
              axis.line.y = element_blank(),
              axis.ticks.y=element_blank(),
              axis.text.y=element_blank(),
              legend.title = element_blank()
            )
        }
        
      }
      
    } 
    
    if(grafico){
      
      return(list(tabla,plot))
      
    } else{
      
      return(tabla)
      
    }
    
  }
  
  output$tabla_frecuencias_unidimensional <- renderDT({
    req(input$file_tablas_frecuencias)
    req(input$var_tabla_unidimensional)
    data_introducidos_tablas_frecuencias <-data_introducidos_tablas_frecuencias()
    variable <- input$var_tabla_unidimensional
    output$primeras_filas_tablas_frecuencias <- renderTable(NULL)
    if (is.numeric(data_introducidos_tablas_frecuencias[[variable]])) {
      if (input$agrupar_checkbox) {
        tabla <- tabla.frecuencias(data_introducidos_tablas_frecuencias[[variable]], grafico = FALSE, agrupar = FALSE)
      } else {
        tabla <- tabla.frecuencias(data_introducidos_tablas_frecuencias[[variable]], grafico = FALSE)
      }
      
      datatable(tabla,
                options = list(
                  dom = 't',
                  paging = FALSE,
                  ordering = FALSE,
                  autoWidth = TRUE),
                width = "20%"
      )
    } else {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada es cualitativa. Por favor, seleccione variables cuantitativas."
      ))
    }
  })
  
  
  output$histograma <- renderPlot({
    req(input$file_tablas_frecuencias)
    req(input$var_tabla_unidimensional)
    
    data_introducidos_tablas_frecuencias <-data_introducidos_tablas_frecuencias()
    variable <- input$var_tabla_unidimensional
    
    if (input$grafico_checkbox && is.numeric(data_introducidos_tablas_frecuencias[[variable]])) {
      if (input$agrupar_checkbox) {
        a <- tabla.frecuencias(data_introducidos_tablas_frecuencias[[variable]], grafico = TRUE, agrupar = FALSE)
      } else {
        a <- tabla.frecuencias(data_introducidos_tablas_frecuencias[[variable]], grafico = TRUE)
      }
      
      plot(a[[2]])
    }
  })
  
  
  observeEvent(input$file_series_temporales, {
    if (tools::file_ext(input$file_series_temporales$datapath) == "csv") {
      data_introducidos_series(read.csv(input$file_series_temporales$datapath))
    } else if (tools::file_ext(input$file_series_temporales$datapath) %in% c("xlsx", "xls")) {
      data_introducidos_series(read_excel(input$file_series_temporales$datapath))
    } else if (tools::file_ext(input$file_series_temporales$datapath) == "RData") {
      data_introducidos_series(readRDS(input$file_series_temporales$datapath))
    } else {
      showModal(modalDialog(
        title = "Error",
        "El formato de archivo no es compatible. Por favor, sube un archivo CSV, XLSX/XLS, o RData."
      ))
    }
    updateSelectInput(session, "var_serie", choices = names(data_introducidos_series()))
  })
  
  output$primeras_observaciones_series <- renderTable({
    if (!is.null(data_introducidos_series())) {
      return(head(data_introducidos_series(), 5))
    }
  })
  
  generar_serie_temporal <- function(data, var_serie, inicio_anual, periodo_inicio,prediccion_tendencia, grafico_series) {
    
    if (missing(prediccion_tendencia)) {
      prediccion_tendencia <- FALSE
    }
    
    serie_temporal <- series.temporales(
      x=data,
      variable = var_serie,
      inicio_anual = inicio_anual,
      periodo_inicio = periodo_inicio,
      prediccion_tendencia = prediccion_tendencia,
      grafico = grafico_series
    )
    
    
    if (!grafico_series) {
      return(list(
        Medias_moviles = serie_temporal$Medias_moviles,
        IVE = serie_temporal$IVE,
        Datos_ajuste_tendencia = serie_temporal$Datos_ajuste_tendencia,
        Modelo_ajuste = serie_temporal$Modelo_ajuste,
        Pronosticos = serie_temporal$Pronosticos,
        Grafico = NULL
      ))
    } else {
      
      output$grafico_output <- renderPlot({
        plot(serie_temporal$Grafico)
      })
      
      return(list(
        Medias_moviles = serie_temporal$Medias_moviles,
        IVE = serie_temporal$IVE,
        Datos_ajuste_tendencia = serie_temporal$Datos_ajuste_tendencia,
        Modelo_ajuste = serie_temporal$Modelo_ajuste,
        Pronosticos = serie_temporal$Pronosticos,
        Grafico = serie_temporal$Grafico
      ))
    }
  }
  
  observeEvent(input$generar_series, {
    req(input$file_series_temporales, input$var_serie, input$inicio_anual, 
        input$periodo_inicio,input$prediccion_tendencia,data_introducidos_series())
    
    
    output$primeras_observaciones_series <- renderTable(NULL)
    
    if (!is.numeric(data_introducidos_series()[[input$var_serie]])) {
      showModal(modalDialog(
        title = "Error",
        "La variable seleccionada debe ser cuantitativa",
        easyClose = TRUE
      ))
      return(NULL)  
    }
    
    
    serie_temporal_result <- generar_serie_temporal(
      data = data_introducidos_series(),
      var_serie = input$var_serie,
      inicio_anual = input$inicio_anual,
      periodo_inicio = input$periodo_inicio,
      prediccion_tendencia = input$prediccion_tendencia,
      grafico_series = input$grafico_series
    )
    
    
    output$medias_moviles_output <- renderTable({
      head(serie_temporal_result$Medias_moviles, 5)
    })
    
    output$ive_output <- renderTable({
      head(serie_temporal_result$IVE, 5)
    })
    
    output$datos_ajuste_output <- renderTable({
      head(serie_temporal_result$Datos_ajuste_tendencia, 5)
    })
    
    serie_temporal_result$Modelo_ajuste <- cbind(" " = rownames(serie_temporal_result$Modelo_ajuste), serie_temporal_result$Modelo_ajuste)
    rownames(serie_temporal_result$Modelo_ajuste) <- NULL
    
    output$modelo_ajuste_output <- renderTable({
      serie_temporal_result$Modelo_ajuste
    })
    
    output$pronosticos_output <- renderTable({
      head(serie_temporal_result$Pronosticos, 5)
    })
  })
}

# Ejecutar la aplicación
shinyApp(ui = ui, server = server)

