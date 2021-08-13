##########################################################################################
# This points the Shiny server tool to any libraries installed with RStudio 
# that means that any library you install on your RStudio instance in this project,
# will be available to the shiny server
##########################################################################################

.libPaths( c( .libPaths(), "/srv/.R/library") )

###############
## Librerías ##
###############

# Declarando el uso de las librerías que usaremos.
library(shiny)
library(DT)
library(plotly)

## Para evaluar el gradiente de la función declarada vamos a usar la 
## función grad del paquete numDeriv que nos evalúa el gradiente en
## un punto.
library(numDeriv)

shiny_path <- "~/shiny-server/"
home_path <- "~/"

options(digits=7)

###############
## Funciones ##
###############

## Declarando la función a optimizar.
funcionA <- function(x) {
   return (x[1]^4 - 2*x[2]*x[1]^2+x[2]^2+x[1]^2-2*x[1]+5)
}

## Declarando la norma que queremos usar.
funcionNorma <- function(x) {
   return (sqrt(x[1]^2+x[2]^2))
}

## Declarando la función busquedaDeLinea, la cual recibe los siguientes parámetros:
## - puntoInicial: El punto desde donde se parte para buscar el mínimo.
## - valorEpsilon: El nivel de precisión que se desea para el resultado obtenido.
## - numeroMaximoIteraciones: El número máximo de iteraciones antes de que se detenga el programa
## 	 	si no ha encontrado el mínimo.
## - funcionAMin: La función que se va a minimizar.
## - numResultadosAImprimir: El número de primeros resultados que se colocan en el data frame que contiene la
## 		tabla de resultados,en caso de que no se proporcione, el valor por defecto es numeroMaximoIteraciones.
##
## La función devuelve un data frame que contiene la tabla de resultados.
busquedaDeLinea <- function(puntoInicial,valorEpsilon,numeroMaximoIteraciones,funcionAMin,numResultadosAImprimir = numeroMaximoIteraciones) {
   
   ## Declarando el data frame vacío que vamos a usar posteriormente para
   ## guardar los datos generados.
   tabla <- data.frame(
      k=integer(),
      xk_1=double(),
      xk_2=double(),
      ValorFPunto=double(),
      NormaFPunto=double(),
      Lambda=double(),
      sk_1=double(),
      sk_2=double(),
      stringsAsFactors=FALSE)
   
   ## Inicialización
   puntoActual <- puntoInicial
   normaMayorQueEpsilon <- TRUE
   k <-0
   
   while (k < numeroMaximoIteraciones && normaMayorQueEpsilon) {
      valorNorma <- funcionNorma(grad(funcionAMin,puntoActual))
      vectorDireccion <- -grad(funcionAMin,puntoActual)
      
      if(valorNorma < valorEpsilon){
         normaMayorQueEpsilon <- FALSE
         cat("Se alcanzó la precisión deseada, el valor óptimo está en: ", puntoActual ," Terminando.\n")
         tabla[nrow(tabla)+1,] <- c(k,puntoActual[1],puntoActual[2],valorFuncion,valorNorma,valorMinLambda$par,vectorDireccion[1],vectorDireccion[2])
      }
      else {
         # Declaramos la función auxiliar que opera con lambda y se basa en la funcionAMin
         flambda <- function(lambda) {
            return (funcionAMin(puntoActual+lambda*vectorDireccion))
         }
         
         # Obtenemos el valor mínimo de lambda
         valorMinLambda <- optim(flambda, par = 0,method = "BFGS")
         
         
         siguientePunto <- puntoActual + valorMinLambda$par*vectorDireccion
         valorFuncion <- funcionAMin(puntoActual)
         valorNorma <- funcionNorma(grad(funcionAMin,puntoActual))
         
         # Guardamos en la tabla solo los primeros numResultadosAImprimir que se le pasó a la función como argumento
         if (k<numResultadosAImprimir) {
            tabla[nrow(tabla)+1,] <- c(k,puntoActual[1],puntoActual[2],valorFuncion,valorNorma,valorMinLambda$par,vectorDireccion[1],vectorDireccion[2])
         }
         
         # Preparación para el siguiente ciclo.
         puntoActual <- siguientePunto
         k <- k+1
      }
   }
   return (tabla)
}

## Declarando una función que hace el manejo de los eventos relacionados
## con clicks en el contorno para seleccionar puntos.
xy_point <- function(e) {
   if(is.null(e)) return(c(1,2))
   c(e$x,e$y)
}

############################################################
## Declarando valores y funciones para generar el gráfico ##
############################################################

## Tamaño de la caja sobre la cual generamos
boxsize <- 5

## Número de puntos (precisión de la superficie)
pointno <- 50

## Declarando los márgenes
boundx = seq(-boxsize,boxsize,length.out = pointno)
boundy = seq(-boxsize,boxsize,length.out = pointno)

## Expandiendo los vectores
expanded <- expand.grid(boundx, boundy)

## Generando los valores de z
zet <- apply(X=expanded,1,funcionA)
zet <- matrix(zet,byrow = TRUE,ncol = pointno)

## Generando los contornos para la representación 2D de la función
contorno <- plot_ly(x = boundx, y = boundy, z = zet,source = "contorno") %>% 
   add_trace(autocontour=TRUE,contours = list(coloring="heatmap"),
             line = list(color = "#ffffff"),type = "contour",
             colorbar = list(thickness=10,len = 1,tickfont = list(size = 8))) %>% 
             layout(title = list(text="Contornos de superficie", font = list(size=12)),
             paper_bgcolor= "#E8E8E8", plot_bgcolor= "#E8E8E8",autosize = TRUE,yaxis = list(scaleanchor = "x",
             scaleratio = 1,tickfont = list(size = 8)),
             margin = list(l =50,r = 50, t = 50 , b = 50 ,pad = 0),
             xaxis = list(tickfont = list(size = 8),clickmode = "event")) %>%
             config(displayModeBar = F)

## Generando la superficie
superficie <- plot_ly(x=boundx,y=boundy,z = zet,type = "surface",height = 600, width = 600,colorbar=list(thickness=13,len=1,tickfont=list(size=10)))%>% add_trace(
   contours = list(
      z = list(
         show=TRUE,
         usecolormap=TRUE,
         highlightcolor="#ff0000",
         name="Valores",
         project=list(z=TRUE)
      )
   ),showscale = FALSE
)%>%
   colorbar(title = "Valor de la\n función") %>% layout(autosize = TRUE,yaxis = list(scaleanchor = "x", scaleratio = 1,tickfont = list(size = 8)),
                                                        margin = list(l =10,r = 10, t = 10 , b = 10 ,pad = 0),
                                                        xaxis = list(tickfont = list(size = 8)),
                                                        zaxis = list(tickfont = list(size = 8)),
                                                        scene = list(
                                                           camera=list(
                                                              eye = list(x=0, y=-1.8, z=1),center = list(x =0,y=0,z=0)
                                                           ),aspectratio = list(x=1,y=1,z=0.8)
                                                        ))




# Definiendo la interfaz de usuario
ui <- fluidPage(
   titlePanel("Búsquedas de línea"),
   
   sidebarLayout(
      sidebarPanel(style="background-color:#E8E8E8",
                   sliderInput("numIteraciones", h3("Número de iteraciones"),
                               min = 10, max = 1000, value = 20),
                   numericInput("valorEpsilon", 
                                h3("Valor de precisión deseada"), 
                                value = 0.01, step = 0.01),
                   sliderInput("numResultados", h3("Número de resultados a mostrar en la tabla"),
                               min = 0, max = 50 , value = 10),
                   br(),
                   h3("Selección de punto inicial",align="center"),
                   div(plotlyOutput(outputId = "plotContorno",width = "auto",height = "300px"),align="center"),
                   br(),
                   div (actionButton("botonComenzarBusqueda", "Comenzar Búsqueda"),align="center")
                   
      ),
      
      
      mainPanel(
         div(img(src="http://www.izt.uam.mx/wp-content/uploads/2019/01/logo-web-oficial2b.png"),style="text-align: center;"),
         h1("Fundamentos y modelos de optimización: Búsqueda de línea"),
         p("El presente trabajo presenta los resultados de implementar la técnica de optimización búsqueda de línea para la función:"),
         withMathJax("$$ x_{1}^{4}-2x_{2}x_{1}^{2}+x_{2}^{2}+x_{1}^{2}-2x_{1}+5$$"),
         p("Los resultados se generan seleccionando los valores iniciales en las opciones del menú, seleccionando en el plano 2D con un click el punto inicial que se desea usar para el método y dando click en el botón \"Comenzar Búsqueda\" Esto genera 
           resultados que se pueden ver en la tabla inferior. Al generar los puntos se pueden observar en la gráfica a continuación, unidos por una línea que representa el camino de la aproximación."),
         br(),
         div(plotlyOutput("plotSuperficie"),align="center"),
         div(style="height:200px;"),
         p("Representación gráfica de la superficie generada por la función y sus valores en el intervalo:"),
         h2("Resultados:"),
         DT::dataTableOutput("tablaResultados")
      )
   )
   
)

# Definimos la lógica del servidor para hacer el algoritmo y la actualización de la gráfica
server <- function(input, output,session) {
   
   options(warn = -1)
   
   resultados <- NULL
   
   # Atrapando algunos valores de las entradas para usarlos
   numIteraciones <- reactive({input$numIteraciones})
   valorEpsilon <- reactive({input$valorEpsilon})
   numResultados <- reactive({input$numResultados})
   point <- c(1,2)
   
   # Eventos que tienen que ver con el contorno
   output$plotContorno <- renderPlotly({
      event <- event_data("plotly_click",source = "contorno")
      if(is.null(event)==FALSE){
         point <<- xy_point(event)
         
      }else{
         point <<- c(1,2)
      }
      
      contorno %>% 
         add_trace(x=point[1], y=point[2], type="scatter", mode="markers", name="Punto inicial",marker=list(color="#f50505"))
   })
   
   # Eventos que tienen que ver con la superficie
   output$plotSuperficie <- renderPlotly({
      numIteraciones <- isolate(numIteraciones())
      numResultados <- isolate(numResultados())
      valorEpsilon <- isolate(valorEpsilon())
      point <- isolate(point)
      input$botonComenzarBusqueda
      resultados <<- busquedaDeLinea(puntoInicial = point,valorEpsilon = valorEpsilon, numeroMaximoIteraciones = numIteraciones,funcionAMin = funcionA, numResultadosAImprimir=numResultados)
      route <- resultados[,c(2,3,4,5)]
      superficie %>% 
         add_trace(x=route[,1],y=route[,2],z=route[,3],showlegend=FALSE,
                   type = "scatter3d",mode = "lines+markers",
                   marker = list(width = 15,color = route[,4],colorscale = list(c(0,"#34c0eb"),c(route[1,4],"#f02626"))),
                   line = list(width = 2,color = "#000000" ))
   })
   
   # Actualización de la tabla de resultados
   output$tablaResultados = DT::renderDataTable({
      input$botonComenzarBusqueda
      
      format(resultados,10)
   })
   
}

# Ejecutando la aplicación 
shinyApp(ui = ui, server = server)
