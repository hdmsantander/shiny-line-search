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
library(plotly)

## Para evaluar el gradiente de la función declarada vamos a usar la 
## función grad del paquete numDeriv que nos evalúa el gradiente en
## un punto.
library(numDeriv)

##########################################################################################
# For deploying tools on MatrixDS, we created this production variable
# when set to true, your shiny app will run on the shiny server tool upon clicking open
# when set to false, your shiny app will run when you hit the "Run App" button on RStudio
##########################################################################################

production <- TRUE

##########################################################################################
# The shiny server tool uses a different absolute path than RStudio.
# this if statement denotes the correct path for the 2 values of the production variable
##########################################################################################

if(production == FALSE) {
   #if you using the RStudio tool
   shiny_path <- "~/shiny-server/"
   home_path <- "~/"
} else {
   #if you are using the shiny tool
   shiny_path <- "/srv/shiny-server/"
   home_path <- "/srv/"
}

##########################################################################################
# To call a file/artifact in your MatrixDS project use the following line of code
# this example uses the function read.csv
#  my_csv <- read.csv(paste0(home_path,"file_name.csv"))
##########################################################################################


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
## 	 si no ha encontrado el mínimo.
## - funcionAMin: La función que se va a minimizar.
## 
## La función devuelve un data frame que contiene la tabla de resultados.
busquedaDeLinea <- function(puntoInicial,valorEpsilon,numeroMaximoIteraciones,funcionAMin) {
   
   ## Declarando el data frame vacío que vamos a usar posteriormente para
   ## guardar los datos generados.
   tabla <- data.frame(
      k=integer(),
      xk_1=double(),
      xk_2=double(),
      Valorf=double(),
      Normaf=double(),
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
         cat("Se alcanzó la precisión deseada, el valor óptimo está en: ", puntoActual ," Terminando.")
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
         
         # Dado que estamos iterando sobre miles, guardamos resultados cada 100 iteraciones.
         if (k%%100 == 0) {
            tabla[nrow(tabla)+1,] <- c(k,puntoActual[1],puntoActual[2],valorFuncion,valorNorma,vectorDireccion[1],vectorDireccion[2])
         }
         
         # Preparación para el siguiente ciclo.
         puntoActual <- siguientePunto
         k <- k+1
      }
   }
   return (tabla)
}

################################################
## Declarando valores para generar el gráfico ##
################################################

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

# Define UI for application that draws a histogram
ui <- fluidPage(
   titlePanel("Búsquedas de línea"),
   
   sidebarLayout(
      sidebarPanel(sliderInput("numResultados", h3("Número de os"),
                               min = 10, max = 1000, value = 10),
                   numericInput("valorEpsilon", 
                                h3("Valor de precisión deseado"), 
                                value = 0.01),
                   textInput("puntoInicial", 
                                h3("Dos valores separados por comas donde se desea iniciar"), 
                                value ="1,2"),
                   sliderInput("numResultados", h3("Número de resultados deseados"),
                               min = 10, max = 1000, value = 10)
      ),
      
      
      mainPanel(
         div(img(src="http://www.izt.uam.mx/wp-content/uploads/2019/01/logo-web-oficial2b.png"),style="text-align: center;"),
         h1("Fundamentos y modelos de optimización: Búsqueda de línea"),
         p("El presente trabajo presenta los resultados de implementar la técnica de optimización de búsqueda en línea para la función:"),
         withMathJax("$$x_{1}^{4}-2x_{2}x_{1}^{2}+x_{2}^{2}+x_{1}^{2}-2x_{1}+5$$"),
         p("Los resultados se generan colocando los valores iniciales en las opciones del menú y ejecutando el programa. Esto genera 
           resultados que se guardan en una hoja de cálculo que se descarga, asimismo se imprimen las últimas 10 iteraciones."),
         div(plotlyOutput("plot"),align="center"),
         p("Representación gráfica de la superficie generada por la función y sus valores en el intervalo:")
      )
   )
   
)

# Definimos la lógica del servidor para hacer el algoritmo y el plot de la gráfica
server <- function(input, output,session) {
   
   output$plot <- renderPlotly({
      plot_ly(x=boundx,y=boundy,z = matrix(zet,byrow = TRUE,ncol = pointno),type = "surface",height = 500, width = 500)%>% add_trace(
         contours = list(
            z = list(
               show=TRUE,
               usecolormap=TRUE,
               highlightcolor="#ff0000",
               project=list(z=TRUE)
            )
         ),showscale = FALSE
      )%>%
         colorbar(title = "Valor de la\n función")
      })
}

# Run the application 
shinyApp(ui = ui, server = server)
