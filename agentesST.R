
library(shiny)



d <- read.csv("~/Dropbox/sistemas/dataSaul2018.csv")
normalize <- function(x) { 
  return((x - min(x)) / (max(x) - min(x)))
}
# Define UI for application that draws a histogram
ui <- fluidPage(
   
   # Application title
   titlePanel("Modelo basado en agentes competidores para la sincronización de series temporales "),
   
   # Sidebar with a slider input for number of bins 
   sidebarLayout(
      sidebarPanel(
         sliderInput("NA1",
                     "Número de competidores:",
                     min = 10,
                     max = 100,
                     value = 5),
         sliderInput("NA2",
                     "Número de seleccionados:",
                     min = 1,
                     max = 15,
                     value = 1),
         
         sliderInput("Imp.cor",
                     "Importancia de la correlación del ganador con señal:",
                     min = 0,
                     max = 0.95,
                     value = 0.1),
         sliderInput("var",
                     "Importancia de la correlación con el grupo:",
                     min = 0,
                     max = 1,
                     value = 0.0005),
         
         selectInput("Dist.pp",
                     "Tipo de distribución la señal:",
                     c("random","normal","constante","precipitación"),
                     selected = "random"),
         sliderInput("varprec",
                     "Variabilidad de la señal (para señal constante)",
                     min = 1,
                     max = 1000,
                     value = 50),
         sliderInput("eventos",
                     "Número de eventos por simulación:",
                     min = 2,
                     max = 12,
                     value = 10),
         sliderInput("simulacion",
                     "Número aleatóreo para simulaciones:",
                     min = 1,
                     max = 100,
                     value = 1,animate = T),
         numericInput("prog","Parecido en cada unidad de tiempo",value = 1),
      
         tags$b("Tabla 1. Sincronía:"),
        textOutput("tab"),
        textOutput("tab2"),
        textOutput("tab3")
      ),
      
      mainPanel(
        
         plotOutput("distPlot"),
         plotOutput("distPlot3"),
        plotOutput("distPlot2")
        
      )
   )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
   
   output$distPlot <- renderPlot({
    
         c0<-   c(sample(seq(0,100,0.0001),1000000))
     c0<-matrix(ncol = input$NA1,nrow = input$eventos,data = c0)
  
     set.seed(1*input$simulacion)
     f<-   c(sample(seq(0,100,0.0001),1000000))
   
     f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
   
      matplot(c0,type = "l",main="Todos los competidores")
  matlines(f[,1],lwd = 2)

   })
   
   output$distPlot3 <- renderPlot({
     
     

     set.seed(1*input$simulacion)
     f<-   c(sample(seq(0,100,0.0001),1000000))
     f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
    
     if (input$Dist.pp=="precipitación"){
       
       f<- read.csv("dataSaul2018.csv")
       
       f<-data.frame( f[1:input$eventos,3],f[1:input$eventos,3])*1000000
       
     }
     
      if (input$Dist.pp=="constante"){
       f<-abs(rnorm(input$eventos,50, input$varprec))
   
       f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)

     }
     
     if (input$Dist.pp=="normal"){
       f<-abs(rnorm(input$eventos,50, input$varprec))
       
       f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
      
       
     }
     repeat {
      
       c1=   c(sample(seq(0,100,0.0001),1000000))
       c1=matrix(ncol = input$NA1,nrow = input$eventos,data = c1)
       
       c1[,order(cor(c1,f[,1]))]
       c556<- c1[,1:3]
       matplot(c556,type = "l",main="Ganadores en grupo")
       matlines(f,lwd = 2)
       output$tab<- renderText({
     t<-c(cor(c556[,1],f[,1]),cor(c556[,2],f[,1]),cor(c556[,3],f[,1]))
         t
       })
       if (cor(c556[,1],c556[,2])> input$var  & cor(c556[,3],c556[,2]) & cor(c556[,1],f[,1]) > input$var){
         break
       }
       
     }
     
  
   })
   

   output$distPlot2 <- renderPlot({
     
     
     set.seed(1*input$simulacion)
     f<-   c(sample(seq(0,100,0.0001),1000000))
     f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
     
     if (input$Dist.pp=="precipitación"){
       
       f<- read.csv("dataSaul2018.csv")
       
       f<-data.frame( f[1:input$eventos,3],f[1:input$eventos,3])*1000000
       
     }
     
     if (input$Dist.pp=="constante"){
       f<-abs(rnorm(input$eventos,50, input$varprec))
       
       f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
       
       
       
       
     }
     
     if (input$Dist.pp=="normal"){
       f<-abs(rnorm(input$eventos,50, input$varprec))
       
       f<-matrix(ncol = input$NA2,nrow = input$eventos,data = f)
       
       
     }
     repeat {
       
       c1<-  c(sample(seq(0,100,0.0001),1000000))
       c1<-matrix(ncol = input$NA1,nrow = input$eventos,data = c1)
       
       c1[,order(cor(c1,f[,1]))]
       c556<- c1[,1]
       
  
        if (cor(c556,f[,1])> input$Imp.cor)  break
          matplot(f,type = "l",lwd=2,main="Ganador en solitario")
       matlines(c556,lwd = 1)
      
       
       
       
     }
     
     
   })
   
   
 
   
}

# Run the application 
shinyApp(ui = ui, server = server)

