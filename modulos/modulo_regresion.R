# Modulo 1 ####




reg_ui <- function(id, label = "reg") {
  ns <- NS(id)
  
  tabItem(tabName = "reg",
          sidebarLayout(
            sidebarPanel(selectInput(inputId = ns("selector1"),"Seleccion de set de datos",choices = c("iris"="iris","beaver1"="beaver1","mtcars"="mtcars","trees"="trees")),
                         selectInput(inputId = ns("selector2"),"Seleccion de color",choices = c("viridis"="viridis","magma"="magma","cividis"="cividis","plasma"="plasma")),
                         sliderInput(inputId = ns("selector3"),"Tamano:",min=0,max=10,value=5),
                         sliderInput(inputId = ns("selector4"),"Alpha:",min=0,max=1,value=1,animate = TRUE),
                         sliderInput(inputId = ns("selector5"),"Shape:",min=0,max=25,value=16,animate = TRUE),
                         checkboxInput(inputId = ns("lineal"), "Ajustar modelo de regresion lineal", value = FALSE),
                         checkboxInput(inputId = ns("r2"), "Mostar estadistico del modelo", value = FALSE),width=4),
            mainPanel(box(plotOutput(ns("graf1")),width=12))),
          fluidRow(bs4InfoBox(title = "Estadisticas del eje x",value = htmlOutput(ns("stat_x")),color = "info",gradient = TRUE,
                              elevation = 3,fill = TRUE,width = 4),
                   
                   
                   bs4InfoBox(title = "Estadisticas del eje y",value = htmlOutput(ns("stat_y")),color="info",gradient = TRUE,
                               elevation = 3,fill = TRUE,width = 4),
                   bs4InfoBox(title = "Estadisticas del modelo",value = htmlOutput(ns("r2")),color = "primary",gradient = TRUE,
                              elevation = 3,fill = TRUE,width = 4)))
                   
                  }

reg_sv <- function(input, output, session) {
  
  base <- reactive({get(input$selector1)})
  # shape<-as.character(input$selector5)
  
  output$graf1 <- renderPlot({
    
    
    
  if(input$lineal==TRUE) {
  
    data <- base()
    # highchart() %>%
    #   # Data
    #   hc_add_series(data, "scatter", hcaes(x = data[,1], y = data[,2]), name = "Pie")
      ggplot(data,aes(x=data[,1],y=data[,2],color=data[,3]))+
        # geom_point(col="red",size=0.3,alpha=0.7)+geom_smooth(method = "lm")+
        geom_point(size=input$selector3,alpha=input$selector4,shape=input$selector5)+geom_smooth(method = "lm")+
        xlab("Eje x")+ylab("Eje y") + 
        scale_color_viridis_c(option = input$selector2)
      
      # p1 <- gb + scale_color_viridis_c() + ggtitle("'viridis' (default)")
      # p2 <- gb + scale_color_viridis_c(option = "inferno") + ggtitle("'inferno'")
      # p3 <- gb + scale_color_viridis_c(option = "plasma") + ggtitle("'plasma'")
      # p4 <- gb + scale_color_viridis_c(option = "cividis") + ggtitle("'cividis'")
      
    }else{
      data <- base()
      ggplot(data,aes(x=data[,1],y=data[,2],color=data[,3]))+
        # geom_point(col="red",size=0.3,alpha=0.7)+geom_smooth(method = "lm")+
        geom_point(size=input$selector3,alpha=input$selector4,shape=input$selector5)+
        xlab("Eje x")+ylab("Eje y") + 
        scale_color_viridis_c(option=input$selector2)
    }

    })
  
  
  output$r2 <- renderText({
    if(input$r2==TRUE) {
      
    mydata <- base()
    modelo<-lm(data = mydata,mydata[,1]~mydata[,2])
    paste0("R2: ",round(summary(modelo)$r.squared,2),br(),br(),
           "R2 ajustado x: ",round(summary(modelo)$adj.r.squared,2),br(),
           "Ajuste lineal: Y=",round(modelo$coefficients[1],2),"+",round(modelo$coefficients[2],2),"X",br())
    }else{}    
  })
  
  
  output$stat_x <- renderText({
    
    mydata <- base() 
    paste0("Media eje x: ",round(mean(mydata[,1]),2),br(),
           "Desviacion eje x: ",round(sd(mydata[,1]),2),br(),
           "Mediana x: ",round(median(mydata[,1]),2),br(),br(),
           "Minimo eje x: ",round(min(mydata[,1]),2),br(),
           "Maximo eje x: ",round(max(mydata[,1]),2),br())   
  })
  
  output$stat_y <- renderText({
    
    mydata <- base() 
    paste0("Media eje y: ",round(mean(mydata[,2]),2),br(),
           "Desviacion eje y: ",round(sd(mydata[,2]),2),br(),
           "Mediana y: ",round(median(mydata[,2]),2),br(),br(),
           "Minimo eje y: ",round(min(mydata[,2]),2),br(),
           "Maximo eje y: ",round(max(mydata[,2]),2),br())   
  })
  
    

  
}


