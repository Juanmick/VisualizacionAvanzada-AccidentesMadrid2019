library(shiny)
library(ggplot2)
library(dplyr)
library(readr)
library(lubridate) 
library(data.table)
library(sp)
library(lattice)
library(maps)
library(rgdal)
library(tidyr)
library(tibble)
library(sf)

library(cartography)
library(date)
library(hrbrthemes)
library(plotly)


#load("~/datoslimpios.rda")
load(url("https://github.com/Juanmick/ejercicio/blob/master/datospreparados.rda?raw=true"))


#REALIZAMOS ALGUNAS AGRUPACIONES PARA PODER GRAFICAR
#ACCIDENTES POR MESES
meses <- ac %>% 
  group_by(FECHA) %>% tally()
colnames(meses)[2] <- "Accidentes"

#ACCIDENTES POR HORAS INTERACTIVO
horas <- ac %>% 
  group_by(hour(HORA)) %>% tally()
colnames(horas)[2] <- "Accidentes"
colnames(horas)[1] <- "Horas"

#ACCIDENTES POR DIAS
dias <- ac %>% 
  group_by(weekdays(FECHA)) %>% tally()
colnames(dias)[2] <- "Accidentes"
colnames(dias)[1] <- "Dias"
dias$Dias <- factor(dias$Dias, levels = (dias$Dias)[c(3,4,5,2,7,6,1)])
dias$Diasnum <- as.numeric(dias$Dias)
dias <- arrange(dias, Dias)


cc <- scales::seq_gradient_pal("blue", "red", "Lab")(seq(0,1,length.out=100))



#########################################################################
#########################################################################


shinyServer(function(input, output) {
  
  
  output$plot7 <- renderPlotly({
    
    if (input$x == 'me')
      p <- meses %>%
        ggplot( aes(x=FECHA, y=Accidentes)) +
        geom_area(fill="#69b3a2", alpha=0.5) + geom_point()+
        geom_line(color="#69b3a2") +
        scale_x_date(date_breaks = "1 month", date_labels = "%B")+
        ylab("Accidentes") +
        theme(axis.text.x = element_text(size = 11,angle = 25, hjust = 1))
    
    if (input$x == 'ho')
      p <- horas %>%
        ggplot( aes(x=Horas, y=Accidentes)) +
        geom_area(fill="#69b3a2", alpha=0.5) + geom_point()+
        geom_line(color="#69b3a2") +
        ylab("Accidentes") + scale_x_continuous(breaks=seq(0, 24, 1))+
        theme(axis.text.x = element_text(size = 11,angle = 25, hjust = 1))
     
    if (input$x == 'di')
      p <- dias %>%
        ggplot( aes(x=Diasnum, y=Accidentes)) +
        geom_area(fill="#69b3a2", alpha=0.5) + geom_point()+
        geom_line(color="#69b3a2") +
        ylab("Accidentes") + scale_x_continuous(breaks=seq(1, 7, 1),labels=c("Lunes", "Martes", "Miercoles", 'Jueves','Viernes','Sabado','Domingo'))+
        theme(axis.text.x = element_text(size = 11,angle = 25, hjust = 1))
    
    # Activamos la interacción
    p <- ggplotly(p)
    print(p)
 
  })
  
 
  output$plot1 <- renderPlot({
    
    ac1 <- select(ac, -HORA)
    
    t <- filter(ac1, DISTRITO== input$z)
    
    t1 <- t %>% group_by(TIPOACCIDENTE, ESTADOMETEREOLOGICO, TIPOVEHICULO,TIPOPERSONA,RANGOEDAD,SEXO,LESIVIDAD) %>% summarise(accidentes= n())
    
    g <- ggplot(t1, aes_string(x = input$v, y = 'accidentes')) + geom_col() + theme_classic()
    
    g <- g+ theme(axis.text.x = element_text(size = 11,angle = 25, hjust = 1))
    
    g <- g+ aes_string( fill = input$va, width=0.5 ) 
    
    
    print(g)
    
    
  })
  
  
  output$plot2 <- renderPlot({
    
    ac1 <- select(ac, -HORA)
    a <- filter(ac1, DISTRITO== input$xa)
    
    a1 <- a %>% group_by(TIPOACCIDENTE, ESTADOMETEREOLOGICO, TIPOVEHICULO,TIPOPERSONA,RANGOEDAD,SEXO,LESIVIDAD) %>% summarise(accidentes2= n())
    
    g1 <- ggplot(a1, aes_string(x = input$i, y = 'accidentes2')) + geom_col() + theme_classic()
    
    g1 <- g1+ theme(axis.text.x = element_text(size=11, angle = 25, hjust = 1))
    
    g1 <- g1+ aes_string( fill= input$ia, width=0.5 )
    
    print(g1)
    
    
  })
  
  output$plot3 <- renderPlot({
    
    acci2 <- ac %>% 
      group_by_('DISTRITO',
               'GEOCODIGO', 
               input$pi) %>% tally()
    
    shp_madrid_datos <- shp_mad %>% left_join(acci2)
    
    facets <- paste(".~",input$pi)
    m1 <- ggplot(shp_madrid_datos) + geom_sf(aes(fill = n)) + facet_wrap(facets)
    m1 <- m1 + scale_fill_gradient(low = "#FDECDD", high = "#D94701")
    m1 <- m1 + scale_x_continuous(breaks = NULL) + scale_y_continuous(breaks = NULL) 
    m1 <- m1 + ggtitle("ACCIDENTES SEGÚN LA FACETA SELECCIONADA")
    m1 <- m1 + theme(axis.text.y = element_blank(), axis.text.x = element_blank(), plot.title = element_text(face = "bold", size = rel(1.4)), legend.text = element_text(size = rel(1.1)), strip.text = element_text(face = "bold", size = rel(1))) + labs(list(x = "", y = "", fill = "")) 
    
    print(m1)
    
  
    
  })
})