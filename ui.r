library(lubridate) 
library(data.table)
library(dplyr)
library(ggplot2)
library(lubridate)  
library(readr)
library(sp)
library(lattice)
library(ggplot2)
library(maps)
library(rgdal)
library(tidyr)
library(tibble)
library(tidyverse)
library(sf)
library(foreign)
library(ggrepel)
library(cartography)
library(plotly)

#load("~/datalimpia.rda")
load(url("https://github.com/Juanmick/ejercicio/blob/master/datospreparados.rda?raw=true"))

shinyUI(
  navbarPage("Shiny Visualización Avanzada",
             tabPanel("Descripción del trabajo",
                      mainPanel(
                        h1(strong("Ejercicio Visualización Avanzada"), align = "center"),
                        h3("Alumno: Juan Manuel Ortiz", align = "center"),
                        p(''),
                        h2("Análisis de accidentes de tráfico en Madrid 2019", align = "center"),
                        p("En esta aplicación de rshiny, podemos jugar con los distintos gráficos
                          realizados en cada pestaña, de manera que podamos analizar de diversas formas
                          los datos de Accidentes de Madrid en el año 2019."),
                        h3(strong('Origen y fuentes de datos/ficheros:')),
                        p("El dataset de los accidentes de tráfico, se ha obtenido de la página web del Ayto. de Madrid
                          en el siguiente enlace:"),
                        p("https://datos.madrid.es/egob/catalogo/300228-19-accidentes-trafico-detalle.csv"),
                        p('Los datos con la información de los distritos y los ficheros .shp necesarios para los mapas han sido obtenidos en:'),
                        p('https://www.madrid.org/nomecalles/DescargaBDTCorte.icm'),
                        p('Los ficheros de la aplicación, el fichero de preparación de datos, y los datos ya preparados se pueden encontrar en:'),
                        p('https://github.com/Juanmick/ejercicio'),
                        p(''),
                        p(''),
                        h3(strong('Estructura:')),
                        p('La App se estructura de la siguiente manera:'),
                        p('- 1ª Pestaña: Descripción del trabajo y conclusiones'),
                        p('- 2ª Pestaña: Gráficos interactivos con series temporales'),
                        p('- 3ª Pestaña: Gráficos de columnas con un comparador de accidentes por Distritos'),
                        p('- 4ª Pestaña: Mapas de calor en función de la faceta seleccionada'),
                        p(""),
                        p(""),
                        h2(strong("Conclusiones:"), align = "center"),
                        h3("¿Cuándo se producen más accidentes en Madrid?"),
                        p("Si escogemos en el desplegable la gráfica por horas, es fácil interpretar que los accidentes están muy ligados
                          con los horarios laborales:"),
                        p("- Cuando menos accidentes se producen, es durante la noche. Cambia radicalmente durante la mañana cuando comienzan los desplazamientos al trabajo 
                          y se aprecia un gran repunte, entre las 08:00 y 09:00 de la mañana con hasta 2718 accidentes. Posteriormente vuelve a repuntar a las 14:00 coincidiendo 
                          con la hora de la comida con 3427. El pico máximo de accidentes se produjeron a partir de las 18:00, cuando las jornadas de trabajo suelen terminar y 
                          se regresa a casa, con 3656 accidentes.A partir de ese punto, vuelven a descender los accidentes hacia los niveles más apaciguados de la noche."),
                        p(''),
                        p('Seleccionando Meses en el desplegable, podemos ver como influyen los días festivos y vacaciones en los accidentes, disminuyendo estos durante las navidades (finales de diciembre, comienzos de enero)
                        ,la Semana Santa (19 de abril), durante el verano (junio,julio,agosto), registrando el día con menos accidentes el 17 de agosto. '),
                        p("- Puede llamar la atención el día 5 de abril, por ser el día con más accidentes (272) en Madrid, analizando el dataset, se deben principalmente a lluvias producidas ese día."),
                        p(''),
                        p('Finalmente, analizando por días de la semana, podemos apreciar que durante los días laborables, se mantienen estables los accidentes, aunque elevados, observando un repunte el
                          viernes, durante el fin de semana se reducen los accidentes comparados con el resto de la semana.'),
                        p('Las 3 visualizaciones anteriores confirman la clara influencia en los accidentes del uso de los vehículos, para acudir al trabajo.'),
                        p(''),
                        p(''),
                        h3("¿Cómo son los perfiles de los accidentes en Madrid?"),
                        p("Si acudimos a la 2ª pestaña, y seleccionamos como 1er distrito alguno entre Salamanca, Puente de Vallecas o Charmartín, y como 2º alguno entre Villa de Vallecas, Vicalvaro y Barajas
                          podremos ver que los 3 primeros han sido los distritos con mas accidentes, en comparación de los 3 segundos, que son los que menos accidentes tuvieron"),
                        p('Si jugamos con los distintos elementos a analizar podemos observar la misma tónica en todos los distritos:'),
                        p('- La gran mayoría de accidentes se producen con tiempo despejado, y en segundo lugar cuando hay lluvia debil.'),
                        p('- Los tipos de accidentes suelen ser por Colisión fronto-lateral o Alcance y en menor medida por colisión lateral o choque contra un obstáculo fijo.'),
                        p('- El vehículo más involucrado en accidentes es con diferencia el turismo, posteriormente incluiriamos a las motos y furgonetas.'),
                        p('- Se aprecia que practicamente el 75% de los accidentes no requirieron asistencia, y el restante son leves. Por desgracia, hubo 34 fallecidos.'),
                        p('- Por edades se aprecia repartido entre 2 grupos DE 18 A 39 AÑOS	y DE 40 A 64 AÑOS.	'),
                        p('- Los principales implicados en los accidentes son los conductores, y los pasajeros de los vehículos. Pero tambien se aprecian que hubo peatones implicados.'),
                        p('- Según el sexo, es facil concluir que los hombres son más proclives a los accidentes con más del 60% aproximadamente.'),
                        p(''),
                        p(''),
                        h3('Patrones según el mapa de calor'),
                        p('Podemos comprobar como la mayoría de accidentes se produjeron en los distritos del centro, sur y oeste de Madrid, independientemente de las variables que seleccionemos.
                          Es curioso comprobar como los accidentes con nieve se produjeron en el sur, los despeñamientos se dieron en la zona de El Pardo, hubo accidentes 
                          relacionados con tranvias en las zonas del oeste, donde se despliega el Metro Ligero o como en el distrito de Salamanca se aprecia un leve repunte de accidentes entre los más mayores.'),
                        p(''),
                        p(''),
                        h3("Detalles, apreciaciones y conclusiones"),
                        p('- No se ha pretendido realizar un analisis en profundidad del dataset, si no obtener algunas caracteristicas generales que nos describiesen los accidentes en Madrid. 
                          Me he centrado más en desarrollar los gráficos y mapas con librerias a las que no estaba habituado como es el caso de Plotly para gráficos interactivos, el uso de mapas
                          o de las facetas, todo esto, mediante diferentes metodos para obtener los input.'),
                        p('- Una gran parte del tiempo lo he tenido que dedicar a limpiar, ordenar y simplificar el dataset, y configurar los datos para poder darles un mejor uso, como es el caso de las fechas con el paquete lubridate.'),
                        p('- Sin duda, se pueden hacer visualizaciones más efectivas o especificas, pero para ello hay que centrarse en un único motivo, y analizar los datos en función de ello.
                          Yo he querido abarcar los máximos datos posibles, para poder jugar con los distintos tipos de información. En realidad, si uno no se acota bien lo que desea, puede que no termine nunca de realizar visualizaciones.'),
                        p('- El hecho de que el dataset contuviese solo variables categóricas, ha limitado el uso de otras características para gráficos que me hubiese gustado realizar.'),
                        p(),
                        h5("PD: para más información, consultas o dudas, se puede contactar conmigo a traves de:"),
                        HTML('<P>https://github.com/Juanmick</P>'),
                        p('https://www.linkedin.com/in/juan-manuel-ortiz-10956a80/')
                      )),
             
             
             tabPanel("Series temporales",
                      fluidRow(
                        h2("GRÁFICO INTERACTIVO SEGÚN UNIDAD DE TIEMPO"),
                        selectInput('x', 'Seleccione unidad de tiempo:', c("Horas" = 'ho',"Mes" = 'me',"Dias"='di'))),
                      
                      mainPanel(
                        plotlyOutput('plot7'),width=12, align = "center"
                        
                      ),
                        
                      
                      column(
                        width=4, align = "right",
                        plotOutput('plot',
                                   height=500)
                        
                      )
             ),
             
             tabPanel("Comparativa de distritos",
                      sidebarPanel(
                        
                        selectInput('z', 'Elige el distrito 1 a comparar', levels(ac$DISTRITO), selected = levels(ac$DISTRITO)[2]), 
                        selectInput('v', 'Elige el dato a analizar', choices = c('Tipo Accidente'='TIPOACCIDENTE','Tiempo'='ESTADOMETEREOLOGICO','Vehiculo'='TIPOVEHICULO','Persona'='TIPOPERSONA','Edad'='RANGOEDAD','Sexo'='SEXO','Gravedad'='LESIVIDAD'), selected = 'TIPOACCIDENTE'), 
                        selectInput('va', 'Elige el color', choices = c('Tipo Accidente'='TIPOACCIDENTE','Tiempo'='ESTADOMETEREOLOGICO','Vehiculo'='TIPOVEHICULO','Persona'='TIPOPERSONA','Edad'='RANGOEDAD','Sexo'='SEXO','Gravedad'='LESIVIDAD'), selected = 'TIPOACCIDENTE'),
                        
                        selectInput('xa', 'Elige el distrito 2 a comparar', levels(ac$DISTRITO), selected = levels(ac$DISTRITO)[2]),
                        selectInput('i', 'Elige el dato a analizar', choices = c('Tipo Accidente'='TIPOACCIDENTE','Tiempo'='ESTADOMETEREOLOGICO','Vehiculo'='TIPOVEHICULO','Persona'='TIPOPERSONA','Edad'='RANGOEDAD','Sexo'='SEXO','Gravedad'='LESIVIDAD'), selected = 'TIPOACCIDENTE'),
                        selectInput('ia', 'Elige el color', choices = c('Tipo Accidente'='TIPOACCIDENTE','Tiempo'='ESTADOMETEREOLOGICO','Vehiculo'='TIPOVEHICULO','Persona'='TIPOPERSONA','Edad'='RANGOEDAD','Sexo'='SEXO','Gravedad'='LESIVIDAD'), selected = 'TIPOACCIDENTE')
                        
                         
                        ),
                        
                      
                      
                      mainPanel(
                        p(strong("GRÁFICO DISTRITO 1.")),
                        plotOutput('plot1',
                                   height=250),
                        p(strong("GRÁFICO DISTRITO 2.")),
                        plotOutput('plot2',
                                   height=250)
                        
                      )
             ),
             tabPanel("Mapas por facetas",
                      
                      h2("Representación en mapa de los accidentes según las variables", align = "center"),
                      
                      fluidRow(column(selectInput('pi', 'Elige el dato a visualizar', choices = c('Tipo Accidente'='TIPOACCIDENTE','Tiempo'='ESTADOMETEREOLOGICO','Vehiculo'='TIPOVEHICULO','Persona'='TIPOPERSONA','Edad'='RANGOEDAD','Sexo'='SEXO','Gravedad'='LESIVIDAD', selected = 'LESIVIDAD')), width=12, align = "center")),
                               
                      plotOutput('plot3',height=500)
                        
                      )
             )          
      )
