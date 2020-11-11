#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#


library(shiny)
library(tidyverse)
library(readxl)
library(ggplot2)
library(plotly)
library(shinythemes)
library(shinyWidgets)
library(leaflet)
library(htmltools)
library(sf)
library(geojsonsf)
library(rsconnect)
library(magrittr)
library(geojsonio)
library(viridis)

base::load('C:/Users/direcciongeneral/Desktop/Shiny/Prueba publicacion/ShinyDataSet.RData')

ui=fluidPage(
    setBackgroundColor(color='white'),
    tags$style( 
        '.nav-tabs { border-color: #31408C;tabs-color:#31408C}',
        '.nav-tabs-custom .nav-tabs li.active {
             border-top-color: #4176A3;}'),
    h1(img(src='https://encrypted-tbn0.gstatic.com/images?q=tbn%3AANd9GcSRZVxuDKREqFtD1k1m59Q5QtX9lVB8G0CDwA&usqp=CAU',
           width=350,height=150)), 
    h5(strong(em('¿Como es el comportamiento de los délitos?'),style='color:#31408C')), #CABECERA
    
    
    tabsetPanel(
        tabPanel('Scope proyecto',
                 column(1),
                 column(9,
                        br(),
                        h4(strong('¿Qué nos proponemos con este proyecto?'),style='color:#31408C'),
                        p('En el marco del desafío final del programa de', em(strong('Data Analytics de la ',a(href='https://eant.tech/home','EANT'))), ', presentamos esta herramienta, fruto de la recopilación y modelado de datos, para detectar las zonas más seguras e inseguras, cómo se relacionan con otras variables del tipo social, geográfico, económico y demográfico, y presentaremos conclusiones sobre lo estudiado.'),
                        p('A partir de este análisis buscamos que sea posible detectar variables explicativas de patrones de comportamiento de delito. La herramientda puede ser consultada en la toma de decisiones por cualquier interesado y el proposito es que sirva para:'),
                        tags$div(tags$ul(
                            tags$li('Entender la dinámica de los delitos en CABA'),
                            tags$li('Buscar realaciones entre condiciones demograficas y delitos'),
                            tags$li('Segmentar geográficamente los delitos'),
                            tags$li('Identificar las zonas de mayor concentración, comprendiendo los factores que impactan en la seguridad con el fin de hallar maneras, momentos y lugares para reforzarla')),
                            p('Como cada barrio posee variables distinas, el análisis será, en muchos casos, por comuna.'))),
                 column(1)), #HASTA ACÁ INTRODUCCIÓN 
        
        tabPanel('Descripción gráfica de la situación',
                 navlistPanel(
                     ###Grafico1######           
                     tabPanel('Delitos por Comunas',
                              br(),
                              fluidRow(
                                  column(3,
                                         selectInput(inputId='comuna_n',
                                                     label = 'Casos por barrio en cada comuna:',
                                                     choices =sort(unique(tabla_A$comuna)),
                                                     selected = '1'),
                                         tableOutput(outputId='tablacomuna')
                                  ),
                                  column(8,
                                         plotlyOutput(outputId = 'grafico_1'))),
                              br(),
                              p(em('*Datos extraídos de la página de datos abiertos de CABA'))
                     ),
                     ###Grafico2###### 
                     tabPanel('Tipos de delitos por barrios',
                              br(),
                              selectInput(inputId = 'comuna0',
                                          label = 'Elija la comuna a visualizar:',
                                          choices = sort(unique(tabla_B$comuna)),
                                          selected = '6'),
                              br(),
                              plotOutput(outputId = 'grafico_2'),
                              br(),
                              p(em('*Datos extraídos de la página de datos abiertos de CABA'))),
                     
                     
                     ############## Grafico 3 ###################################
                     tabPanel('Los Barrios con Mas delitos',
                              br(),
                              selectInput(inputId = 'delito',
                                          label = 'Elija el tipo de delito:',
                                          choices = c('Total',sort(unique(tabla_B$subtipo_delito_2))),
                                          selected = 'Total'),
                              p(strong('Indicar como desea ver los datos')),
                              radioButtons(inputId = 'datos',
                                           label = '',
                                           choices = c('Absolutos' = 'Absolutos', 'Por Habitantes' = 'Por Habitantes'),
                                           inline = TRUE,
                                           selected = 'Absolutos'),
                              plotlyOutput(outputId = 'grafico_3'),
                              br(),
                              p(em('*Datos extraídos de la página de datos abiertos de CABA - Habitantes por barrio - censo año 2010'))),
                     
                     ##Grafico 3 interactivo --> play de cantidad por franja horaria ##############3                       
                     tabPanel('Evolución de los delitos por horario',
                              br(),
                              selectInput(inputId = 'tipo_delito',
                                          label = 'Seleccione el delito que desea analizar:',
                                          choices = sort(unique(tabla_f$subtipo_delito_2)),
                                          selected = 'Doloso'),
                              
                              selectInput(inputId = 'comuna_4',
                                          label = 'Seleccione la/s comuna/s:',
                                          choices = sort(unique(tabla_f$comuna)),
                                          selected = '1',
                                          multiple = TRUE),
                              p(strong(em('NOTA: No seleccionar mas de 5 comunas'))),
                              plotlyOutput(outputId = 'grafico_4'),
                              br(),
                              p(em('*Datos extraídos de la página de datos abiertos de CABA'))
                     ),
                     tabPanel('Información Comuna - Barrios ',
                              br(),
                              p('Para analizar de manera descriptiva la situación de delitos se tomaron los datasets de',a(href='https://data.buenosaires.gob.ar/dataset','datos abiertos del GCBA')),
                              p('Los barrios que pertenecen a cada comuna son:'),
                              tags$div(tags$ul(
                                  tags$li('COMUNA 1: Retiro, San Nicolás, Puerto Madero, San Telmo, Monserrat y Constitución'),
                                  tags$li('COMUNA 2: Recoleta'),
                                  tags$li('COMUNA 3: San Cristóbal y Balvanera'),
                                  tags$li('COMUNA 4: La Boca, Barracas, Parque Patricios y Nueva Pompeya'),
                                  tags$li('COMUNA 5: Almagro y Boedo'),
                                  tags$li('COMUNA 6: Caballito'),
                                  tags$li('COMUNA 7: Flores y Parque Chacabuco'),
                                  tags$li('COMUNA 8: Villa Soldati, Villa Riachuelo y Villa Lugano'),
                                  tags$li('COMUNA 9: Parque Avellaneda, Liniers y Mataderos'),
                                  tags$li('COMUNA 10: Villa Real, Monte Castro, Versalles, Floresta, Vélez Sársfield y Villa Luro'),
                                  tags$li('COMUNA 11: Villa Gral. Mitre, Villa Devoto, Villa del Parque y Villa Santa Rita'),
                                  tags$li('COMUNA 12: Coghlan, Saavedra, Villa Urquiza y Villa Pueyrredón'),
                                  tags$li('COMUNA 13: Belgrano, Núñez y Colegiales'),
                                  tags$li('COMUNA 14: Palermo'),
                                  tags$li('COMUNA 15: Chacarita, Villa Crespo, Paternal, Villa Ortúzar, Agronomía y Parque Chas')
                              )))# cierra el tab panel de info comuna. 
                 ) # cierra el avlistPanel
        ),
        tabPanel('Mapas',
                 fluidRow(column(3,
                                 br(),
                                 selectInput(inputId = 'rank',
                                             label = 'Seleccione el tipo de delito',
                                             choices = names(df_delitos_2019)[3:8]),
                                 sliderInput(inputId = "year",
                                             label = "Seleccione el año a mapear",
                                             min = 2016,
                                             max = 2019,
                                             value = 2019,
                                             step = 1,
                                             round = 0,
                                             sep= 
                                 ),
                                 br(),
                                 
                                 br(),
                                 textOutput(outputId = 'texto'),
                                 br(),
                                 
                                 br(),
                                 fluidRow(column(2),
                                          column(8,
                                                 tableOutput(outputId = 'tabla_rank')),
                                          column(2))),
                          column(8,
                                 br(),
                                 br(),
                                 br(),
                                 br(),
                                 leafletOutput(outputId = 'mapas', width = "100%", height = 700)))),
        
        tabPanel('Conclusiones',
                 br(),
                 fluidRow(column(1),
                          column(10,
                                 h4('En base a lo analizado, concluimos:'),
                                 p('El conjunto de variables elegidas es relevante para explicar la dinámica de los delitos en las diferentes comunas de la Ciudad.'),
                                 p('La Ciudad de Buenos Aires tiene una dinámica delictiva en la que inciden factores demográficos, socioeconómicos y situacionales. El proyecto se propuso entender esta dinámica y crear un mapa del delito e identificar más variables que inciden en ella.'),
                                 p('Es preciso hacer una distinción entre los delitos en numero absolutos y en numero relativos a la cantidad de habitantes por comuna: en el caso de los delitos en valores absolutos, Palermo, Balvanera y Flores están siendo los barrios con mayor candidad de delitos. Si tomamos los valores relativos, vemos que San Nicolas, San Telmo y Monserrat son los más inseguros. Se hace esta distinción porque son barrios con menor cantidad de habitantes pero que reciben mucho turismo y mayor cantidad de gente yendo a trabajar a esas zonas. '),
                                 p('Los homicidios en la capital son poco frecuentes, siendo las zonas de Flores, Retiro, Villa Lugano y Barracas donde se registraron en mayor cantidad.'),
                                 p('Los barrios con afluencia de turismo como Palermo, San Nicolás y Recoleta son proclives a los hurtos, mientras que los hurtos automotores son frecuentes en Villa Lugano, Caballito, Flores y también Palermo.'),
                                 p('Los robos son frecuentes en la zona de Palermo (gran caudal turístico), Balvanera (con mucho movimiento de gente durante todo el día), Flores y Caballito. '),
                                 p('En el análisis quisimos verificar si los llamados “Barrios Populares”, inciden de alguna manera en la cantidad de delitos en zonas aledañas. El único indicio de relación se da en la zona de Retiro, que tiene la villa más extensa de la ciudad y la situación de seguridad en los barrios de Recoleta, San Nicolás donde son comunes los robos y hurtos.  '),
                                 p('La zona que es por lejos la más segura es Puerto Madero, donde tiene jurisdicción la Prefectura. '),
                                 br(),
                                 br(),
                                 br(),
                                 br()
                          ))),
        
        
        
        tabPanel('Nosotros',
                 column(1),
                 column(10,
                        br(),
                        h3(strong('El equipo'),style='color:#31408C'),
                        br(),
                        h5('Luis Bellati - Licenciado en Dirección de Negocios (UCES) - ', a(href='https://www.linkedin.com/in/luis-bellati-41541212/','LinkedIn')),
                        h5('Rocío Belén Gonzalez - Actuaria (UBA) - ',
                           a(href='https://www.linkedin.com/in/rocio-belen-gonzalez/','LinkedIn')),
                        h5('Ignacio Ramirez - Licenciado en Comercialización (UADE) - ',
                           a(href='https://www.linkedin.com/in/ignaciorv/','LinkedIn')),
                        
                        hr(),
                        br(),
                        h3(strong('Los profesores'),style='color:#31408C'),
                        br(),
                        h5('Romina Méndez'),
                        h5('Julio Spairani'),
                        br(),
                        hr(),
                        br(),
                        h3(strong('La escuela'),style='color:#31408C'),
                        br(),
                        h4('EANT - Escuela Argentina de Nuevas Tecnologías'),
                        h5('Escuela de ',a(href='https://eant.tech/escuela-de-ciencias-de-datos','Ciencia de Datos')),
                        br(),
                        hr(),
                        br(),
                        h3(strong('El proyecto'),style='color:#31408C'),
                        br(),
                        h5('Este proyecto fue llevado a cabo en el marco del desafío final del ',
                           a(href='https://eant.tech/escuela-de-ciencias-de-datos/programas/data-analytics','Programa de Data Analytics'),
                           'realizado entre el 22 de julio y el 14 de octubre del 2020.'),
                        h5(' '),
                        h5('Para esto, utilizamos diversas bases de datos, obtenidas de:'),
                        h5(tags$div(tags$ul(
                            tags$li(a(href='https://data.buenosaires.gob.ar/dataset','Datos Abiertos de la Ciudad de Buenos Aires')),
                            br(),
                            tags$li(a (href=                                               'http://datos.techo.org/fa_IR/dataset/argentina-relevamiento-nacional-de-barrios-populares-2018','Fundación Techo')),
                            br(),
                            tags$li(a     (href='https://www.indec.gob.ar/indec/web/Institucional-Indec-BasesDeDatos-1','Encuesta Permanente de Hogares (Nacional - INDEC)')),
                            
                            h5('Desarrollamos el análisis utilizando Rstudio, con los siguientes paquetes:'),
                            h5(tags$div(tags$ul(
                                tags$li('tidyverse'),
                                br(),
                                tags$li('shiny'),
                                br(),
                                tags$li('htmltools'),
                                br(),
                                tags$li('ggplot2'),
                                br(),
                                tags$li('plotly'),
                                br(),
                                tags$li('readxl'),
                                br(),
                                tags$li('shinythemes'),
                                br(),
                                tags$li('shinyWidgets'),
                                br(),
                                tags$li('htmltools'),
                                br(),
                                tags$li('geojsonsf'),
                                br(),
                                tags$li('rsconnect'),
                                br(),
                                tags$li('magrittr'),
                                br(),
                                tags$li('geojsonio')
                                
                            ))))))))))
server=function(input,output){
    
    #GRAFICOS
    
    output$tablacomuna=renderTable({
        tabla_A %>% 
            filter(tabla_A$comuna==input$comuna_n) %>% 
            rename(Total=n) %>% 
            select(barrio,Total)%>% 
            arrange(desc(Total))
    })
    
    
    ##  grafico 1 _ shiny ##
    output$grafico_1=renderPlotly({
        tabla_A %>%
            select(comuna,n) %>% 
            group_by(comuna) %>% 
            summarise(CASOS=sum(n)) %>%
            plot_ly(
                values=~CASOS,
                labels =~comuna,
                text=~paste(CASOS, 'casos'),
                textinfo='label+percent',
                insidetextfont=list(color='white'),
                hoverinfo='text',
                type='pie',
                hole=0.5,
                showlegend=FALSE,
                marker=list(colors=okabe_2)
            ) %>% 
            layout(title=list(text='Casos totales periodo 2016 - 2019',
                              font=list(color='black',size=16))
            )
        
    })  
    
    ## grafico_2 shiny ###
    output$grafico_2=renderPlot({
        tabla_B %>% 
            ggplot(data = tabla_B %>% 
                       filter(comuna==input$comuna0),
                   mapping = aes(x=subtipo_delito_2,y=n, fill=subtipo_delito_2)
            )+
            geom_col(color='black',show.legend = FALSE)+
            scale_fill_manual(values = okabe_1)+
            theme_bw()+
            theme(plot.title = element_text(hjust = 0.5,size = 20),
                  axis.text = element_text(size = 10))+
            facet_grid(barrio~.)+
            coord_flip()+
            labs(title='Tipos de Delito por comuna',
                 x='',
                 y='')
    })
    
    #### grafico_3 shiny ###
    output$grafico_3=renderPlotly({
        if(input$delito=='Total'){(
            if(input$datos=='Absolutos'){
                tabla_A%>%
                    mutate(barrio = fct_reorder(barrio,n)) %>%
                    top_n(n=15, barrio) %>% 
                    ggplot(aes(x=barrio,
                               y=n,
                               fill=barrio))+
                    geom_col(show.legend = FALSE,
                             alpha= 0.5)+
                    scale_fill_manual(values = okabe_2)+
                    coord_flip()+
                    labs(title = '     Top 15 - Barrios con mayor cantidad de Delitos',
                         x='Barrios',
                         y='Cantidad')
            }
            else if(input$datos=='Por Habitantes'){
                Tabla_E %>% 
                    select(barrio, prop) %>% 
                    mutate(barrio = fct_reorder(barrio,prop),
                           prop = round(prop, digits = 2)) %>% 
                    top_n(n=15, barrio) %>% 
                    ggplot(aes(x=barrio,
                               y=prop,
                               fill=barrio))+
                    geom_col(show.legend = FALSE,
                             alpha= 0.5)+
                    scale_fill_manual(values = okabe_2)+
                    coord_flip()+
                    labs(title = '     Top 15 - Barrios con mayor cantidad de Delitos  por Habitantes',
                         x='Barrios',
                         y='Relación')
            }
        )}
        else ({ 
            if(input$datos=='Absolutos'){
                tabla_B %>% 
                    filter(subtipo_delito_2==input$delito) %>% 
                    mutate(barrio = fct_reorder(barrio,n)) %>% 
                    top_n(n=10, barrio) %>% 
                    ggplot(aes(x=barrio,
                               y=n,
                               fill=barrio))+
                    geom_col(show.legend = FALSE,
                             alpha= 0.5)+
                    scale_fill_manual(values = okabe_2)+
                    coord_flip()+
                    labs(title = '     Top 10 - Barrios con mayor cantidad de Delitos',
                         x='Barrios',
                         y='Cantidad')
            }
            else if(input$datos=='Por Habitantes'){
                tabla_D %>% 
                    filter(subtipo_delito_2==input$delito) %>% 
                    mutate(barrio = fct_reorder(barrio,prop),
                           prop = round(prop, digits = 2)) %>% 
                    top_n(n=10, barrio) %>% 
                    ggplot(aes(x=barrio,
                               y=prop,
                               fill=barrio))+
                    geom_col(show.legend = FALSE,
                             alpha= 0.5)+
                    scale_fill_manual(values = okabe_2)+
                    coord_flip()+
                    labs(title = '     Top 10 - Barrios con mayor cantidad de Delitos  por Habitantes',
                         x='Barrios',
                         y='Relación')
            }
        })
    })    
    
    ######### grafico_4 shiny#####
    output$grafico_4=renderPlotly({
        tabla_f %>% 
            filter(comuna==input$comuna_4) %>% 
            filter(subtipo_delito_2==input$tipo_delito) %>% 
            plot_ly(
                x = ~franja_horaria, 
                y = ~n,
                split = ~comuna,
                type = 'bar',
                mode = 'markers', 
                line = list(simplyfy = F)
            )%>% 
            layout(
                xaxis = list(
                    title = "Franja Horaria",
                    zeroline = F
                ),
                yaxis = list(
                    title = "Cantidad de Casos",
                    zeroline = F
                ))
    })
    
    
    #MAPAS
    
    output$mapas=renderLeaflet({
        if (input$rank=='Homicidios' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_homicidios(df_delitos_2019$Homicidios), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Homicidios), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_homicidios, values = ~ df_delitos_2019$Homicidios, 
                          title = "Homicidios 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Homicidios' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_homicidios(df_delitos_2018$Homicidios), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Homicidios), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_homicidios, values = ~ df_delitos_2018$Homicidios, 
                          title = "Homicidios 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Homicidios' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_homicidios(df_delitos_2017$Homicidios), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Homicidios), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_homicidios, values = ~ df_delitos_2018$Homicidios, 
                          title = "Homicidios 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Homicidios' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_homicidios(df_delitos_2016$Homicidios), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Homicidios), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_homicidios, values = ~ df_delitos_2016$Homicidios, 
                          title = "Homicidios 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        #mapas Robo automotor
        
        else if (input$rank=='Robo_Automotor' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robo_automotor(df_delitos_2019$Robo_Automotor), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Robo_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robo_automotor, values = ~ df_delitos_2019$Robo_Automotor, 
                          title = "Robo Automotor 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Robo_Automotor' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robo_automotor(df_delitos_2018$Robo_Automotor), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Robo_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robo_automotor, values = ~ df_delitos_2018$Robo_Automotor, 
                          title = "Robo Automotor 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Robo_Automotor' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robo_automotor(df_delitos_2017$Robo_Automotor), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Robo_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robo_automotor, values = ~ df_delitos_2017$Robo_Automotor, 
                          title = "Robo Automotor 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Robo_Automotor' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robo_automotor(df_delitos_2016$Robo_Automotor), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Robo_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robo_automotor, values = ~ df_delitos_2016$Robo_Automotor, 
                          title = "Robo Automotor 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        #mapas robos
        
        
        else if (input$rank=='Robos' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robos(df_delitos_2019$Robos), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Robos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robos, values = ~ df_delitos_2019$Robos, 
                          title = "Robos 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Robos' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robos(df_delitos_2018$Robos), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Robos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robos, values = ~ df_delitos_2018$Robos, 
                          title = "Robos 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Robos' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robos(df_delitos_2017$Robos), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Robos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robos, values = ~ df_delitos_2017$Robos, 
                          title = "Robos 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Robos' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_robos(df_delitos_2016$Robos), group = 'Homicidios', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Robos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_robos, values = ~ df_delitos_2016$Robos, 
                          title = "Robos 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        # Hurtos
        
        
        else if (input$rank=='Hurtos' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurtos(df_delitos_2019$Hurtos), group = 'Hurtos', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Hurtos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurtos, values = ~ df_delitos_2019$Hurtos, 
                          title = "Hurtos 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Hurtos' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurtos(df_delitos_2018$Hurtos), group = 'Hurtos', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Hurtos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurtos, values = ~ df_delitos_2018$Hurtos, 
                          title = "Hurtos 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Hurtos' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurtos(df_delitos_2017$Hurtos), group = 'Hurtos', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Hurtos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurtos, values = ~ df_delitos_2017$Hurtos, 
                          title = "Hurtos 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Hurtos' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurtos(df_delitos_2016$Hurtos), group = 'Hurtos', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Hurtos), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurtos, values = ~ df_delitos_2016$Hurtos, 
                          title = "Hurtos 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        # Hurto Automotor
        
        
        else if (input$rank=='Hurto_Automotor' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurto_automotor(df_delitos_2019$Hurto_Automotor), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Hurto_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurto_automotor, values = ~ df_delitos_2019$Hurto_Automotor, 
                          title = "Hurto Automotor 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Hurto_Automotor' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurto_automotor(df_delitos_2018$Hurto_Automotor), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Hurto_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurto_automotor, values = ~ df_delitos_2018$Hurto_Automotor, 
                          title = "Hurto Automotor 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Hurto_Automotor' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurto_automotor(df_delitos_2017$Hurto_Automotor), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Hurto_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurto_automotor, values = ~ df_delitos_2017$Hurto_Automotor, 
                          title = "Hurto Automotor 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Hurto_Automotor' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_hurto_automotor(df_delitos_2016$Hurto_Automotor), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Hurto_Automotor), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_hurto_automotor, values = ~ df_delitos_2016$Hurto_Automotor, 
                          title = "Hurto Automotor 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        #Siniestro Vial
        
        
        else if (input$rank=='Siniestro_Vial' & input$year=="2019" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_siniestro_vial(df_delitos_2019$Siniestro_Vial), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2019$Siniestro_Vial), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_siniestro_vial, values = ~ df_delitos_2019$Siniestro_Vial, 
                          title = "Siniestros Viales 2019") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
        else if (input$rank=='Siniestro_Vial' & input$year=="2018" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_siniestro_vial(df_delitos_2018$Siniestro_Vial), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2018$Siniestro_Vial), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_siniestro_vial, values = ~ df_delitos_2018$Siniestro_Vial, 
                          title = "Siniestros Viales 2018") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Siniestro_Vial' & input$year=="2017" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_siniestro_vial(df_delitos_2017$Siniestro_Vial), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2017$Siniestro_Vial), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_siniestro_vial, values = ~ df_delitos_2017$Siniestro_Vial, 
                          title = "Siniestros Viales 2017") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        else if (input$rank=='Siniestro_Vial' & input$year=="2016" ){
            
            leaflet(comunas_geojson) %>% addProviderTiles("CartoDB") %>% 
                addPolygons(color = "#444444", weight = 1, smoothFactor = 0.5, 
                            opacity = 1, fillOpacity = 0.5, fillColor = ~palnumeric_siniestro_vial(df_delitos_2016$Siniestro_Vial), group = 'Hurto Automotor', highlightOptions = highlightOptions(color = "white", weight = 2, bringToFront = TRUE), 
                            label = paste0(comunas_geojson$BARRIOS, " ", 'cantidad:', " ", df_delitos_2016$Siniestro_Vial), labelOptions = labelOptions(direction = "auto") ) %>% 
                addLegend(position = "topright", pal = palnumeric_siniestro_vial, values = ~ df_delitos_2016$Siniestro_Vial, 
                          title = "Siniestros Viales 2016") %>% 
                addMarkers (data = df_comisarias, lng = ~long, lat = ~lat, popup = ~htmlEscape(nombre), icon = ~ makeIcon(  iconUrl = comisarias$icono, iconWidth = 12, iconHeight = 12), group = 'Comisarias' ) %>% 
                addPolygons(data=barrios_pop, stroke = TRUE, color = 'red', weight = 1, smoothFactor = 2, fillOpacity = 0.9, fillColor = 'red', label = ~nombre_bar,  group = 'Barrios Populares') %>%
                addLayersControl(baseGroups = c('Nada', "Barrios Populares", 'Comisarias'), position = 'topleft')
        }
        
        
    })
    
    
}
shinyApp(ui=ui,server=server)