

mydat <- data.table( TITLE = emlak$TITLE,
                     londd=emlak$lon,
                     latdd=emlak$lat,
                     location=emlak$LOCATION,
                     m2=emlak$M2,
                     price=emlak$PRICE)

mydat$TITLE <- as.character(emlak$TYPE)
emlak$TYPE <- as.factor(emlak$TYPE)



#Set up ui
ui <- shinyUI(fluidPage(
  
  tags$head(
    tags$style(HTML("

              .well{


                    min-height: 20px;
                    padding: 19px;
                    margin-bottom: 19px;
                    background-color: rgba(175, 205, 133, 0.31);
                    
                    border: 1px solid #e31f16;
                    border-radius: 4px;

            
                }

                            .col-sm-4{
                  width: 49.33333333%
                
                }
      
                .container-fluid {

                    padding-right: 20px;
                    padding-left: 20px;
                    margin-bottom: 15px;
                    margin-right: auto;
                    margin-left: auto

                }


    
                  
                label {

                      display: inline-block;
                      max-width: 100%;
                      margin-bottom: 5px;
                      font-weight: 500;
                      color: #ca192b;

                }
                    

        
                  h1 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #ca192b;
                    margin-bottom: 20px;
                  }

                  h2 {
                    font-family: 'Lobster', cursive;
                    font-weight: 500;
                    line-height: 1.1;
                    color: #ca192b;
                    margin-bottom: 13px;
                  }
                  h3 {
                    font-size : 13px
               
                    
                    }
                    


                    
                  body {
                      background-color: rgba(71, 245, 255, 0.14);
                    }
                    
                    "))
    
  
    
    ),
  
  
  
  
  
  fluidRow(column(width = 12, offset = 5),fluidPage(leafletOutput(outputId="lmap"))),
  
  

  sidebarPanel(h1("Fiyat Belirle", width=5),
          
               numericInput(inputId="Metre", label = "M2 Giriniz " ,min = 20 , max = 100000 , value = "120"
                            ),
               
               
               textInput(inputId="Loc" ,  label = "Konum Giriniz " ),
           
               
               
               verbatimTextOutput("bbb" ),
               
               Position="Left "),
               
  
  sidebarPanel(h2("Ev Ara", width=5),
               
               selectInput(inputId="AppFlag",label=h4("KONUM"), 
                           choices=setNames(object=mydat$location,
                                            nm=mydat$location)
               ),
               

               sliderInput(inputId="MatFlag" ,label=h4("FIYAT"), min = 200000 , max = 1000000 , value = c(12,1500)),
               
               
        
               Position="Left "),
  
  
  fluidRow(h3("{Atasehir = 1, Avcilar = 2, Bahcelievler = 3, BahcelievlerYenibosna = 4, BeylikduzuGurpinar = 5,
              BeylikduzuKavakli = 6, BeylikduzuMerkez = 7, BeylikduzuYakupli = 8, Esenyurt = 9, EsenyurtHaramidere = 10,
              KadikoyGoztepe = 11, KadikoyKozyatagi = 12, KadikoySuadiye= 13, KartalRahmanlar = 14,  KartalYakacik = 15, 
              MaltepeFeyzullah = 16, MaltepeIdealTepe = 17, SancakTepeSamandira = 18, SancaktepeYenidogan = 19,
              SariyerMerkez = 20, SultanbeyliMimarsinan= 21, SultanbeyliNecipFazil = 22 }"))
               
  
  #App mainPanel content and styles
  
  
  
  
))

#Set up server
server <- function(input, output){
  

  output$bbb <- renderText( {predict(model , data.frame( M2 = input$Metre , LOCATION = input$Loc )) })

     
  #Build leaflet map
  lmap <- leaflet(data=mydat)%>%
    addProviderTiles("Esri.WorldTopoMap", 
                     options =providerTileOptions(noWrap = TRUE)) %>%
  
    fitBounds(~min(londd), ~min(latdd), ~max(londd), ~max(latdd))
    
  
  #Filter data
  datFilt <- reactive({
    
    
    mydat[ mydat$price >= input$MatFlag[1] & mydat$price <= input$MatFlag[2] & location
             %in% input$AppFlag]
    
  
  })
  

  
  #Add markers based on selected flags
  observe({
    if(nrow(datFilt())==0) {
      print("Nothing selected")
      leafletProxy("lmap") %>%
        clearMarkerClusters()
    } else { #print(paste0("Selected: ", unique(input$InFlags&input$InFlags2)))
      print("Something Selected")
      leafletProxy("lmap", data=datFilt()) %>% 
        clearMarkerClusters() %>% 
        addMarkers(lng=~londd, lat=~latdd,
                   popup = ~paste( location ,"<br>", "The Price is: ", price),
                   labelOptions = labelOptions(noHide = F, direction = 'auto'),
                   options = markerOptions(riseOnHover = TRUE),
                   clusterOptions = markerClusterOptions())
    }
  })
  
  output$lmap <- renderLeaflet(lmap)
}

#Run app
shinyApp(ui = ui, server = server)