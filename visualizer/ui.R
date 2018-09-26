#install.packages("leaflet")
library(shiny)
library(leaflet)

options(device = 'cairo')

# Choices for drop-downs
# name your variables and specifiy a short name 
vars <- c(
  "Particulate Matter" = "pm25", #"Is SuperZIP?" = "superzip",
  "Carbon Monoxide" = "CO", #"Centile score" = "centile",
  "Nitrogen Oxide" = "NO",
  "Nitrogen Dioxide" = "NO2",
  "Ozone" = "O3"
)

# build user interface main page and panels 
shinyUI(navbarPage("Adult Changes in Thought - Air Pollution / Community Air Monitoring in Puget Sound (CAMPS)", id = "nav",
                   theme = "bootstrap.css",
                   
                   
# main panel of map using leaflet                    
                   
                   tabPanel(strong("Map / Mapa"),
                            div(class = "outer",
                                
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  includeScript("gomap.js")
                                ),
                                tags$head(includeScript("google-analytics.js")),
                                
                                leafletOutput("map", width = "100%", height ="100%"),
                                
                                # absolutePanel(
                                #   id = "controls", class = "panel panel-default", fixed = TRUE,
                                #   draggable = TRUE, bottom = 20, left = "auto", right = 10, top = "auto",
                                #   width = 310, height = "auto",
                                #   HTML('<button data-toggle="collapse" data-target="#demo">Pollutant Information</button>'),
                                #   tags$div(id = 'demo',  class="collapse in",
                                #            htmlOutput('poldesc'))),
                                
                                # Shiny versions prior to 0.11 should use class="modal" instead.
                                absolutePanel(
                                  id = "controls",
                                  class = "panel panel-default",
                                  fixed = TRUE,
                                  draggable = TRUE,
                                  top = 60,
                                  #left = "auto",
                                  right = 120,
                                  bottom = "auto",
                                  width = 330,
                                  height = "auto",
                                  
                                  h4("Daily Concentration / Concentración Diaria"),
                                  h4(radioButtons("language",(""),c("English"="en", "Español"="sp"),
                                                  inline=T)),
                                  #textOutput("translateMessage"),
                                  dateInput("date", label = h4("Date / Fecha"), value = max(data_wide$date_day, na.rm=T), min="2017-01-01"),
                                  
                                  selectInput("color", label=h4("Pollutant / Contaminante"), vars, selected="pm25"),
                                  
                                  HTML('<button data-toggle="collapse" data-target="#demo">Information / Información </button>'),
                                  tags$div(id = 'demo',  class="collapse", 
                                           style = "overflow-y:scroll; max-height: 40vh",
                                           htmlOutput('poldesc'))
                                ),
                                
                                fixedPanel(
                                  id = "cite", 
                                  class = "panel panel-default", 
                                  draggable = F, bottom = 10, left = 10, right = "auto", top = "auto",
                                  width = "auto", height = "auto",
                                  HTML('<button data-toggle="collapse" data-target="#cites">INFORMATION</button>'),
                                  tags$div(id = 'cites',  #class="collapse",
                                           htmlOutput('citedesc'))
                                                 )
                                              )
                                           ),
# tab panel for hourly time series plots 

                   tabPanel(strong("Hourly Data / Datos al Hora"),
                            pageWithSidebar(
                              headerPanel('Hourly Data Plot, Select Sites and Dates'),
                              sidebarPanel(
                                dateInput("date1", label = "Date / Fecha", value =max(data_wide$date_day, na.rm=T)),
                                
                                radioButtons(
                                  inputId="radio",
                                  label="Site Selection Options:",
                                  choices=list(
                                    "All",
                                    "Manual Select"
                                  ),
                                  selected="All"),
                                
                                conditionalPanel(
                                  condition = "input.radio != 'All'",
                                  checkboxGroupInput(
                                    "tssites", 
                                    "Show these sites:",
                                    choices=site_locations$site )),
                                
                                
                                selectInput("tsvars", label=h4("Pollutant / Contaminante"), vars, selected="pm25"),
                                sliderInput("ylimpm", h4("Select Plot Limits"),min=0,step=10,value=10,max=10)
                              ),
                              mainPanel(
                                
                                plotOutput("tsPoll", height = 400),
                                tags$hr(),
                                
                                strong(textOutput("tsNotationtitle")),
                                tags$p(textOutput("tsNotation")),
                                htmlOutput('notreg'),
                                tags$hr(),
                                tags$em(strong(h3(tags$a(href="https://goo.gl/forms/Iq0Gy5ltu06CK7C22",
                                                         "Feedback / Comentarios", target="_blank"))))
                              )
                            )

                   ),
                   
# behavior modifcations panel 

                  tabPanel(strong("Health Tips / Consejos de Salud"),
                           fluidPage(
                             
                             mainPanel(
                               h3("How can you reduce you exposure? "),
                            "For most people in our area, the risk that air pollution will harm their health is relatively small. However, some people are more sensitive to the effects of air pollution, including children, the elderly, and people with asthma or heart conditions.",  
                            h4("If you are concerned about your exposure to air pollution because of your health or other factors, here are some things you can do:"),
                            tags$li("limit wood burning"),
                            tags$li("close windows in your home on sunny summer afternoons or when air quality is poor"),
                            tags$li("stay indoors on sunny summer afternoons when ozone is usually higher"),
                            tags$li("avoid outdoors physical activity on days when air quality is poor"),
                            tags$li("avoid biking, walking, or running on high-traffic roadways or near areas where wood is being burned"), hr(),              
                             h3("¿Como puedes reducir tu exposicion?"), 
                            "Para la mayoria de las personas en nuestra area, el riesgo de que la contaminacion del aire perjudique su salud es relativamente poco. Sin embargo, algunas personas son mas sensibles a los efectos de la contaminacion del aire, incluidos los niños, los ancianos y las personas con asma o afecciones cardiacas.",
                            h4("Si le preocupa su exposicion a la contaminacion del aire debido a su salud u otros factores, aqui hay algunas cosas que puede hacer:"),
                            tags$li("limitar la quema de madera"),
                            tags$li("cierre las ventanas de su casa en las soleadas tardes de verano o cuando la calidad del aire es deficiente"),
                            tags$li("quedese adentro en las soleadas tardes de verano cuando el ozono suele ser mas alto"),
                            tags$li("evite la actividad fisica al aire libre los dias en que la calidad del aire es deficiente"),
                            tags$li("evite andar en bicicleta, caminar o correr en carreteras de mucho transito o cerca de areas donde se quema madera")  
                             )                
                           )
                         ),                         
# reporting events panel 


                tabPanel(strong("Report Events / Reporte Eventos"),
                        fluidPage(
                            
                            mainPanel(
                              h3("Report Events "),
                              "Help us understand air quality in your community. Please report any air events in your community, these include smoke, fires, smells, etc." ,hr(),
                              h3("Reporte Eventos"),
                              "Ayudenos a entender la calidad del aire en su comunidad. Informe cualquier evento de aire en su comunidad, estos incluyen humo, incendios, olores, etc.",
                               hr(),
                              h4(tags$a(href="https://docs.google.com/forms/d/e/1FAIpQLSdnL-sWowYTvanvheaJxAEEKkVVEsDperzrzgnmE9MmppJCbw/viewform?embedded=true", 
                                     tags$em("Reporting form / Formulario de informe"), target = "_blank")))
                        )                
                        
                  ),                         

                  
# about us panel 

                   tabPanel(strong("About / Sobre Nosotros"),
                            fluidPage(
                              
                              mainPanel(
                                h3("Adult Changes in Thought - Air Pollution "),
                                "Welcome to the visualization of the ACT- Air Pollution monitoring project. This four-year project, funded by the National Institute of Aging (NIA) and National Institute of Environmental Health Sciences (NIEHS), has the primary goal of understanding the impact of air pollution on Alzheimer's disease and dementia." ,hr(),
                                h3("Cambios en el Pensamiento de los Adultos - Contaminación del Aire") ,
                                
                                "Bienvenido a la visualización del proyecto ACT- Control de la contaminación del aire. Este proyecto de cuatro años, financiado por el Instituto Nacional de Envejecimiento (NIA) y el Instituto Nacional de Ciencias de la Salud Ambiental (NIEHS) tiene el objetivo principal de comprender el impacto de la contaminación del aire en la enfermedad de Alzheimer y la demencia.",hr(),
                                
                                h3("Community Air Monitoring"), 
                                "The Community Air Monitoring in Puget Sound (CAMPS) companion study has the primary objective of understanding the air pollution health risks faced by communities in Puget Sound, to engage residents and to provide concerned communities the opportunity to take a decisive part of a community engaged air pollution study."  ,br(),
                                       h4(tags$a(href="http://deohs.washington.edu/camps",
                                                    "Please visit our website for updates on our study")) ,hr(),                                
                                
                                h3("Estudio del aire de la Communidad"), 
                                "El estudio acompañante (CAMPS) tiene el objetivo principal de comprender los riesgos para la contaminación del aire que enfrentan las comunidades en Puget Sound, involucrar a los residentes y brindar a las comunidades interesadas la oportunidad de tomar una parte decisiva en un estudio de contaminación del aire en la comunidad." ,br(),
                                      h4(tags$a(href="http://deohs.washington.edu/camps",
                                              "Visite nuestro sitio web para obtener actualizaciones sobre nuestro estudio"))                                 
                                

                                
                                
                              )))
                                ))
                                  

