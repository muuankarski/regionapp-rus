library(shiny)


# Choices for drop-downs

shinyUI(navbarPage("Regions of Russia", id="nav",
                   
                   tabPanel("Time-series & Maps",
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css"),
                                  # Hide the red error messages!!!
                                  tags$style(type="text/css",
                                             ".shiny-output-error { visibility: hidden; }",
                                             ".shiny-output-error:before { visibility: hidden; }"
                                             )
                                             
                                ),
                                
                                plotOutput("plot_big", width="100%", height = "100%"),
                                
                                
                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 350, height = "auto",
                                              
                                              
                                              h3("Customize the analysis"),
                                              #submitButton("Refresh! - Always Press TWICE!", icon("refresh")),
                                              
                                              uiOutput("ui_class"),
                                              uiOutput("ui_indicator"),
                                              radioButtons("plots", h5("Look at"), choices=c("Absolute Time-series"="line",
                                                                                              "Relative Time-series"="rela")),
                                              uiOutput("ui_timespan"),
                                              uiOutput("ui_year_rel"),
                                              radioButtons("maps", h5("Show"), choices=c("Only line-plot"= "No",
                                                                                         "Map with line-plot"="Yes")),
                                              uiOutput("ui_year_map"),
                                          #    uiOutput("ui_adjust_axis"),
                                              radioButtons("subset_region", h5("Scheme for subsetting regions"), 
                                                           choices=c("Economic Regions"="economic_regions",
                                                                     "Federal Districts"="federal_district",
                                                                     "Regions"="region")),                                              
                                              uiOutput("ui_region")
                                              
                                ),
                                uiOutput("small_plot")
                                )),
                                
#                                 absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
#                                               top = "auto", left = 350, right = "auto", bottom = 60,
#                                               width = 550, height = 450,
#                                               
#                                               radioButtons("subset_region", h5("Scheme for subsetting regions"), 
#                                                            choices=c("Economic Regions"="economic_regions",
#                                                                      "Federal Districts"="federal_district",
#                                                                      "Regions"="region")),                                              
#                                               uiOutput("ui_region")
#                                 #                                               textOutput("test"),
#                                 #                                               tableOutput("test"))
#                             ),


#                                 absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
#                                               top = "auto", left = 350, right = "auto", bottom = 60,
#                                               width = 550, height = 450,
#               
#                                               plotOutput("plot_small", width = 550))#,
# #                                               textOutput("test"),
# #                                               tableOutput("test"))
#                                               )
                   

                  tabPanel("Scatterplots",

                           
                           div(class="outer",
                               
                               tags$head(
                                 # Include our custom CSS
                                 includeCSS("styles.css")

                                 
                               ),
                               
                               tabsetPanel(type= "tabs", position= "above",
                                 tabPanel("Single year plot", plotOutput("plot_asso", width="100%", height = "750px")),
                                 tabPanel("All years plot", plotOutput("plot_asso_all", width="100%", height = "750px"))
                               ),
                           
                           
                            #    plotOutput("plot_asso", width="100%", height = "100%"),
                                
                                 
                               

                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 550, height = "auto",

                                              h3("Define the variables"),
                                              uiOutput("ui_class_x"),
                                              uiOutput("ui_indicator_x"),
                                              uiOutput("ui_class_y"),
                                              uiOutput("ui_indicator_y"),
                                              uiOutput("ui_year_asso"),
                                              uiOutput("ui_adjust_asso_plot"),
                                              radioButtons("subset_region_asso", h5("Scheme for subsetting regions"), 
                                                           choices=c("Economic Regions"="economic_regions",
                                                                     "Federal Districts"="federal_district",
                                                                     "Regions"="region")),
                                              uiOutput("ui_region_asso")
                                              
                                              
                                              
              
#                                              plotOutput("plot_small", width = 550))#,
#                                               textOutput("test"),
#                                               tableOutput("test"))
                                              )

                           
                           )),

                  tabPanel("Parallel coordinates",
                            
                            
                            div(class="outer",
                                
                                tags$head(
                                  # Include our custom CSS
                                  includeCSS("styles.css")
                                  
                                  
                                ),
                                
                                tabsetPanel(type= "tabs", position= "above",
                                            #tabPanel("Single year plot", plotOutput("plot_para", width="100%", height = "750px")),
                                            tabPanel("All years plot", plotOutput("plot_para", width="100%", height = "750px"))
                                ),
                                
                                
                                
                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = 60, left = "auto", right = 20, bottom = "auto",
                                              width = 300, height = "auto",
                                              
                                              h3("Define the variables"),
                                              uiOutput("ui_class_var1"),
                                              uiOutput("ui_indicator_var1"),
                                              uiOutput("ui_class_var2"),
                                              uiOutput("ui_indicator_var2"),
                                              uiOutput("ui_class_var3"),
                                              uiOutput("ui_indicator_var3"),
                                              uiOutput("ui_class_var4"),
                                              uiOutput("ui_indicator_var4"),
                                              uiOutput("ui_year_para")
                                              
                                ),
                                
                                absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                                              top = "auto", left = 350, right = "auto", bottom = 60,
                                              width = 250, height = 450,
                                              
                                              radioButtons("subset_region_para", h5("Scheme for subsetting regions"), 
                                                           choices=c("Economic Regions"="economic_regions",
                                                                     "Federal Districts"="federal_district",
                                                                     "Regions"="region")),
                                              uiOutput("ui_region_para")
                                              
                                                            )
                                                                
                            )
         
         
),

tabPanel("About",
         
         h3("Source code"),
         tags$a(href="https://github.com/muuankarski/regionapp-rus", "Source code available at Github"),
         h3("Licencing")
         
         
)
                            
))
