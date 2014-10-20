
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)
library(reshape2)


shinyServer(function(input, output) {
  

  
    
  datInput_att <- reactive({
    
     load("data/attribute_data.rda")
     load("data/key_adm1.rda")
     dfA <- merge(dat,dfA,by.x="russian",by.y="region")
     dfA
  })

  datInput_map <- reactive({
  
    load("data/map_adm1.rda")
    map

  })

  output$ui_class <- renderUI({
    dfA <- datInput_att()

    dfA$class <- factor(dfA$class)
    levelit1 <- levels(dfA$class)          
    ind1 <- selectInput("class", h5("Pick a class"),choices = levelit1, selected=levelit1[1], width = "300px")
    list(ind1)
  })
  
  
output$ui_indicator <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_en", h5("Pick an indicator"),choices = levelit1, selected=levelit1[5], width = "300px")
  list(ind1)
})

output$ui_timespan <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
  
  maxyear <- max(dfA$variable)
  minyear <- min(dfA$variable)

  ip1 <- sliderInput('timespan', h5('Time span for time-series'), min=minyear, max=maxyear, value=c(minyear,maxyear))
  
  list(ip1)
})

output$ui_year_rel <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
  
  maxyear <- max(dfA$variable)
  minyear <- min(dfA$variable)
  
  if (input$plots == "line") {
    ip2 <- ""
  }
  if (input$plots == "rela") {
    ip2 <- sliderInput('year_rel', h5('Year when all regions 100'), min=minyear, max=maxyear, value=minyear+2)
  }
  list(ip2)
})

output$ui_year_map <- renderUI({
  dfA <- datInput_att()

  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]

  maxyear <- max(dfA$variable)
  minyear <- min(dfA$variable)
  
  if (input$maps == "Yes") {
    ip3 <- sliderInput('year_map', h5('Year to map'), min=minyear, max=maxyear, value=maxyear)
  }
  if (input$maps == "No") {
    ip3 <- ""
  }
  list(ip3)
})




output$ui_adjust_axis <- renderUI({
  dfA <- datInput_att()
  dfA$indicator_en <- as.character(dfA$indicator_en)
  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  
  maxvalue <- max(dfA$value)
  minvalue <- min(dfA$value)
  
  if (input$plots == "line" | input$plots == "rela") {
    ip0 <- sliderInput('adjust_yaxis', h5('Adjust Y-axis'), min=0, max=maxvalue, value=c(0,maxvalue), step = 1)  
  }
  list(ip0)
})

output$ui_region <- renderUI({
  dfA <- datInput_att()
  levels_federal_district <- as.character(levels(dfA$federal_district))[-1]
  levels_economic_regions <- as.character(levels(dfA$economic_regions))[-1]
  levels_regions <- as.character(levels(dfA$region_en))[-1]
  
  if (input$subset_region == "economic_regions") {
    ip1 <- checkboxGroupInput("subreg_economic_regions", h6("Select region:"),inline = TRUE, choices = levels_economic_regions, selected = levels_economic_regions)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
      }
  if (input$subset_region == "federal_district") {
    ip1 <- checkboxGroupInput("subreg_federal_district", h6("Select region:"),inline = TRUE, choices = levels_federal_district, selected = levels_federal_district)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
  }
  if (input$subset_region == "region") {
    ip1 <- selectInput("subreg_region1", h6("Select region 1:"), choices = c(NA,levels_regions), selected = levels_regions[2])
    ip2 <- selectInput("subreg_region2",  h6("Select region 2:"), choices = c(NA,levels_regions), selected = levels_regions[3])
    ip3 <- selectInput("subreg_region3",  h6("Select region 3:"), choices = c(NA,levels_regions), selected = levels_regions[4])
    ip4 <- selectInput("subreg_region4",  h6("Select region 4:"), choices = c(NA,levels_regions), selected = levels_regions[5])
    ip5 <- selectInput("subreg_region5",  h6("Select region 5:"), choices = c(NA,levels_regions), selected = levels_regions[6])
  }
  list(ip1,ip2,ip3,ip4,ip5)
})


#  output$test <- renderTable({
# 
#    dfA <- datInput_att()
#    dfA$indicator_en <- as.character(dfA$indicator_en)
#    dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
#    dfA$indicator_en <- factor(dfA$indicator_en)
#    if (input$plots == "rela") {
#      library(reshape2)
#      df.wide <- dcast(dfA, ID + indicator_en + region_en + federal_district + economic_regions ~ variable, value.var="value")
#       df.wide <- cbind(df.wide[1:5],df.wide[-1:-5] / eval(parse(text=paste0("df.wide$`",input$year_rel,"`"))) * 100)
#       dfA <- melt(df.wide, id.vars=c("ID","indicator_en","region_en","federal_district","economic_regions"))
# #      head(dfA)
#     } 
# head(dfA)
#    
#  })


### plot functions

## colors

myColors <- c("dodgerblue2","#E31A1C", # red
              "green4",
              "#6A3D9A", # purple
              "#FF7F00", # orange
              "black","gold1",
#               "skyblue2","#FB9A99", # lt pink
#               "palegreen2",
#               "#CAB2D6", # lt purple
#               "#FDBF6F", # lt orange
#               "gray70", "khaki2",
#              "maroon","orchid1","deeppink1",
              "blue1","steelblue4",
              "darkturquoise","green1","yellow4","yellow3",
              "darkorange4","brown")



  
  output$small_plot <- renderUI({
    if (input$maps == "Yes") {
    absolutePanel(id = "controls", class = "modal", fixed = TRUE, draggable = TRUE,
                  top = 60, left = 350, right = "auto", bottom = "auto",
                  width = 550, height = 450,
                  
                  plotOutput("plot_small", width = 550))
    }
    
  }) 
  








plot_line <- function(x) {
  dfA <- datInput_att()

  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
  
  # relativise
  
  if (input$plots == "rela") {
    library(reshape2)
    df.wide <- dcast(dfA, ID + indicator_en + region_en + federal_district + economic_regions ~ variable, value.var="value")
    df.wide <- cbind(df.wide[1:5],df.wide[-1:-5] / eval(parse(text=paste0("df.wide$`",input$year_rel,"`"))) * 100)
    dfA <- melt(df.wide, id.vars=c("ID","indicator_en","region_en","federal_district","economic_regions"))
    dfA$variable <- as.numeric(levels(dfA$variable))[dfA$variable]
  }
  
#  if (input$plots == "line") {
    dfA <- dfA[dfA$variable >= input$timespan[1],]
    dfA <- dfA[dfA$variable <= input$timespan[2],]
#  }  

  
  if (input$subset_region == "economic_regions") dfA <- dfA[dfA$economic_regions %in% input$subreg_economic_regions,]
  if (input$subset_region == "federal_district") dfA <- dfA[dfA$federal_district %in% input$subreg_federal_district,]
  if (input$subset_region == "region") dfA <- dfA[dfA$region_en %in% c(input$subreg_region1,
                                                                              input$subreg_region2,
                                                                              input$subreg_region3,
                                                                              input$subreg_region4,
                                                                              input$subreg_region5),]
  
    if (input$subset_region == "economic_regions") {
      color_obj <- "factor(economic_regions)"
      #
      names(myColors) <- levels(dfA$economic_regions)
      library(ggplot2)
      colScale <- scale_colour_manual(name = "Economic Regions",
                                      values = myColors)
    }
    if (input$subset_region == "federal_district") {
      color_obj <- "factor(federal_district)"
      #
      names(myColors) <- levels(dfA$federal_district)
      library(ggplot2)
      colScale <- scale_colour_manual(name = "Federal Districts",
                                      values = myColors)
    }
    if (input$subset_region == "region") {
      color_obj <- "factor(region_en)"
      #
      library(ggplot2)
      library(RColorBrewer)
      myColors <- brewer.pal(5,"Set1")
      colScale <- scale_colour_manual(name = "Regions",
                                      values = myColors)
    }
  
  library(ggplot2)
  require(grid)

if (input$plots == "rela") {
  line_100 <- geom_hline(aes_string(yintercept = 100), colour="Black", linetype="dashed")
  line_rel <- geom_vline(aes_string(xintercept = input$year_rel), colour="Black", linetype="dashed") #+
    #annotate("text", x = input$year_rel , y=median(dfA$value, na.rm = TRUE)*2, label = "Year all 100", colour="Black", size=5)
}
if (input$plots != "rela") {
  line_100 <- geom_blank()
  line_rel <- geom_blank()
}

if (input$maps == "Yes") {
  line_map <- geom_vline(aes_string(xintercept = input$year_map), colour="Dim grey", linetype="dashed") #+
#     annotate("text",x = input$year_map ,y=median(dfA$value, na.rm = TRUE)*2,
#              label = "Map year",colour="Dim grey", size=5)
}

if (input$maps == "No") {
  line_map <- geom_blank()
}


print(ggplot(dfA, aes_string(x="variable",y="value",group="region_en",color=color_obj)) +
          geom_point(size=3) + geom_line()  +
         labs(title = as.character(input$indicator_en)) +
          theme_bw() +
          theme(text = element_text(family="Open Sans")) +
          # map year annotation
          line_map +
          # ----------------------- #
          # map relative annotation
          line_100 +
          line_rel +
          
          theme(legend.position="top") +
          theme(legend.text=element_text(size=12)) +
          #theme(legend.title = element_blank()) +
          guides(color=guide_legend(nrow=2)) +
          geom_text(data=merge(dfA, 
                               aggregate(variable ~ region_en, dfA, max),
                               by=c("variable","region_en"), all.y=TRUE),
                    aes(x=variable, y = value, label=region_en),
                    hjust=-0.1,vjust=-1,size=4) + 
          geom_text(data=merge(dfA, 
                               aggregate(variable ~ region_en, dfA, min),
                               by=c("variable","region_en"), all.y=TRUE),
                    aes(x=variable, y = value, label=region_en),
                    hjust=.5,vjust=-1,size=4) +
           coord_cartesian(xlim=c(min(dfA$variable-1),max(dfA$variable)+3),
                           ylim=input$adjust_yaxis) +
          colScale
  )
}

plot_map <- function(x) {
  
  dfA <- datInput_att()
  dfA$indicator_en <- as.character(dfA$indicator_en)
  dfA <- dfA[dfA$indicator_en == as.character(input$indicator_en),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  
  # relativise
  
  if (input$plots == "rela") {
    library(reshape2)
    df.wide <- dcast(dfA, ID + indicator_en + region_en + federal_district + economic_regions ~ variable, value.var="value")
    df.wide <- cbind(df.wide[1:5],df.wide[-1:-5] / eval(parse(text=paste0("df.wide$`",input$year_rel,"`"))) * 100)
    dfA <- melt(df.wide, id.vars=c("ID","indicator_en","region_en","federal_district","economic_regions"))
    dfA$variable <- as.numeric(levels(dfA$variable))[dfA$variable]
  }
  
  
  if (input$subset_region == "economic_regions") dfA <- dfA[dfA$economic_regions %in% input$subreg_economic_regions,]
  if (input$subset_region == "federal_district") dfA <- dfA[dfA$federal_district %in% input$subreg_federal_district,]
  if (input$subset_region == "region") dfA <- dfA[dfA$region_en %in% c(input$subreg_region1,
                                                                       input$subreg_region2,
                                                                       input$subreg_region3,
                                                                       input$subreg_region4,
                                                                       input$subreg_region5),]
  
  dfA <- dfA[dfA$variable == input$year_map,]
  
  library(maptools)
  library(rgdal)
  library(mapproj)

  shape <- datInput_map()
  
    # subset map to match number of regions in data
  shape <- shape[shape@data$ID_1 %in% as.character(dfA$ID),]
  
  # prepare for spCbind using row.names
  ## rename rows
  dfA <- dfA[!duplicated(dfA[c("ID")]),] # remove possible duplicates!
    
  row.names(shape) <- as.character(shape$ID_1)
  row.names(dfA) <- as.character(dfA$ID)
  ## order
  dfA <- dfA[order(row.names(dfA)), ]
  shape <- shape[order(row.names(shape)), ]
  ## bind
  
  shape <- spCbind(shape, dfA)
  
  library(ggplot2)
  library(rgeos)
  library(RColorBrewer)
  library(scales)
  
  shape$id <- rownames(shape@data)
  map.points <- fortify(shape, region = "id")
  map.df <- merge(map.points, shape, by = "id")
  
  cnames <- stats:::aggregate.formula(cbind(long, lat) ~ region_en, data=map.df, mean) # region names
  cnames <- merge(cnames,dfA[c("region_en","value")],by="region_en") 
  cnames$value <- round(cnames$value, 1)

  
  
  if (input$plots == "rela") {
    fill <- scale_fill_gradient2(low = "#018571", high = "#a6611a", mid = "#f5f5f5", midpoint = 100)
  }
  if (input$plots == "line") {
    fill <- scale_fill_continuous(low="#ffffb2", high="#bd0026")
  }  
  

  
  print(ggplot(map.df, aes(long,lat,group=group)) +
          geom_polygon(aes(fill = value), colour = alpha("white", 1/2), size = 0.2) +
          geom_polygon(data = map.df, aes(long,lat), 
                       fill=NA, 
                       color = "white",
                       size=0.1) + # white borders
          geom_text(data=cnames, aes(long, lat, label = region_en, group=region_en), size=3, color="black") +
          geom_text(data=cnames, aes(long, lat, label = value, group=region_en), size=3, vjust=-1, color="black") +
          coord_map(project="orthographic", orientation=c(30, 85, 0)) +
          theme(legend.position="top") +
          theme(text = element_text(family="Open Sans")) +
          fill
  )
}

  

  output$plot_big <- renderPlot({
    
    if (input$maps == "Yes") {
      plot_map()
    }
      
    if (input$maps == "No") {
      plot_line()
    }
  })

output$plot_small <- renderPlot({
  
  if (input$maps == "Yes") {
    plot_line()
  }
  
  if (input$maps == "No") {
    #plot_map()
  }
})

## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##
##       Associations
## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##

# X-var

output$ui_class_x <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_x", h5("Pick a class for X-var"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_indicator_x <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_x),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_x", h5("Pick an indicator for X-var"),choices = levelit1, selected = levelit1[3], width = "300px")
  list(ind1)
})

# Y-var

output$ui_class_y <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_y", h5("Pick a class for Y-var"),choices = levelit1, selected = levelit1[2],  width = "300px")
  list(ind1)
})


output$ui_indicator_y <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_y),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_y", h5("Pick an indicator for Y-var"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_year_asso <- renderUI({
  dfA <- datInput_att()
  
  dfX <- dfA[dfA$indicator_en == as.character(input$indicator_x),]
  dfY <- dfA[dfA$indicator_en == as.character(input$indicator_y),]
  dfZ <- merge(dfX[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               dfY[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               by=c("ID","variable"))
  
  dfZ$value.x[dfZ$value.x == 0] <- NA
  dfZ$value.y[dfZ$value.y == 0] <- NA
  dfZ <- dfZ[!is.na(dfZ$value.x),]
  dfZ <- dfZ[!is.na(dfZ$value.y),]
  
  year_list <- unique(dfZ$variable)
  
  ip3 <- selectInput('year_asso', h5('Year'), choices= year_list,
                     selected = year_list[1])

  list(ip3)
})


output$ui_region_asso <- renderUI({
  dfA <- datInput_att()
  levels_federal_district <- as.character(levels(dfA$federal_district))[-1]
  levels_economic_regions <- as.character(levels(dfA$economic_regions))[-1]
  levels_regions <- as.character(levels(dfA$region_en))[-1]
  
  ip0 <- h3("Subset the regions")
  
  if (input$subset_region_asso == "economic_regions") {
    ip1 <- checkboxGroupInput("subreg_economic_regions_asso", h6("Select region:"),inline = TRUE, choices = levels_economic_regions, selected = levels_economic_regions)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
  }
  if (input$subset_region_asso == "federal_district") {
    ip1 <- checkboxGroupInput("subreg_federal_district_asso", h6("Select region:"),inline = TRUE, choices = levels_federal_district, selected = levels_federal_district)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
  }
  if (input$subset_region_asso == "region") {
    ip1 <- selectInput("subreg_region1_asso", h6("Select region 1:"), choices = c(NA,levels_regions), selected = levels_regions[2])
    ip2 <- selectInput("subreg_region2_asso",  h6("Select region 2:"), choices = c(NA,levels_regions), selected = levels_regions[3])
    ip3 <- selectInput("subreg_region3_asso",  h6("Select region 3:"), choices = c(NA,levels_regions), selected = levels_regions[4])
    ip4 <- selectInput("subreg_region4_asso",  h6("Select region 4:"), choices = c(NA,levels_regions), selected = levels_regions[5])
    ip5 <- selectInput("subreg_region5_asso",  h6("Select region 5:"), choices = c(NA,levels_regions), selected = levels_regions[6])
  }
  list(ip1,ip2,ip3,ip4,ip5)
})


output$ui_adjust_asso_plot <- renderUI({
  
#   dfA <- datInput_att()
#   
#   dfX <- dfA[dfA$indicator_en == as.character(input$indicator_x),]
#   dfY <- dfA[dfA$indicator_en == as.character(input$indicator_y),]
#   dfZ <- merge(dfX[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
#                dfY[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
#                by=c("ID","russian","region_en","economic_regions","federal_district","variable"), all.x=TRUE)
#   
#   dfZ <- dfZ[dfZ$variable == input$year_asso, ]
#   
#   max_x <- max(dfZ$value.x)
#   max_y <- max(dfZ$value.y)
#   
#   ip1 <- sliderInput('axis_x', h5('Limit x-axis'), min=0, max=max_x, value=c(0,max_x))
#   ip2 <- sliderInput('axis_y', h5('Limit y-axis'), min=0, max=max_y, value=c(0,max_y))
  ip2 <- h4("Adjust the plot ")
  ip3 <- selectInput('smooth_method', h5('Method for smoothing'), 
                     choices= c("no smoothing",
                                "loess",
                                "glm",
                                "lm"
                                ))
  
  
  
                     
#   ip3 <- selectInput('year_asso', h5('Year'), choices= year_list,
#                      selected = year_list[1])
  
  list(ip2,ip3)
})


# output$test <- renderTable({
#   
#   dfA <- datInput_att()
#   
#   dfX <- dfA[dfA$indicator_en == as.character(input$indicator_x),]
#   dfY <- dfA[dfA$indicator_en == as.character(input$indicator_y),]
#   dfZ <- merge(dfX[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
#                dfY[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
#                by=c("ID","russian","region_en","economic_regions","federal_district","variable"), all.x=TRUE)
#   
#   dfZ <- dfZ[dfZ$variable == input$year_asso, ]
#   head(dfZ)
#   
# })



plot_asso <- function(x) {
  
  
  dfA <- datInput_att()
    
  dfX <- dfA[dfA$indicator_en == as.character(input$indicator_x),]
  dfY <- dfA[dfA$indicator_en == as.character(input$indicator_y),]
  dfZ <- merge(dfX[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               dfY[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               by=c("ID","russian","region_en","economic_regions","federal_district","variable"))
  
  dfZ$value.x[dfZ$value.x == 0] <- NA
  dfZ$value.y[dfZ$value.y == 0] <- NA
  dfZ <- dfZ[!is.na(dfZ$value.x),]
  dfZ <- dfZ[!is.na(dfZ$value.y),]

  dfZ <- dfZ[dfZ$variable == input$year_asso, ]
  
  if (input$subset_region_asso == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_asso,]
  if (input$subset_region_asso == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_asso,]
  if (input$subset_region_asso == "region") dfZ <- dfZ[dfZ$region_en %in% c(input$subreg_region1_asso,
                                                                       input$subreg_region2_asso,
                                                                       input$subreg_region3_asso,
                                                                       input$subreg_region4_asso,
                                                                       input$subreg_region5_asso),]
  
  
  if (input$subset_region_asso == "economic_regions") {
    color_obj <- "factor(economic_regions)"
    #
    names(myColors) <- levels(dfZ$economic_regions)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Economic Regions",
                                    values = myColors)
  }
  if (input$subset_region_asso == "federal_district") {
    color_obj <- "factor(federal_district)"
    #
    names(myColors) <- levels(dfZ$federal_district)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Federal Districts",
                                    values = myColors)
  }
  if (input$subset_region_asso == "region") {
    color_obj <- "factor(region_en)"
    #
    library(ggplot2)
    library(RColorBrewer)
    myColors <- brewer.pal(5,"Set1")
    colScale <- scale_colour_manual(name = "Regions",
                                    values = myColors)
  }
  
    
  
  if (input$smooth_method == "no smoothing") {
    smoothing <- geom_blank()
  }
  if (input$smooth_method == "loess") {
    smoothing <- geom_smooth(method = loess)
  }
  if (input$smooth_method == "glm") {
    smoothing <- geom_smooth(method = glm)
  }
  if (input$smooth_method == "lm") {
    smoothing <- geom_smooth(method = lm)
  }  
  
  
  print(ggplot(dfZ, aes_string(x="value.x",y="value.y",group="1",color=color_obj,label="region_en")) + 
    geom_point(size=3) + geom_text(vjust=1, hjust=-.1,family = "Open Sans", size=3.5) +
    smoothing +
    coord_cartesian(xlim=input$axis_x,ylim=input$axis_y) +
    colScale +
    theme_bw() +
    theme(legend.position =  "top")
  )
  
  
 
  

}

plot_asso_all <- function(x) {
  
  
  dfA <- datInput_att()
  
  dfX <- dfA[dfA$indicator_en == as.character(input$indicator_x),]
  dfY <- dfA[dfA$indicator_en == as.character(input$indicator_y),]
  dfZ <- merge(dfX[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               dfY[c("ID","russian","region_en","federal_district","economic_regions","indicator_ru","indicator_en","variable","value")],
               by=c("ID","russian","region_en","economic_regions","federal_district","variable"))
  
  dfZ$value.x[dfZ$value.x == 0] <- NA
  dfZ$value.y[dfZ$value.y == 0] <- NA
  dfZ <- dfZ[!is.na(dfZ$value.x),]
  dfZ <- dfZ[!is.na(dfZ$value.y),]
  
  
  if (input$subset_region_asso == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_asso,]
  if (input$subset_region_asso == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_asso,]
  if (input$subset_region_asso == "region") dfZ <- dfZ[dfZ$region_en %in% c(input$subreg_region1_asso,
                                                                            input$subreg_region2_asso,
                                                                            input$subreg_region3_asso,
                                                                            input$subreg_region4_asso,
                                                                            input$subreg_region5_asso),]
  
  
  if (input$subset_region_asso == "economic_regions") {
    color_obj <- "factor(economic_regions)"
    #
    names(myColors) <- levels(dfZ$economic_regions)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Economic Regions",
                                    values = myColors)
  }
  if (input$subset_region_asso == "federal_district") {
    color_obj <- "factor(federal_district)"
    #
    names(myColors) <- levels(dfZ$federal_district)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Federal Districts",
                                    values = myColors)
  }
  if (input$subset_region_asso == "region") {
    color_obj <- "factor(region_en)"
    #
    library(ggplot2)
    library(RColorBrewer)
    myColors <- brewer.pal(5,"Set1")
    colScale <- scale_colour_manual(name = "Regions",
                                    values = myColors)
  }
  

  
  if (input$smooth_method == "no smoothing") {
    smoothing <- geom_blank()
  }
  if (input$smooth_method == "loess") {
    smoothing <- geom_smooth(method = loess)
  }
  if (input$smooth_method == "glm") {
    smoothing <- geom_smooth(method = glm)
  }
  if (input$smooth_method == "lm") {
    smoothing <- geom_smooth(method = lm)
  }  
  #aes_string(x="value.x",y="value.y",group="1",color=color_obj,label="region_en")
  
  plot <-  ggplot(dfZ, aes(x=value.x,y=value.y,group=1,label=region_en,color=economic_regions)) + 
      geom_point(size=3) + geom_text(vjust=1, hjust=-.1,family = "Open Sans", size=3.5) +
      smoothing +
      #coord_cartesian(xlim=input$axis_x,ylim=input$axis_y) +
      colScale + # ei toimi nyt kun makroalueiden mukainen väritys ei päällä (ei toimi korrelaatiofunktion takia)
      theme_bw() +
      theme(legend.position =  "top") +
      facet_wrap(~variable, scale= "free")
    
    
  library(plyr)
  cors <- ddply(dfZ, .(variable), summarise, cor = round(cor(value.x, value.y, "pairwise.complete.obs"), 2))
  print(  
    plot + geom_text(data=cors, aes(label=paste0("cor = ", cor, sep="")), 
                     x=-Inf,y=Inf,hjust=-.6,vjust=2,
                     family="Open Sans", size=6, color="Dim grey")
  )
  
}

output$plot_asso <- renderPlot({
  
plot_asso()

  })


output$plot_asso_all <- renderPlot({
  
  plot_asso_all()
  
})


## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##
##       Parallel Coordinates Plot
## --------------------------------------------------------------- ##
## --------------------------------------------------------------- ##


# 1-var

output$ui_class_var1 <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_var1", h5("Pick a class for variable 1"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_indicator_var1 <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_var1),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_var1", h5("Pick an indicator for variable 1"),choices = levelit1, selected = levelit1[3], width = "300px")
  list(ind1)
})

# 2-var

output$ui_class_var2 <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_var2", h5("Pick a class for variable 2"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_indicator_var2 <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_var2),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_var2", h5("Pick an indicator for variable 2"),choices = levelit1, selected = levelit1[4], width = "300px")
  list(ind1)
})

# 3-var

output$ui_class_var3 <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_var3", h5("Pick a class for variable 3"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_indicator_var3 <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_var3),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_var3", h5("Pick an indicator for variable 3"),choices = levelit1, selected = levelit1[5], width = "300px")
  list(ind1)
})

# 4-var

output$ui_class_var4 <- renderUI({
  dfA <- datInput_att()
  dfA$class <- factor(dfA$class)
  levelit1 <- levels(dfA$class)          
  ind1 <- selectInput("class_var4", h5("Pick a class for variable 4"),choices = levelit1, width = "300px")
  list(ind1)
})


output$ui_indicator_var4 <- renderUI({
  dfA <- datInput_att()
  
  dfA <- dfA[dfA$class == as.character(input$class_var4),]
  dfA$indicator_en <- factor(dfA$indicator_en)
  levelit1 <- levels(dfA$indicator_en)          
  ind1 <- selectInput("indicator_var4", h5("Pick an indicator for variable 4"),choices = levelit1, selected = levelit1[6], width = "300px")
  list(ind1)
})



output$ui_year_para <- renderUI({
  dfA <- datInput_att()
  
  df1 <- dfA[dfA$indicator_en == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$indicator_en == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$indicator_en == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$indicator_en == as.character(input$indicator_var4),]

  dw1 <- dcast(df1, region_en + variable ~ indicator_en, value.var="value")
  dw2 <- dcast(df2, region_en + variable ~ indicator_en, value.var="value")
  dw3 <- dcast(df3, region_en + variable ~ indicator_en, value.var="value")
  dw4 <- dcast(df4, region_en + variable ~ indicator_en, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("region_en","variable"))
  dwx <- merge(dwx,dw3,by=c("region_en","variable"))
  dwx <- merge(dwx,dw4,by=c("region_en","variable"))
  
  dwx <- na.omit(dwx) 

  year_list <- unique(dwx$variable)
  
  ip3 <- selectInput('year_para', h5('Year'), choices= year_list,
                     selected = year_list[1])
  
  list(ip3)
})


output$ui_region_para <- renderUI({
  dfA <- datInput_att()
  levels_federal_district <- as.character(levels(dfA$federal_district))[-1]
  levels_economic_regions <- as.character(levels(dfA$economic_regions))[-1]
  levels_regions <- as.character(levels(dfA$region_en))[-1]
  
  ip0 <- h3("Subset the regions")
  
  if (input$subset_region_para == "economic_regions") {
    ip1 <- checkboxGroupInput("subreg_economic_regions_para", h6("Select region:"),inline = TRUE, choices = levels_economic_regions, selected = levels_economic_regions)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
  }
  if (input$subset_region_para == "federal_district") {
    ip1 <- checkboxGroupInput("subreg_federal_district_para", h6("Select region:"),inline = TRUE, choices = levels_federal_district, selected = levels_federal_district)
    ip2 <- ""
    ip3 <- ""
    ip4 <- ""
    ip5 <- ""
  }
  if (input$subset_region_para == "region") {
    ip1 <- selectInput("subreg_region1_para", h6("Select region 1:"), choices = c(NA,levels_regions), selected = levels_regions[2])
    ip2 <- selectInput("subreg_region2_para",  h6("Select region 2:"), choices = c(NA,levels_regions), selected = levels_regions[3])
    ip3 <- selectInput("subreg_region3_para",  h6("Select region 3:"), choices = c(NA,levels_regions), selected = levels_regions[4])
    ip4 <- selectInput("subreg_region4_para",  h6("Select region 4:"), choices = c(NA,levels_regions), selected = levels_regions[5])
    ip5 <- selectInput("subreg_region5_para",  h6("Select region 5:"), choices = c(NA,levels_regions), selected = levels_regions[6])
  }
  list(ip1,ip2,ip3,ip4,ip5)
})


plot_para <- function(x) {
  
  
  dfA <- datInput_att()
  
  df1 <- dfA[dfA$indicator_en == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$indicator_en == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$indicator_en == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$indicator_en == as.character(input$indicator_var4),]
  
#   df1 <- dfA[dfA$indicator_en == "mining", ]
#   df2 <- dfA[dfA$indicator_en == "Education", ]
#   df4 <- dfA[dfA$indicator_en == "all", ]
#   df3 <- dfA[dfA$indicator_en == "Transport and communications", ]
  
  
  dw1 <- dcast(df1, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw2 <- dcast(df2, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw3 <- dcast(df3, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw4 <- dcast(df4, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("region_en","variable","federal_district","economic_regions"))
  dwx <- merge(dwx,dw3,by=c("region_en","variable","federal_district","economic_regions"))
  dwx <- merge(dwx,dw4,by=c("region_en","variable","federal_district","economic_regions"))
  
  dfZ <- na.omit(dwx) 
  

  
  #dfZ <- dfZ[dfZ$variable == 2010,]
  
  if (input$subset_region_para == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_para,]
  if (input$subset_region_para == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_para,]
  if (input$subset_region_para == "region") dfZ <- dfZ[dfZ$region_en %in% c(input$subreg_region1_para,
                                                                            input$subreg_region2_para,
                                                                            input$subreg_region3_para,
                                                                            input$subreg_region4_para,
                                                                            input$subreg_region5_para),]
  
    if (input$subset_region_para == "economic_regions") {
    color_obj <- "factor(economic_regions)"
    #
    names(myColors) <- levels(dfZ$economic_regions)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Economic Regions",
                                    values = myColors)
  }
  if (input$subset_region_para == "federal_district") {
    color_obj <- "factor(federal_district)"
    #
    names(myColors) <- levels(dfZ$federal_district)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Federal Districts",
                                    values = myColors)
  }
  if (input$subset_region_para == "region") {
    color_obj <- "factor(region_en)"
    #
    library(ggplot2)
    library(RColorBrewer)
    myColors <- brewer.pal(5,"Set1")
    colScale <- scale_colour_manual(name = "Regions",
                                    values = myColors)
  }
  #library(GGally)
# Generate basic parallel coordinate plot
# p <- ggparcoord(data = dfZ,                 
#                 # Which columns to use in the plot
#                 columns = 5:8,                 
#                 # Which column to use for coloring data
#                 groupColumn = 1,                 
#                 # Allows order of vertical bars to be modified
#                 order = 6,                
#                 # Do not show points
#                 showPoints = FALSE,                
#                 # Turn on alpha blending for dense plots
#                 alphaLines = 0.6,                
#                 # Turn off box shading range
#                 shadeBox = NULL,                
#                 # Will normalize each column's values to [0, 1]
#                 scale = "uniminmax"#, # try "std" also + boxplot = TRUE
#                 ) +
#                 theme_minimal() +  # Start with a basic theme 
#                 scale_y_continuous(expand = c(0.02, 0.02)) + 
#                 scale_x_discrete(expand = c(0.02, 0.02)) + # Decrease amount of margin around x, y values
#                 theme(axis.ticks = element_blank()) + # Remove axis ticks and labels
#                 theme(axis.title = element_blank()) + # Remove axis ticks and labels
#                 theme(axis.text.y = element_blank()) + # Remove axis ticks and labels
#                 theme(legend.position = "top") #+


dfZZ <- dfZ

names(dfZZ)[2] <- "year"

dfZZ$ID <- 1:nrow(dfZZ)

range01 <- function(x){(x-min(x))/(max(x)-min(x))}

dfZZ[5] <- range01(dfZZ[5])
dfZZ[6] <- range01(dfZZ[6])
dfZZ[7] <- range01(dfZZ[7])
dfZZ[8] <- range01(dfZZ[8])

df.x <- melt(dfZZ,id.vars=c("ID","region_en","federal_district","year"), 
             measure.vars = c(input$indicator_var1,
                              input$indicator_var2,
                              input$indicator_var3,
                              input$indicator_var4))

df.x$variable <- factor(df.x$variable, levels=c(input$indicator_var1,
                                                input$indicator_var2,
                                                input$indicator_var3,
                                                input$indicator_var4))

df.x <- df.x[df.x$year == input$year_para,]

enddata <- df.x[df.x$variable == input$indicator_var4,]
thirddata <- df.x[df.x$variable == input$indicator_var3,]
seconddata <- df.x[df.x$variable == input$indicator_var2,]
begindata <- df.x[df.x$variable == input$indicator_var1,]

p <- ggplot(df.x,aes(x=variable,y=value,colour=federal_district,group=ID))+
  geom_line() +
  geom_text(data=enddata, 
            aes(x=4, y=value,label=region_en),
            hjust=-.2,size=3.5,family="Open Sans") +
  geom_text(data=begindata, 
            aes(x=1, y=value,label=region_en), 
            hjust=1,size=3.5,family="Open Sans") +
  geom_text(data=thirddata, 
            aes(x=3, y=value,label=region_en), 
            hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
  geom_text(data=seconddata, 
            aes(x=2, y=value,label=region_en), 
            hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
  theme_bw() +
  theme(text = element_text(family="Open Sans")) +
  theme(legend.position="top") +
  theme(legend.text=element_text(size=12)) +
  guides(color=guide_legend(nrow=2))
  

# px <- ggplot(df.x,aes(x=variable,y=value,colour=federal_district,group=ID))+
#   geom_line() + facet_wrap(~year)
#   theme(legend.position="none")


print(p)



}

plot_para_all <- function(x) {
  
  
  dfA <- datInput_att()
  
  df1 <- dfA[dfA$indicator_en == as.character(input$indicator_var1),]
  df2 <- dfA[dfA$indicator_en == as.character(input$indicator_var2),]
  df3 <- dfA[dfA$indicator_en == as.character(input$indicator_var3),]
  df4 <- dfA[dfA$indicator_en == as.character(input$indicator_var4),]
  
  #   df1 <- dfA[dfA$indicator_en == "mining", ]
  #   df2 <- dfA[dfA$indicator_en == "Education", ]
  #   df4 <- dfA[dfA$indicator_en == "all", ]
  #   df3 <- dfA[dfA$indicator_en == "Transport and communications", ]
  
  
  dw1 <- dcast(df1, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw2 <- dcast(df2, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw3 <- dcast(df3, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  dw4 <- dcast(df4, region_en + variable + federal_district + economic_regions ~ indicator_en, value.var="value")
  
  dwx <- merge(dw1,dw2,by=c("region_en","variable","federal_district","economic_regions"))
  dwx <- merge(dwx,dw3,by=c("region_en","variable","federal_district","economic_regions"))
  dwx <- merge(dwx,dw4,by=c("region_en","variable","federal_district","economic_regions"))
  
  dfZ <- na.omit(dwx) 
  
  
  
  #dfZ <- dfZ[dfZ$variable == 2010,]
  
  if (input$subset_region_para == "economic_regions") dfZ <- dfZ[dfZ$economic_regions %in% input$subreg_economic_regions_para,]
  if (input$subset_region_para == "federal_district") dfZ <- dfZ[dfZ$federal_district %in% input$subreg_federal_district_para,]
  if (input$subset_region_para == "region") dfZ <- dfZ[dfZ$region_en %in% c(input$subreg_region1_para,
                                                                            input$subreg_region2_para,
                                                                            input$subreg_region3_para,
                                                                            input$subreg_region4_para,
                                                                            input$subreg_region5_para),]
  
  if (input$subset_region_para == "economic_regions") {
    color_obj <- "factor(economic_regions)"
    #
    names(myColors) <- levels(dfZ$economic_regions)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Economic Regions",
                                    values = myColors)
  }
  if (input$subset_region_para == "federal_district") {
    color_obj <- "factor(federal_district)"
    #
    names(myColors) <- levels(dfZ$federal_district)
    library(ggplot2)
    colScale <- scale_colour_manual(name = "Federal Districts",
                                    values = myColors)
  }
  if (input$subset_region_para == "region") {
    color_obj <- "factor(region_en)"
    #
    library(ggplot2)
    library(RColorBrewer)
    myColors <- brewer.pal(5,"Set1")
    colScale <- scale_colour_manual(name = "Regions",
                                    values = myColors)
  }

  
  dfZZ <- dfZ
  
  names(dfZZ)[2] <- "year"
  
  dfZZ$ID <- 1:nrow(dfZZ)
  
  range01 <- function(x){(x-min(x))/(max(x)-min(x))}
  
  dfZZ[5] <- range01(dfZZ[5])
  dfZZ[6] <- range01(dfZZ[6])
  dfZZ[7] <- range01(dfZZ[7])
  dfZZ[8] <- range01(dfZZ[8])
  
  df.x <- melt(dfZZ,id.vars=c("ID","region_en","federal_district","year"), 
               measure.vars = c(input$indicator_var1,
                                input$indicator_var2,
                                input$indicator_var3,
                                input$indicator_var4))
  
  df.x$variable <- factor(df.x$variable, levels=c(input$indicator_var1,
                                                  input$indicator_var2,
                                                  input$indicator_var3,
                                                  input$indicator_var4))
  
  #df.x <- df.x[df.x$year == input$year_para,]
  
  enddata <- df.x[df.x$variable == input$indicator_var4,]
  thirddata <- df.x[df.x$variable == input$indicator_var3,]
  seconddata <- df.x[df.x$variable == input$indicator_var3,]
  begindata <- df.x[df.x$variable == input$indicator_var1,]
  
  
  p <- ggplot(df.x,aes(x=variable,y=value,colour=federal_district,group=ID))+
    geom_line() + facet_wrap(~year) +
    geom_text(data=enddata, 
              aes(x=4, y=value,label=region_en),
              hjust=-.2,size=3.5,family="Open Sans") +
    geom_text(data=begindata, 
              aes(x=1, y=value,label=region_en), 
              hjust=1,size=3.5,family="Open Sans") +
    geom_text(data=thirddata, 
              aes(x=3, y=value,label=region_en), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    geom_text(data=seconddata, 
              aes(x=2, y=value,label=region_en), 
              hjust=.3,vjust=-.5,size=3.5,family="Open Sans") +
    theme_bw() +
    theme(text = element_text(family="Open Sans")) +
    theme(legend.position="top") +
    theme(legend.text=element_text(size=12)) +
    guides(color=guide_legend(nrow=2))
    
  
  
  
  print(p)
  
  

  
  
}

output$plot_para <- renderPlot({
  
plot_para()  

})

output$plot_para_all <- renderPlot({
  
plot_para_all()

  
})

  
})

