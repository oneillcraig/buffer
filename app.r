#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#



# Final App, dashboard


library(shiny)
library(shinydashboard)
library(leaflet)
library(sf)
library(sp)
library(tidyverse)
library(rgdal)
library(shinycssloaders)
library(png)


### Buffer Maker Code

# switched from reds color scheme to divergent RdYlGn, couldn't figure out how to reverse palette, so manually inputed the values

color <- colorFactor(palette = c("#1a9850", "#91cf60", "#d9ef8b", "#fee08b", "#fc8d59", "#d73027"), 
                     domain = c(0, 1, 2, 3, 4 , 5, 6), 
                     na.color = "transparent")



dbHeader <- dashboardHeader(title = "Buffer Maker for KML Files",
                            
                            tags$li(a(href = 'https://www.mccormickbiologicinc.com/',
                                      img(src = 'mbilogo.png',
                                          title = "Company Home", height = "30px"),
                                      style = "padding-top:10px; padding-bottom:10px;"),
                                    class = "dropdown"
                            ))



# Define UI for application that draws a histogram
ui <- dashboardPage(skin = ("green"),
                    
                    
                    
                    
                    
                    dbHeader,
                    
                    
                    
                    dashboardSidebar(
                      
                      
                      sidebarMenu(
                        
                        menuItem("Instructions", tabName = "tab_7"),
                        menuItem("Buffer Maker", tabName = "tab_4"),
                        
                        img(src='mbilogo.png', align = "center", height = 150)
                        
                      )
                      
                    ),
                    
                    
                    
                    dashboardBody(
                      tabItems(
                        tabItem(tabName = "tab_7",
                                
                                fluidRow(
                                  
                                  box(tags$h4("Instructions"), p("Upload your .kml file from Google Earth.  Hit a button.  Save new buffer file as a .kml.  Open in Google Earth.  Done."), width = 6),
                                  box(tags$h4("Web Application Development"), p("This web application was developed to Save time at MBI"), width = 6),
                                  box(tags$h4("Contact Information"), p("Craig O'Neill - craig@mccormickbiologicinc.com"), width = 6),
                                  
                                  # These have to be in the www folder in the project 
                                  
                                  box(img(src= 'bnlllg.JPG', align = "left", height = 310), img(src= 'DinkeyActivities.png', height = 320), width = 12),
                                  box(p("Caption."), width = 12)
                                  
                                  
                                  )
                                
                                  ),
                        
                        
                        tabItem(tabName = "tab_4",
                                fluidRow(
                                  box(fileInput("mapfile", "Choose KMZ File",
                                                multiple = FALSE,
                                                accept = c("application/vnd.google-earth.kml+xml")),
                                      
                                      tags$hr(),
                                      radioButtons("type", "Type",
                                                   choices = c(KMZ = ".kmz",
                                                               KML = ".kml"),
                                                   selected = ".kmz"),
                                      tags$hr(),
                                      numericInput("Buffer Distance", "Input Buffer Distance", value = 0),
                                      radioButtons("dist", "Measurement Type",
                                                   choices = c(Meters = "meters",
                                                               Feet = "feet"),
                                                   selected = "feet")),
                                  
                                  
                                  
                                  box(leafletOutput("my_map1", height = 432), 
                                      p("This This is your map.", width = 12),
                                      downloadButton("downloadData", "Download Your KML"))
                                  ))
                        )
))        
                    
                        
server <- function(input, output){
  
  data <- reactive ({
    x <- input$mapfile
  })
  
  output$my_map1 <- renderLeaflet({
    
    df <- data()
    
    ogmap <- leaflet(df) %>% 
      addTiles()
    
    ogmap
    
  })
  
  
  output$my_graph1 <- renderPlot({
    # generate bins based on input$bins from ui.R
    x    <- SDI260_CFL_Change$CFL_Change 
    
    
    output$summary <- renderPrint({
      x <- SDI260_CFL_Change$CFL_Change             # Define x again
      y <- summary(x, digits = 3)
      z <- round(y, digits = 2)
      z
    })
    
    ggplot(SDI260_CFL_Change, aes(SDI260_CFL_Change$CFL_Change)) +
      geom_histogram(binwidth = 0.08, alpha = 1, color = "black", size = 0.5, aes(fill = cut(CFL_Change, c(-Inf, 0.04, Inf)))) +
      scale_fill_manual(name = "CFL_Change", values = c("(-Inf,0.04]" = "forestgreen",
                                                        "(0.04, Inf]" = "firebrick"),
                        labels = c("Less Than 0", "Greater Than 0")) +
      theme_bw() +
      theme(legend.position = "none") +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      labs(title = "Change in Fire Severity Across the Landscape", x = "Conditional Flame Length Change (ft)", y = "Count") 
    
  })
  
  
  
  
  
  #####Topo Info Begin   #######
  #  output$my_graph2 <- renderLeaflet({
  
  
  # TOPOGRAPHICAL INFORMATION
  
  #    tiffmap <- subset(tiff_stack, input$class, drop=TRUE)
  
  
  #    pal <- colorNumeric(c("#0C2C84", "#41B6C4", "#FFFFCC"), values(tiffmap),
  #                        na.color = "transparent")
  #leaflet(private_tclass) %>% 
  #addTiles() %>% 
  
  
  #    leaflet(private_tclass) %>% 
  #      addTiles() %>% 
  #      addRasterImage(tiffmap, colors = pal, opacity = 0.8) %>% 
  #      addLegend (pal = pal, values = values(tiffmap),
  #                 title = input$class) %>% 
  #      addPolygons(color = "black",
  #                  weight = 0.5, fill = NA) %>%
  #      addPolygons(data = dinkey_df,
  #                  weight = 1,
  #                  color = "black",
  #                  fillColor = "transparent")
  
  
  
  
  
  #  })
  
  ###### FIRE HISTORY#####  
  
  output$my_graph3 <- renderLeaflet({
    
    
    
    
    
    
    regime_sub <- regime_class %>%
      filter(FireRegime == input$regime_class)
    
    leaflet(regime_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palfireregime(RegimeNames),
                  fillOpacity = 0.5,
                  group = "Historical Fire Regime") %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent",
                  group = "Dinkey Boundary") %>%
      addPolygons(data = private_tclass,
                  weight = 1,
                  color = "black",
                  fillColor = "darkblue",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      
      
      addLayersControl(
        baseGroups = c("Historical Fire Regime"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  ##### ALSO FIRE HISTORY #####    
  output$my_graph7 <- renderLeaflet({
    
    
    
    
    
    
    
    
    
    cond_sub <- cond_class %>%
      filter(Departure == input$cond_class)
    
    leaflet(cond_sub) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "black",
                  fillColor = ~palcond(condNames),
                  fillOpacity = 0.5,
                  group = "Change in Fire Regime") %>%
      addPolygons(data = dinkey_df,
                  weight = 1,
                  color = "black",
                  fillColor = "transparent",
                  group = "Dinkey Boundary") %>%
      addPolygons(data = private_tclass,
                  weight = 0.5,
                  color = "black",
                  fillColor = "darkblue",
                  fillOpacity = 0.5,
                  group = "Private Parcels") %>%
      
      addLayersControl(
        baseGroups = c("Change in Fire Regime"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  
  #### FOREST COVER ######  
  output$my_graph4 <- renderLeaflet({
    
    
    leaflet() %>% 
      addTiles() %>% 
      addPolygons(data = dinkey_df,
                  weight = 3,
                  color = "black",
                  fillColor = "grey",
                  fillOpacity = 0,
                  group = "Dinkey Boundary") %>%
      addPolygons(data = SAF_class,
                  weight = 0.5,
                  color = "black",
                  fillColor = ~palrainbow(SAFNames),
                  fillOpacity = 0.5,
                  group = "Vegetation") %>%
      # addPolygons(data = ponderosa,
      # weight = 0.5,
      #color = "black",
      #fillColor = "green",
      #fillOpacity = 0.5,
      #group = "Ponderosa") %>%
      addPolygons(data = private_tclass,
                  weight = 2,
                  color = "white",
                  fillColor = "yellow",
                  fillOpacity = 0,
                  group = "Private Parcels") %>%
      addLegend(pal = palrainbow, 
                values = SAFNames,
                title = "Forest Cover Types") %>%
      
      
      addLayersControl(
        baseGroups = c("Fire Resistant", "Not Fire Resistant", "All Vegetation"),
        overlayGroups = c("Dinkey Boundary", "Private Parcels"),
        options = layersControlOptions(collapsed = FALSE)
      )
    
    
  })
  
  ##### FIRE SEVERITY OF PRIVATE LANDS#####  
  
  output$my_graph5 <- renderLeaflet({
    
    
    private_map <- DataT %>%
      filter(Treatment == input$treatment)
    
    leaflet(private_map) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "Black",
                  fillColor = ~color(private_map$Severity),
                  fillOpacity = .9) %>%
      addPolygons(data = dinkey_df,
                  weight = 2.0,
                  color = "Grey",
                  fillColor = "Transparent",
                  opacity = 1.0) %>% 
      addLegend (pal = color, values = DataT$Severity,
                 title = "Fire Severity Level",
                 opacity = 1.0)
  })
  
  
  #### FIRE SEVERITY OF PRIVATE LANDS######    
  output$my_graph8 <- renderLeaflet({
    
    
    
    private_map <- DataT %>%
      filter(Treatment == input$treatment)
    
    leaflet(private_map) %>% 
      addTiles() %>% 
      addPolygons(weight = 0.5,
                  color = "Black",
                  fillColor = ~color(private_map$Severity),
                  fillOpacity = .9) %>%
      addPolygons(data = dinkey_df,
                  weight = 2.0,
                  color = "Grey",
                  fillColor = "Transparent",
                  opacity = 1.0) %>% 
      addLegend (pal = color, values = DataT$Severity,
                 title = "Fire Severity Level",
                 opacity = 1.0)
  })
  
  ####Carbon CBA ######  
  
  output$distPlot <- renderPlot({
    x <- carbon$DifferenceToDate
    Years <- input$Year 
    pick_model <- input$Climate_Model
    ggplot(subset(carbon, Year == Years & Climate_Model == pick_model), aes(x = Discount_Rate, y = DifferenceToDate)) +
      geom_col(fill = c("darkblue", "lightblue")) +
      theme_bw(base_size = 13) +
      ylab("Cumulative Value ($US Millions)") +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Carbon Sequestration (in $US)") +
      #scale_fill_manual(name="Treatment", values = c("SDI 300" = "cyan3", "SDI 260" = "purple")) +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(-0.5, 15))) +
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      coord_flip() + 
      geom_hline(yintercept = 0, size = 0.5, colour = "black",
                 linetype = "dotted") +
      guides(fill = FALSE)
  })
  
  output$distPlot2 <- renderPlot({
    x <- cba$NPV
    Stakeholders <- input$Stakeholder 
    pick_treatment <- input$Treatment_Type
    pick_model2 <- input$Climate_Model2
    ggplot(subset(cba, Stakeholder == Stakeholders & Treatment_Type == pick_treatment & Climate_Model2 == pick_model2), aes(x = Discount_Rate, y = NPV)) +
      geom_col(fill = c("darkgreen", "lightgreen")) +
      theme_bw(base_size = 13) +
      ylab("Net Value (US$ Millions)") +
      #scale_fill_manual(name="Treatment", values = c("SDI 300" = "cyan3", "SDI 260" = "purple")) +
      xlab("Discount Rate (%)") + 
      ggtitle("Value of Fuel Treatments (in $US)") +
      theme(plot.title = element_text(hjust = 0.5)) +
      scale_y_continuous(expand = c(0,0), limits = (c(0, 350))) +
      coord_flip() + 
      theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank()) +
      guides(fill = FALSE)
  })
  
  #####Cost Calculator ######  
  
  #addresses2 <- addresses1 %>% 
  #st_set_geometry(NULL) %>% 
  #select(Addr1, Addr2, Improvement, LandValue, GA25, PA25, NT_Loss, Opt21_Loss, Acres)
  #addresses2$LandValue <- as.numeric(levels(addresses2$LandValue))[addresses2$LandValue]
  #addresses2$Improvement <- as.numeric(levels(addresses2$Improvement))[addresses2$Improvement]
  
  
  #control <- input$Addr1
  
  #GA2 <- addresses2 %>% 
  #subset(Addr1 == control) %>% 
  #select(GA2)
  
  #addrInput <- reactive({
  #a <- addresses2 %>% 
  #subset(Addr1 == input$Addr1)%>% 
  #select(LandValue)
  #return(a)
  #})
  
  #output$Table1 <- renderTable(addrInput(),
  #colnames = FALSE)
  
  addrLandValue <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(LandValue)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
    
    
    
  })
  
  output$LandVal1 <- renderTable(addrLandValue(),
                                 colnames = FALSE)
  
  
  addrImprove <- reactive({
    b <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement)
    b[,1] <- sapply(b[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(b)
  })
  
  output$Improve1 <- renderTable(addrImprove(),
                                 colnames = FALSE,
                                 digits = 2)
  
  
  #output$LandVal1 <- renderText({
  #W <- addresses2$LandValue[addresses2$Addr1 %in% input$"Addr1"]
  #as.vector(W)
  
  #})
  
  #output$Improve1 <- renderText({
  #Imp1 <- addresses2$Improvement[addresses2$Addr1 %in% input$"Addr1"]
  #as.vector(Imp1)
  
  #})
  
  output$Address2 <- renderTable({
    Address2 <- addresses2$Addr2[addresses2$Addr1 %in% input$"Addr1"]
    
  }, colnames = FALSE)
  
  
  addrTotal <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue) %>% 
      mutate(Total = Improvement + LandValue) %>% 
      select(Total)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$Total1 <- renderTable(addrTotal(),
                               colnames = FALSE)
  
  addrIgnit <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(GA25) 
    return(a)
  })
  
  output$FireIgnit1 <- renderTable(addrIgnit(),
                                   colnames = FALSE)
  
  DefaultIgnit <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1) %>% 
      select(GA25)
    return(a)
  })
  
  output$IgnitSlider <- renderUI({
    sliderInput("FireIgnit2", 
                "Custom FireIgnit Value", 
                min = 0.01, 
                max = 0.75, 
                value = YYY, 
                step = 0.01)
  })
  
  #updateSliderInput(session, "upSlider",
  #label = "FireIgnit2",
  #value = GA25,
  #min = 0.01,
  #max = 0.75,
  #step = 0.001)
  
  addrHVRA <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(NT_HVRA) 
    return(a)
  })
  
  output$FireSev1 <- renderTable(addrHVRA(),
                                 colnames = FALSE)
  
  addrLoss <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$Loss1 <- renderTable(addrLoss(),
                              colnames = FALSE,
                              digits = 2)
  
  
  
  
  addrTreatmentCost1 <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Acres) %>% 
      mutate(TreatmentCost = ((Acres * 2750)*3)) %>% 
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",round(x,2)))
    return(a)
  })
  
  output$TreatmentCost1 <- renderTable(addrTreatmentCost1(),
                                       colnames = FALSE)
  
  
  addrTreatmentsCostHT <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Acres) %>% 
      mutate(TreatmentCost = ((Acres / 0.2)*2025*3)) %>% 
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$TreatmentcostsHT <- renderTable(addrTreatmentsCostHT(),
                                         colnames = FALSE)
  
  addrSavingMech <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss, Acres) %>% 
      mutate(MechCost = (Acres * 2750)*3) %>% 
      mutate(Tot_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      mutate(SavingsMech = Tot_Loss - MechCost) %>% 
      select(SavingsMech)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$SavingMech <- renderTable(addrSavingMech(),
                                   colnames = FALSE)
  
  addrSavingHT <- reactive({
    a <- addresses2 %>% 
      subset(Addr1 == input$Addr1)%>% 
      select(Improvement, LandValue, GA25, NT_Loss, Acres) %>% 
      mutate(HTCost = (Acres / 0.2)*2025*3) %>% 
      mutate(Tot_Loss = (Improvement + LandValue) * GA25 * NT_Loss) %>% 
      mutate(SavingsHT = Tot_Loss - HTCost) %>% 
      select(SavingsHT)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$SavingHT <- renderTable(addrSavingHT(),
                                 colnames = FALSE)
  
  addrNewSev <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>%
      subset(Treat == input$TreatExt) %>% 
      select(HVRA) #%>% 
    #a[,1] <- sapply(a[,1], function(x) paste0("$",round(x,2)))
    return(a)
  })
  
  output$NewSev <- renderTable(addrNewSev(),
                               colnames = FALSE)
  
  
  addrNewDamages <- reactive({
    b <- input$TreatExt
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(Improvement, LandValue, GA25, HVRA) %>% 
      mutate(Tot_NT_Loss = (Improvement + LandValue) * GA25 * HVRA) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$NewDamages <- renderTable(addrNewDamages(),
                                   colnames = FALSE)
  
  addrDiff <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(Improvement, LandValue, GA25, HVRA, Ntbase) %>% 
      mutate(Tot_NT_Loss = ((Improvement + LandValue) * GA25 * HVRA) - Ntbase) %>% 
      select(Tot_NT_Loss)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$DiffNT <- renderTable(addrDiff(),
                               colnames = FALSE)
  
  addrDiffa <- reactive({
    a <- input$landvalue * input$FireProb2 #* input$FireSev2
    #a <- as.data.frame(a)
    b <- addressesCust %>% 
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == input$TreatExt) %>% 
      select(HVRA)
    b <- b[1,]
    b <- as.numeric(b)
    ab <- as.data.frame(a*b)
    ab <- sapply(ab, function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(ab)
  })
  
  output$DiffNTa <- renderTable(addrDiffa(),
                                colnames = FALSE)
  
  addrTcost <- reactive({
    a <- addresses3 %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  #Would need to stack the data by treatment ext
      select(TreatmentCost)
    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(a)
  })
  
  output$TreatChoice <- renderTable(addrTcost(),
                                    colnames = FALSE)
  
  #  addrnet <- reactive({
  #    a <- addresses3 %>% 
  #      subset(Addr1 == input$Addr1)%>% 
  #      subset(Treat == input$TreatExt) %>%  
  #      select(Improvement, LandValue, GA25, HVRA, Ntbase, TreatmentCost) %>% 
  #      mutate(Tot_NT_Loss = (Ntbase - (Improvement + LandValue) * GA25 * HVRA) - TreatmentCost) %>% 
  #      select(Tot_NT_Loss)
  #    
  #    a[,1] <- sapply(a[,1], function(x) paste0("$",format(round(x,2), nsmall = 2)))
  #    return(a)
  #  })
  
  #output$netgain <- renderTable(addrnet(),
  #                               colnames = FALSE)  
  
  
  ##Update this part for the Avoided Costs Section  
  addrAvoideda <- reactive({
    a <- input$landvalue * input$FireProb2 #* input$FireSev2
    #a <- as.data.frame(a)
    b <- addressesCust %>% 
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == input$TreatExt) %>% 
      select(HVRA)
    b <- b[1,]
    b <- as.numeric(b)
    c <- addresses3 %>% #No Treatment Loss (Pulls Severity from Address Lookup)
      subset(Addr1 == input$Addr1) %>% 
      subset(Treat == "NT") %>% 
      select(HVRA) #Pulls up the address based on the lookup, filters databse to that address and the NT HVRA value, pulls the HVRA.  Convert that to a numeric  
    c <- c[1,]
    c <- as.numeric(c) #No Treatment Alternative
    ab <- as.data.frame(a*b) #After Treatment Fire Loss
    abc <- as.data.frame(c - ab) #Savings
    t <- addressesCust %>% 
      subset(Addr1 == input$Addr1)%>% 
      subset(Treat == input$TreatExt) %>%  
      select(TreatmentCost)
    abct <- as.data.frame(abc - t)
    abct <- sapply(abct, function(x) paste0("$",format(round(x,2), nsmall = 2)))
    return(abct)
  })  
  
  output$netgain <- renderTable(addrAvoideda(),
                                colnames = FALSE)  
  
  
}




shinyApp(ui = ui, server = server)
