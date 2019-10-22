library(ggplot2)
library(shiny)
library(leaflet)    # The map-making package
library(geojsonio)  # A package for geographic and spatial data, requires the latest version of dplyr
library(dplyr)      # Used for data manipulation and merging
library(htmltools) 
library(plotly)
library(kableExtra)
library(gridExtra)

#Data Wrangling 
data <- read.csv("https://query.data.world/s/z4eoe7dmv2yxlnk3ccb43givzghipc", stringsAsFactors = FALSE)
names(data) <- c("Country", "CPI (2015)", "GDP per capita (2015)")

de <-data.frame("Russia",29, 9478.80)
names(de)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
newdf  <- rbind (data, de)

de2<-data.frame("United States of America", 76, 56115.72)
names(de2)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (newdf, de2)

de3<-data.frame("Republic of Serbia", 40, 5235.14)
names(de3)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de3)

de4<-data.frame("Slovakia", 51, 16088.28)
names(de4)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de4)

de5<-data.frame("Democratic Republic of the Congo", 23, 1851.2)
names(de5)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de5)

de6<-data.frame("United Republic of Tanzania", 30, 853.34)
names(de6)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de6)

de7<-data.frame("South Korea", 56, 27221.52)
names(de7)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de7)

de8<-data.frame("Republic of the Congo", 23, 1815.2)
names(de8)<-c("Country", "CPI (2015)", "GDP per capita (2015)")
my_data  <- rbind (my_data, de8)

my_data <- my_data[-c(147, 114, 119, 123,32,135,135,32), ]

#Data does not exist for other countries. 


#Preperaring for the map 
shapeurl <- "https://raw.githubusercontent.com/johan/world.geo.json/master/countries.geo.json"
WorldCountry <- geojson_read(shapeurl, what = "sp")
CountryCorruption <- left_join(data.frame(Name = as.character(WorldCountry$name)), my_data, by = c("Name" ="Country"))
#CountryCorruption[,1] <- as.numeric(as.character(CountryCorruption[,1]))

pal <- colorBin("viridis", domain = CountryCorruption[,2])
myLabels <- paste("<strong>", "Country:" , WorldCountry$name)
myPopups <- paste("CPI (2015):", CountryCorruption[,2], "<br/>", "GDP per capita (2015):", CountryCorruption[,3])

#Preparing for the Dataset tab 
Lowest_CPI_Countries <- filter(data, data[,2] < 19)
Lowest_GDP_Countries <- filter(data, data[,3] < 550)
Highest_CPI_Countries <- filter(data, data[,2] > 80)
Highest_GDP_Countries <- filter(data, data[,3] > 50720)

#Shiny App UI
shinyApp(
    ui = fluidPage(navbarPage("CPI and GDP (2015)",
                              tabPanel("Plot",
                                       h4("Histogram for CPI or GDP per capita (2015)"),
                                       h6("CPI: Corruption Perception Index" ), 
                                       h6("GDP: Gross Domestic Product"),
                                       varSelectInput("variable", "Please select the variable of your choice:", data[,2-3]),
                                       mainPanel(plotOutput("plot"))),
                              tabPanel("Map",
                                       h4("World Map showing CPI and GDP per capita (2015)"),
                                       #h6("Red: CPI less than 25th quantile"),
                                       #h6("Blue: CPI highter than 25th quantile"),
                                       leafletOutput(outputId = "mymap")),
                              tabPanel("Scatter Plot", 
                                       h4("Scatterplot plot for CPI and GDP per capita (2015)"),
                                       plotlyOutput(outputId = "scatter")),
                              tabPanel("Dataset",
                                       h4("Dataset showing CPI and GDP per capita (2015)"),
                                       (tabsetPanel(type = "tabs", 
                                                    tabPanel("Complete Dataset", tableOutput("Dataset")),
                                                    tabPanel("Low CPI countries", plotOutput("table")),
                                                    tabPanel("Low GDP countries", plotOutput("tableGDP")),
                                                    tabPanel("High CPI countries", plotOutput("tableCPIH")),
                                                    tabPanel("High GDP countries", plotOutput("tableGDPH"))
                                       ))))),
    
    
    #Shiny App server
        server = function(input, output,session) {
        output$plot <- renderPlot({
            ggplot(data, aes(!!input$variable)) + geom_histogram(fill="#F8766D", color="black", linetype="solid", alpha = 0.7)
            
        })
        
        output$mymap <- renderLeaflet({
            leaflet(WorldCountry) %>%
                addTiles() %>%
                addPolygons(fillColor = pal(CountryCorruption[,2]),
                            weight = 1,
                            opacity = 1,
                            color = "black",
                            fillOpacity = 0.8,
                highlight = highlightOptions(weight = 3,
                                             color = "white",
                                             fillOpacity = 0.8,
                                             bringToFront = TRUE),
                                             label = lapply(myLabels, HTML),
                                             popup = myPopups)%>%
             addLegend(pal = pal, values = CountryCorruption[,2], title = "CPI (2015)", position = "bottomleft")
        })
        
        output$scatter <- renderPlotly({
            p<- ggplot(data, aes(x=data[,2], y=data[,3],
                                    text = paste(data[,1] ,
                                                 "<br>CPI Index : ",data[,2],
                                                 "<br>GDP per capita : ", data[,3]))) + 
                geom_point(color = "#F8766D")+
                geom_smooth(method=lm)+
                ggtitle("CPI vs. GDP per capita (2015)") +
                xlab("CPI (2015)") + ylab("GDP per capita (2015)")
            ggplotly(p, tooltip = "text") 
        })
        
            output$table <- renderPlot({
            grid.table(head(Lowest_CPI_Countries,nrow(Lowest_CPI_Countries)))
        })
        
        output$tableGDP <- renderPlot({
            grid.table(head(Lowest_GDP_Countries,nrow(Lowest_GDP_Countries)))
        })
        
        output$tableCPIH <- renderPlot({
            grid.table(head(Highest_CPI_Countries,nrow(Highest_CPI_Countries)))
        })
        
        output$tableGDPH <- renderPlot({
            grid.table(head(Highest_GDP_Countries,nrow(Highest_GDP_Countries)))
        })
        
        
        output$Dataset <- function() ({
            req(data)
            data %>%
                knitr::kable("html") %>%
                kable_styling(bootstrap_options = c("striped", "hover", "condensed", "responsive"), full_width = F, position = "center", font_size = 10)%>%
                column_spec(2, bold = T, color = "black")  %>%
                row_spec(which(data[,2]<29), bold = T, color = "white", background = "#F8766D")%>%
                row_spec(0, bold = T, color = "white", background = "black")%>%
                footnote(general = "The highlighted rows refer to the countries with CPI less than the first quartile CPI for all countries.")
            
        })
        
    })
#References: 
#https://shiny.rstudio.com/tutorial/
#https://stackoverflow.com/ 
#Help taken from Prof. Miller to debug the code as well as to improve the overall layout of the app. 

