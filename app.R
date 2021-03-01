library(shiny)
library(shinydashboard)
library(ggplot2)


ui <- dashboardPage(
    dashboardHeader(title= "WA Potato Data"),
    
    dashboardSidebar(
        sidebarMenu(
            menuItem("Acreage and Yields", tabName = "Acreage and Yields", icon = icon("leaf")),
            menuItem("WA Potato Pathogens", tabName = "WA Potato Pathogens", icon = icon("th"))
        )
    ),
    
    dashboardBody(
        fluidRow(column(6,plotOutput('AveragePotatoAcreage'),plotOutput('AveragePotatoYield')),
                 column(6,plotOutput('YearlyAcreage'),plotOutput('YearlyYield'))
        )))



server <- function(input, output, session) { 
    output$AveragePotatoAcreage <- renderPlot({
        ggplot(us.states,
               aes(x=long, y=lat, group=group))+
            geom_path(size=0.25)+
            geom_path(data=ca.provinces, size=0.25,color="grey60")+
            geom_polygon(data=subset(us.states, NAME_1 %in% c('Idaho', 'Oregon', 'California', 'Utah', 'Montana', 'Nevada', 'Wyoming', 'Washington' )),
                         aes(x=long, y=lat, group=group), fill="white", colour = "grey50", size=0.50)+ 
            geom_polygon(data=JoinedAverageCounties,
                         aes(x=long, y=lat, group=group,
                             fill=AveAcreage), ###Fill color based on acreage value
                         color="black", lwd=0.3, alpha=0.8) +
            scale_fill_gradient(low="blue", high="red",
                                limits=(c(0, 45000))) +
            theme_classic() + coord_map("conic", lat0 = 30) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(fill = "Acreage")+
            labs(caption = "Counties shown in grey were not included in the NASS data set.
       (https://quickstats.nass.usda.gov/results/0D0B7A8C-6590-34F9-9FB3-74F270777F23)") +
            theme(plot.caption = element_text(hjust = 0.5))+ 
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            coord_map("bonne", lat0 = 50,
                      xlim = c(-125, -114),ylim = c(44, 50))+
            xlab("Longitude") +
            ylab("Latitude") +
            ggtitle("WA County Historic Potato Acreage Averages 1965-2014")
    })
    output$AveragePotatoYield <- renderPlot({
        ggplot() 
    })
    
    output$YearlyAcreage <- renderPlot({
        ggplot(us.states,
               aes(x=long, y=lat, group=group))+
            geom_path(size=0.25)+
            geom_path(data=ca.provinces, size=0.25,color="grey60")+
            geom_polygon(data=subset(us.states, NAME_1 %in% c('Idaho', 'Oregon', 'California', 'Utah', 'Montana', 'Nevada', 'Wyoming', 'Washington' )),
                         aes(x=long, y=lat, group=group), fill="white", colour = "grey50", size=0.50)+ 
            geom_polygon(data=SubsetYears1965to1969,
                         aes(x=long, y=lat, group=group,
                             fill=Value), ###Fill color based on acreage value
                         color="black", lwd=0.3, alpha=0.8) +
            scale_fill_gradient(low="blue", high="red",
                                limits=(c(0, 45000))) +
            facet_wrap(~ Year)+ ###Create a map based on each year
            theme_classic() + coord_map("conic", lat0 = 30) + 
            theme(plot.title = element_text(hjust = 0.5)) +
            labs(fill = "Acreage")+
            labs(caption = "Counties shown in grey were not included in the NASS data set.
       (https://quickstats.nass.usda.gov/results/0D0B7A8C-6590-34F9-9FB3-74F270777F23)") +
            theme(plot.caption = element_text(hjust = 0.5))+ 
            theme(panel.background = element_rect(fill = "white", colour = "grey50"))+
            coord_map("bonne", lat0 = 50,
                      xlim = c(-125, -114),ylim = c(44, 50))+
            xlab("Longitude") +
            ylab("Latitude")
    })
    
    output$YearlyYield <- renderPlot({
        ggplot()
    })
}

shinyApp(ui, server)