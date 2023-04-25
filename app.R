# ---------visualization of suicide rates in england 
# By Andrew  P Plemper 

# I am now going to load packages and read in the data. 

library(dplyr)
library(janitor)
library(tidyr)
library(tidyverse)
library(shiny)
library(hrbrthemes)
library(plotly)
library(shinythemes)

# reading in data 
raw_data<-read.csv("data/raw_data.csv")

# dropping the first 2 columns of df 

df<- raw_data[-c(1,2),]

# and making the first row the title 

df<- df %>% row_to_names(row_number = 1) 

# selecting only the area, sex and rates per 100k columns 

df<- df %>% 
  select("Sex", contains("Area of usual residence"), contains("per 100,000"))

# and changing the names of the columns

colnames(df)<- gsub("..Rate.per.100.000...note.4.", "", 
                    colnames(df))
colnames(df)[2] = "Area"

# converting the data into tidy (long) format 

df <- df %>%
  pivot_longer(c( '2021','2020', '2019', '2018', '2017', '2016', '2015', '2014', '2013', 
                  '2012', '2011', '2010', '2009','2008', '2007', '2006', '2005', '2004', 
                  '2003', '2002', '2001', '2000', '1999', '1998', '1997', '1996','1995', 
                  '1994', '1993', '1992', '1991', '1990', '1989', '1988', '1987', '1986', 
                  '1985', '1984', '1983', '1982', '1981' ),
               names_to = "year", values_to = "rates_per_100k")


# making  sure the data classes are appropriate 

df$rates_per_100k <- as.numeric(df$rates_per_100k)
df$Area <- as.character(df$Area)
df$Sex <-as.character(df$Sex)


# ordering the area variables from north to south 
# I am going to code this one backwards so it runs from top to bottom 
# north to south when i flip the ggplot using coord_flip()
df$Area <- factor(df$Area, levels = c("South West", "South East", 
                                      "London",
                                      "East", "West Midlands",
                                      "East Midlands", 
                                      "Yorkshire and The Humber", 
                                      "North West",
                                      "North East" ))

# creating a mean data frame containing data on the mean for all regions 

mean_data<- df %>% 
  group_by(year, Sex) %>% 
  summarise(rates_per_100k = mean(rates_per_100k))

# and plotting a chart of mean of all areas 

ggplot(mean_data, aes(x = year, y = rates_per_100k, color = Sex)) +
  geom_point()+
  ggtitle(" Mean suicide rates per 100K for all regions of England")+ 
  xlab("Year") + 
  ylab("Mean suicide rates per 100K") 

# creating a df for mean data for all years  

mean_data1<- df %>% 
  group_by(Area, Sex) %>% 
  summarise(rates_per_100k = mean(rates_per_100k))


#plotting a histogram of mean suicides in England by sex and Area from all 
#time points

ggplot(mean_data1, aes(x = Area, y = rates_per_100k, fill = Sex)) + 
  geom_col( position = "dodge")+ 
  ggtitle("1981-2021: Mean suicide rates per 100K for males, females 
 and persons by region of England")+ xlab("Region of England") + 
  ylab("Mean suicide rates per 100K")+ 
  coord_flip()
# plotting a line graph of mean suicides in England by sex and Area from all 
#time points

ggplot(mean_data, aes(x = year, y = rates_per_100k, group = Sex)) +
  geom_line()+
  ggtitle(" Mean suicide rates per 100K for all regions of England")+ 
  xlab("Year") + 
  ylab("Mean suicide rates per 100K") 


# adding mean data for all regions to the df 

# first creating a name for the variables in the mean_data frame 

mean_data<- mean_data %>% 
  add_column(Area = "Mean for all regions" )

# now adding mean_data to the main df #

df1<- rbind(df, mean_data )

# making sure the type of data is appropriate 

df1$year <- as.numeric(df1$year)

# ------------------creating the shiny app -------------------


#creating the interface for multiple tabs and selecting a theme. 
ui<- navbarPage(theme = shinytheme("flatly"),
                title =  "Suicide in England ",
                # adding a tab panel for the description page 
                tabPanel("Description",
                         mainPanel(
                           h1(strong("Suicide Rates in England: Data Visualisation", align = "center")),
                           br(),
                           p("This visualisation uses data from the national office of Statistics 
               on suicides in England from 1981-2021. The original report can be accessed", 
                             # inserting link to ONS webpage 
                             a("here.", href = "https://www.ons.gov.uk/peoplepopulationandcommunity/birthsdeathsandmarriages/deaths/bulletins/suicidesintheunitedkingdom/2021registrations")),
                           h2("Purpose of this visualisation "),
                           p( "The purpose of this visualisation is to examine the trends in suicide rates 
   between sexes and for each region of England from 1981-2021.",
                              br(), 
                              h3("How to use this app"),
                              "This visualisation has been designed so you can compare two specific regions at the same time. To do this simply choose a
   region from the dropdown box and the data for that region will be displayed in the corresponding scatterplot. You can also 
   choose to display the mean suicide rates in England by selecting this from the dropdown menu. If you want to look at the value
   for a specific data point, hover over it with your mouse to view the display."  
                              , 
                              br(), 
                              "The boundaries for each region of England can be seen in the map below:"), 
                           # including an image of map of England 
                           img(src = "Eng2.jpg", height = 500, width = 400),
                           
                           p("When you are ready please click the tab entitled  “Visualisation” in the tab panel to continue." )
                         )
                ),   
                
                
                #creating the panel for the Visualisation
                tabPanel("Visualisation",
                         titlePanel(h1("Suicide rates in England.", align = "center")),
                         sidebarLayout(
                           sidebarPanel(
                             helpText("Use the dropdown box to select the region you wish to view."),
                             # adding a dropdown box in the side panel 
                             selectInput("Area", label = ("Choose a region to display in output 1"), 
                                         choices = c ("North East", 
                                                      "North West", "Yorkshire and The Humber",
                                                      "East Midlands",
                                                      "West Midlands", 
                                                      "East",
                                                      "London", 
                                                      "South East", 
                                                      "South West",
                                                      "Mean for all regions"), 
                                         selected = "North East"),
                             
                             # adding a second drop down box in the side panel 
                             selectInput("Area2", label = ("Choose a region To display in output 2"), 
                                         choices = c ("North East", 
                                                      "North West", "Yorkshire and The Humber",
                                                      "East Midlands",
                                                      "West Midlands", 
                                                      "East",
                                                      "London", 
                                                      "South East", 
                                                      "South West", 
                                                      "Mean for all regions"), 
                                         selected = "Mean for all regions"), 
                             
                           ),
                           # creating the output of each widget 
                           mainPanel(
                             plotlyOutput(outputId = "plot", width = "100%"),
                             plotlyOutput(outputId = "plot2", width = "100%")
                           )
                         )  
                )
                
)

# Define server logic
server <- function(input, output) {
  
  # assigning a plot to the output I created in the UI 
  
  output$plot <- renderPlotly({
    
    # telling shiny to select data based on widget input. 
    
    data1 <- filter(df1, Area == input$Area)
    
    # creating ggplot using the ggplotly function for interactive data points. 
    
    ggplotly( ggplot(data1, aes(x = year, y = rates_per_100k, color = Sex, group = Sex,
                                
                                # specifying what text i want to appear when users hovers over data points.  
                                
                                text = paste(Area,Sex,"in", year,
                                             "<br>Suicides Per 100k =", 
                                             round(rates_per_100k, digits = 1)))) +
                geom_line()+
                geom_point(size = 1.5, color = "black")+
                
                #adding a trend line to the plot
                
                geom_smooth(se = FALSE, linetype="11", size =.8, color = "cornsilk4")+ 
                
                #changing the frequency of dates of the x axis
                
                scale_x_continuous(n.breaks = 10)+
                # adding labels and titles, with interactive code to change the title 
                # with widget use. 
                labs(x="Year", 
                     y= "Suicide rates per 100,000", 
                     title = paste("Output1:", print(input$Area)))+
                # changing the color scheme to something more appealing.
                
                scale_color_manual(values=c("hotpink", "dodgerblue", "green2"))+
                theme(panel.background = element_rect(fill = 'snow'),
                      panel.grid.major = element_line(colour = "cornsilk4"),
                      panel.grid.minor = element_line(colour = "grey"),
                      plot.background = element_rect(fill = 'cornsilk'))+
                
                # specifying the y axis and axis of the plotly graph. 
                
                ylim(0,25), tooltip = "text")%>% 
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      
      #Disabling the plotly toolbar and setting the size of the graph. 
      
      config(displayModeBar = F)%>% 
      layout(height = 400, width = 800)
    
  })
  
  # repeating the same process as above for the second graphs 
  # substituting the first output for the second
  
  output$plot2 <- renderPlotly({
    
    
    data2 <- filter(df1, Area == input$Area2)
    
    ggplotly( ggplot(data2, aes(x = year, y = rates_per_100k, color = Sex, group = Sex,
                                text = paste(Area,Sex,"in",year,
                                             "<br>Suicides Per 100k =", 
                                             round(rates_per_100k, digits = 1)))) +
                geom_point(size = 1.5, color = "black")+
                geom_line()+
                geom_smooth(se = FALSE, linetype="11", size =.8, color = "cornsilk4")+ 
                scale_x_continuous(n.breaks = 10)+
                labs(x="Year", 
                     y= "Suicide rates per 100,000", 
                     title = paste("Output2:", print(input$Area2)))+
                scale_color_manual(values=c("hotpink", "dodgerblue", "green2"))+
                theme(panel.background = element_rect(fill = 'snow'),
                      panel.grid.major = element_line(colour = "cornsilk4"),
                      panel.grid.minor = element_line(colour = "grey"),
                      plot.background = element_rect(fill = 'cornsilk'))+
                ylim(0,25), tooltip = "text")%>% 
      layout(xaxis = list(fixedrange = TRUE)) %>%
      layout(yaxis = list(fixedrange = TRUE)) %>%
      config(displayModeBar = F)%>% 
      layout(height = 400, width = 800)
    
    
    
  })
  
  
  
  
}  

# Run the application 
shinyApp(ui = ui, server = server)
