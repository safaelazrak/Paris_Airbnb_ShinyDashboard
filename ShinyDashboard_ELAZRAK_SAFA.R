library(tidyr)
library(shiny)
library(dplyr)
library(stringr)
library(tidyverse)
library(skimr)
library(ggmap)
library(ggplot2)
library(writexl)
library(plotly)
library(shinydashboard)
library(DT)
library(highcharter)
#library(jsonlite)
#library(geojsonsf)
#library(geojsonio)
library(corrplot)
library(srvyr)
library(leaflet)
library(ggpubr)
library(lubridate)
library(RColorBrewer)
library(shinyalert)
library(shinycssloaders)



# Loading and cleaning the data


My_data <- load("C:/Users/safae/Downloads/ELAZRAK_SAFA/AirBnB.Rdata") 

New_data <- select(L, listing_id = id, Host_id= host_id, Host_name= host_name, bathrooms, bedrooms, 
                   beds, bed_type, Equipments= amenities, Type= property_type, Room= room_type, 
                   Nb_of_guests= accommodates,Price= price, guests_included, minimum_nights, 
                   maximum_nights,availability_over_one_year= availability_365, instant_bookable, 
                   cancellation_policy, city, Adresse= street, Neighbourhood=neighbourhood_cleansed, 
                   city_quarter=zipcode, latitude, longitude, security_deposit, transit, 
                   host_response_time, Superhost= host_is_superhost, Host_since= host_since, 
                   Listing_count= calculated_host_listings_count, Host_score= review_scores_rating, 
                   reviews_per_month,number_of_reviews)

# Removing the '$' character :

New_data$Price <- substring(gsub(",", "", as.character(New_data$Price)),2)

#Changing the data type : 

New_data$bathrooms <- as.numeric((New_data$bathrooms))
New_data$bedrooms <- as.numeric((New_data$bedrooms))
New_data$beds <- as.numeric((New_data$beds))
New_data$Price <- as.numeric((New_data$Price))
New_data$guests_included <- as.numeric((New_data$guests_included))
New_data$minimum_nights <- as.numeric((New_data$minimum_nights))
New_data$maximum_nights <- as.numeric((New_data$maximum_nights))
New_data$availability_over_one_year <- as.numeric((New_data$availability_over_one_year))
New_data$security_deposit <- as.numeric((New_data$security_deposit))
New_data$Listing_count <- as.numeric((New_data$Listing_count))
New_data$Host_score <- as.numeric((New_data$Host_score))
New_data$reviews_per_month <- as.numeric((New_data$reviews_per_month))
New_data$number_of_reviews <- as.numeric((New_data$number_of_reviews))

# Setting the price range :

New_data <- New_data %>% filter(New_data$Price >= 0 & New_data$Price <= 1000)

# Filling the missing values with the mean :
## Bathrooms
m = mean(New_data$bathrooms,na.rm = TRUE) 
sel = is.na(New_data$bathrooms) 
New_data$bathrooms[sel] = m

## Bedrooms
m = mean(New_data$bedrooms,na.rm = TRUE)
sel = is.na(New_data$bedrooms)
New_data$bedrooms[sel] = m

##Beds 
m = mean(New_data$beds,na.rm = TRUE) 
sel = is.na(New_data$beds)
New_data$beds[sel] = m

# Setting the city quarters (Arrondissements):

New_data$city = str_sub(New_data$city,1, 5)
New_data$city_quarter = str_sub(New_data$city_quarter, -2)
New_data <- subset(New_data, New_data$city == 'Paris' & New_data$city_quarter != "" & New_data$city_quarter <= 20 & New_data$city_quarter != '00' & New_data$city_quarter != ' ')
New_data$Neighbourhood <- as.character(New_data$Neighbourhood)

# Removing the duplicates : 

New_data %>% distinct(listing_id, .keep_all = TRUE)

#Correcting names of Neighborhoods:

New_data[New_data == "PanthÃ©on"] <- "Panthéon"
New_data[New_data == "OpÃ©ra"] <- "Opéra"
New_data[New_data == "EntrepÃ´t"] <- "Entrepôt"
New_data[New_data == "Ã‰lysÃ©e"] <- "Elysée"
New_data[New_data == "MÃ©nilmontant"] <- "Mesnilmontant"
New_data[New_data == "HÃ´tel-de-Ville"] <- "Hôtel-de-Ville"

# Computing needed for visit frequency over year :

table <- inner_join(New_data, R,by = "listing_id")
tab1 <- select(New_data,listing_id,city,city_quarter)
table = mutate(table,year = as.numeric(str_extract(table$date, "^\\d{4}")))

# Computing needed for the number of apartments per owner :
count_by_host_1 <- New_data %>% 
  group_by(Host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  ungroup() %>%
  mutate(groups = case_when(
    number_apt_by_host == 1 ~ "001",
    between(number_apt_by_host, 2,10) ~ "002-010",
    number_apt_by_host > 10 ~ "011-153"))


count_by_host_2 <- count_by_host_1 %>%
  group_by(groups) %>%
  summarise(counting = n())


# Listings by Property type :
whole_property_type_count <- table(New_data$Type)
property_types_counts <- table(New_data$Type,exclude=names(whole_property_type_count[whole_property_type_count[] < 4000]))

count_of_others <- sum(as.vector(whole_property_type_count[whole_property_type_count[] < 4000]))
property_types_counts['Others'] <- count_of_others
property_types <- names(property_types_counts)
counts <- as.vector(property_types_counts)
percentages <- scales::percent(round(counts/sum(counts), 2))
property_types_percentages <- sprintf("%s (%s)", property_types, percentages)
property_types_counts_df <- data.frame(group = property_types, value = counts)


#Average price per Neighbourhood :

average_prices_per_arrond <- aggregate(cbind(New_data$Price),
                                       by = list(arrond = New_data$city_quarter),
                                       FUN = function(x) mean(x))

# Whole data map :

df <- select(L,longitude,neighbourhood,latitude,price)
df %>% select(longitude,neighbourhood,
              latitude,price)

# Superhost map :

dfsuperhost <- select(New_data,longitude,Neighbourhood,latitude,Price)
dfsuperhost <- filter(New_data, Superhost =="t")

# Hosts table :

count_by_host_3 <- New_data %>%
  group_by(Host_id) %>%
  summarise(number_apt_by_host = n()) %>%
  arrange(desc(number_apt_by_host))

top_listings_by_owner <- count_by_host_3 %>%
  top_n(n=20, wt = number_apt_by_host)

knit_print.data.frame <- top_listings_by_owner



###################################################################################



# Building the shiny app:


ui <- dashboardPage(
  
  dashboardHeader(title = "Airbnb Analysis"),
  
  dashboardSidebar(
       
    sidebarMenu(
       
       menuItem("Dashboard",tabName="dashboard", icon=icon("dashboard")),
                
       menuItem("First Analysis",tabName="firstanalysis", icon=icon("bar-chart-o"),  
          menuSubItem("Listings", tabName="apartments"),
          menuSubItem("Hosts", tabName ="hosts")),
    
       menuItem("Detailed Analysis", tabName = "analysis", icon=icon("table"),
          menuSubItem("Generalities", tabName="generalities"),
          menuSubItem("Price / Features", tabName ="priceother"),
          menuSubItem("Price / Neighbourhood", tabName="neighbourhood"),
          menuSubItem("Further analysis", tabName="furtheranalysis")),
      
       menuItem("Maps",tabName="map", icon=icon("map")),
      
       menuItem("Raw Data",tabName="rawdata", icon=icon("database"))
  )),
  
  dashboardBody(
    
    tabItems(
      tabItem(tabName = "dashboard", 
              useShinyalert(),
              fluidRow(tags$head(tags$style(HTML(".small-box {height: 100px}"))),
                       valueBox("Paris", "France", icon = icon("map"), width = 3,color="teal"),
                       valueBoxOutput("meanprice",  width = 3),
                       valueBoxOutput("numsuperhosts",  width = 3),
                       valueBoxOutput("sumlistings",  width = 3)),
              fluidPage(tags$img(src= 'Paris image.png', width = "100%"))),
      
      tabItem(tabName ="apartments",
              #h1("Listings Dashboard"),
              
              fluidRow(
                box(title= "Listings by room type", width = 6,
                  plotOutput("roomtype")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE),
              
                box(title= "Listings by property type",width =6,
                  plotOutput("propertytype")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
                box(title="Average Price according to Room Type", plotOutput("priceroom")%>% withSpinner(color="#0dc5c1"), width=6, 
                    solidHeader = FALSE, collapsible = TRUE ),
                box(title= "< Interactive plot >    Top 10 neighbourhoods by Number of listings"
                          ,width =6,
                    plotlyOutput("top10neighbourhoods")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE)
              ),  
              fluidRow(
                box(title= "< Interactive plot >    Number and type of listings under 1000 $",width =12,
                    plotlyOutput("numbertypelistings")%>% withSpinner(color="#0dc5c1"), status = "primary", solidHeader = FALSE, collapsible = TRUE)),         
              ),  
    
     tabItem(tabName ="hosts",
            #h1("Hosts Dashboard"),
            fluidRow(
              box(title= "Number of Apartments per owner", width = 6,  plotOutput("numberapart")%>% withSpinner(color="#0dc5c1"), 
                  status = "primary", solidHeader = FALSE, collapsible = TRUE),
              box(title= "Hosts in contrast with Superhosts",  plotOutput("superhosts")%>% withSpinner(color="#0dc5c1"), 
                  status = "primary", solidHeader = FALSE, width = 6, collapsible = TRUE)
            ),
            fluidRow(
              box(title = "Airbnb growth: evolution of new hosts over time", width=12,plotOutput ("airbnbgrowth")%>% withSpinner(color="#0dc5c1"),
                  status = "primary", solidHeader = FALSE, collapsible = TRUE )
            ),
            fluidRow(
              box(title = "Table of the TOP 20 owners within the data (according to their number of Listings)", width=12, status= "success",solidHeader = FALSE, collapsible = TRUE,
                  DTOutput('tablehost')))
            ),
    
     tabItem(tabName ="generalities",
            #h2("Analysis of the price / different features"),
            fluidRow(
              box(title= "< Interactive plot >    Number of listings by Neighbourhood",plotlyOutput("listings")%>% withSpinner(color="#0dc5c1"), width = 12, 
                  status = "success", solidHeader = FALSE, collapsible = TRUE)),
            fluidRow(  
              box(title= "< Interactive plot >    Visit frequency over the years", plotlyOutput("visitfreq")%>% withSpinner(color="#0dc5c1"), width = 12, 
                  status = "success", solidHeader = FALSE, collapsible = TRUE)
              ),
            ),
    
    tabItem(tabName ="priceother",
            #h2("Further analysis of renting prices"),
            
             fluidRow(
               box(title="Comparing Price with your selected feature",plotOutput("features")%>% withSpinner(color="#0dc5c1"), height = 600, width = 12, 
                   status = "warning", solidHeader = FALSE, collapsible = TRUE,
                   selectInput(inputId = "variable", "Choose a feature to analyse:", 
                               choices = c("Beds", "Bathrooms","Bedrooms"),
                               selected = NULL, multiple = FALSE))),
               fluidRow(
                 box(title="Comparing all features", plotOutput("allfeatures")%>% withSpinner(color="#0dc5c1"), height = 600, width = 12, 
                     status = "warning", solidHeader = FALSE, collapsible = TRUE),
               )),
  
    tabItem(tabName ="furtheranalysis",
            fluidRow(
              box(title= "Price and availability", plotOutput ("priceavailability")%>% withSpinner(color="#0dc5c1"),
                  status = "warning", solidHeader = FALSE, collapsible = TRUE),
              box(title= "Availability of the listings over a year", highchartOutput ("availabilityoveryear") %>% withSpinner(color="#0dc5c1"),
                  status = "warning", solidHeader = FALSE, collapsible = TRUE)),
              fluidRow(
              box(title= "Impact of 'Instant bookable' on the price", plotOutput("instantbookable")%>% withSpinner(color="#0dc5c1"), width=4, 
                  status = "warning", solidHeader = FALSE, collapsible = TRUE),
              box(title= "Impact of 'Cancellation policy' on the price",width = 4, plotOutput("cancelpolicy")%>% withSpinner(color="#0dc5c1"), 
                  status = "warning", solidHeader = FALSE, collapsible = TRUE),
              box(title= "Impact of 'Host response time' on the price", width = 4, plotOutput("responsetime")%>% withSpinner(color="#0dc5c1"), 
                  status = "warning", solidHeader = FALSE, collapsible = TRUE))),
  
    tabItem(tabName ="neighbourhood",
           
            fluidRow(
               box(title= "< Interactive plot >    Average daily price per neighbourhood",width = 12, plotlyOutput("averageprice")%>% withSpinner(color="#0dc5c1"), 
                   status = "success", solidHeader = FALSE, collapsible = TRUE)),
            fluidRow(
                box(title= "Number of rented Apartments  over the years",width = 12, plotOutput("numbrented")%>% withSpinner(color="#0dc5c1"), 
                    status = "success", solidHeader = FALSE, collapsible = TRUE)),
            fluidRow(
                box(title= "Price range within Neighbourhood", width = 12, plotOutput("pricerangeneighbourhood")%>% withSpinner(color="#0dc5c1"),
                    status = "success", solidHeader = FALSE, collapsible = TRUE),
            )),
  
    tabItem(tabName ="rawdata",
            skin = "blue",
            fluidRow(
              box(title = "Data used for the analysis", width=12, status= "success",solidHeader = FALSE, collapsible = TRUE,
              DTOutput('table'))),
            fluidRow(
              box(status = "success", solidHeader = FALSE, 
                  shinyjs::useShinyjs(),
                  useShinyalert(), 
                  actionButton("init", "Download", icon = icon("download")),
                  downloadButton('downloadData', 'Download',style = "visibility: hidden;"))
            )),
  
    tabItem(tabName ="map",
            fluidPage(
              box(title = "Overview of the whole data", width = 12,
                leafletOutput("mapoverview"), status = "success", solidHeader = FALSE, collapsible = TRUE),
              box(title = "Plotting only Superhosts listings", width = 12,
                  leafletOutput("superhostmap"), status = "success", solidHeader = FALSE, collapsible = TRUE)
            ))
    ) #tabItems
  )  #dashbody

)#dashPage

server <- function(input,output){
  
      # SHINY ALERT # 
  
  shinyalert("Welcome aboard !", "My name is Safa ElAzrak
  
               This is my personal Analysis 
             of the Airbnb dataset of 2017 
                     ")
  
     # VALUE BOXES #
  
  output$meanprice <- renderValueBox({
    valueBox(
      round(mean(New_data$Price),0), "Mean Price", icon = icon("dollar"),
      color = "aqua"
    )
  })
  
  output$numsuperhosts <- renderValueBox({
    valueBox(
      sum(New_data$Superhost == "t"), "No.of Superhosts", icon = icon("user"),
      color = "light-blue"
    )
  })
  
  output$sumlistings <- renderValueBox({
    valueBox(
      nrow(New_data), "No. of listings", icon = icon("list"),
      color = "blue"
    )
  })
  
      # PLOT 1 : No. of listings by Neighbourhood #
  
  output$listings <- renderPlotly({
    
   p4 <- ggplot(New_data, aes(x = fct_infreq(Neighbourhood), fill = Room)) +
      geom_bar() +
      labs(x = "Neighbourhood", y = "No. of listings") +
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))
     #scale_fill_brewer(palette="blues")
    ggplotly(p4)
  })
  
      # PLOT 2 : Average price by Neighbourhood #
  
  output$averageprice <- renderPlotly ({
    
    
    p3 <- ggplot(data = average_prices_per_arrond, aes(x = arrond, y = V1))+
      geom_bar(stat = "identity", fill = "lightblue", width = 0.7)+
      geom_text(aes(label = round(V1, 2)), size=4)+
      coord_flip()+
      labs(
        x = "City quarters", y = "Average daily price")+
      theme(legend.position = "bottom",axis.text.x = element_text(angle = 90, hjust = 1))+
      scale_fill_brewer(palette= "Dark2") 
    ggplotly(p3)
  })
  
  # PLOT 3 : Number of Apartments by host #
  
  output$numberapart <- renderPlot ({

      ggplot(count_by_host_2, aes(x = "", y = counting)) +  
      geom_col(aes(fill = factor(groups)),color = "white")+
      geom_text(aes(y = counting / 1.23, label = counting),
                 color = "black",size = 3)+
      labs(x = "", y = "", fill = "Number of apartments\ by host")+
      scale_fill_brewer(palette="Paired") +
      coord_polar(theta = "y")+
      theme_void()
  })
  
  # PLOT 4 : No. of Superhosts in the dataset #
  
  output$superhosts <- renderPlot ({
    
    ggplot(New_data) +
      geom_bar(aes(x='' , fill=Superhost)) + #, width = 8
      coord_polar(theta='y') +
      scale_fill_brewer(palette="Paired")+
      theme_void()
   #theme_minimal() 
    
  })
  
  # PLOT 5: airbnb growth : number of new hosts over time #
  
  output$airbnbgrowth <- renderPlot({
    
   
    new_hosts_data <- drop_na(New_data, c("Host_since"))
    
    # Calculate the number of new hosts for each year (except for 2017 since our data is not complete for this year)
    new_hosts_data$Host_since <- as.Date(new_hosts_data$Host_since, '%Y-%m-%d')
    new_hosts_data <- new_hosts_data[new_hosts_data$Host_since < as.Date("2017-01-01"),]
    new_hosts_data <- new_hosts_data[order(as.Date(new_hosts_data$Host_since, format="%Y-%m-%d")),]
    new_hosts_data$Host_since <- format(as.Date(new_hosts_data$Host_since, "%Y-%m-%d"), format="%Y-%m")
    new_hosts_data_table <- table(new_hosts_data$Host_since)
    
    
    plot(as.Date(paste(format(names(new_hosts_data_table), format="%Y-%m"),"-01", sep="")), 
         as.vector(new_hosts_data_table), type = "l", xlab = "Time", ylab = "Number of new hosts", col = "Blue")
  })
  
  # PLOT 6 : Listings by Room type #
  
  output$roomtype <- renderPlot ({
    
    room_types_counts <- table(New_data$Room)
    room_types <- names(room_types_counts)
    counts <- as.vector(room_types_counts)
    percentages <- scales::percent(round(counts/sum(counts), 2))
    room_types_percentages <- sprintf("%s (%s)", room_types, percentages)
    room_types_counts_df <- data.frame(group = room_types, value = counts)
    
    ggplot(room_types_counts_df, aes(x = "", y = value, fill = room_types_percentages))+
      geom_bar(width = 1, stat = "identity")+
      coord_polar("y", start = 0)+
      scale_fill_brewer("Room Types", palette ="Paired")+
      ylab("")+
      xlab("")+
      labs(fill="")+
      #theme(axis.ticks = element_blank(), panel.grid = element_blank(), axis.text = element_blank())+
      geom_text(aes(label = percentages), size = 4, position = position_stack(vjust = 0.5))+
      theme_void()
  })
  
  # PLOT 7 : Listings by Property type #
  
  output$propertytype <- renderPlot ({
    
    ggplot(property_types_counts_df, aes(x="",y = value, fill=property_types_percentages))+
      geom_bar(width = 1,stat = "identity")+
      coord_polar("y",start = 0)+
      scale_fill_brewer("Property Types", palette ="Paired")+
      ylab("")+
      xlab("")+
      labs(fill="")+
      #theme(axis.ticks = element_blank(),panel.grid = element_blank(),axis.text = element_blank())+
      geom_text(aes(label = percentages),size= 4 ,position = position_stack(vjust = 0.5))+
      theme_void()
    
  })
  
  # PLOT 8 : Number and type of listing under 1000 $ #
  
  output$numbertypelistings <- renderPlotly ({
    
    
    p7 <- ggplot(New_data, aes(x = Price, fill = Room)) +
      geom_histogram(position = "dodge") +
      scale_fill_manual(values = c("#efa35c", "#4ab8b8", "#1b3764"), name = "Room Type") +
      labs(x = "Price per night", y = "Number of listings") +
      theme(plot.title=element_text(vjust=2, face = "bold"), 
            axis.title.x=element_text(vjust=-1, face = "bold"),
            axis.title.y=element_text(vjust=4, face = "bold"))
    ggplotly(p7)
  })
  
  # PLOT 9 : Top 10 neighb. by listings #
  
  output$top10neighbourhoods <- renderPlotly ({
    
    p30<- New_data %>%
      group_by(Neighbourhood) %>%
      dplyr::summarize(num_listings = n(), 
                       borough = unique(Neighbourhood)) %>%
      top_n(n = 10, wt = num_listings) %>%
      ggplot(aes(x = fct_reorder(Neighbourhood, num_listings), 
                 y = num_listings, fill = borough)) +
      scale_fill_brewer(palette ="Spectral")+
      geom_col() +
      coord_flip() +
      theme(legend.position = "none") +
      labs(x = "Neighbourhood", y = "No. of listings")
    
    ggplotly(p30)
  })
    
  # PLOT 10 : Price range by neighbourhood #
  
  output$pricerangeneighbourhood <- renderPlot({
    
    height <- max(New_data$latitude) - min(New_data$latitude)
    width <- max(New_data$longitude) - min(New_data$longitude)
    Paris_borders <- c(bottom  = min(New_data$latitude)  - 0.1 * height, 
                       top     = max(New_data$latitude)  + 0.1 * height,
                       left    = min(New_data$longitude) - 0.1 * width,
                       right   = max(New_data$longitude) + 0.1 * width)
    map <- get_stamenmap(Paris_borders, zoom = 12)
    p8<- ggmap(map) +
      geom_point(data = New_data, 
                 mapping = aes(x = longitude, y = latitude, col = log(Price))) +
                 scale_color_distiller(palette = "RdYlGn", direction = 1)
    p8
    })
  
  # PLOT 11 : Price & Cancellation policy #
  
  output$cancelpolicy <- renderPlot ({
    
    ggplot(data = New_data, 
           aes(x = cancellation_policy, y = Price,color=cancellation_policy)) +
      geom_boxplot(outlier.shape = NA) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1)) +
      
      theme(plot.title = element_text(color = "Darkviolet", size = 12, face = "bold", hjust = 0.5))+
      coord_cartesian(ylim = c(0, 500))
    
  })
  
  # PLOT 12 : Price & host response time #
  
  output$responsetime <- renderPlot ({
    
    Host_data_without_null_host_response_time <-subset(New_data,host_response_time != "N/A" & host_response_time != "")
    
    ggplot(data = Host_data_without_null_host_response_time, 
                              aes(x = host_response_time, y = Price,color=host_response_time)) + 
      geom_boxplot(outlier.shape = NA) +
      theme(axis.text.x = element_text(angle = 90, hjust = 1))+
      
      theme(plot.title = element_text(color = "Darkviolet", size = 12, face = "bold", hjust = 0.5))+
      coord_cartesian(ylim = c(0, 500))
  })
  
  # PLOT 13 : Price & selected feature #
  
  output$features <- renderPlot ({

    
    if(input$variable == 'Bathrooms'){
      
      a<- ggplot(data = New_data, aes(x = bathrooms, y = Price, color=bathrooms)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      
      plot(a)
      
    }
    
    if(input$variable == 'Bedrooms'){
      b <- ggplot(data = New_data, aes(x = bedrooms, y = Price, color=bedrooms)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      plot(b)
    }
    
    if(input$variable == 'Beds'){
      c <- ggplot(data = New_data, aes(x = beds, y = Price, color=beds)) +
        geom_jitter(width = 0.1,height = 0.2,size=0.1)
      plot(c)
    }
    
  })
  
  # PLOT 14 : Price & all features #
  
  output$allfeatures <- renderPlot ({
    
    a1<- ggplot(data=New_data) +
      geom_smooth(mapping = aes(x=Price,y=beds),xlim=500, method = 'gam', col='grey')
    a2<- ggplot(data=New_data) +
      geom_smooth(mapping = aes(x=Price,y=bedrooms),xlim=500,method = 'gam', col='blue')
    a3<- ggplot(data=New_data) +
      geom_smooth(mapping = aes(x=Price,y=bathrooms),xlim=500,method = 'gam', col='violet')
    a4<- ggplot(data=New_data) +  
      geom_smooth(mapping = aes(x=Price,y=Nb_of_guests),xlim=500,method = 'gam', col='black')
       
    ggarrange(
              a1,
              a2,
              a3,
              a4,
              nrow=2,
              ncol=2,
              align = "hv")
    
  })
 
  # PLOT 15 : Mean price by room type #
  
  output$priceroom <- renderPlot ({
    
    New_data %>% 
      
      group_by(Room) %>% 
      summarise(mean_price = mean(Price, na.rm = TRUE)) %>% 
      ggplot(aes(x = reorder(Room, mean_price), y = mean_price, fill = Room)) +
      geom_col(stat ="identity", fill="#357b8a") +
      coord_flip() +
      theme_minimal()+
      labs(x = "Room Type", y = "Price") +
      geom_text(aes(label = round(mean_price,digit = 2)), hjust = 1.0, color = "white", size = 3.5) +
       
      xlab("Room Type") + 
      ylab("Mean Price")
  })
  
  # PLOT 16 : Visit frequency over years #
  
   output$visitfreq <- renderPlotly ({
     
    p6 <- ggplot(table) +
      geom_bar(aes(y =city_quarter ,fill=factor(year)))+
      scale_size_area() +
      labs( x="Frequency", y="City quarter",fill="Year")+
      scale_fill_brewer(palette ="Spectral")
    
    ggplotly(p6)
  })
  
   # PLOT 17 : Number of rented apartments #
   
   output$numbrented <- renderPlot ({
     
     table["date"] <- table["date"] %>% map(., as.Date)
     
     # Generating a table that aggregate data from data and id and count them
     # to get the number of renting by host and date
     longitudinal  <- table %>% group_by(date, Neighbourhood) %>% summarise(count_obs = n())
     
    ggplot(longitudinal,aes(x = date,y = count_obs,group = 1))+ 
      geom_line(size = 0.5,colour = "#67aad6") +  
      stat_smooth(color = "#FF5AAC",method = "loess")+  
      scale_x_date(date_labels = "%Y")+  
      labs(x = "Year",y = "No. Rented Appartment")+  
      facet_wrap(~ Neighbourhood)
    
   })
   
   # PLOT 18 : Price and availability #
   
   output$priceavailability <- renderPlot ({
     
     ggplot(New_data, aes(availability_over_one_year, Price)) +
       
       geom_point(alpha = 0.2, color = "#336666") +
       geom_density(stat = "identity", alpha = 0.2) +
       xlab("Availability over a year") +
       ylab("Price") 
       #ggtitle("Relation between Price & availability")
   })
   
   # PLOT 19 : availability over year #
   
   output$availabilityoveryear <- renderHighchart ({
     
     hchart(New_data$availability_over_one_year, color = "#336666", name = "Availability") %>%
       #hc_title(text = "Availability of the listings") %>%
       hc_add_theme(hc_theme_ffx())
   })
   
   # PLOT 20 : price & 'instant bookable' #
   
   output$instantbookable <- renderPlot({
     
     ggplot(data = New_data, aes(x = instant_bookable, y = Price,color=instant_bookable)) +
       geom_boxplot(outlier.shape = NA) +coord_cartesian(ylim = c(0, 500))
     
   })
   
   # PLOT 21 : map overview #
   
   output$mapoverview <- renderLeaflet ({
     
      leaflet(df) %>%  
        setView(lng = 2.3488, lat = 48.8534 ,zoom = 10) %>%
        addTiles() %>% 
        addMarkers(clusterOptions = markerClusterOptions()) %>%
        addMiniMap()
     
   })
   
   # PLOT 22 : map overview #
   
   output$superhostmap <- renderLeaflet ({
     
     leaflet(dfsuperhost %>% select(longitude,Neighbourhood,
                                    latitude,Price))%>%
       setView(lng = 2.3488, lat = 48.8534 ,zoom = 10) %>%
       addTiles() %>% 
       addMarkers(clusterOptions = markerClusterOptions()) %>%
       addMiniMap()
     
   })
   
   # OUTPUT 23 : DATA SET #
   
   output$table <- renderDT(
     New_data[,c("listing_id","Host_name","Price","Neighbourhood","city_quarter")], 
     caption = 'This is a simple caption of the table',
     options = list(searching = FALSE,pageLength = 5, dom = 't' )
   )
   
   # OUTPUT 24 : DATA TOP 20 HOSTS #
   
   output$tablehost <- renderDT(
     top_listings_by_owner, options = list(searching = FALSE,pageLength = 5
   ))
   
   # OUTPUT 25 : DOWNOALD THE DATA SET #
   
   global <- reactiveValues(response = FALSE)
   
   observeEvent(input$init,{
     shinyalert("Confirmation", 
                "Do you want to download the data?", 
                type = "success",
                callbackR = function(x) {
                  global$response <- x
                },
                showCancelButton = TRUE
     )
   })
   
   
   # OUTPUT 26 : DOWNOALD BUTTON #
   
   observeEvent(global$response,{
     if(global$response){
       shinyjs::runjs("document.getElementById('downloadData').click();")
       global$response <- FALSE}
   })
   
   output$downloadData <- downloadHandler(
     filename = function() {paste("data-", Sys.Date(), ".csv", sep="")},
     content = function(con) {write.csv(New_data, con)})
   
}

shinyApp(ui, server)




















