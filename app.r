library(highcharter)
library(zoo)
library(lubridate)
#library(tidyverse)
library(DBI)
library(odbc)
library(DT)
library(shiny)
library(shinydashboard)
#library(ggplot2)
library(dplyr)
library(scales)
#library(semantic.dashboard)
library(shinyWidgets)
library(shinythemes)
require(shiny)
require(highcharter)
library(shinyjs)
#install.packages("DBI")
library(dashboardthemes)
library(shinydashboardPlus)
library(htmltools)
require(maps)
require(viridis)
library(haven)
library(shinyBS)
#library(fst)
library(haven)
library(reactable)
#library(igraph)
library(arrow)

#library(fontawesome)
#library(writexl)
#library(tidyfst)
library(shinycssloaders)
library(readxl)
library(bslib)
library(tigris)
library(sf)
library(leaflet)
library(leaflet.extras)


#library(devtools)
#install.packages("shinycssloaders")
cols <- turbo(15)

HM_COLS <- c(  "#eaecee",
               "#d5d8dc",
               "#abb2b9",
               "#808b96",
               "#566573",
             
               "#d6eaf8",
               "#85c1e9",
               "#3498db",
               "#2874a6",
               "#154360",
               
               "#f5b041",
               "#d68910",
               "#dc7633",
               "#ba4a00",
               "#6e2c00"

)

#### Campaign SAS Dataset ####

#campaign <- read_sas("/n04/data/PNMD/PNMD/PROJECTS/Campaign_Dashboard/DATA/campaign_finalv3.sas7bdat")
campaign <- arrow::read_parquet("/n04/data/PNMD/PNMD/PROJECTS/Campaign_Dashboard/DATA/campaign_finalv3.parquet")
campaign <- campaign %>% 
  mutate(
    MY=as.Date(Job_Created_At),  # Creates a by month date field that the tables use
  ) %>% 
  mutate(Mbrs = case_when(
    ECI != '' ~ 1,
    TRUE ~ 0))

Calls <- read_excel("/n04/data/PNMD/PNMD/PROJECTS/Campaign_Dashboard/DATA/Calls.xlsx") %>% 
  select(-Members)


#### Map Data start ####
# counties <- counties(cb = TRUE)
#states <- states(cb = TRUE)



# counties_trim <- counties %>% 
#   select(GEOID,NAME,STUSPS,-geometry)

# usgeo <- st_read("geo/cb_2014_us_county_5m.shp") %>%
#   mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))




# 
# geo_data <- campaign %>%
#   filter(County_Fips != "") %>%
#   distinct(ECI,County_Fips,.keep_all = TRUE) %>%
#   group_by(County_Fips,STATE) %>%
#   summarise(Members = sum(Mbrs))
# #
# geo_data <- rename(geo_data, c("fips" = "County_Fips"))

# get shapefiles (download shapefiles [here][1] : http://www2.census.gov/geo/tiger/GENZ2014/shp/cb_2014_us_county_5m.zip )

 

#### Filters For Vendor #### 

Dist_Vendor <- campaign %>% 
  distinct(Vendor,Campaign_Name,.keep_all = TRUE) %>% 
  filter(Vendor == 'Highmark')

Dist_Client <- campaign %>% 
  distinct(Client_Name,.keep_all = TRUE) %>% 
  filter(Client_Name != "")

#### Shiny Themes ####
source('app_scripts/shinyBEAThemes.R', local = TRUE)
source('app_scripts/theme.R', local = TRUE)
source('app_scripts/utils.R', local = TRUE)


#### CSS Download Button - Reactable ####
csvDownloadButton <- function(id, filename = "data.csv", label = "Download as CSV") {
  tags$button(
    tagList(icon("download", style = "padding:5px; border-radius: 5px;"), label),
    onclick = sprintf("Reactable.downloadDataCSV('%s', '%s')", id, filename)
  )
}

####Valuebox function####

valueBoxSpark <- function(value, title, sparkobj = NULL, subtitle, info = NULL, 
                          icon = NULL, color = "aqua", width = 4, href = NULL){
  
  shinydashboard:::validateColor(color)
  
  if (!is.null(icon))
    shinydashboard:::tagAssert(icon, type = "i")
  
  info_icon <- tags$small(
    tags$i(
      class = "fa fa-info-circle fa-lg",
      title = info,
      `data-toggle` = "tooltip",
      style = "color: rgba(255, 255, 255, 0.75);"
      
    ),
    # bs3 pull-right 
    # bs4 float-right
    class = "pull-right float-right"
  )
  
  boxContent <- div(
    class = paste0("small-box bg-", color),
    div(
      class = "inner",
      tags$small(title),
      if (!is.null(sparkobj)) info_icon,
      h3(value),
      if (!is.null(sparkobj)) sparkobj,
      p(subtitle)
    ),
    # bs3 icon-large
    # bs4 icon
    if (!is.null(icon)) div(class = "icon-large icon", icon, style = "z-index; 0")
  )
  
  if (!is.null(href)) 
    boxContent <- a(href = href, boxContent)
  
  div(
    class = if (!is.null(width)) paste0("col-sm-", width), 
    boxContent
  )
}

####UI####

ui <- dashboardPage(
  
  dashboardHeader(
    title=span( "Marketing Performance", style= "color:white; font-size:15px; ") #font-family:  'Poppins'
    ,controlbarIcon = span(icon("filter"),"Filters", style= "color:#363936; font-size:20px; padding-left:10px;  font-size: 14px;")
    ,leftUi = tagList(#actionButton("screenshot", "Screenshot") 
      actionBttn(
        
        inputId = "screenshot",
        label = span(tags$img(src='https://upload.wikimedia.org/wikipedia/commons/5/54/Highmark_Logo.svg'), style= "font-size:15px; color:#363936" ) ,
        size = "sm",
        style = "stretch"
      )
    ),
    #### Help Button ####
    dropdownMenu(
      type = "notifications",
      icon = icon("circle-info",style = "color: #008DD1"),
      badgeStatus = NULL,
      headerText = "Definitions:",
      
      
      notificationItem(icon = icon("file"),text = tags$b("Delivered = Message delivered",
                                                         tags$br(),
                                                         "successfully")),
      notificationItem(icon = icon("envelope-open"),text = tags$b("Email Engaged = Member opened",
                                                                  tags$br(),
                                                                  "their email message")),
      notificationItem(icon = icon("comment"),text = tags$b("SMS Engaged = Member clicked",
                                                            tags$br(),
                                                            "their SMS message")),
      notificationItem(icon = icon("comment"),text = tags$b("Push Engaged = Member clicked",
                                                            tags$br(),
                                                            "their push message")),
      notificationItem(icon = icon("comment"),text = tags$b("Unique Members = All distinct",
                                                            tags$br(),
                                                            "members in all campaigns")),
      notificationItem(icon = icon("comment"),text = tags$b("Total Communications = Number of",
                                                            tags$br(),
                                                            "contacts (not unique)")),
      notificationItem(icon = icon("comment"),text = tags$b("Activation = Active w/in",
                                                            tags$br(),
                                                            "7 days of contact")),
      notificationItem(icon = icon("comment"),text = tags$b("Activation Rate = # Actives /",
                                                            tags$br(),
                                                            "# Delivered"))
      
    )
    
    
    #### start help card ####
    
    #### end help card ####
    
    
    
    
    
    
    # tags$li(class = "dropdown",
    #         dropMenu(
    #           dropdownButton("Info",status = 'success', icon = icon('fasr')),
    #           h3(strong('Definitions')),
    #           br(),
    #           h5('This is really helpful'),
    #           textInput('text', 'Email'),
    #           textInput('text2', 'SMS'),
    #           placement = "bottom",
    #           style="color: #fff; background-color: #337ab7; border-color: #2e6da4",
    #           arrow = TRUE),
    # 
    #       )
  ),
  
  ### dashboardSidebar ####
  dashboardSidebar(
    sidebarMenu(id="menu1"
                ,width = 280
                ,collapsed = F
                ,sidebarMenu(
                  menuItem("Data Visuals", icon = icon("chart-simple"), tabName = "dashTab1")
                  ,menuItem("Campaign Summary", icon = icon("list"), tabName = "dashTab2")
                  # ,menuItem("Summary Metrics", icon = icon("database"), tabName = "dashTab2")
                  ,menuItem("Activations", icon = icon("pie-chart"), tabName = "dashTab3")
                  ,menuItem("Healthmap", icon = icon("map"), tabName = "dashTab4")
                  ,menuItem("Definitions", icon = icon("list"), tabName = "dashTab5")
                ))
    
    
  ),
  
  #### dashboardBody ####
  dashboardBody(
    useShinyjs(),
    theme_royalblue_gradient,
    ##to stop the flashing while recalculating 
    tags$style(".recalculating { opacity: inherit !important; }"),
    tags$link(
      rel = "stylesheet", 
      href="https://fonts.googleapis.com/css2?family=Poppins&display=swap"),
    includeCSS("app_scripts/dashboard.css"),
    tags$head(tags$style('#foo .box-header{ display: none}')) , # Box with no header
    #Picker Input dropdown wrap text
    tags$head(
      tags$style(
        HTML(
          "              
      .dropdown-menu{max-width: 350px !important;}                                      
      .dropdown-menu>li>a {white-space: wrap;}
 
        "
        )
      )
    ),

    tabItems(
      
      #### dashtab 1####
      tabItem("dashTab1", 
              
              #### Fluid Row ####  
              
              
              fluidRow(
                
                #style="padding:0px; margin:0px;"
                #tags$head(tags$style(HTML(".small-box {height:10px}"))),
                height = 20
                ,uiOutput("Delivered")
                ,uiOutput("Engagement")
                ,uiOutput("Members")
                
                ,column(8
                        # try to add popover
                        ,style="padding:0px; margin:0px;"
                        
                        ,fluidRow(
                          style="padding:0px; margin:0px;margin-left:5px;"
                          ,box(width = 12, id = "foo",shinycssloaders::withSpinner(highchartOutput("CampTrends"))
                          )
                          
                        )
                        
                        
                        
                        ,box(width = 4,id = "foo",reactableOutput("Email_Table",height = 250)
                             
                             #,box(width = 3,id = "foo",highchartOutput("3",height = 280  ))
                             #,box(width =3,id = "foo",highchartOutput("4",height = 280  ))
                             
                        ) 
                        
                        ,box(width = 4,id = "foo",reactableOutput("SMS_Table",height = 250  )
                             #,highchartOutput("SMSColumn",height = 140  )
                        ) 
                        
                        ,box(width = 4,id = "foo",reactableOutput("Push_Table",height = 250 )
                             #,highchartOutput("PushColumn",height = 160  )
                        )
                        ,box(width = 4,id = "foo",reactableOutput("Enr_Table",height = 230)
                        )
                        ,box(width = 4,id = "foo",reactableOutput("Calls_Table",height = 230)
                        )
                )
                
                ,column(4
                        
                        ,style="padding:0px; margin:0px;"
                        ,fluidRow(
                          
                          style="padding:0px; margin:0px;"
                          
                          ,box(width = 12,id = "foo"
                               ,highchartOutput("VendorPie",height = 330)
                               ,highchartOutput("VendorColumn",height = 360  )
                               
                          )
                        )
                        
                )),
              
              
              
              #,box(width = 4,id = "foo",highchartOutput("VendorColumn",height = 280  ))),
              
              
              
              
      ) # End Dashtab1
      #### dashTab2 - Summary Metrics ####
      ,tabItem("dashTab2" 
               ,br()
               ,box(id="foo"
                    ,status="primary"
                    ,width=12
                    , style= 'padding:0px;  margin:0px; "margin-left:15px; margin-bottom:10px;' 
                    
                      
                      
                      ,side = "right"
                      , title=span(icon("database"), textOutput("tabsetgeneralTabs", inline = T), style= "color:#1d5a78; font-size:20px; padding-left:10px;")
                      ,selected = "Campaign Summary"
                      ,tabPanel("Campaign Summary"
                                ,fluidRow(  style= "margin-left:0px; margin-bottom:10px;" ,
                                            hidden(actionButton("expand_btn", "Expand rows")
                                                   ,actionButton("collapse_btn", "Collapse rows")
                                            )
                                            ,csvDownloadButton("Campaign Summary", filename = "Campaign Summary"))
                                ,reactableOutput("Campaign_Table" )
                      )
                      
                      #,tabPanel("SMS Campaign Results")
                    
               )
      )
      #### end dashTab2  ####
      
      ####Dashtab3 ####
      ,tabItem("dashTab3" 
               ,br()
               ,box(id="foo"
                    ,style= 'padding:0px;  margin:0px; "margin-left:15px; margin-bottom:10px;'
                    ,status="primary"
                    ,width = 12
                    ,shinycssloaders::withSpinner(highchartOutput("CTATrends",height = 500))
                     
                    
                    # ,tabBox(  
                    #   type = "tabs"
                    #   ,id="generalTabs2"
                    #   ,width = "100%"
                      ,side = "right"
                      , title=span(icon("database"), textOutput("tabsetgeneralTabs", inline = T), style= "color:#1d5a78; font-size:20px; padding-left:10px;")
                      ,selected = "Activations"
                      ,tabPanel("Activations"
                                ,fluidRow(column(12
                                                  
                                                  ,style="padding:0px; margin:0px;"
                                                  
                                                  ,fluidRow(
                                                    style="padding:0px; margin:0px;margin-left:5px;"
                                                    # ,box(width = 12, id = "foo",shinycssloaders::withSpinner(highchartOutput("CTATrends"))
                                                    # )
                                                    
                                                  )
                                                  
                                                  
                                ))
                                
                      )
                      
                      #,tabPanel("SMS Campaign Results")
                    #)
               )
      )
      ####EndDashtab3####
      
      ####Dashtab4 ####
      ,tabItem("dashTab4" 
               ,br()
               ,box(id="foo"
                    ,status="primary"
                    ,width=12
                    , style= 'padding:0px;  margin:0px; "margin-left:15px; margin-bottom:10px;' #
                    ,shinycssloaders::withSpinner(leafletOutput("healthmap",height = 750))
                    #,tabBox(  
                      # ,type = "tabs"
                      # ,id="generalTabs2"
                      #,width = "100%"
                      ,side = "right"
                      , title=span(icon("map"), textOutput("tabsetgeneralTabs", inline = T), style= "color:#1d5a78; font-size:20px; padding-left:10px;")
                      ,selected = "Healthmap"
                      
                      
                      #,tabPanel("SMS Campaign Results")
                    #)
               )
      )
      ####EndDashtab4####
      
      ####Dashtab5 ####
      
      ,tabItem("dashTab5" 
               ,br()
               ,navset_card_tab(
                 height = 450,
                 full_screen = TRUE,
                 #title = "Definitions",
                 nav_panel(
                   "Marketing Dashboard Engagement Definitions",
                   card_title(""),
                   "Delivered = Message delivered successfully",
                   tags$br(),
                   "Email Engaged = Member opened their email message",
                   tags$br(),
                    "SMS Engaged = Member clicked their SMS message",
                   tags$br(),
                   "Push Engaged = Member clicked their push message",
                   tags$br(),
                   "Unique Members = All distinct members in all campaigns",
                   tags$br(),
                   "Total Communications = Number of contacts (not unique)",
                   tags$br(),
                   "Activation = Active w/in 7 days of contact",
                   tags$br(),
                   "Activation Rate = # Actives / # Delivered"
                 ),
                 # nav_panel(
                 #   "Leaflet",
                 #   card_title("A leaflet plot"),
                 #   leaflet_widget
                 # ),
                 # nav_panel(
                 #   shiny::icon("circle-info"),
                 #   markdown("Learn more about [htmlwidgets](http://www.htmlwidgets.org/)")
                  #)
               )
               
      )
      ####EndDashtab5####
      
    )
  )
  
  ### controlbar####
  , controlbar = dashboardControlbar(
    id = "controlbar",
    disable = FALSE,
    width = 350,
    collapsed = TRUE,
    #overlay = TRUE,
    skin = "light",
    #skin = "transparent",
    controlbarMenu(
      id = "menu",
      
      controlbarItem(
        #### Filters #### 
        "Filters"
        
        ,conditionalPanel(
          condition = "input.menu1  != 'dashTab5'"
          ,uiOutput('resetable_input')
          
          
          ,splitLayout(
            style="margin-top:300px;",
            actionBttn(inputId = "button1",
                       #label = "Submit",
                       label = span( icon("filter"), "Submit", style= "font-size:15px;" ),
                       style = "fill", 
                       block = TRUE,
                       #icon=icon("filter"),
                       color = "primary"
            ),
            actionBttn(inputId = "reset_input",
                       label = span( icon("refresh"), "Reset inputs", style= "font-size:15px;" ),
                       style = "fill", 
                       color = "success",
                       block = TRUE
                       #,icon = icon("bars")
            )
            
          )
          ,br()
          ,splitLayout(style="padding-bottom:5px; margin:0px; margin-top:-10px;"
                       # ,actionButton(inputId = "buttonEmail", label = "Hide Email Box", icon = icon("envelopes-bulk"), width="100%" )
                       # ,actionButton(inputId = "buttonRelay", label = "Hide Relay Box", icon = icon("comments") , width="100%" )
          ) 
        )),
      #### Other #### 
      controlbarItem("Other"
                     
                     
      ))
    
  ) # controlbar close
  
  
)
server <- function(input, output, session) {
  
  #### filters buttons Picker Inputs  ####     
  output$resetable_input <- renderUI({
    times <- input$reset_input
    
    div(id=letters[(times %% length(letters)) + 1]
        ,dateRangeInput('dateRange', 
                        label = 'Filter Campaigns by date',
                        start = min(campaign$MY) , end = max(campaign$MY) #Sys.Date() #as.Date('2023-12-31'),
                        
        )
        ,pickerInput('Client_Name', label="Client Name", choices=sort(unique(Dist_Client$Client_Name)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign),
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        ,pickerInput('Camp_ID', label="Master Campaign Id", choices=sort(unique(Dist_Vendor$Master_Campaign_ID)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign),
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        
        ,pickerInput('MX_Num', label="Tactic Number", choices=sort(unique(Dist_Vendor$MX_Number)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign),
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        
        ,pickerInput('Campaign', label="Campaign", choices=sort(unique(Dist_Vendor$Tactic)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign), 
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        
        ,pickerInput('Channel', label="Channel", choices=unique(campaign$Channel) , multiple = TRUE , #selected = unique(selected_campaign$Campaign),
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        
        ,pickerInput('Vendor', label="Vendor", choices=sort(unique(campaign$Vendor)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign),
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        ,pickerInput('LOB', label="LOB", choices=sort(unique(campaign$LOB)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign), 
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
        ,pickerInput('MARKET', label="Market", choices=sort(unique(campaign$MARKET)) , multiple = TRUE , #selected = unique(selected_campaign$Campaign), 
                     options = list(`actions-box` = TRUE , `live-search`=TRUE))
    )
  })
  ##
  #### reactiveVal - filteredData()  #### 
  filteredData <- reactiveVal(campaign)
  
  ####submit button filtered choices ####  
  observeEvent(input$button1, { 
    filteredData(campaign %>% 
                   filter(
                     (MY >= input$dateRange[1] & MY <= input$dateRange[2]) &
                       (Tactic %in% input$Campaign | is.null(input$Campaign)) &
                       (Channel %in% input$Channel | is.null(input$Channel)) 
                     &(MX_Number %in% input$MX_Num | is.null(input$MX_Num))
                     &(Master_Campaign_ID %in% input$Camp_ID| is.null(input$Camp_ID))
                     &(Vendor %in% input$Vendor| is.null(input$Vendor))
                     &(LOB %in% input$LOB| is.null(input$LOB))
                     &(Client_Name %in% input$Client_Name| is.null(input$Client_Name))
                     &(MARKET %in% input$MARKET| is.null(input$MARKET))
                   ))
    
  }) 
  
  #### observeEvent - reset button ####
  observeEvent(input$reset_input, {
    filteredData(campaign)
    
    
  })
  
  #### observe event expand ####
  # observeEvent(input$expand_btn, {
  #   # Expand all rows
  #   updateReactable("Campaign Summary", expanded = TRUE)
  # })
  
  #### observeEvent - collapse_btn ####
  # observeEvent(input$collapse_btn, {
  #   # Collapse all rows
  #   updateReactable("Campaign Summary", expanded = FALSE)
  # })
  
  #### renderText - Box Title | Text#### 
  output$Campaign_Table <- renderText({
    #outputText = ifelse(is.na(input$Campaign), 'Summary Metric', input$Campaign)
    if (is.null(input$Campaign)){'Summary Metrics'}
    else {input$Campaign}
  }) 
  
  ### KPI 1 Delivered -  #### 
  
  output$Delivered<- renderUI({
    delivered <- filteredData() %>% 
      summarise(
        Sent=sum(Sent),
        
        Delivered=sum(Delivered),
        
        Delivered_Rate = scales::percent(accuracy = .01,Delivered/Sent))
    
    
    
    
    
    valueBoxSpark(
      #value = prettyNum(member$Email, big.mark = ","),
      value =  comma(delivered$Delivered),
      
      title = toupper("Total Delivered"),
      sparkobj = NULL,
      subtitle = paste0("Delivered Rate ",delivered$Delivered_Rate),
      info = "Total Delivered",
      icon = icon("envelopes-bulk"),
      width = 4,
      
      color = "blue",
      
      
      href = NULL
    )
    
    
    
  })
  
  ### KPI 2 Engagement - renderUI (Summary Boxes | KPI v2 OLD) #### 
  
  output$Engagement <- renderUI({
    Engagement <- filteredData() %>%  
      summarise(Members = n_distinct(ECI),
                Sent=sum(Sent),
                
                Delivered=sum(Delivered),
                Delivered_Rate = percent(accuracy = .01,Delivered/Sent),
                
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
                Action_Taken=sum(Action_Taken),
                Action_Taken_Rate = percent(accuracy = .01,Action_Taken/Sent),
                
                Unique_Eng_Rate = percent(accuracy = .01,Engaged/Members),
                # Email_Bounced=sum(coalesce(`Emails Bounced`,0)),
                # Unsubscribed=sum(coalesce(Unsubscribed,0)),
                
                # Bounce_Rate = percent(accuracy = .01,Bounced/Sent),
                # Opened_Rate = percent(accuracy = .01,Opened/Sent),
                # Clicked_Rate = percent(accuracy = .01,Clicked/Sent),
                # Unsub_Rate = percent(accuracy = .01,Unsubscribed/Sent),
                
      )
    
    
    
    valueBoxSpark(
      #value = prettyNum(member$Email, big.mark = ","),
      value =  comma(Engagement$Engaged),
      
      title = toupper("Total Engaged"),
      sparkobj = NULL,
      subtitle = paste0("Engagement Rate ", Engagement$Engagement_Rate),
      info = "Members that opened their email or clicked their SMS message",
      icon = icon("tablet-screen-button"),
      width = 4,
      color = "blue",
      
      
      href = NULL
      
    )
    
    
    
  })
  
  ### KPI 3 Members   #### 
  
  output$Members <- renderUI({
    member <- filteredData() %>%  
      summarise(Members = n_distinct(ECI),
                
                Sent=sum(Sent),
                
                Delivered=sum(Delivered),
                Delivered_Rate = percent(accuracy = .01,Delivered/Sent),
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
                Action_Taken=sum(Action_Taken),
                Action_Taken_Rate = percent(accuracy = .01,Action_Taken/Delivered),
                Unique_Eng_Rate = percent(accuracy = .01,Members/Delivered)
                
                
                # Email_Bounced=sum(coalesce(`Emails Bounced`,0)),
                # Unsubscribed=sum(coalesce(Unsubscribed,0)),
                
                # Bounce_Rate = percent(accuracy = .01,Bounced/Sent),
                # Opened_Rate = percent(accuracy = .01,Opened/Sent),
                # Clicked_Rate = percent(accuracy = .01,Clicked/Sent),
                # Unsub_Rate = percent(accuracy = .01,Unsubscribed/Sent),
      ) 
    
    
    
    
    valueBoxSpark(
      value =  comma(member$Members),
      
      title = toupper("Total Unique Members"),
      #sparkobj = hc,
      subtitle =paste0("Total Communications ", comma(member$Sent)),
      #subtitle =paste0(""),
      
      info = "",
      icon = icon('people-group'),
      width = 4,
      
      color = "blue",
      
      
      href = NULL
    )
    
  })
  ####Bar Chart Data ####
  
  
  # email_line <- chart %>%
  #   filter(Channel == "Email")
  
  # SMS_line <- chart %>%
  #   filter(Channel == "SMS")
  
  
  
  
  ####Trends Bar####
  
  output$CampTrends <- renderHighchart({ 
    
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Channel', event.point.name);}")
    
    
    #### Trends 03 Email Engagement ####
    Trends_03 <- filteredData() %>%
      filter(Channel == 'Email') %>%
      group_by(MY) %>%
      summarise(Delivered= sum(Delivered),
                Engaged=sum(Opened)) %>% 
      mutate(Engaged_Rate = round(Engaged/Delivered*100,2))
    
    
    #### Trends 01 Delivered by Channel ####
    Trends_01 <- filteredData() %>%
      
      mutate(Legend = paste(Channel,"Delivered")) %>%
      mutate(Channel = case_when(
        Channel == 'SMS' ~ 'SMS',
        Channel == 'Email' ~ 'Email',
        TRUE ~ "U")) %>%
      filter(Channel != "U") %>%
      
      group_by(MY,Channel,Legend) %>%
      summarise(Delivered= sum(Delivered),
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = percent(accuracy = .01,Engaged/Delivered))
    
    # #Platform Engaged
    # Trends_02 <- wrData %>% filter(Platform2 == !!platformCategory2) %>%  
    #   mutate(Legend = ifelse(Platform == 'AMH', substr(Platform2, 5, nchar(Platform2)),
    #                          substr(Platform2, 12, nchar(Platform2))))
    ####Trends 04 SMS Engagement####
    Trends_04 <- filteredData() %>%
      filter(Channel == 'SMS') %>%
      group_by(MY) %>%
      summarise(Delivered= sum(Delivered),
                Engaged=sum(Clicked)) %>% 
      mutate(Engaged_Rate = round(Engaged/Delivered*100,2))
    
    ####Trends Highchart####
    hc <- highchart() %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_title( margin=0, text = "Engagement By Channel", align="center", style = list(color = "#003963", fontWeight = "bold"))  %>%
      hc_xAxis(
        #dateTimeLabelFormats = list(day = '%m  %Y')
        dateTimeLabelFormats = list(day = '%b') #month 
        ,type = "datetime", gridLineWidth= 0
        ,labels = list(style = list(color =  "#003963",fontWeight = "bold", fontSize = "10px"))
        
      ) %>%
      hc_yAxis_multiples(
        list(title = list(text = " ")
             ,labels = list( format = "{value:,.0f}", style = list(fontWeight = "bold", color =  "#003963") ) 
             #,max=100, #tickPixelInterval= 30, 
             ,showFirstLabel = TRUE, showLastLabel=TRUE,  opposite = FALSE),
        list(title = list(text = " ") 
             #,min=min(Members$Rate)  ,max = max(Members$Rate)
             ,labels=list(format = '{value}%' , style = list(fontWeight = "bold", color =  "#003963")) 
             ,showFirstLabel = TRUE ,showLastLabel = TRUE, opposite = TRUE
             #,tickPixelInterval= 30 
        )
      ) %>%
      hc_plotOptions(
        column = list( 
          maxPointWidth=40
          #, stacking = barStacking()
          , dataLabels = list(
            enabled=F
            
            #enabled=slabel()
            #,format= '{y}%' #suffix*
            ,format= '{y}' #suffix*
          )) )  %>%
      hc_legend(enabled= T,text = " ", verticalAlign= "top",align= "center") %>% 
      #### Trends HC Axis ####
    hc_add_series(Trends_01,type = "column", 
                  name= unique(Trends_01$Legend),
                  hcaes(x=MY, y=Delivered,group = Channel)
    ) %>%
      # hc_add_series(Trends_02,type = input$chartType1 , 
      #               name= unique(Trends_02$Legend),
      #               hcaes(x=INC_MTH_DT, y=Members)
      # ) %>%
      hc_add_series(Trends_03,  type="spline" ,
                    dashStyle = "longdash", lineWidth= 1,
                    zIndex=100,
                    name = "Email Engaged",
                    hcaes(x=MY,y=Engaged_Rate),
                    yAxis=1,
                    visible=T,
                    tooltip = list(valueSuffix= '%')) %>%
      hc_add_series(Trends_04,  type="spline" ,
                    dashStyle = "longdash", lineWidth= 1,
                    zIndex=100,
                    name = "SMS Engaged",
                    hcaes(x=MY,y=Engaged_Rate),
                    yAxis=1,
                    visible=T,
                    tooltip = list(valueSuffix= '%')) %>%
      hc_rangeSelector(
        enabled = T,
        inputEnabled = F,
        verticalAlign = "bottom",
        selected = 2, #2 = 12m
        buttons = list(
          #list(type = 'week', count = 1, text = 'Week'),
          #list(type = 'month',count = 1,  text = '1m'),
          #list(type = 'month',count = 2,  text = '3m'),
          list(type = 'month',count = 5,  text = '6m'),
          list(type = 'month',count = 8,  text = '9m'),
          list(type = 'month',count = 11,  text = '12m'),
          #list(type = 'year', count = 1,  text = '1y'),
          #list(type = 'ytd', text = 'YTD'),
          list(type = 'all', text = 'All')
        )
      ) %>%
      hc_chart(
        style = list(fontFamily = "Poppins" ) 
        ,options3d = list(enabled = T, beta = 2, alpha = 7)
        ,backgroundColor= 'white' #'white'
        ,borderColor= 'transparent'
        # ,borderRadius= 20
        # ,borderWidth= 2
        ,type= 'line'
        ,spacingTop=20
        #,spacingLeft=20
        ,spacingRight=20
        #,displayErrors= TRUE
      ) %>% 
      ####Trends hc colors####
    #hc_colors(c(HM_COLORS[2], HM_COLORS[3],HM_COLORS[1],HM_COLORS[4] ))   %>%
    hc_colors(c(HM_COLORS[2], HM_COLORS[3],"#1b9616", HM_COLORS[1]))   %>%
      hc_legend(enabled= T,text = " ", verticalAlign= "top",align= "center") %>%    
      hc_plotOptions(
        column = list( 
          maxPointWidth=10
          #, stacking = barStacking()
          , dataLabels = list(
            enabled=F
            #enabled=slabel()
            #,format= '{y}%' #suffix*
            ,format= '{y}' #suffix*
          )) 
        ,spline = list(
          marker= list(enabled=TRUE,  symbol="circle") #lineWidth= 1,  fillColor= 'white', lineColor= NULL,
          ,dashStyle = "ShortDashDot", lineWidth= 1
          ,dataLabels = list(
            #enabled=slabel()
            enabled=F
            ,crop=FALSE
            ,overflow= 'none'
            ,inside= FALSE
            ,y= 30 
            ,borderRadius= 4 # roundedges
            ,borderWidth= 1
            ,borderColor= NULL
            , style = list(
              fontFamily="Helvetica"
              ,color = NULL
              ,fontWeight = 600
            )))
      ) %>% 
      hc_xAxis(
        tickInterval   =24 * 3600 * 1000*30, allowDecimals= F, #Show all months
        #dateTimeLabelFormats = list(day = '%m  %Y')
        dateTimeLabelFormats = list(day = '%b') #month 
        #dateTimeLabelFormats = list(month = '%b', day= '%b %e', week= '%b %e') #, category=unique(Pilot$DATE)
        ,type = "datetime"
        , gridLineWidth= 0
        ,labels = list(style = list(color =  "#003963",fontWeight = "bold", fontSize = "10px"))
      ) %>%
      hc_add_theme(hm_theme)
    hc
  })
  
  
  
  
  #### Vendor Column Chart #####
  output$VendorColumn  <- renderHighchart({   
    
    dss <- filteredData() %>%
      mutate(Legend = paste(Vendor,Channel)) %>%
      
      
      group_by(Vendor,Channel,Legend) %>%
      summarise(Delivered= sum(Delivered)  
                ,Engaged = sum(Opened,Clicked)
                ,Engaged_Rate = round(sum(Engaged/Delivered*100),2)
                
                # Engagement_Rate = percent(accuracy = .01,Engaged/Impressions)
      )
    
    
    VendorColumn(dss, title = '')
    
    
  })
  VendorColumn <- function(dss, type="column", pointWidth=25, whiteSpace= 'nowrap', title = '') {
    
    dss%>%
      hchart(type, hcaes(x=Vendor,y=Engaged_Rate,group = Legend),stacking = FALSE)   %>%
      #hc_add_theme(hm_theme) %>%
      hc_chart(backgroundColor= "white"
               ,options3d = list(enabled = F, beta = 0, alpha = 7)
               ,style = list(fontFamily = "Poppins" ) 
               ,spacingRight=15
      ) %>%
      #hc_colors(cols)   %>%
      hc_exporting(enabled = TRUE) %>% 
      #hc_tooltip(crosshairs = TRUE, valueSuffix= '') %>%
      hc_tooltip(crosshairs = TRUE, 
                 
                 headerFormat= '<b>{point.group}</b><br>',
                 #pointFormat= '{point.Vendor}',
                 pointFormat= '{point.Vendor}:</b><br> {point.Channel}  {point.y}%'
      ) %>%
      hc_title(  enabled= F,
                 useHTML= F,
                 margin=0,
                 text= "Engaged Rate By Vendor",
                 align = "center",style = list(useHTML = TRUE, color="#003963", fontWeight = "bold")) %>%   # "#003963", fontSize = "8px",fontWeight = "normal"
      hc_xAxis(title = list(text = "")
               ,labels=list(enabled=T, style = list(color = "#003963",fontWeight = "bold",width=10, whiteSpace= whiteSpace)) 
               ,gridLineWidth= 0
               #,categories=unique(dss$Legend)
               
               
      ) %>% 
      hc_add_theme(hm_theme) %>% 
      hc_yAxis(title = list(text = "")
               ,labels = list(enabled=T, style = list(color =  "#003963"))
               ,gridLineWidth= 1, gridLineDashStyle= 'LongDash'
      ) %>%
      hc_legend(enabled= T,text = " ", verticalAlign= "bottom",align= "left"
                ,itemStyle = list(fontSize = "10px",useHTML = TRUE) ) %>% 
      hc_plotOptions(
        
        column = list(
          pointWidth=20,
          dataLabels = list(enabled=F
                            
                            #,format= '{y}'
                            ,crop=FALSE
                            ,overflow= 'none'
                            ,inside= FALSE
                            ,y= -10
                            ,borderRadius= 4
                            ,backgroundColor= 'rgba(240, 240, 240,1)'
                            ,margin=1
                            ,borderWidth= 1
                            ,borderColor= NULL
                            , style = list(
                              fontFamily="Poppins"
                              ,color = NULL #'white'
                              ,fontWeight = 600
                              #,sortKey= 'x'
                            )
          )
          
          # color = list(
          #   linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
          #   stops = list(
          #     c(0, '#1d89ff'),
          #     c(1, '#2c538d')
          #   )
          # )
        ) ) %>% 
      
      hc_colors(c("#51d6ff",
                  "#003963",
                  "#008DD1",
                  "#0CB161",
                  "#F7987D",
                  "#01b7f0",
                  "#006D41",
                  "#9CD5BA",
                  "#FAC1B3",
                  "#EF4056",
                  "#FFD961")
      )
    
    
  }
  
  
  ####Vendor Column End####
  
  
  ####Vendor Pie####
  
  VendorPie <- function(dss, chartHeight= 320, options3d = F
                        , slicedOffset = 10, legendMargin=0
                        , titleMargin=0, subtitleMargin=0
                        , subtitle ="",
                        title ="Vendor Delivered Distribution") {
    
    Lvl1dfStatus <- data_frame(name = dss$Legend,y = dss$Delivered_Perc,participants=dss$Participants, response_text=dss$Vendor)
    
    hc <- highchart() %>% 
      hc_size(height= chartHeight) %>%
      hc_add_series(data=Lvl1dfStatus, type = "pie", name=" ", colorByPoint = TRUE, showInLegend=TRUE
                    
                    ,dataLabels = list(enabled=F)
                    ,tooltip=list (
                      headerFormat= '<b>{point.response_text}</b><br>',
                      #pointFormat= '{point.name}',
                      pointFormat= '{point.response_text}:</b><br> Delivered: {point.y}%',
                      crosshairs = TRUE,
                      shared=TRUE,
                      #borderWidth = 1,
                      sort = TRUE,
                      table = TRUE
                      #clusterFormat= 'Clustered points: {point.clusterPointsAmount}'
                    )
      ) %>%
      
      hc_tooltip(crosshairs = TRUE, 
                 #shared = TRUE 
                 #borderWidth = 2
                 #valueSuffix= '%'
                 pointFormat= '{point.Legend}:</b><br> {point.y}%'
      ) %>%
      hc_colors(c('#1493ffe6',
                  '#2c538d',
                  
                  '#18d2d9',
                  '#f88508',
                  '#96dcff'
      ))  %>%
      
      hc_chart(style = list(fontFamily = "Poppins" ) 
               ,borderColor= 'white'
               ,backgroundColor= 'white'
               ,spacingTop=15
               ,spacingLeft=0
               ,spacingRight=0
               # ,spacingBottom=10
               ,options3d = list(enabled = options3d                  #Options: F = no 3D | T = 3D
                                 ,beta = 0, alpha = 30)
      ) %>%
      #hc_exporting(enabled = TRUE, margin=10) %>% 
      
      #hc_legend(enabled= F,text = " ", verticalAlign= "top",align= "center") %>%
      #hc_add_theme(hm_theme) %>%
      
      hc_legend(enabled= T,text = " ", verticalAlign= "bottom",align= "center", margin=legendMargin
                ,itemStyle = list(fontSize = "12px",useHTML = TRUE) ) %>%
      hc_subtitle(text= subtitle ,align="center", margin=subtitleMargin  ,style = list(useHTML = TRUE, color = "#003963",fontSize = "14px")) %>%
      
      hc_title(  enabled= T,
                 margin=titleMargin,
                 text= title, #dss$question[1],
                 align="center",
                 style = list(color="#003963",fontWeight = "bold")) %>%   # "#003963", fontSize = "8px",fontWeight = "normal"
      
      hc_plotOptions(
        pie = list(
          stacking = FALSE
          , allowPointSelect = TRUE
          , showInLegend = TRUE
          , depth= 35
          , shadow= T
          , cursor= T
          , slicedOffset= slicedOffset
          #, startAngle= 0
          ,size= 140
          #, edgeWidth= 1
          #, edgeColor= 'white'
          #, marginLeft  = 10
          #, marginRight  = 10
          #, margin = 30
        )
      ) %>%
      hc_colors(c("#51d6ff",
                  "#003963",
                  "#008DD1",
                  "#0CB161",
                  "#F7987D",
                  "#01b7f0",
                  "#006D41",
                  "#9CD5BA",
                  "#FAC1B3",
                  "#EF4056",
                  "#FFD961"))  
    
    
    hc
    
    
  }
  
  #### PIE HC Data####
  
  output$VendorPie <- renderHighchart({   
    
    pie_data <- filteredData() %>%
      group_by(Vendor) %>%
      summarise(Members = n_distinct(ECI),
                Sent=sum(Sent),
                Delivered=sum(Delivered),
                
                Delivered_Rate = scales::percent(accuracy = .01,Delivered/Sent),
                Delivered_Perc = scales::percent(accuracy = .01,Delivered/sum(Delivered)),
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = scales::percent(accuracy = .01,Engaged/Delivered),
                Action_Taken=sum(Action_Taken),
                Action_Taken_Rate = scales::percent(accuracy = .01,Action_Taken/Sent),
                
                Unique_Eng_Rate = scales::percent(accuracy = .01,Engaged/Members)) %>%
      mutate(Delivered_Perc = round(Delivered/sum(Delivered)*100,2))
    dss <- pie_data %>% 
      
      mutate( Legend = paste(Vendor, ": ",paste0(Delivered_Perc,"%") )   ,           
              Participants = format(round(as.numeric(Members), 0), nsmall=0, big.mark=",")
      ) 
    
    
    VendorPie(dss, legendMargin=5, titleMargin=5, subtitle ="")
    
    
  })
  
  ##
  
  #### End Vendor Pie ####
  #### Email Table ####
  
  output$Email_Table <- renderReactable({
    
    Email  <- filteredData() %>%
      filter(Channel == "Email") %>%
      group_by(Channel) %>% 
      summarise(
        Members = n_distinct(ECI),
        Sent = sum(Sent),
        
        Delivered=sum(Delivered),
        Member_Rate = scales::percent(accuracy = .01,Delivered/Members),
        
        Opened=sum(Opened),
        Opened_Rate = scales::percent(accuracy = .01,Opened/Delivered),
        Clicked=sum(Clicked),
        Click_Rate = scales::percent(accuracy = .01,Clicked/Delivered),
        Bounced = sum(Bounced),
        Sent_Rate = scales::percent(accuracy = .01,(Sent - Bounced)/Sent),
        Delivered_Rate = scales::percent(accuracy = .01,(Delivered - Bounced)/Sent),
        Bounce_Rate = scales::percent(accuracy = .01,Bounced/Delivered),
        Unsubscribed = sum(Unsubscribed),
        Unsubscribe_Rate = scales::percent(accuracy = .01,Unsubscribed/Delivered),
        Enrolled = sum(Enrolled),
        Enrolled_Rate = scales::percent(accuracy = .01,Enrolled/Opened)
      )
    email_data <- tibble(
      
      Volume = c(
        comma(Email$Sent)
        ,comma(Email$Delivered)
        ,comma(Email$Opened)
        ,comma(Email$Clicked)
        ,comma(Email$Bounced)
        ,comma(Email$Unsubscribed)
        #,comma(Email$Enrolled)
      ),
      Rate = c(
        Email$Sent_Rate
        ,Email$Delivered_Rate
        ,Email$Opened_Rate
        ,Email$Click_Rate
        ,Email$Bounce_Rate
        ,Email$Unsubscribe_Rate
        #,Email$Enrolled_Rate
      )
    )
    email_data <- data_frame(email_data)
    rownames(email_data) <- c("Sent","Delivered","Opened","Clicked","Bounced","Unsub")
    
    email_data$`Emails` <- rownames(email_data)
    
    email_data <- email_data[,c(3,1,2)]
    
    
    
    
    
    reactable(email_data
              ,rownames = FALSE
              ,defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              )
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              ,h3("Candidates whose followers are loyal only to them")
              
              
              
              
    )
    
  })
  
  #### End Email Table####
  ### SMS Table ####
  output$SMS_Table <- renderReactable({
    
    SMS  <- filteredData() %>%
      filter(Channel == "SMS") %>%
      summarise(
        Members = n_distinct(ECI),
        Sent = sum(Sent),
        
        Delivered=sum(Delivered),
        Member_Rate = scales::percent(accuracy = .01,Delivered/Members),
        
        Opened=sum(Opened),
        Opened_Rate = scales::percent(accuracy = .01,Opened/Delivered),
        Clicked=sum(Clicked),
        Click_Rate = scales::percent(accuracy = .01,Clicked/Delivered),
        Action=sum(Action_Taken),
        Action_Rate = scales::percent(accuracy = .01,Action/Delivered),
        Bounced = sum(Bounced),
        Sent_Rate = scales::percent(accuracy = .01,(Sent - Bounced)/Sent),
        Delivered_Rate = scales::percent(accuracy = .01,(Delivered - Bounced)/Sent),
        Bounce_Rate = scales::percent(accuracy = .01,Bounced/Delivered),
        Unsubscribed = sum(Unsubscribed),
        Unsubscribe_Rate = scales::percent(accuracy = .01,Unsubscribed/Delivered)
      )
    sms_data <- tibble(
      
      Volume = c(
        comma(SMS$Sent)
        ,comma(SMS$Delivered)
        ,comma(SMS$Clicked)
        ,comma(SMS$Action)
        ,comma(SMS$Bounced)
        #,comma(SMS$Unsubscribed)
      ),
      Rate = c(
        SMS$Sent_Rate
        ,SMS$Delivered_Rate
        ,SMS$Click_Rate
        ,SMS$Action_Rate
        ,SMS$Bounce_Rate
        #,SMS$Unsubscribe_Rate
      )
    )
    sms_data <- as.data.frame(sms_data)
    rownames(sms_data) <- c("Sent","Delivered","Clicked","Action Taken","Failed")
    
    
    
    sms_data$`SMS` <- rownames(sms_data)
    
    sms_data <- sms_data[,c(3,1,2)]
    
    
    
    
    reactable(sms_data
              ,rownames = FALSE
              ,defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              )
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              
    )
    
  })
  
  #### End SMS Table ####
  #### Push Table ####
  output$Push_Table <- renderReactable({
    
    PUSH  <- filteredData() %>%
      filter(Channel == "Push Notification") %>%
      summarise(
        Members = n_distinct(ECI),
        Sent = sum(Sent),
        
        Delivered=sum(Delivered),
        Member_Rate = scales::percent(accuracy = .01,Delivered/Members),
        
        Opened=sum(Opened),
        Opened_Rate = scales::percent(accuracy = .01,Opened/Delivered),
        Clicked=sum(Clicked),
        Click_Rate = scales::percent(accuracy = .01,Clicked/Delivered),
        Bounced = sum(Bounced),
        Sent_Rate = scales::percent(accuracy = .01,(Sent - Bounced)/Sent),
        Delivered_Rate = scales::percent(accuracy = .01,(Delivered - Bounced)/Sent),
        Bounce_Rate = scales::percent(accuracy = .01,Bounced/Delivered),
        Unsubscribed = sum(Unsubscribed),
        Unsubscribe_Rate = scales::percent(accuracy = .01,Unsubscribed/Delivered)
      )
    push_data <- tibble(
      
      Volume = c(
        comma(PUSH$Sent)
        ,comma(PUSH$Delivered)
        #,comma(SMS$Opened)
        ,comma(PUSH$Clicked)
        ,comma(PUSH$Bounced)
        #,comma(SMS$Unsubscribed)
      ),
      Rate = c(
        PUSH$Sent_Rate
        ,PUSH$Delivered_Rate
        #,SMS$Opened_Rate
        ,PUSH$Click_Rate
        ,PUSH$Bounce_Rate
        #,SMS$Unsubscribe_Rate
      )
    )
    push_data <- as.data.frame(push_data)
    rownames(push_data) <- c("Sent","Delivered","Clicked","Failed")
    
    push_data$`Push` <- rownames(push_data)
    
    push_data <- push_data[,c(3,1,2)]
    
    reactable(push_data
              ,rownames = FALSE
              ,defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              )
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              
    )
    
    
  })
  
  #### End Push Table ####
  
  #### Enroll Table ####
  
  output$Enr_Table <- renderReactable({
    
    Em_Enroll  <- filteredData() %>%
      filter(Vendor == "Vida") %>%
      filter(Channel == "Email") %>%
      summarise(
        
        Delivered=sum(Delivered),
        
        Enrolled = sum(Enrolled),
        Enrolled_Rate = scales::percent(accuracy = .01,Enrolled/Delivered)
      ) %>%
      mutate(Channel = "Email")
    
    sms_Enroll  <- filteredData() %>%
      filter(Vendor == "Vida") %>%
      filter(Channel == "SMS") %>%
      summarise(
        Delivered=sum(Delivered),
        Enrolled = sum(Enrolled),
        Enrolled_Rate = scales::percent(accuracy = .01,Enrolled/Delivered)
      ) %>%
      mutate(Channel = "SMS")
    
    ENR <- rbind(Em_Enroll,sms_Enroll)
    
    enroll_data <- tibble(
      
      Enrolled = c(
        comma(ENR$Enrolled)
      ),
      Rate = c(
        ENR$Enrolled_Rate
      )
    )
    enroll_data <- as.data.frame(enroll_data)
    rownames(enroll_data) <- c("Vida Email","Vida SMS")
    
    enroll_data$`Vendor` <- rownames(enroll_data)
    
    enroll_data <- enroll_data[,c(3,1,2)]
    
    
    
    
    
    reactable(enroll_data
              ,rownames = FALSE
              ,defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              )
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              
              
              
              
              
    )
    
  })
  
  #### End Enroll Table####
  
  #### Calls Table ####
  
  output$Calls_Table <- renderReactable({
    
    
    
    
    
    
    
    reactable(Calls
              
              ,defaultColDef = colDef(
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
              )
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              
              
              
              
              
    )
    
  })
  
  #### End Calls Table####
  
  #### Push Column Chart #####
  output$PushColumn <- renderHighchart({   
    
    dss <- filteredData() %>%
      filter(Channel == "Push Notification") %>% 
      
      
      
      group_by(Vendor) %>%
      summarise(Impressions= sum(Delivered)  
                ,Engaged = sum(Engaged)
                ,Engaged_Rate = round(sum(Engaged/Impressions*100),2)
                
                # Engagement_Rate = percent(accuracy = .01,Engaged/Impressions)
      )
    
    
    PushColumn(dss, title = '')
    
    
  })
  PushColumn <- function(dss, type="column", pointWidth=25, whiteSpace= 'nowrap', title = '') {
    
    dss%>%
      hchart(type, hcaes(x=Vendor,y=Engaged_Rate,group = Vendor),stacking = FALSE)   %>%
      #hc_add_theme(hm_theme) %>%
      hc_chart(backgroundColor= "white"
               ,options3d = list(enabled = F, beta = 0, alpha = 7)
               ,style = list(fontFamily = "Poppins" ) 
               ,spacingRight=15
      ) %>%
      #hc_colors(cols)   %>%
      hc_exporting(enabled = FALSE) %>% 
      #hc_tooltip(crosshairs = TRUE, valueSuffix= '') %>%
      hc_tooltip(crosshairs = TRUE, 
                 
                 headerFormat= '<b>{point.group}</b><br>',
                 #pointFormat= '{point.Vendor}',
                 pointFormat= '{point.Vendor}:</b><br> Clicked Rate  {point.y}%'
      ) %>%
      hc_title(  enabled= F,
                 useHTML= F,
                 margin=0,
                 text= "Push Clicked Rate",
                 align = "center",verticalAlign = "bottom",style = list(useHTML = TRUE, color="#003963")) %>%   # "#003963", fontSize = "8px",fontWeight = "normal"
      hc_xAxis(title = list(text = "")
               ,labels=list(enabled=T, style = list(color = "#003963",fontWeight = "bold",width=10, whiteSpace= whiteSpace)) 
               ,gridLineWidth= 0
               #,categories=unique(dss$Legend)
               
               
      ) %>% 
      hc_add_theme(hm_theme) %>% 
      hc_yAxis(title = list(text = "%")
               ,labels = list(enabled=T, style = list(color =  "#003963"))
               ,gridLineWidth= 1, gridLineDashStyle= 'LongDash'
      ) %>%
      
      hc_plotOptions(
        
        column = list(
          pointWidth=pointWidth,
          dataLabels = list(enabled=F
                            
                            #,format= '{y}'
                            ,crop=FALSE
                            ,overflow= 'none'
                            ,inside= FALSE
                            ,y= -10
                            ,borderRadius= 4
                            ,backgroundColor= 'rgba(240, 240, 240,1)'
                            ,margin=1
                            ,borderWidth= 1
                            ,borderColor= NULL
                            , style = list(
                              fontFamily="Poppins"
                              ,color = NULL #'white'
                              ,fontWeight = 600
                              #,sortKey= 'x'
                            )
          )
          
          # color = list(
          #   linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
          #   stops = list(
          #     c(0, '#1d89ff'),
          #     c(1, '#2c538d')
          #   )
          # )
        ) ) %>% 
      hc_legend(enabled= F,text = " ", verticalAlign= "top",align= "center") %>%
      
      hc_colors(c("#51d6ff",
                  "#003963",
                  "#008DD1",
                  "#0CB161",
                  "#F7987D",
                  "#01b7f0",
                  "#006D41",
                  "#9CD5BA",
                  "#FAC1B3",
                  "#EF4056",
                  "#FFD961")
      )
    
    
  }
  
  
  ####Push Column End####
  
  #### Push Column Chart #####
  output$SMSColumn <- renderHighchart({   
    
    dss <- filteredData() %>%
      filter(Channel == "SMS") %>% 
      
      
      
      group_by(Vendor) %>%
      summarise(Impressions= sum(Delivered)  
                ,Clicked = sum(Engaged)
                ,Clicked_Rate = round(sum(Engaged/Impressions*100),2)
                
                # Engagement_Rate = percent(accuracy = .01,Engaged/Impressions)
      )
    
    
    SMSColumn(dss, title = '')
    
    
  })
  SMSColumn <- function(dss, type="column", pointWidth=25, whiteSpace= 'nowrap', title = '') {
    
    dss%>%
      hchart(type, hcaes(x=Vendor,y=Impressions,group = Vendor),stacking = FALSE)   %>%
      #hc_add_theme(hm_theme) %>%
      hc_chart(backgroundColor= "white"
               ,options3d = list(enabled = F, beta = 0, alpha = 7)
               ,style = list(fontFamily = "Poppins" ) 
               ,spacingRight=15
      ) %>%
      #hc_colors(cols)   %>%
      hc_exporting(enabled = FALSE) %>% 
      #hc_tooltip(crosshairs = TRUE, valueSuffix= '') %>%
      hc_tooltip(crosshairs = TRUE, 
                 
                 headerFormat= '<b>{point.group}</b><br>',
                 #pointFormat= '{point.Vendor}',
                 pointFormat= '{point.Vendor}:</b><br> Impressions  {point.y}'
      ) %>%
      hc_title(  enabled= F,
                 useHTML= F,
                 margin=0,
                 text= "SMS Impressions",
                 align = "center",verticalAlign = "bottom",style = list(useHTML = TRUE, color="#003963")) %>%   # "#003963", fontSize = "8px",fontWeight = "normal"
      hc_xAxis(title = list(text = "")
               ,labels=list(enabled=T, style = list(color = "#003963",fontWeight = "bold",width=10, whiteSpace= whiteSpace)) 
               ,gridLineWidth= 0
               #,categories=unique(dss$Legend)
               
               
      ) %>% 
      hc_add_theme(hm_theme) %>% 
      hc_yAxis(title = list(text = "")
               ,labels = list(enabled=T, style = list(color =  "#003963"))
               ,gridLineWidth= 1, gridLineDashStyle= 'LongDash'
      ) %>%
      
      hc_plotOptions(
        
        column = list(
          pointWidth=pointWidth,
          dataLabels = list(enabled=F
                            
                            #,format= '{y}'
                            ,crop=FALSE
                            ,overflow= 'none'
                            ,inside= FALSE
                            ,y= -10
                            ,borderRadius= 4
                            ,backgroundColor= 'rgba(240, 240, 240,1)'
                            ,margin=1
                            ,borderWidth= 1
                            ,borderColor= NULL
                            , style = list(
                              fontFamily="Poppins"
                              ,color = NULL #'white'
                              ,fontWeight = 600
                              #,sortKey= 'x'
                            )
          )
          
          # color = list(
          #   linearGradient = list(x1 = 0, x2 = 0, y1 = 0, y2 = 1),
          #   stops = list(
          #     c(0, '#1d89ff'),
          #     c(1, '#2c538d')
          #   )
          # )
        ) ) %>% 
      hc_legend(enabled= F,text = " ", verticalAlign= "top",align= "center") %>%
      
      hc_colors(c("#51d6ff",
                  "#003963",
                  "#008DD1",
                  "#0CB161",
                  "#F7987D",
                  "#01b7f0",
                  "#006D41",
                  "#9CD5BA",
                  "#FAC1B3",
                  "#EF4056",
                  "#FFD961")
      )
    
    
  }
  
  
  ####SMS Column End####
  
  #### Campaign Table ####
  output$Campaign_Table <- renderReactable({
    
    campaign_data  <- filteredData()%>%
      filter(Vendor == "Highmark") %>%
      group_by(Tactic,MX_Number,Master_Campaign_ID) %>%
      summarise(
        Members = n_distinct(ECI),
        Sent = sum(Sent),
        Delivered=sum(coalesce(Delivered,0)),
        `Delivered Rate` = scales::percent(accuracy = .01,Delivered/Sent),
        Engaged=sum(Opened,Clicked),
        `Engaged Rate` = scales::percent(accuracy = .01,Engaged/Delivered),
        `Action Taken`=sum(Action_Taken),
        `Action Taken Rate` = scales::percent(accuracy = .01,`Action Taken`/Delivered),
        Bounced=sum(Bounced),
        `Bounced Rate` = scales::percent(accuracy = .01,Bounced/Sent),
        Unsubscribed=sum(Unsubscribed),
        `Unsubscribed Rate` = scales::percent(accuracy = .01,Unsubscribed/Delivered)
      ) %>% 
      ungroup()
    campaign_data <- rename(campaign_data, c("MX Number" = "MX_Number"))
    campaign_data <- rename(campaign_data, c("Master Campaign ID" = "Master_Campaign_ID"))
    
    # Pretty Numbers
    
    campaign_data$Members<-prettyNum(campaign_data$Members, big.mark = ",")
    campaign_data$Sent<-prettyNum(campaign_data$Sent, big.mark = ",")
    campaign_data$Delivered<-prettyNum(campaign_data$Delivered, big.mark = ",")
    campaign_data$Engaged<-prettyNum(campaign_data$Engaged, big.mark = ",")
    campaign_data$`Action Taken`<-prettyNum(campaign_data$`Action Taken`, big.mark = ",")
    campaign_data$Bounced<-prettyNum(campaign_data$Bounced, big.mark = ",")
    campaign_data$Unsubscribed<-prettyNum(campaign_data$Unsubscribed, big.mark = ",")
    
    
    reactable(campaign_data
              # ,columns = list(
              #   Sent = colDef(format = colFormat(separators = TRUE)),
              #   Delivered = colDef(format = colFormat(separators = TRUE))
              # )
              
              ,defaultColDef = colDef(
                
                header = function(value) gsub(".", " ", value, fixed = TRUE),
                cell = function(value) format(value, nsmall = 0),
                align = "center",
                minWidth = 70,
                headerStyle = list(background = "#f7f7f8")
                ,filterable = TRUE
              )
              
              , resizable = TRUE
              ,bordered = TRUE
              ,highlight = TRUE
              
              
              
              
    )
    
  })
  #### End Campaign Table ####
  
  ####CTA TAB #### 
  output$CTATrends <- renderHighchart({ 
    
    ClickFunction <- JS("function(event) {Shiny.onInputChange('Channel', event.point.name);}")
    
    
    
    
###|MX_Number == "MX2878447"|MX_Number == "MX2878566"  
    #### Trends 01 Activations by Channel ####
    Trends_01 <- filteredData() %>%
      filter(MX_Number == "MX2961854"|MX_Number == "MX2878447"|MX_Number == "MX2878566"|MX_Number == "MX2962696") %>% 
      mutate(Legend = paste(Channel,"Activations")) %>%
      mutate(Channel = case_when(
        Channel == 'SMS' ~ 'SMS',
        Channel == 'Email' ~ 'Email',
        TRUE ~ "U")) %>%
      filter(Channel != "U") %>%
      
      group_by(Job_Created_At,Channel,Legend) %>%
      summarise(Delivered= sum(Delivered),
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
                Activated = sum(MHM_Active),
                Activation_Rate = percent(accuracy = .01,Activated/Engaged))
    
    #### Trends_02  Active Rate ####
    Trends_02 <- filteredData() %>%
    filter(MX_Number == "MX2961854"|MX_Number == "MX2878447"|MX_Number == "MX2878566"|MX_Number == "MX2962696") %>% 
      mutate(Legend = paste(Channel,"Activation Rate")) %>%
      mutate(Channel = case_when(
        Channel == 'SMS' ~ 'SMS',
        Channel == 'Email' ~ 'Email',
        TRUE ~ "U")) %>%
      filter(Channel != "U") %>%
      
      group_by(Job_Created_At,Channel,Legend) %>%
      summarise(Delivered= sum(Delivered),
                Engaged=sum(Opened,Clicked),
                Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
                Activated = sum(MHM_Active)) %>% 
      mutate(Activation_Rate = round(Activated/Delivered*100,2))  
    
    #### Trends 03 Vida Email Active Rate ####
    # Trends_03 <- campaign %>%
    # filter(MX_Number == "MX2878447") %>% 
    #   mutate(Legend = paste(Tactic,Channel,"Activation Rate")) %>%
    #   mutate(Channel = case_when(
    #     Channel == 'SMS' ~ 'SMS',
    #     Channel == 'Email' ~ 'Email',
    #     TRUE ~ "U")) %>%
    #   filter(Channel != "U") %>%
    #   
    #   group_by(Job_Created_At,Legend) %>%
    #   summarise(Delivered= sum(Delivered),
    #             Engaged=sum(Opened,Clicked),
    #             Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
    #             Activated = sum(MHM_Active)) %>% 
    #   mutate(Activation_Rate = round(Activated/Delivered*100,2)) 
    
    ####Trends 04 Mental Health Email Activation####
    # Trends_04 <- campaign %>%
    # filter(MX_Number == "MX2961854") %>% 
    #   mutate(Legend = paste(Tactic,Channel,"Activation Rate")) %>%
    #   mutate(Channel = case_when(
    #     Channel == 'SMS' ~ 'SMS',
    #     Channel == 'Email' ~ 'Email',
    #     TRUE ~ "U")) %>%
    #   filter(Channel != "U") %>%
    #   
    #   group_by(Job_Created_At,Legend) %>%
    #   summarise(Delivered= sum(Delivered),
    #             Engaged=sum(Opened,Clicked),
    #             Engagement_Rate = percent(accuracy = .01,Engaged/Delivered),
    #             Activated = sum(MHM_Active)) %>% 
    #   mutate(Activation_Rate = round(Activated/Delivered*100,2)) 
    
    #### Activations ####
    hc <- highchart() %>%
      hc_exporting(enabled = TRUE) %>% 
      hc_title( margin=0, text = "Activations By Channel", align="center", style = list(color = "#003963", fontWeight = "bold"))  %>%
      hc_xAxis(
        #dateTimeLabelFormats = list(day = '%m  %Y')
        dateTimeLabelFormats = list(day = '%b') #month 
        ,type = "datetime", gridLineWidth= 0
        ,labels = list(style = list(color =  "#003963",fontWeight = "bold", fontSize = "10px"))
        
      ) %>%
      hc_yAxis_multiples(
        list(title = list(text = " ")
             ,labels = list( format = "{value:,.0f}", style = list(fontWeight = "bold", color =  "#003963") ) 
             #,max=100, #tickPixelInterval= 30, 
             ,showFirstLabel = TRUE, showLastLabel=TRUE,  opposite = FALSE),
        list(title = list(text = " ") 
             #,min=min(Members$Rate)  ,max = max(Members$Rate)
             ,labels=list(format = '{value}%' , style = list(fontWeight = "bold", color =  "#003963")) 
             ,showFirstLabel = TRUE ,showLastLabel = TRUE, opposite = TRUE
             #,tickPixelInterval= 30 
        )
      ) %>%
      hc_plotOptions(
        column = list( 
          maxPointWidth=40
          #, stacking = barStacking()
          , dataLabels = list(
            enabled=F
            
            #enabled=slabel()
            #,format= '{y}%' #suffix*
            ,format= '{y}' #suffix*
          )) )  %>%
      hc_legend(enabled= T,text = " ", verticalAlign= "top",align= "center") %>% 
        
      #### Trends HC Axis ####
    hc_add_series(Trends_01,type = "column", 
                  name= unique(Trends_01$Legend),
                  hcaes(x=Job_Created_At, y=Activated,group = Legend)
    ) %>%
      hc_add_series(Trends_02,  type="spline" ,
                    dashStyle = "longdash", lineWidth= 1,
                    zIndex=100,
                    name = unique(Trends_02$Legend),
                    hcaes(x=Job_Created_At,y=Activation_Rate,group = Legend),
                    yAxis=1,
                    visible=T,
                    tooltip = list(valueSuffix= '%')) %>%
      # hc_add_series(Trends_03,  type="spline" ,
      #               dashStyle = "longdash", lineWidth= 1,
      #               zIndex=100,
      #               name = "Vida Email Activation Rate",
      #               hcaes(x=Job_Created_At,y=Activation_Rate),
      #               yAxis=1,
      #               visible=T,
      #               tooltip = list(valueSuffix= '%')) %>%
      # hc_add_series(Trends_04,  type="spline" ,
      #               dashStyle = "longdash", lineWidth= 1,
      #               zIndex=100,
      #               name = "Vida Email Activation Rate",
      #               hcaes(x=Job_Created_At,y=Activation_Rate),
      #               yAxis=1,
      #               visible=T,
      #               tooltip = list(valueSuffix= '%')) %>%
      hc_rangeSelector(
        enabled = T,
        inputEnabled = F,
        verticalAlign = "bottom",
        selected = 2, #2 = 12m
        buttons = list(
          #list(type = 'week', count = 1, text = 'Week'),
          #list(type = 'month',count = 1,  text = '1m'),
          #list(type = 'month',count = 2,  text = '3m'),
          list(type = 'month',count = 5,  text = '6m'),
          list(type = 'month',count = 8,  text = '9m'),
          list(type = 'month',count = 11,  text = '12m'),
          #list(type = 'year', count = 1,  text = '1y'),
          #list(type = 'ytd', text = 'YTD'),
          list(type = 'all', text = 'All')
        )
      ) %>%
      hc_chart(
        style = list(fontFamily = "Poppins" ) 
        ,options3d = list(enabled = T, beta = 2, alpha = 7)
        ,backgroundColor= 'white' #'white'
        ,borderColor= 'transparent'
        # ,borderRadius= 20
        # ,borderWidth= 2
        ,type= 'line'
        ,spacingTop=20
        #,spacingLeft=20
        ,spacingRight=20
        #,displayErrors= TRUE
      ) %>% 
      ####Trends hc colors####
    #hc_colors(c(HM_COLORS[2], HM_COLORS[3],HM_COLORS[1],HM_COLORS[4] ))   %>%
    hc_colors(c(HM_COLORS[2], HM_COLORS[3],"#1b9616", HM_COLORS[1]))   %>%
      hc_legend(enabled= T,text = " ", verticalAlign= "top",align= "center") %>%    
      hc_plotOptions(
        column = list( 
          maxPointWidth=10
          #, stacking = barStacking()
          , dataLabels = list(
            enabled=F
            #enabled=slabel()
            #,format= '{y}%' #suffix*
            ,format= '{y}' #suffix*
          )) 
        ,spline = list(
          marker= list(enabled=TRUE,  symbol="circle") #lineWidth= 1,  fillColor= 'white', lineColor= NULL,
          ,dashStyle = "ShortDashDot", lineWidth= 1
          ,dataLabels = list(
            #enabled=slabel()
            enabled=F
            ,crop=FALSE
            ,overflow= 'none'
            ,inside= FALSE
            ,y= 30 
            ,borderRadius= 4 # roundedges
            ,borderWidth= 1
            ,borderColor= NULL
            , style = list(
              fontFamily="Helvetica"
              ,color = NULL
              ,fontWeight = 600
            )))
      ) %>% 
      hc_xAxis(
        tickInterval   =24 * 3600 * 1000*30, allowDecimals= F, #Show all months
        #dateTimeLabelFormats = list(day = '%m  %Y')
        dateTimeLabelFormats = list(day = '%b') #month 
        #dateTimeLabelFormats = list(month = '%b', day= '%b %e', week= '%b %e') #, category=unique(Pilot$DATE)
        ,type = "datetime"
        , gridLineWidth= 0
        ,labels = list(style = list(color =  "#003963",fontWeight = "bold", fontSize = "10px"))
      ) %>%
      hc_add_theme(hm_theme)
    hc
  })
  
  ####END CTA TAB ####
  
  
  #### Healthmap ####

  # create popups
  # incomepopup <- paste0("County: ", example$name, ", avg income = $", example$Mbrs)
  #poppopup <- paste0("County: ", example$NAME, ", Members = ", example$Members)
  # yearpopup <- paste0("County: ", example$name, ", avg year = ", example$Mbrs)
  # lifepopup <- paste0("County: ", example$name, ", avg life expectancy = ", example$Mbrs)
  
  # create color palettes
  # yearPalette <- colorNumeric(palette = "Blues", domain=example$pop)
  # lifePalette <- colorNumeric(palette = "Purples", domain=example$pop)
  # incomePalette <- colorNumeric(palette = "Reds", domain=example$pop)

  
  ####Create Map####
  output$healthmap <- renderLeaflet({

    usgeo <- st_read("fips/cb_2014_us_county_5m.shp") %>%
      mutate(fips = as.numeric(paste0(STATEFP, COUNTYFP)))
    
    
    # Get data set for population 
    geo_data <- filteredData() %>% 
      filter(County_Fips != "") %>%
      distinct(ECI,County_Fips,.keep_all = TRUE) %>%
      group_by(County_Fips,STATE) %>%
      summarise(Members = sum(Mbrs),

                Vida_a = sum(ifelse(MX_Number == "MX2961854", MHM_Active, 0), na.rm = T),
                Vida_b = sum(ifelse(MX_Number == "MX2878566", MHM_Active, 0), na.rm = T),
                Vida_c = sum(ifelse(MX_Number == "MX2878566", MHM_Active, 0), na.rm = T),
                Vida_d = sum(ifelse(MX_Number == "MX2962696", MHM_Active, 0), na.rm = T)
                ) %>% 
      mutate(Vida_Activations = sum(Vida_a,Vida_b,Vida_c,Vida_d))
 
                
    geo_data <- rename(geo_data, c("fips" = "County_Fips"))
    
    geo_data <- st_as_sf(geo_data %>%
                           left_join(usgeo)) 
    
    geo_data$geometry <- st_zm(geo_data$geometry, drop = T, what = "ZM")
    
    geo_data <- st_transform(geo_data, "+proj=longlat +datum=WGS84")

    ## End population
    
    # get data set for member layer
    geo_mem <- geo_data

    
    
    # get data set for activations layer
    
      # geo_activs <- geo_data %>% 
      #   filter(Vida_Activations >0)
      

    # 
    # geo_activs$geometry <- st_zm(geo_activs$geometry, drop = T, what = "ZM")
    # 
    # geo_activs <- st_transform(geo_activs, "+proj=longlat +datum=WGS84")
    # end activations
      
    #### Pallettes####

    
    poppopup <- paste0("State: ", geo_mem$STATE,", ","County: ", geo_mem$NAME, ", Members = ", comma(geo_mem$Members))

    
   bins_mem = c(0,250,500,1000,2000,3000,5000,10000,20000,30000,40000,50000,60000,100000,200000)
   bins_act = c(0,5,10,20,30,40,50,60,80,100,125,150,250,500,1000)
    
    popPalette <- colorBin(palette = HM_COLS
                           ,bins = bins_mem, na.color = "#808080",domain=geo_mem$Members)
    
    ActPalette <- colorBin(palette = HM_COLS ,bins = bins_act, domain=geo_mem$Vida_Activations)
    Actpopup <- paste0("State: ", geo_mem$STATE,", ","County: ", geo_mem$NAME, ", Vida Activations = ", comma(geo_mem$Vida_Activations))

    
      
      
      
    
    
    leaflet(geo_data) %>%
      addProviderTiles("CartoDB.Positron") %>%
      setView(lng = -78, lat = 41, zoom =7) %>% 
      # Members Layer
      addPolygons(
        color = ~popPalette(geo_mem$Members),
        
        weight = 1.5,
        smoothFactor = .1,
        fillOpacity = .6,
        popup = poppopup,
        label = paste0("State: ", geo_mem$STATE,", ","County: ", geo_mem$NAME, ", Members = ", comma(geo_mem$Members)),
        #color = ~popPalette(example$Mbrs),
        group = "Members",
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE)
      ) %>% 
    
     #Activations Layer
      
      addPolygons(
        color = ~ActPalette(geo_mem$Vida_Activations),
        
        weight = 1.5,
        smoothFactor = .1,
        fillOpacity = .6,
        popup = Actpopup,
        label = paste0("State: ", geo_mem$STATE,", ","County: ", geo_mem$NAME, ", Activations = ", comma(geo_mem$Vida_Activations)),
        #color = ~popPalette(example$Mbrs),
        group = "Vida Activations",
        highlightOptions = highlightOptions(color = "white", weight = 2,
                                            bringToFront = TRUE)
      ) %>%

      
      
      
      #Group Control
      addLayersControl(
        baseGroups=c("Members","Vida Activations"),
        position = "bottomleft",
        options = layersControlOptions(collapsed = FALSE)
      )

    
    
    
  })
  #### END Test HM ####
  
  #### End Server #### 
} 
shinyApp(ui, server)
