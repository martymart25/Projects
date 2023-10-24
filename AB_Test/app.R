if(! require(shiny)) install.packages( "shiny")
if(! require(shinydashboard)) install.packages( "shinydashboard")
if(! require(shinyWidgets)) install.packages( "shinyWidgets")
if(! require(shinythemes)) install.packages( "shinythemes")
if(! require(shinydashboardPlus)) install.packages( "shinydashboardPlus")
if(! require(shinyjs)) install.packages( "shinyjs")
if(! require(htmlwidgets)) install.packages( "htmlwidgets")
if(! require(dplyr)) install.packages( "dplyr")
if(! require(tidyr)) install.packages( "tidyr")
if(! require(ggplot2)) install.packages( "ggplot2")
if(! require(plotly)) install.packages( "plotly")
if(! require(kableExtra)) install.packages( "kableExtra")
if(! require(fontawesome)) install.packages( "fontawesome")
if(! require(lubridate)) install.packages( "lubridate")
if(! require(rapportools)) install.packages( "rapportools")
if(! require(bit64)) install.packages( "bit64")


# Load Clicks Data
load("clicks.R")

# Load Game Activity Data 
load("Game Activity.R")

dates_cl <- clicks_df |>
  mutate(date = as.Date(event_datetime)) |>
  select(date,event_category) |>
  distinct()

dates_g <- game |>
  mutate(date = as.Date(ActivityDate)) |>
  select(date,event_category) |>
  distinct()

dates <- dates_cl |>
  inner_join(dates_g,c("date","event_category"),multiple = "first")

# # Load New Games
# load("new_games.R")

## Rename dataframe
clicks <- clicks_df |>
  mutate(event_id = paste(event_datetime,event_uuid,sep = "-")) |>
  group_by(user_id,event_info_1,event_category,event_location) |>
  reframe(clicks = length(event_id))

activity <- game |>
  mutate(vip_cat = ifelse(tolower(Viplevel) %in% c('gold','platinum','diamond'),'VIP',
                          'Non-VIP')
         ,ActivityDate = as.Date(ActivityDate))



## Convert event_datetime to utc8
clicks1 <- clicks


activity1 <- activity |>
  select(-event_category)



## Create assigning function
ab_func <- function(id) {
  res <- c()
  
  for(i in 1:length(id)) {
    
      if(id[i]%%2 == 0) 
      res[i] <- "Graphyte"
      
      else if(id[i]%%2 > 0) 
      res[i] <-"ICore"
       
      else NULL
    
  }
  
  res

}

## Create outlier detection function

r.zscore <- function(x) coalesce((x-median(x,na.rm=T))/mad(x),0)
## New Games Data


new_games_clicks <- clicks1 |>
  filter(event_category == 'New Games') |>
  group_by(user_id,event_location,event_info_1,event_category) |>
  reframe(clicks = sum(clicks))

new_games_game <- activity1 |>
  group_by(Id,vip_cat,GameId,GameName,Vendor) |>
  summarise_if(is.numeric,sum)

new_games_clicks_agg <- new_games_clicks |>
  filter(clicks > 0) |>
  group_by(user_id,event_info_1,event_category) |>
  summarise_if(is.numeric,sum)


new_games_df <- new_games_clicks_agg |>
  inner_join(activity1,by = c("user_id" = "Id"
                              ,"event_info_1" = "GameId")
             ,multiple = "all") |>
  group_by(user_id,vip_cat,event_category,event_info_1) |>
  reframe(clicks = unique(clicks)
          ,active_days = length(unique(ActivityDate))
          ,Bet_Count = sum(Bet_Count)
          ,Handle_USD = sum(Handle_USD)
          ,GGR_USD = sum(GGR_USD)) |>
  group_by(user_id,vip_cat,event_category) |>
  summarise_if(is.numeric,sum) |>
  select(-event_info_1) |>
  ungroup() |>
  mutate(recommendation = ab_func(user_id))

new_games_df$clicks[is.na(new_games_df$clicks)] <- 0


## Special Selection Data
special_selection_clicks <- clicks1 |>
  filter(event_category == "Special Selection") |>
  group_by(user_id,event_location,event_info_1,event_category) |>
  reframe(clicks = sum(clicks))

special_selection_game <- activity1 |>
  group_by(Id,vip_cat,GameId,GameName,Vendor) |>
  summarise_if(is.numeric,sum)

special_selection_clicks_agg <- special_selection_clicks |>
  filter(clicks > 0) |>
  group_by(user_id,event_info_1,event_category) |>
  summarise_if(is.numeric,sum)


special_selection_df <- special_selection_clicks_agg |>
  inner_join(activity1,by = c("user_id" = "Id"
                                             ,"event_info_1" = "GameId")
             ,multiple = "all") |>
  group_by(user_id,vip_cat,event_category,event_info_1) |>
  reframe(clicks = unique(clicks)
          ,active_days = length(unique(ActivityDate))
          ,Bet_Count = sum(Bet_Count)
          ,Handle_USD = sum(Handle_USD)
          ,GGR_USD = sum(GGR_USD)) |>
  group_by(user_id,vip_cat,event_category) |>
  summarise_if(is.numeric,sum) |>
  select(-event_info_1) |>
  ungroup() |>
  mutate(recommendation = ab_func(user_id))

special_selection_df$clicks[is.na(special_selection_df$clicks)] <- 0

# New Games per Week
new_games_wk <- clicks_df |>
  filter(as.Date(event_datetime) >= week_min_date &
           as.Date(event_datetime) <= week_max_date &
           event_category == "New Games") |>
  group_by(user_id,event_info_1,event_category,event_location,Week) |>
  reframe(week_min_date = as.Date(min(week_min_date))
          ,week_max_date = as.Date(min(week_max_date))
          ,clicks = length(unique(paste(event_datetime,event_uuid,sep = "-"))))

new_games_wksummary <- new_games_wk |>
  group_by(user_id,event_info_1,event_category,Week) |>
  reframe(week_min_date = as.Date(min(week_min_date))
          ,week_max_date = as.Date(min(week_max_date))
          ,clicks = sum(clicks)) |>
  inner_join(activity1, by = c("user_id" = "Id"
                                    ,"event_info_1" = "GameId"),
             multiple = "all") |>
  filter(ActivityDate >= week_min_date &
           ActivityDate <= week_max_date + 1) |>
  group_by(user_id,vip_cat,event_category,event_info_1,Week) |>
  reframe(clicks = min(clicks)
          ,active_days = length(unique(ActivityDate))
          ,Bet_Count = sum(Bet_Count)
          ,Handle_USD = sum(Handle_USD)
          ,GGR_USD = sum(GGR_USD))


new_games_wksummary_event <- new_games_wk |>
  group_by(user_id,event_info_1,event_category,Week,event_location) |>
  reframe(week_min_date = as.Date(min(week_min_date))
          ,week_max_date = as.Date(min(week_max_date))
          ,clicks = sum(clicks)) |>
  inner_join(activity1, by = c("user_id" = "Id"
                               ,"event_info_1" = "GameId"),
             multiple = "all") |>
  filter(ActivityDate >= week_min_date &
           ActivityDate <= week_max_date + 1) 
             

## Create UI
ui <- dashboardPage(
  options = list(sidebarExpandOnHover = TRUE),
  dashboardHeader(title = "",titleWidth = "100vh",
                  tags$li(class = "dropdown", 
                          id = "title",
                          tags$a(tags$img(src = "logo.png",height = 50, width = 70),
                                 "GRAPHYTE A/B TESTING"),
                          tags$style(
                            HTML("
                            #title {
                              font-weight: bold;
                              font-family: Calibri,Sans-serif;
                              font-size: 23px;
                              letter-spacing: 0.3px;
                              color: white;
                              position: fixed;
                              left: 0;
                              transform: translate(-12px,-20%);
                              padding: 0px;
                            }
                            
                             #title > a:hover {
                                  background-color: transparent !important;
                                  color: white !important;
                             } 
                              
                              
                            #sub {
                            text-indent:5px;
                            }
                              ")
                            
                          )
                  )),
  dashboardSidebar(
    collapsed = TRUE,
    minified = TRUE,
    tags$script(JS("document.getElementsByClassName('sidebar-toggle')[0].style.visibility = 'hidden';")),
    sidebarMenu(id = "dashboard" ,
                  menuItem("Special Selection",
                           tabName = "special_selection_ab_test",
                           icon  = icon("trophy")),
                 
                  menuItem("New Games",
                                    icon = icon("gamepad",class = "fa-regular"),
                              menuItem("Overall Significance Test",
                                     tabName = "new_games_overall",
                                     icon = icon("dashboard")),
                              menuItem("Weekly Significance Test",
                                  tabName = "new_games_weekly",
                                  icon = icon("calendar-week")))
                  )

                  ),
  dashboardBody(
    tags$head(
      tags$style(HTML("
                      .skin-blue .main-header .logo {
                      background-color:  #004C87;
                      text-align: left;
                      font-weight: bold;
                      font-family: Calibri,Sans-serif;
                      font-size: 19px;
                      color: white;
                      padding:1px;0
                      position: fixed;
                      }
                    
                      
                      .skin-blue .main-header .logo:hover {
                      background-color:  #004C87;
                      color: white;
                      padding:1px;
                      width: 250px;
                      }
                      
                      .skin-blue .main-header .logo>a {
                      background-color:  #004C87;
                      text-align: left;
                      font-weight: bold;
                      font-family: Calibri,Sans-serif;
                      font-size: 19px;
                      color: white;
                      padding:1px;
                      width: 230px;
                      }
                      
                      .skin-blue .main-header .logo>a:hover {
                      background-color:  #004C87;
                      color: white;
                      padding:1px;
                      width: 230px;
                      }
                      
                      .skin-blue .main-header .navbar {
                      background-color: #004C87;
                      padding:0px;
                      width:100%;
                      float:none;
                      }
                      
                      
                      .skin-blue .main-sidebar{
                      background-color: #383e4d;
                      color: white;
                      font-family: Calibri,Sans-serif;
                      position:fixed;
                      }
                      
                      .skin-blue .main-sidebar .sidebar .sidebar-menu{
                      background-color: #383e4d;
                      color: white;
                      font-family: Calibri,Sans-serif;
                      text-indent: 0px;
                      }
                      
                      .skin-blue .main-sidebar .sidebar .sidebar-menu:hover{
                      background-color: #383e4d;
                      color: white;
                      font-weight: bold;
                      font-family: Calibri,Sans-serif;
                      }
                      
                      
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .active a{
                      background-color: #383e4d;
                      color: #caab72;
                      font-weight: bold;
                      font-family: Calibri,Sans-serif;
                      }
                      
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu a {
                      background-color: #383e4d;
                      color: white;
                      font-weight: lighter;
                      font-family: Calibri,Sans-serif;
                      padding: 5px;
                      text-indent: 5%;
                      word-spacing: 2px;
                      }
                      
                      .skin-blue .main-sidebar .sidebar .sidebar-menu .treeview-menu .active a {
                      background-color: #383e4d;
                      color: #caab72;
                      font-weight: bold;
                      font-family: Calibri,Sans-serif;
                      padding: 5px;
                      }
                      
                      .content-wrapper, .right-side {
                      background-color: #d3d4d7;
                      overflow-x:hidden;
                      }
                      .sidebar-toggle{
                      position: fixed; 
                      left: 0;
                      }
                      
                       .main-header .logo{
                      width: 100% !important;
                      position:fixed;
                       }
                       
                       #page{
                       border-radius: 10px;
                       margin:-10px;
                       height:80vh;
                       display:flex;
                       padding:10px;
                       }
                       
                       #html {
                       height:180px;
                       margin:0px;
                       margin-bottom: 2px;
                       margin-top: 10px;
                       }
                       
                       #box {
                       border-radius: 10px;
                       margin-bottom: 2px;
                       margin-top: 10px;
                       overflow-y:auto;
                       }
                       
                       #box2 {
                       border-radius: 10px;
                       margin-bottom: 2px;
                       margin-top: 10px;
                       overflow:auto;
                       height:360px;
                       }
                       
                       #filterbox .box-header{
                        display:none;
                       }
                       
                       #filterbox {
                       border-radius: 10px;
                       margin-top: 10px;
                       margin-bottom: 2px;
                       }
                       
                       #desc {
                       border-radius: 10px;
                       margin-bottom: 2px;
                       margin-top: 10px;
                       height:100%;
                       overflow:auto;
                       }
                       
                     .title {
                      font-size:24px;
                      font-weight:bold;
                      color:#caab72;
                      text-align:center;
                      padding-top: 0px;
                      margin-top: -15px;
                      margin-bottom: -15px;
                      background-color:#195d93;
                      width:100vw;
                      margin-left:-15px;
                      overflow:auto;
                      border-style: ridge;
                      }
                       
                       #box1 {
                       height:700px;
                       overflow-y:auto;
                       border-radius: 10px;
                       margin-bottom: 2px;
                       margin-top: 10px;
                       }
                       
                       .subtitle{
                       color: #FFFAF0;
                       margin-top: -20px;
                       font-size:13px;
                       font-weight:bold;
                       text-align:left;
                       margin-left: -10px;
                       overflow:auto;
                       }
                       

                      "))
    ),
    
    tabItems(
      tabItem(tabName = "special_selection_ab_test",
              div(class = "title","Special Selection Games Significance Test"),
              div(class = "subtitle",paste("Data runs from"
                                           ,format(as.Date(min(as.Date(dates$date[dates$event_category== 'Special Selection']))),"%B %d, %Y")
                                           ,"-"
                                           ,format(as.Date(max(as.Date(dates$date[dates$event_category== 'Special Selection']))),"%B %d, %Y"))),
             
              fillPage(title = "special selection",
                div(id = "page",
                fillRow(
                        width = "100%",
                        height = "100vh",
                        flex = c(3,2),
                  column(width = 12,
                         div(id = 'column',
                          fluidRow(
                           box(id = "box",
                         htmlOutput("ss_test"),
                           width = 12,
                           status = "primary",
                           solidHeader = TRUE,
                           title = HTML("<b>RESULTS!</b>")
                       )),
                         fluidRow(
                           box(id = "box",title = HTML("<b>GRAPHYTE</b>"),
                             tableOutput("ss_graphytable"),
                             align = "left",
                             width = 12,
                             height = "100vh")
                           ),

                         fluidRow(box(id = "box",title = HTML("<b>ICORE</b>"),
                             tableOutput("ss_icoretable"),
                             align = "left",
                             width = 12,
                             height = "100vh"
                         )),
                         fluidRow(
                           box(id = "box",
                           plotlyOutput("ss_bar",width = "100%",inline = TRUE,height = "400px"),
                           width = 12,
                           height = "100vh"
                         )),
                         fluidRow(style = "diplay:flex;",
                                         box(id = "desc",
                                           title = HTML("<b>HYPOTHESIS</b>"),
                                           solidHeader = TRUE,
                                           column(12,div(id ="html",htmlOutput("ss_hypothesis", fill = TRUE))),
                                           align = "left",
                                           status = "primary",
                                           width = 6),
                                           
                                         box(id = "desc",
                                           title = HTML("<b>RECOMMENDATION</b>"),
                                           solidHeader = TRUE,
                                           column(12,div(id ="html",htmlOutput("ss_recommendation", fill = TRUE))),
                                           align = "left",
                                           status = "primary",
                                           width = 6)
                                  ),
                       fluidRow(br()))),
                            column(
                                width = 12,
                                div(id = "column",
                                fluidRow(width = "100%",
                                        height = "100vh",
                                        style = "display:flex;",
                                        
                                        box(id = "filterbox",width = 12,height = "15vh",
                                                            status = "info",
                                            column(6,pickerInput("columns",
                                                    "INFERENCE FOR", 
                                                  choices= c('TURNOVER USD','DAILY AVERAGE TURNOVER USD','ACTIVE DAYS','CLICKS'),
                                                    selected = "CLICKS",
                                                    options = list(`actions-box` = FALSE),multiple = FALSE)),
                                            column(6,
                                                     awesomeRadio("outlier",
                                                     "Exclude Outliers",
                                                     choices = c("Yes","No"),
                                                     selected = "Yes"))))
                          ,
                          fluidRow(
                            box(id = "box2",
                             title = HTML("<b>UAP per Event Location</b>"),
                             tableOutput("ss_event_table"),
                             width = 12,
                             height = "100vh"
                             )),
                           fluidRow(
                           box(id = "box1",
                             title = HTML("<b>UAP per Game</b>"),
                             div(style = "overflow-y:auto;", tableOutput("ss_game_table")),
                             width = 12,
                             height = "100px"
                           ))
                           
                  
                )))
                
              )
              ) 
      ),
      tabItem(tabName = "new_games","New Games"),
      tabItem(tabName = "new_games_overall",
              div(class = "title", "Overall New Games Significance Test"),
              div(class = "subtitle",paste("Data runs from",format(as.Date(min(as.Date(dates$date[dates$event_category== 'New Games']))),"%B %d, %Y")
                                           ,"-"
                                           ,format(as.Date(max(as.Date(dates$date[dates$event_category== 'New Games']))),"%B %d, %Y"))),
              
              fillPage(title = "New Games",
                       div(id = "page",
                           fillRow(
                             width = "100%",
                             height = "100vh",
                             flex = c(3,2),
                             column(width = 12,
                                    div(id = 'column',
                                        fluidRow(
                                          box(id = "box",
                                              htmlOutput("ng_test"),
                                              width = 12,
                                              status = "primary",
                                              solidHeader = TRUE,
                                              title = HTML("<b>RESULTS!</b>")
                                          )),
                                        fluidRow(
                                          box(id = "box",title = HTML("<b>GRAPHYTE</b>"),
                                              tableOutput("ng_graphytable"),
                                              align = "left",
                                              width = 12,
                                              height = "100vh")
                                        ),
                                        
                                        fluidRow(box(id = "box",title = HTML("<b>ICORE</b>"),
                                                     tableOutput("ng_icoretable"),
                                                     align = "left",
                                                     width = 12,
                                                     height = "100vh"
                                        )),
                                        fluidRow(
                                          box(id = "box",
                                              plotlyOutput("ng_bar",width = "100%",inline = TRUE,height = "400px"),
                                              width = 12,
                                              height = "100vh"
                                          )),
                                        fluidRow(style = "diplay:flex;",
                                                 box(id = "desc",
                                                     title = HTML("<b>HYPOTHESIS</b>"),
                                                     solidHeader = TRUE,
                                                     column(12,div(id ="html",htmlOutput("ng_hypothesis", fill = TRUE))),
                                                     align = "left",
                                                     status = "primary",
                                                     width = 6),
                                                 
                                                 box(id = "desc",
                                                     title = HTML("<b>RECOMMENDATION</b>"),
                                                     solidHeader = TRUE,
                                                     column(12,div(id ="html",htmlOutput("ng_recommendation", fill = TRUE))),
                                                     align = "left",
                                                     status = "primary",
                                                     width = 6)
                                        ),
                                        fluidRow(br()))),
                             column(
                               width = 12,
                               div(id = "column",
                                   fluidRow(width = "100%",
                                            height = "100vh",
                                            style = "display:flex;",
                                            
                                            box(id = "filterbox",width = 12,height = "15vh",
                                                status = "info",
                                                column(6,pickerInput("columns1",
                                                                     "INFERENCE FOR", 
                                                                     choices= c('TURNOVER USD','DAILY AVERAGE TURNOVER USD','ACTIVE DAYS','CLICKS'),
                                                                     selected = "CLICKS",
                                                                     options = list(`actions-box` = FALSE),multiple = FALSE)),
                                                column(6,
                                                       awesomeRadio("outlier1",
                                                                    "Exclude Outliers",
                                                                    choices = c("Yes","No"),
                                                                    selected = "Yes"))))
                                   ,
                                   fluidRow(
                                     box(id = "box2",
                                         title = HTML("<b>UAP per Event Location</b>"),
                                         tableOutput("ng_event_table"),
                                         width = 12,
                                         height = "100vh"
                                     )),
                                   fluidRow(
                                     box(id = "box1",
                                         title = HTML("<b>UAP per Game</b>"),
                                         div(style = "overflow-y:auto;",tableOutput("ng_game_table")),
                                         width = 12,
                                         height = "100px"
                                     ))
                                   
                                   
                               )))
                           
                       )
              ) )
      ,tabItem(tabName = "new_games_weekly",
               
               div(class = "title", "Weekly New Games Significance Test"),
               div(class = "subtitle",paste("Data runs from",format(as.Date(min(as.Date(dates$date[dates$event_category== 'New Games']))),"%B %d, %Y")
                                            ,"-"
                                            ,format(as.Date(max(as.Date(dates$date[dates$event_category== 'New Games']))),"%B %d, %Y"))),
               
               fillPage(title = "Weekly New Games",
                        div(id = "page",
                            fillRow(
                              width = "100%",
                              height = "100vh",
                              flex = c(3,2),
                              column(width = 12,
                                     div(id = 'column',
                                         fluidRow(
                                           box(id = "box",
                                               htmlOutput("ng_wk_test"),
                                               width = 12,
                                               status = "primary",
                                               solidHeader = TRUE,
                                               title = HTML("<b>RESULTS!</b>")
                                           )),
                                         fluidRow(
                                           box(id = "box",title = HTML("<b>GRAPHYTE</b>"),
                                               tableOutput("ng_wk_graphytable"),
                                               align = "left",
                                               width = 12,
                                               height = "100vh")
                                         ),
                                         
                                         fluidRow(box(id = "box",title = HTML("<b>ICORE</b>"),
                                                      tableOutput("ng_wk_icoretable"),
                                                      align = "left",
                                                      width = 12,
                                                      height = "100vh"
                                         )),
                                         fluidRow(
                                           box(id = "box",
                                               plotlyOutput("ng_wk_bar",width = "100%",inline = TRUE,height = "400px"),
                                               width = 12,
                                               height = "100vh"
                                           )),
                                         fluidRow(style = "diplay:flex;",
                                                  box(id = "desc",
                                                      title = HTML("<b>HYPOTHESIS</b>"),
                                                      solidHeader = TRUE,
                                                      column(12,div(id ="html",htmlOutput("ng_wk_hypothesis", fill = TRUE))),
                                                      align = "left",
                                                      status = "primary",
                                                      width = 6),
                                                  
                                                  box(id = "desc",
                                                      title = HTML("<b>RECOMMENDATION</b>"),
                                                      solidHeader = TRUE,
                                                      column(12,div(id ="html",htmlOutput("ng_wk_recommendation", fill = TRUE))),
                                                      align = "left",
                                                      status = "primary",
                                                      width = 6)
                                         ),
                                         fluidRow(br()))),
                              column(
                                width = 12,
                                div(id = "column",
                                    fixedRow(width = "100%",
                                             height = "100vh",
                                             style = "display:flex;",
                                             
                                             box(id = "filterbox",width = 12,height = "15vh",
                                                 status = "info",
                                                 column(4,pickerInput("week",
                                                                      "WEEK", 
                                                                      choices= paste('Week',sort(unique(new_games_wksummary$Week))),
                                                                      selected = paste('Week',max(unique(toupper(new_games_wksummary$Week)))),
                                                                      options = list(`actions-box` = FALSE),multiple = FALSE)),
                                                 column(4,pickerInput("columns2",
                                                                      "INFERENCE FOR", 
                                                                      choices= c('TURNOVER USD','DAILY AVERAGE TURNOVER USD','ACTIVE DAYS','CLICKS'),
                                                                      selected = "CLICKS",
                                                                      options = list(`actions-box` = FALSE),multiple = FALSE)),
                                                 column(4,
                                                        awesomeRadio("outlier2",
                                                                     "Exclude Outliers",
                                                                     choices = c("Yes","No"),
                                                                     selected = "Yes"))))
                                    ,
                                    fluidRow(
                                      box(id = "box2",
                                          title = HTML("<b>UAP per Event Location</b>"),
                                          tableOutput("ng_wk_event_table"),
                                          width = 12,
                                          height = "100vh"
                                      )),
                                    fluidRow(
                                      box(id = "box1",
                                          title = HTML("<b>UAP per Game</b>"),
                                          div(style = "overflow-y:auto;",tableOutput("ng_wk_game_table")),
                                          width = 12,
                                          height = "100px"
                                      ))
                                    
                                    
                                )))
                            
                        )
               ))
  )
  ),
  title = "Graphyte A/B Testing"
)


## Create Server
server <- shinyServer(
  function(input,output,session){

    special_selection_data <- reactive({
      
      df <- special_selection_df |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe("turnover usd" = sum(Handle_USD),
                'active days' = sum(active_days),
                `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
                clicks = sum(clicks),
                `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    special_selection_data_vip <- reactive({
      
      df <- special_selection_df |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe("turnover usd" = sum(Handle_USD),
                'active days' = sum(active_days),
                `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
                clicks = sum(clicks),
                `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    
    output$ss_event_table <- renderTable({
      
      summary1 <- special_selection_clicks |>
        inner_join(special_selection_game, c("user_id" = "Id" ,"event_info_1" = "GameId"), multiple = "all") |>
        mutate(recommendation = ab_func(user_id))
      
      
      summary1 |>
        mutate(event_location = tocamel(tolower(event_location),upper = TRUE,sep = " ")) |>
        group_by(event_location,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = "0") |>
        rename("Event Location" = event_location) 
      
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh",
    spacing = "s")
    
    output$ss_game_table <- renderTable({
      
      summary1 <- special_selection_clicks |>
        inner_join(special_selection_game, c("user_id" = "Id" ,"event_info_1" = "GameId"), multiple = "all") |>
        mutate(recommendation = ab_func(user_id))
      
      summary1 |>
        group_by(GameName,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = '0') |>
        rename("Game"= GameName)
      
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100px",
    spacing = "s")
    
    output$ss_hypothesis <- renderText({
      
      
      
      paste("<h4><b>Null Hypothesis:</b></h4> There is no significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.
          \n<h4><b>Alternative Hypothesis:</b></h4> There is a significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.")
      
      
    })
    
    
    output$ss_recommendation <- renderText({
      paste("<h4><b>Graphyte:</b></h4> 
           <p>User that clicks and wagered on special selection tab that has even User ID.</p>
           \n<h4><b>ICore:</b></h4> <p>User that clicks and wagered on special selection tab that has odd User ID.</p>")
    })
  
    
    
    output$ss_graphytable <- renderTable({
      
      dat <- special_selection_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      dat_vip <- special_selection_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      if (tolower(input$columns) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      graphy <- summary |>
        filter(RECOMMENDATION == "Graphyte") |>
        select(-RECOMMENDATION)
      
      graphy
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    output$ss_icoretable <- renderTable({
      
      dat <- special_selection_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      dat_vip <- special_selection_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      
      
      if (tolower(input$columns) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      icore <- summary |>
        filter(RECOMMENDATION == "ICore")|>
        select(-RECOMMENDATION)
      icore
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    
    output$ss_bar <- renderPlotly({
      
      data <- special_selection_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      dat <- data |>
        group_by(recommendation) |>
        reframe(total = sum(Values),
                average = mean(Values))
      
      plot <- ggplot(dat,
                     aes(x = recommendation,
                         y = average,
                         text = paste("Recommendation:",recommendation,
                                      paste0("\nTOTAL ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(total))),
                                      paste0("\nAVERAGE ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(average)))
                                      
                         )
                     )) +
        geom_bar(stat="identity", fill = "steelblue",color = "steelblue") + 
        geom_text(aes(label = paste0(ifelse(tolower(input$columns) == "turnover usd" |
                                              tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(average,accuracy = 0.01))),
                  position = position_stack(vjust = .50)) +
        labs(
          title = paste("Average", gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          y = paste("Average",gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          x = "Recommendation"
          
        ) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = .50, face = "bold"),
              panel.border = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
      
      ggplotly(plot,tooltip = "text") |>
        config(displayModeBar = FALSE)

      
    })
    
    
    output$ss_test <- renderText({
      
      dat <- special_selection_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      summary <- dat |>
        group_by(recommendation)|>
        reframe(mean = mean(Values))
      
      tryCatch(
        expr = {
          t.test <- t.test(Values~recommendation,dat,alternative = "greater", paired = FALSE, conf.level = 0.95)
          if (round(t.test$p.value,2) <= 0.05) {
            paste("There is a high probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b>in Graphyte vs. <b>"
                  ,paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b> in ICore is <b>significantly higher</b>.")
          } else if (round(t.test$p.value,2) > 0.05) {
            paste("There is a low probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                  ,paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
          } else {NULL}       
        }
        ,error = function(e) paste("<b>Value is essentially constant</b>. Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                                   ,paste0(ifelse(tolower(input$columns) == "turnover usd" | tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
      )     
      
      
      
       })
    
## New Games AB Test Overall
    
    new_games_data <- reactive({
      
      df <- new_games_df |>
        group_by(user_id,recommendation) |>
        reframe("turnover usd" = sum(Handle_USD),
              'active days' = sum(active_days),
              `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
               clicks = sum(clicks),
              `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns1))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier1 == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    new_games_data_vip <- reactive({
      
      df <- new_games_df |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe("turnover usd" = sum(Handle_USD),
                'active days' = sum(active_days),
                `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
                clicks = sum(clicks),
                `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns1))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier1 == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    
    
    output$ng_event_table <- renderTable({
      
      summary1 <- new_games_clicks |>
        inner_join(new_games_game, c("user_id" = "Id" ,"event_info_1" = "GameId"), multiple = "all") |>
        mutate(recommendation = ab_func(user_id))
      
      
      summary1 |>
        mutate(event_location = tocamel(tolower(event_location),upper = TRUE,sep = " ")) |>
        group_by(event_location,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = '0') |>
        rename("Event Location" = event_location) 
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh",
    spacing = "s")
    
    output$ng_game_table <- renderTable({
      
      summary1 <- new_games_clicks |>
        inner_join(new_games_game, c("user_id" = "Id" ,"event_info_1" = "GameId"), multiple = "all") |>
        mutate(recommendation = ab_func(user_id))
      
      
      summary1 |>
        group_by(GameName,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = '0') |>
        rename("Game"= GameName)
      
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100px",
    spacing = "s")
    
    output$ng_hypothesis <- renderText({
      
      
      
      paste("<h4><b>Null Hypothesis:</b></h4> There is no significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.
          \n<h4><b>Alternative Hypothesis:</b></h4> There is a significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.")
      
      
    })
    
    
    output$ng_recommendation <- renderText({
      paste("<h4><b>Graphyte:</b></h4> 
           <p>User that clicks and wagered on special selection tab that has even User ID.</p>
           \n<h4><b>ICore:</b></h4> <p>User that clicks and wagered on special selection tab that has odd User ID.</p>")
    })
  
    
    
    output$ng_graphytable <- renderTable({
      
      dat <- new_games_data() |>
          group_by(user_id,recommendation) |>
          reframe(Values = sum(Values),
                  `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
          ungroup()
      
      dat_vip <- new_games_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
    
      if (tolower(input$columns1) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns1) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns1) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns1) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      graphy <- summary |>
        filter(RECOMMENDATION == "Graphyte") |>
        select(-RECOMMENDATION)
      
      graphy
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    output$ng_icoretable <- renderTable({
      
      dat <- new_games_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      dat_vip <- new_games_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      
      if (tolower(input$columns1) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns1) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns1) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns1) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      icore <- summary |>
        filter(RECOMMENDATION == "ICore")|>
        select(-RECOMMENDATION)
      icore
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    
    output$ng_bar <- renderPlotly({
      
      data <- new_games_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      dat <- data |>
        group_by(recommendation) |>
        reframe(total = sum(Values),
                average = mean(Values))
      
      plot <- ggplot(dat,
                     aes(x = recommendation,
                         y = average,
                         text = paste("Recommendation:",recommendation,
                                      paste0("\nTOTAL ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(total))),
                                      paste0("\nAVERAGE ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(average)))
                                      
                         )
                     )) +
        geom_bar(stat="identity", fill = "steelblue",color = "steelblue") + 
        geom_text(aes(label = paste0(ifelse(tolower(input$columns) == "turnover usd" |
                                              tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(average,accuracy = 0.01))),
                  position = position_stack(vjust = .50)) +
        labs(
          title = paste("Average", gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          y = paste("Average",gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          x = "Recommendation"
          
        ) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = .50, face = "bold"),
              panel.border = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
      
      ggplotly(plot,tooltip = "text") |>
        config(displayModeBar = FALSE)
      
      
      
    })
    
    output$ng_test <- renderText({
      
      dat <- new_games_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      summary <- dat |>
        group_by(recommendation)|>
        reframe(mean = mean(Values))
      
      
      tryCatch(
        expr = {
          t.test <- t.test(Values~recommendation,dat,alternative = "greater", paired = FALSE, conf.level = 0.95)
          if (round(t.test$p.value,2) <= 0.05) {
            paste("There is a high probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns1),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b>in Graphyte vs. <b>"
                  ,paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b> in ICore is <b>significantly higher</b>.")
          } else if (round(t.test$p.value,2) > 0.05) {
            paste("There is a low probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns1),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                  ,paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
          } else {NULL}       
        }
        ,error = function(e) paste("<b>Value is essentially constant</b>. Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                                   ,paste0(ifelse(tolower(input$columns1) == "turnover usd" | tolower(input$columns1) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns1),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
      )     
      
    })  
  
## New Games AB Test Weekly
    
    new_games_weekly_data <- reactive({
      
      df <- new_games_wksummary |>
        filter(paste("Week",Week) == input$week ) |>
        group_by(user_id) |>
        reframe("turnover usd" = sum(Handle_USD),
                'active days' = sum(active_days),
                `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
                clicks = sum(clicks),
                `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        mutate(recommendation = ab_func(user_id)) |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns2))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier2 == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    new_games_weekly_data_vip <- reactive({
      
      df <- new_games_wksummary |>
        filter(paste("Week",Week) == input$week ) |>
        group_by(user_id,vip_cat) |>
        reframe("turnover usd" = sum(Handle_USD),
                'active days' = sum(active_days),
                `daily average turnover usd` = sum(Handle_USD)/sum(active_days),
                clicks = sum(clicks),
                `ggr usd` = sum(GGR_USD,na.rm= TRUE)) |>
        ungroup() |>
        mutate(recommendation = ab_func(user_id)) |>
        pivot_longer(cols =  c(`turnover usd`,`daily average turnover usd`,`active days`,`clicks`), 
                     names_to = "Metrics",
                     values_to = "Values") |>
        filter(Metrics == tolower(input$columns2))  |>
        group_by(recommendation) |>
        mutate(outlier = ifelse(abs(r.zscore(Values)) > 10, "Yes","No"))
      
      
      if(input$outlier2 == "Yes"){
        
        df |> filter(outlier == "No")
        
        
      } else {df}
      
    })
    
    
    
    output$ng_wk_event_table <- renderTable({
      
      summary1 <- new_games_wksummary_event |>
        filter(paste("Week",Week) == input$week ) |>
        mutate(recommendation = ab_func(user_id))
      
      
      summary1 |>
        mutate(event_location = tocamel(tolower(event_location),upper = TRUE,sep = " ")) |>
        group_by(event_location,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = '0') |>
        rename("Event Location" = event_location) 
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh",
    spacing = "s")
    
    output$ng_wk_game_table <- renderTable({
      
      summary1 <- new_games_wksummary_event |>
        filter(paste("Week",Week) == input$week ) |>
        mutate(recommendation = ab_func(user_id))
      
      
      summary1 |>
        group_by(GameName,recommendation) |>
        reframe(UAP = coalesce(length(unique(user_id)))) |>
        ungroup() |>
        arrange(recommendation,desc(UAP)) |>
        mutate(UAP = scales::comma(UAP)) |>
        pivot_wider(names_from = recommendation,
                    values_from = UAP,
                    values_fill = '0') |>
        rename("Game"= GameName)
      
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100px",
    spacing = "s")
    
    output$ng_wk_hypothesis <- renderText({
      
      
      
      paste("<h4><b>Null Hypothesis:</b></h4> There is no significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.
          \n<h4><b>Alternative Hypothesis:</b></h4> There is a significant difference in ",tolower(input$columns), " of graphyte recommended players compared to icore recommended players.")
      
      
    })
    
    
    output$ng_wk_recommendation <- renderText({
      paste("<h4><b>Graphyte:</b></h4> 
           <p>User that clicks and wagered on special selection tab that has even User ID.</p>
           \n<h4><b>ICore:</b></h4> <p>User that clicks and wagered on special selection tab that has odd User ID.</p>")
    })
    
    
    
    output$ng_wk_graphytable <- renderTable({
      
      dat <- new_games_weekly_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      dat_vip <- new_games_weekly_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      if (tolower(input$columns2) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns2) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns2) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns2) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      graphy <- summary |>
        filter(RECOMMENDATION == "Graphyte") |>
        select(-RECOMMENDATION)
      
      graphy
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    output$ng_wk_icoretable <- renderTable({
      
      dat <- new_games_weekly_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      dat_vip <- new_games_weekly_data_vip() |>
        group_by(user_id,recommendation,vip_cat) |>
        reframe(Values = sum(Values),
                `ggr usd` = sum(`ggr usd`,na.rm = TRUE)) |>
        ungroup()
      
      
      if (tolower(input$columns2) == "clicks") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "UAP %" = scales::percent(length(unique(user_id))/length(unique(dat$user_id))),
                  CLICKS = scales::comma(sum(Values)),
                  "CLICKS %" = scales::percent(sum(Values)/sum(dat$Values)),
                  "AVERAGE CLICKS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN CLICKS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
      } else if (tolower(input$columns2) == "active days") {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "ACTIVE DAYS" = scales::comma(sum(Values)),
                  "AVERAGE ACTIVE DAYS" = scales::comma(mean(Values),accuracy = 0.01),
                  "MEDIAN ACTIVE DAYS" = scales::comma(median(Values))) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
        
      } else if (tolower(input$columns2) == "turnover usd") {
        
        summary <- dat_vip |>
          group_by(recommendation,vip_cat) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() 
        
        overall <- dat |>
          group_by(recommendation) |>
          reframe(vip_cat = "Overall",
                  UAP = scales::comma(length(unique(user_id))),
                  "TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "GGR USD" = scales::dollar(sum(`ggr usd`),accuracy = 0.01),
                  "AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup()
        
        summary <- rbind(summary,overall) |>
          rename(RECOMMENDATION = recommendation,
                 "VIP CATEGORY" = vip_cat)
        
        
        
      } else if (tolower(input$columns2) == 'daily average turnover usd') {
        
        summary <- dat |>
          group_by(recommendation) |>
          reframe(UAP = scales::comma(length(unique(user_id))),
                  "TOTAL DAILY AVERAGE TURNOVER USD" = scales::dollar(sum(Values),accuracy = 0.01),
                  "AVERAGE  DAILY AVERAGE TURNOVER USD" = scales::dollar(mean(Values),accuracy = 0.01),
                  "MEDIAN  DAILY AVERAGE TURNOVER USD" = scales::dollar(median(Values),accuracy = 0.01)) |>
          ungroup() |>
          rename(RECOMMENDATION = recommendation)
        
        
      } else {summary <- NA}
      
      icore <- summary |>
        filter(RECOMMENDATION == "ICore")|>
        select(-RECOMMENDATION)
      icore
      
    },
    striped = TRUE,
    hover = TRUE,
    bordered = TRUE,
    width = "100vw",
    align = "c",
    height = "100vh")
    
    
    
    output$ng_wk_bar <- renderPlotly({
      
      data <- new_games_weekly_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      dat <- data |>
        group_by(recommendation) |>
        reframe(total = sum(Values),
                average = mean(Values))
      
      plot <- ggplot(dat,
                     aes(x = recommendation,
                         y = average,
                         text = paste("Recommendation:",recommendation,
                                      paste0("\nTOTAL ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(total))),
                                      paste0("\nAVERAGE ",input$columns,": ",paste0(ifelse(tolower(input$columns) == "turnover usd","$",""),scales::comma(average)))
                                      
                         )
                     )) +
        geom_bar(stat="identity", fill = "steelblue",color = "steelblue") + 
        geom_text(aes(label = paste0(ifelse(tolower(input$columns) == "turnover usd" |
                                              tolower(input$columns) == "daily average turnover usd" ,"$",""),scales::comma(average,accuracy = 0.01))),
                  position = position_stack(vjust = .50)) +
        labs(
          title = paste("Average", gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          y = paste("Average",gsub("Usd","USD",tocamel(tolower(input$columns),upper = TRUE,sep = " ")),"by Recommendation"),
          x = "Recommendation"
          
        ) +
        theme_bw() +
        theme(panel.background = element_blank(),
              panel.grid = element_blank(),
              plot.title = element_text(hjust = .50, face = "bold"),
              panel.border = element_blank(),
              axis.ticks.y = element_blank(),
              axis.text.y = element_blank())
      
      ggplotly(plot,tooltip = "text") |>
        config(displayModeBar = FALSE)
      
      
      
    })
    
    output$ng_wk_test <- renderText({
      
      dat <- new_games_weekly_data() |>
        group_by(user_id,recommendation) |>
        reframe(Values = sum(Values)) |>
        ungroup()
      
      summary <- dat |>
        group_by(recommendation)|>
        reframe(mean = mean(Values))
      
      
      tryCatch(
        expr = {
          t.test <- t.test(Values~recommendation,dat,alternative = "greater", paired = FALSE, conf.level = 0.95)
          if (round(t.test$p.value,2) <= 0.05) {
            paste("There is a high probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns2),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b>in Graphyte vs. <b>"
                  ,paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b> in ICore is <b>significantly higher</b>.")
          } else if (round(t.test$p.value,2) > 0.05) {
            paste("There is a low probability<b>",paste0("<b>(",scales::percent(1-t.test$p.val),")</b>"),"</b>that Graphyte and ICore have significant difference in",paste0(tocamel(tolower(input$columns2),upper = TRUE,sep = " "),".")
                  ,"Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                  ,paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
          } else {NULL}       
        }
        ,error = function(e) paste("<b>Value is essentially constant</b>. Therefore, the difference between average of<b>",paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "Graphyte"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b>in Graphyte vs.<b>"
                                   ,paste0(ifelse(tolower(input$columns2) == "turnover usd" | tolower(input$columns2) == "daily average turnover usd" ,"$",""),scales::comma(summary$mean[summary$recommendation == "ICore"],accuracy = 0.01)),tocamel(tolower(input$columns2),upper = TRUE,sep = " "),"</b>in ICore is <b>not significant</b>.")
      )     
      
    })  
    
  

})
    

## RUN APP
shinyApp(ui = ui,server = server)


