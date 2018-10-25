library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(rsconnect)

advertiser = sort(unique(df$advertiser_name))
source_channel = unique(df$channel_name)

# Define UI for application that draws a histogram
  ui <- dashboardPage( skin = 'black',
                       dashboardHeader(title = 'Vice Media Advertising Analysis',
                                       titleWidth = 200),
  
  # Sidebar Content
  dashboardSidebar(
    width = 200,
    sidebarMenu(
      menuItem("Welcome", tabName = "welcome", icon = icon("home")),
      menuItem("Revenue", tabName = "revenue", icon = icon("compass")),
      menuItem("Ad Rank", tabName = "ad_rank", icon = icon("compass")),
      menuItem("Cost per Conversion", tabName = 'cpc', icon = icon("compass")),
      menuItem("Analysis", tabName= "conclusion", icon= icon("certificate")))
  ),
    
  dashboardBody(
    tabItems(
#### Welcome Tab ####
      tabItem(tabName = "welcome",
              h1("Introduction to Analysis"),
              br(),
              fluidRow(
                column(width = 8,
                       div(class = 'my-class',
                           p("I started this project off by merging all the dataframes together. I used a jupyter notebook to perform this merge.
                              I used the merge function in pandas to join these tables together, specifically, I used an 
                             inner join for advertisers and source_channels. I joined advertisers to 
                             advertising_performance, then joined source_channels to the new merged dataframe to get the proper dataframe. "
                             ), # close p tag
                           h4('Outliers'),
                           p('Once I got my dataframe in working order, time began to analyze. I saw the dataset had two outliers. I removed both these data points from our dataset.
                             The outliers were Cloverprises, which had an ad spend of $1326.44 for SEO/Organic channel, and Primpoly, which had an ad spend of $810.74 for Direct channel.'
                             ), # close p tag
                           h4('Shiny'),
                           p("Majority of my analysis was done in a jupyter notebook that I've included in the git hub. 
                             I used a jupyter notebook because it allows me to investigate the data, enabling me to visualize my results.
                             Once I was comfortable with the information, I saved the dataset and imported the new csv file into R to create a dashboard with shiny."
                             ), # close p tag
                           p('The Sidebar on the left has my analysis.'), # close p tag
                           h2('Goal'),
                           p("The goal of this analysis is to improve advertising performance, by looking at ad rank and improving the cost per conversion for advertisers."), # close p tag
                           h2("What You Will See"),
                           p("My goal is to find a way to decrease the cost per conversion for our advertisers, but at the same time increase their ad spend.
                            I will use website ad rank to show how I will achieve my gaol."),# close p tag
                           h3('Assumptions'),
                           p('Assumption 1: Ad rank 1 is the best, ad rank 10 is the worst, and to move up a rank, advertisers need to pay Vice more.'), # close p tag
                           p('Assumption 2: Cost per conversion remains constant.'), # close p tag
                           h2('Skip to Analysis'),
                           p("If you're interested in the final result, go to the Analysis tab.")
                           ) # close div tag
                  ) # close column tag
              ) # close fuildRow
              ), # close tabItem

#### Revenue ####
      tabItem(tabName = 'revenue',
              h1("Revenue Generation"),
              fluidRow(
                column(width = 12,
                       wellPanel(
                         checkboxGroupInput('sourceChannel', label =h3("Select Channels:"), source_channel, source_channel, inline =TRUE) # close select input
                         ) #close wellPanel
                       ) # close column
                ), # close fluidRow
              fluidRow(
                h3('General Information about Channels'),
                column(width = 12,
                       wellPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Revenue", plotlyOutput('channelRevenue'),
                                              p('This is the sum of revenue that Vice Media makes from each channel source.')),
                                     tabPanel("Impressions", plotlyOutput("channelImpressions"),
                                              p('This is the sum of impressions that Vice Media generates from each channel source.')),
                                     tabPanel("Clicks", plotlyOutput('channelClicks'),
                                              p('This is the sum of clicks that Vice Media generates from each channel source.')),
                                     tabPanel("Conversions", plotlyOutput("channelConversions"),
                                              p('This is the number of conversions Vice Media generates from each channel source.')),
                                     tabPanel("Click Through Rate", plotlyOutput("ctr"),
                                              p("This is the click through rate for each channel source.")),
                                     tabPanel("Click to Conversion", plotlyOutput('clickConversion'),
                                              p('This is the click to conversion rate for each channel source.')),
                                     tabPanel("Cost per Conversion", plotlyOutput("costConversion"),
                                              p("I'm going to be referring to cost per conversion, aka CPC, throughout this presentation. 
                                                Note that the lower the cost per conversion is, the better it is for the advertiser because they are spending less on conversions."))
                                     ) # close tabsetPanel
                         ) # close wellPanel 
                       ) # close column
                ), # close fluidRow
                fluidRow(
                  column(width = 12,
                         wellPanel(
                           checkboxGroupInput("advertiserName", 
                                              label = h3('Select Advertisers:'), 
                                              advertiser, advertiser,
                                              inline =TRUE
                                              ) # close checkboxGroupInput
                           ) # close wellPanel
                       ) # close column
                  ), # close fluidRow
                fluidRow(
                  column(width = 12,
                         h3('General Information about Advertisers'),
                         wellPanel(
                           tabsetPanel(type = "tabs",
                                     tabPanel("Revenue", plotlyOutput('adRevenue'),
                                              p('This is the sum of revenue that Vice Media makes for each advertiser.')),
                                     tabPanel("Impressions", plotlyOutput("adImpression"),
                                               p('This is the sum of impressions that Vice Media generates for each advertiser.')),
                                     tabPanel("Clicks", plotlyOutput('adClick'),
                                              p('This is the sum of clicks that Vice Media generates for each advertiser.')),
                                     tabPanel("Conversions", plotlyOutput("adConversion"),
                                              p('This is the number of conversions Vice Media generates for each advertiser.')),
                                     tabPanel("Click Through Rate", plotlyOutput("adctr"),
                                              p("This is the click through rate for each advertiser.")),
                                     tabPanel("Click to Conversion", plotlyOutput('adclickConversion'),
                                              p('This is the click to conversion rate for each advertiser.')),
                                     tabPanel("Cost per Conversion", plotlyOutput("adcostConversion"))
                           ) # close tabsetPanel
                         ) # close wellPanel
                  ) # close column
                ) # close fluidRow
              ),# close tabItem

#### Advertising Tab ####
      tabItem(tabName = 'ad_rank',
              h2("Analyzing Website Ad Rank"),
              fluidRow(
                column(width = 12,
                       wellPanel(
                         checkboxGroupInput('source2Channel', label =h3("Select Channels:"), source_channel, source_channel, inline =TRUE) # close select input
                         ) #close wellPanel
                       ) # close column
                ), # close fluidRow
              fluidRow(
                column(width = 12,
                       wellPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Revenue", plotlyOutput('rankRevenue')),
                                     tabPanel("Impressions", plotlyOutput("rankImpression")),
                                     tabPanel("Clicks", plotlyOutput('rankClick')),
                                     tabPanel("Conversions", plotlyOutput("rankConversion")),
                                     tabPanel("Click Through Rate", plotlyOutput("rankctr")),
                                     tabPanel("Click to Conversion", plotlyOutput('rankclickConversion')),
                                     tabPanel("Cost per Conversion", plotlyOutput("rankcostConversion"))
                           ) # close tabsetPanel
                       ) # close wellPanel
                  ) # close column
                ) # close fluidRow
              ), # close tabItem

#### Cost Per Conversion Tab ####
  tabItem(tabName = 'cpc',
          h2('Improving Cost per Conversion for Advertisers'),
          p('I want you to look at revenue, from each advertiser, and then look at the cost per conversion, for each advertiser. I am trying to see whether the advertisers, in red, position their ads in an ineffective website ad rank. If
            these ads are in a rank that has a higher cost per conversion, I want to know if moving the ad into a different web rank would benefit the advertiser.'), # close p tag
          fluidRow(
            column(width = 12,
                   wellPanel(
                         tabsetPanel(type = "tabs",
                                     tabPanel("Revenue", plotlyOutput('advertisementRevenue')),
                                     tabPanel("Cost per Conversion", plotlyOutput("advertisementcostConversion"),
                                              p('Anything in blue means that this advertiser is generating conversions cheaper than the mean. The advertisers in red 
                                                are spending more per conversion than the mean. The mean cost per conversion is $0.3592')) # close tabpanel
                                     ) # close tabsetPanel
                         ) # close wellPanel
                   ) # close column
            ), # close fluidRow
          fluidRow(
            column(width = 2,
                       wellPanel(
                         div(class = "check-box",
                             checkboxGroupInput("channel3Name", label = h4('Select Channel/s:'),
                                                source_channel,
                                                selected = source_channel) # close checkboxGroupInput
                             ) # close div tag
                         ) # close wellPanel
                       ), # close column
            column(width = 10,
                   wellPanel(
                     checkboxGroupInput("advertiser3Name", 
                                        label = h3('Select Advertisers:'), 
                                        advertiser, 'Saturn Security',
                                        inline =TRUE
                                        ) # close checkboxGroupInput
                     ) # close wellPanel
                   ) # close column
            ), # close fluidRow
          wellPanel(
            fluidRow(
               h3('Saturn Security'),
               p("Lets look at why Saturn Security, who has the largest spend, has the fifth worst cost per conversion."), # close p tag
               p("We see that Social Media has a large CPC, which is not good. Filtering out the other channels, besides Social Media, we see that the average cost per conversion
                 for Social Media is $0.3268, but for Saturn Security, their cost per conversion is $.908. Let's look at the datatable, under the graphs, to see whats going on here."), # close p tag
               p("I see that we are showing this ad in the worst rank for Social Media channel. Thats why the cost per conversion for social media is so high for Saturn. A large portion is spent in rank 6."), # close p tag
               column(width =4,
                     wellPanel(
                       tabsetPanel(type = "tabs",
                                   tabPanel("CPC", plotlyOutput('adchannelCPC')),
                                   tabPanel("Revenue per Channel", plotlyOutput("adchannelRevenue")),
                                   tabPanel('Conversion per Channel', plotlyOutput('adchannelConversion'))
                                   ) # close tabsetPanel
                       ) # close wellPanel
                     ), # close column 
               column(width = 4,
                      wellPanel(
                        tabsetPanel(type = "tabs",
                                    tabPanel("Average CPC Per Channel",plotlyOutput('channelCPC'))
                                    ) # close tabsetPanel
                        ) # close wellPanel
                      ), # close column
               column(width = 4,
                      wellPanel(
                        tabsetPanel(type ='tabs',
                                    tabPanel("Average CPC for Ad Rank",plotlyOutput('channelrankCPC'))
                                    ) # close tabsetPanel
                        ) # close wellPanel
                      ) # close column
               ) # close fluidRow
            ), # close wellPanel
          wellPanel(
            fluidRow(
                DT::dataTableOutput('advertiserChannelCPC')
                ) # close fluidRow
            ) # close wellPanel
          ), # close tabItem
            
#### Conclusion Tab ####
  tabItem(tabName = 'conclusion',
          h2('Steps taken in Analysis'),
          fluidRow(
            column(width = 2,
                       wellPanel(
                         div(class = "check-box",
                             checkboxGroupInput("channel4Name", label = h4('Select Channel/s:'),
                                                source_channel,
                                                selected = 'Social Media') # close checkboxGroupInput
                             ) # close div tag
                         ) # close wellPanel
                       ), # close column
            column(width = 10,
                   wellPanel(
                     checkboxGroupInput("advertiser4Name", 
                                        label = h3('Select Advertisers:'), 
                                        advertiser, 'Saturn Security',
                                        inline =TRUE
                                        ) # close checkboxGroupInput
                     ) # close wellPanel
                   ) # close column
            ), # close fluidRow
          fluidRow(
            column(width = 12,
                   wellPanel(
                     tabsetPanel(type ='tabs',
                                 tabPanel('Step 1', 
                                          h4("Look at which advertiser is in red. For this situation we are looking closely at Saturn Security."), # close h3 tag
                                          plotlyOutput("adcostCon")
                                          ),# close tabPanel
                                 tabPanel('Step 2',
                                          h4("As we saw in the Cost per Conversion tab, for Saturn Security, Social Media had a very high CPC, compared to the average. "), # close h4 tag
                                          fluidRow(
                                            column(width = 6,
                                                   wellPanel(
                                                   plotlyOutput('adchannelCPC2')) # close column
                                                   ), # close wellPanel
                                            column(width = 6,
                                                   wellPanel(
                                                   plotlyOutput('channelCPC2')) # close Column
                                                   ) # close wellPanel
                                            ) # close fluidRow
                                          ), # close tabPanel
                                tabPanel('Step 3',
                                         h4("Looking at average cost per conversion in each ad rank and looking at the datatable under the graph, we can see why Saturn Security is performing so poorly in 
                                            Social Media category. Saturn has their most expensive ad spend in rank 6, which is the worst rank for cost per conversion."), # close h4 tag
                                         wellPanel(
                                           fluidRow(plotlyOutput('adrank2')), # close fluidRow
                                           fluidRow(DT::dataTableOutput('advertiserChannelCPC2')) # close fluidRow
                                           ) # close wellPanel
                                         ), # close tabPanel
                                
                                tabPanel('Step 4',
                                         h4("Now that we see why Saturn Security is performing so poorly, let's fix that. Let's move the spend from rank 6 to a better rank. 
                                            For this example, I'm going to be moving the ad spend from rank 6 to rank 4."), # close h4 tag
                                         h4("The current cost per conversion for ad rank 4 is $0.2158."),
                                         h4("The current spend for Saturn's worst performing ad is $390.79"),
                                         h4("To see what happens if we move Saturn's spend from ad rank 6 to ad rank 4, we divide spend/cpc, $390.74/$0.2158. The number of conversions becomes 1811.")
                                         ), # close tabPanel
                                tabPanel('Step 5',
                                         h4("We're going to see the outcome of this change for Saturn Secuirty cost per conversion."),
                                         fluidRow(
                                           column(width = 6,
                                                  wellPanel(
                                                    plotlyOutput('beforeAfter')
                                                    ) # close wellPanel
                                                  ), # close column
                                           column(width = 6,
                                                  wellPanel(
                                                    plotlyOutput('beforeAfterCPC')
                                                    ) # close wellPanel
                                                  ) # close column
                                           ) # close fluidRow
                                         ), # close tabPanel
                                tabPanel('Step 6',
                                         h4("Now we're going to see what happened to Saturn Security cost per conversion for each channel."), # close h4 tag
                                         fluidRow(
                                           column(width = 6,
                                                  wellPanel(
                                                    plotlyOutput('channelbefore')
                                                    ) # close wellPanel
                                                  ), # close column
                                           column(width = 6,
                                                  wellPanel(
                                                    plotlyOutput('channelafter')
                                                    ) # close wellPanel
                                                  ) # close column
                                           ) # close fluidRow
                                         ), # close tabPanel
                                tabPanel('Step 7',
                                         h4("Now with Saturn Security social media channel decreasing its cost per conversion. Let's take a look at all advertisers cost per conversion."), # close h4 tag
                                         wellPanel(
                                           plotlyOutput('adcostCon2')
                                           ) # close wellPanel
                                         ), # close tabPanel
                                tabPanel('Conclusion',
                                         h4("We saw from the analysis that we improved the cost per conversion for Saturn Security. This doesn't necessarily mean that it helps Vice Media, however it does. 
                                            By looking at which advertisers cost per conversion performs poorly, when we take the spend from a rank and place it into a different ad rank, usually moving up a rank, means that we can charge the advertisers
                                            more because they're in a higher ad rank position. Therefore, Vice Media benefits from helping the advertisers reduce cost per conversion.") # close h4 tag
                                ) # close tabPanel
                                ) # close tabsetPanel
                     ) # close wellPanel
              
            ) # close column 
          ) # clost fluidRow
          ) # close tabItem
) # close tabItems
)# close dashboardBody
)# closeing dashboardPage
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  #### Welcome ####
  
  #### Revenue ####
  
  ## data for revenue ##
  channel <- df %>%
    group_by(channel_name) %>%
    summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())
  
  # channelRevenue plot
  output$channelRevenue <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, revenue), 
              y =  ~revenue, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Revenue: $', revenue)) %>%
        layout(
          title = "Sum of Revenue per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Revenue"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
    # channelImpressions plot
  output$channelImpressions <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, impression), 
              y =  ~impression, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Impressions: ', impression)) %>%
        layout(
          title = "Sum of Impressions per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Impression"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # channelClicks plot
  output$channelClicks <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, click),
              y =  ~click, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Clicks: ', click)) %>%
        layout(
          title = "Sum of Clicks per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Clicks"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # channelConversions plot
  output$channelConversions <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, conversion), 
              y =  ~conversion, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Conversions: ', conversion)) %>%
        layout(
          title = "Sum of Conversions per Channel",
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Conversions"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
       # ctr plot
  output$ctr <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, ctr), 
              y =  ~ctr, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Click Through Rate: ',ctr,'%')) %>%
        layout(
          title = "Click Through Rate per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Click through Rate"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # clickConversion plot
  output$clickConversion <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, conversion_rate), 
              y =  ~conversion_rate, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Click to Conversion Rate: ',conversion_rate, '%')) %>%
        layout(
          title = "Click to Conversion Rate per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Click to Conversion Rate"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # costConversion plot
  output$costConversion <- renderPlotly({
    channel = channel %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Channel",
          autosize =T,
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = TRUE
          ) # close layout
    ) # close return
  })# close renderPlotly
  
  ## data for advertisers ##
  advertisers = df %>%
    group_by(advertiser_name) %>%
    summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())
  
        # adRevenue plot
  output$adRevenue <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, revenue), 
              y =  ~revenue, 
              color = ~advertiser_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Revenue: $',revenue)) %>%
        layout(
          title = "Sum of Revenue per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Revenue"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # adImpression plot
  output$adImpression <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, impression), 
              y =  ~impression, 
              color = ~advertiser_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Impressions: ', impression)) %>%
        layout(
          title = "Sum of Impression per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Impression"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # adClick plot
  output$adClick <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, click), 
              y =  ~click, 
              color = ~advertiser_name, 
              type = "bar",      
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Clicks: ', click)) %>%
        layout(
          title = "Sum of Clicks per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Click"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # adConversion plot
  output$adConversion <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, conversion), 
              y =  ~conversion, 
              color = ~advertiser_name, 
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Conversions: ', conversion)) %>%
        layout(
          title = "Sum of Conversions per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Conversion"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # adctr plot
  output$adctr <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, ctr), 
              y =  ~ctr, 
              color = ~advertiser_name, 
              type = "bar",  
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Click Through Rate: ', ctr,'%')) %>%
        layout(
          title = "Click Through Rate per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Click Through Rate"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # adclickConversion plot
  output$adclickConversion <- renderPlotly({
   advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, conversion_rate), 
              y =  ~conversion_rate, 
              color = ~advertiser_name, 
              type = "bar",           
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Click to Conversion Rate: ',conversion_rate,'%')) %>%
        layout(
          title = "Click to Conversion Rate per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Click to Conversion Rate"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # adcostConversion plot
  output$adcostConversion <- renderPlotly({
   advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName)
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~advertiser_name,
              type = "bar", 
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
  #### Ad Rank ####
  
  ## data for ad rank ##
  
  rank = df %>%
  group_by(website_ad_rank, channel_name) %>%
  summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())

          # rankRevenue plot
  output$rankRevenue <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~revenue, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Revenue: $', revenue)) %>%
        layout(
          title = "Sum of Revenue per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Revenue"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
          # rankRevenue plot
  output$rankImpression <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~impression, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Impression: ', impression)) %>%
        layout(
          title = "Sum of Impressions per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Impressions"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
            # rankClick plot
  output$rankClick <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~click, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Click: ', click)) %>%
        layout(
          title = "Sum of Clicks per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Clicks"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
  
            # rankConversion plot
  output$rankConversion <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~conversion, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Conversion: ', conversion)) %>%
        layout(
          title = "Sum of Conversions per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Conversions"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # rankctr plot
  output$rankctr <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~ctr, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Click Through Rate: ', ctr,'%')) %>%
        layout(
          title = "Click Through Rate per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Click Through Rate"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # rankclickConversion plot
output$rankclickConversion <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~conversion_rate, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Click to Conversion Rate: ', conversion_rate,'%')) %>%
        layout(
          title = "Click to Conversion Rate per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Click to Conversion Rate"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly
  
      # adcostConversion plot
output$rankcostConversion <- renderPlotly({
    rank = rank %>% filter(channel_name%in%input$source2Channel)
    return(
      plot_ly(x = ~website_ad_rank, 
              y = ~conversion_spend, 
              color = ~channel_name, 
              data = rank, 
              type = 'bar',
              hoverinfo = 'text', 
              text = ~paste('Channel: ', channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Website Ad Rank",
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = TRUE,
          barmode = 'stack'
          ) # close layout
    ) # close return
  })# close renderPlotly

#### Cost Per Conversion ####

advertisers 
        # advertisementRevenue plot
  output$advertisementRevenue <- renderPlotly({
    advertisers = advertisers
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, revenue), 
              y =  ~revenue, 
              color = ~advertiser_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Revenue: $',revenue)) %>%
        layout(
          title = "Sum of Revenue per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Revenue"),
          showlegend = FALSE,
          margin = list(b = 110)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
        # advertisementcostConversion plot
  output$advertisementcostConversion <- renderPlotly({
    advertisers = advertisers
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~advertiser_name,
              type = "bar", 
              marker=list( color=ifelse(advertisers$conversion_spend>.35982,"red","blue") , opacity=0.5 , size=30),
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = FALSE,
          margin = list(b = 110)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
  adNameChannel = df %>%
    group_by(channel_name,advertiser_name) %>%
    summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())

       # adchannelCPC plot
  output$adchannelCPC <- renderPlotly({
    
    adNameChannel = adNameChannel %>% 
      filter(advertiser_name%in%input$advertiser3Name) %>% 
      filter(channel_name%in%input$channel3Name)
    
    return(
      plot_ly(data = adNameChannel, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Channel: ',channel_name,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = ("Cost per Conversion in each Channel for Saturn Security"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1.5)),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
  # adchannelRevenue plot
output$adchannelRevenue <- renderPlotly({
    
    adNameChannel = adNameChannel %>% 
      filter(advertiser_name%in%input$advertiser3Name) %>% 
      filter(channel_name%in%input$channel3Name)
    
    return(
      plot_ly(data = adNameChannel, 
              x = ~reorder(channel_name, revenue), 
              y =  ~revenue, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Channel: ',channel_name,
                            '<br> Revenue: $', revenue)) %>%
        layout(
          title = ("Revenue for each Channel for Saturn Security"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Revenue"),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 

# adchannelConversion plot
output$adchannelConversion <- renderPlotly({
    
    adNameChannel = adNameChannel %>% 
      filter(advertiser_name%in%input$advertiser3Name) %>% 
      filter(channel_name%in%input$channel3Name)
    
    return(
      plot_ly(data = adNameChannel, 
              x = ~reorder(channel_name, conversion), 
              y =  ~conversion, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Channel: ',channel_name,
                            '<br> Conversions: ', conversion)) %>%
        layout(
          title = ("Conversion for each Channel for Saturn Security"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Conversion"),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
  # channelCPC plot
  output$channelCPC <- renderPlotly({
  
    channel = channel %>% 
      filter(channel_name%in%input$channel3Name)
    
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ',channel_name,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = ("Average Cost per Conversion for every Channel"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1.5)),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
  # data for channelrankCPC
 rank = df %>%
  group_by(website_ad_rank, channel_name) %>%
  summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())
 
  # channelrankCPC plot
  output$channelrankCPC <- renderPlotly({
    rank = rank %>% 
      filter(channel_name%in%input$channel3Name)
    
    return(
      plot_ly(data = rank, 
              x = ~reorder(website_ad_rank, conversion_spend),
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ',channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = ("Average Cost per Conversion for every Channel"),
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = F,
          barmode = 'stack',
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
  # data for datatable
  cpc = df
  cpc$cost_per_conversion = cpc$total_ad_spend/cpc$ad_conversions
  
  output$advertiserChannelCPC <- DT::renderDataTable({
    DT::datatable(cpc[(cpc$advertiser_name%in%input$advertiser3Name) & (cpc$channel_name%in%input$channel3Name),],
                  options = list(scrollX = TRUE))
      })
  #### Conclusion ####
  
  adNameChannel2 = df %>%
    group_by(channel_name,advertiser_name) %>%
    summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())
  
         # adcostCon plot
  output$adcostCon <- renderPlotly({
    advertisers = advertisers
    return(
      plot_ly(data = advertisers, 
              x = ~reorder(advertiser_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~advertiser_name,
              type = "bar", 
              marker=list(color=ifelse(advertisers$advertiser_name == 'Saturn Security','red', 'green') , opacity=0.5 , size=30),
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = FALSE,
          margin = list(b = 110)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
  
       # adchannelCPC2 plot
        # adchannelCPC plot
  output$adchannelCPC2 <- renderPlotly({
    
    adNameChannel2 = adNameChannel2 %>% 
      filter(advertiser_name%in%input$advertiser4Name) %>% 
      filter(channel_name%in%input$channel4Name)
    
    return(
      plot_ly(data = adNameChannel2, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ',channel_name,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
             
        layout(
          title = ("Cost per Conversion in each Channel for Saturn Security"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1)),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
   # channelCPC2 plot
  output$channelCPC2 <- renderPlotly({
  
    channel = channel %>% 
      filter(channel_name%in%input$channel4Name)
    
    return(
      plot_ly(data = channel, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ',channel_name,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = ("Average Cost per Conversion for every Channel"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1)),
          showlegend = TRUE,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
  
    # adrank2 plot
  output$adrank2 <- renderPlotly({
    rank = rank %>% 
      filter(channel_name%in%input$channel4Name)
    
    return(
      plot_ly(data = rank, 
              x = ~reorder(website_ad_rank, conversion_spend),
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Channel: ',channel_name,
                            '<br> Ad Rank: ', website_ad_rank,
                            '<br> Cost per Conversion: $', conversion_spend)) %>%
        layout(
          title = ("Average Cost per Conversion for every Channel"),
          xaxis = list(title = "Website Ad Rank"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = T,
          barmode = 'stack',
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 

  # data for datatable
  cpc = df
  cpc$cost_per_conversion = cpc$total_ad_spend/cpc$ad_conversions
  
  output$advertiserChannelCPC2 <- DT::renderDataTable({
    DT::datatable(cpc[(cpc$advertiser_name%in%input$advertiser4Name) & (cpc$channel_name%in%input$channel4Name),],
                  options = list(scrollX = TRUE))
      })
  
  # data for step 5
  beforesaturnChannelCPC = df %>%
  filter(advertiser_name == 'Saturn Security') %>%
  filter(channel_name == 'Social Media') %>%
  group_by(channel_name,advertiser_name) %>%
  summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())

aftersaturnChannelCPC = beforesaturnChannelCPC

beforesaturnChannelCPC$channel_name = 'Before'
aftersaturnChannelCPC$channel_name = 'After'

aftersaturnChannelCPC$conversion = 2442
aftersaturnChannelCPC$conversion_spend = .25138

  
  beforeAfter = rbind(beforesaturnChannelCPC, aftersaturnChannelCPC) # joining before and after together
  
  # beforeAfter plot
  output$beforeAfter <- renderPlotly({
    return(
      plot_ly(data = beforeAfter,  
              x = ~channel_name , 
              y = ~conversion, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                           '<br> Channel: ','Social Media',
                           '<br> Conversion: ',conversion))%>%
        layout(
          xaxis = list(title = 'Social Media'),
          yaxis = list(title = "Conversion"),
          showlegend = TRUE
        ) # close layout
    ) # close return
  })# close renderPlotly

    # beforeAfterCPC plot
  output$beforeAfterCPC <- renderPlotly({
    return(
      plot_ly(data = beforeAfter,  
              x = ~channel_name , 
              y = ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                           '<br> Channel: ','Social Media',
                           '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          xaxis = list(title = 'Social Media'),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = TRUE
        ) # close layout
    ) # close return
  })# close renderPlotly
  
  # data for step 6
  beforesaturnTotalCPC = df %>%
  filter(advertiser_name == 'Saturn Security') %>%
  group_by(channel_name,advertiser_name) %>%
  summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              revenue = sum(total_ad_spend),
              ctr = round((sum(ad_clicks)/sum(ad_impressions))*100,2),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(total_ad_spend)/sum(ad_clicks),
              conversion_spend = round(sum(total_ad_spend)/sum(ad_conversions),4),
              count =n())

aftersaturnTotalCPC = beforesaturnTotalCPC

aftersaturnTotalCPC[5, 5] = c(2442)
aftersaturnTotalCPC[5,10] = c(.2531)

    # channelbefore plot
 output$channelbefore <- renderPlotly({
   
    return(
      plot_ly(data = beforesaturnTotalCPC, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                           '<br> Channel: ',channel_name,
                           '<br> Cost per Conversion: $',conversion_spend)) %>%
             
        layout(
          title = ("Before"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1)),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 

 # channelafter plot
 output$channelafter <- renderPlotly({
   
    return(
      plot_ly(data = aftersaturnTotalCPC, 
              x = ~reorder(channel_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~channel_name, 
              type = "bar",
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                           '<br> Channel: ',channel_name,
                           '<br> Cost per Conversion: $',conversion_spend)) %>%
             
        layout(
          title = ("After"),
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Cost per Conversion", range = c(0,1)),
          showlegend = F,
          margin = list(b = 75)
          ) # close layout
    ) # close return
  })# close renderPlotly 
 

advertisers2 = advertisers

advertisers2[26,9]  = c(.308) 
 
 # adcostCon2 plot
 output$adcostCon2 <- renderPlotly({
    return(
      plot_ly(data = advertisers2, 
              x = ~reorder(advertiser_name, conversion_spend), 
              y =  ~conversion_spend, 
              color = ~advertiser_name,
              type = "bar", 
              marker=list(color=ifelse(advertisers$advertiser_name == 'Saturn Security','red', 'green') , opacity=0.5 , size=30),
              hoverinfo = 'text', 
              text = ~paste('Advertiser: ', advertiser_name,
                            '<br> Cost per Conversion: $',conversion_spend)) %>%
        layout(
          title = "Cost per Conversion per Advertiser",
          xaxis = list(title = "Advertiser"),
          yaxis = list(title = "Cost per Conversion"),
          showlegend = FALSE,
          margin = list(b = 110)
          ) # close layout
    ) # close return
  })# close renderPlotly 

  
} # close server
# Run the application 
shinyApp(ui = ui, server = server)

# scale step 2 
