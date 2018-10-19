library(shiny)
library(shinydashboard)
library(plotly)
library(ggplot2)
library(dplyr)
library(tidyr)
library(DT)
library(rsconnect)

data = read.csv('./merged.csv', stringsAsFactors = F)
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
      menuItem("Channels", tabName = "channel_name", icon = icon("compass")),
      menuItem("Advertisers", tabName = "advertiser_name", icon = icon("compass")),
      menuItem("Conclusion", tabName= "conclusion", icon= icon("certificate")))
  ),
    
  dashboardBody(
    tabItems(
#### Welcome Tab ####
      tabItem(tabName = "welcome",
              h2("Introdution to Analysis"),
              br(),
              fluidRow(
                column(width = 8,
                       div(class = 'my-class',
                           p("I started this project off by merging all the dataframes together. I used a jupyter notebook to perform with merge.
                              I used the merge function in pandas to join these tables together, specifically,I used an 
                             inner join for advertisers and source_channels. I joined advertisers to 
                             advertising_performance, then joined source_channels to the new merged dataframe to get the proper dataframe. "
                             ), # close p tag
                           p("Once I got my dataframe in working order, time began to analyze.
                             Majority of my analysis was done in a jupyter notebook that I've included in the git hub. 
                             I used a jupyter notebook because it allows me to investigate the data, enabling me to visualize my results.
                             Once I was comfortable with the information I had, I saved the dataset and imported the new csv file into R to create a dashboard with shiny."
                             ), # close p tag
                           p('The Sidebar on the left has my analysis.'), # close p tag
                           h2('Goal'),
                           p("The goal of this analysis is to improve advertising performance."), # close p tag
                           h2("What You Will See"),
                           p("My goal in this analysis is to find a way to increase our conversion's without reducing or increasing our budget.
                            From my research, I saw that SEO/Organic had the highest click to conversion relative to all other channels. Looking at advertisers in the SEO/Organic channel,
                             I found that Nero Enterprise is the second highest budget, but has the lowest click to conversion rate compared to all other advertisers. In the conclusion section I explain
                             how using the buget orginially intended for Nero, and allocating it to two other advertising budgets, specifically, Lionesscape and Flukord, helps generate more clicks, which eventually leads
                             to conversions.") # close p tag
                           ) # close div tag
                  ) # close column tag
              ), # close fuildRow
              wellPanel(
                fluidRow(
                  h1('Before & After Distribution of Nero Enterprise Budget to Lionesscape and Flukord'),
                  column(width = 6,
                         wellPanel(
                           h3('Total Ad Spend Before & After Nero Budget Distribution'),
                           plotlyOutput('beforeNeroSpend')
                           ) # close wellPanel
                         ), # close column
                  column(width = 6,
                         wellPanel(
                           h3('Total Ad Clicks Before & After Nero Budget Distribution'),
                           plotlyOutput('beforeNeroClick')
                           ) # close wellPanel
                         ) # close column
                  ) #close fluidRow
                ) # close wellPanel
              ), # close tabItem
      # end welcome
      
#### Channel Tab ####
      tabItem(tabName = 'channel_name',
              h2("Analyzing Each Channel Performance"),
              fluidRow(
                column(width = 12,
                       div(class = "my-class",
                           p("This page allows you to manipulate each channel source and see how they performed against eachother. Scroll down for a page explanation.") # close p tag
                           ) # close div tag
                       ) # close column 
                ), # close fluidRow
              br(),
              wellPanel(
              fluidRow(
                column(width = 2,
                       wellPanel(
                         div(class = "check-box",
                             checkboxGroupInput("channelName", label = h4('Select Channel/s:'),
                                                source_channel,
                                                selected = source_channel) # close checkboxGroupInput
                             ) # close div tag
                         ) # close wellPanel
                       ), # close column
                column(width = 10,
                       wellPanel(
                         h3('Total Ad Spend'),
                         plotlyOutput('adSpend')
                       ) # close wellPanel
                ) # close column
                )# close fluidRow
              ), # close wellPanel
              br(),
              fluidRow(
                column(width = 4,
                       wellPanel(
                         h3('Total Ad Impressions'),
                         plotlyOutput("adImpressions")
                         ) # close wellPanel
                       ), # close column
                column(width = 4,
                       wellPanel(
                         h3('Total Ad Clicks'),
                         plotlyOutput('adClicks')
                         ) # close wellPanel
                       ), # close column
                column(width = 4,
                       wellPanel(
                         h3('Total Ad Conversion'),
                         plotlyOutput('adConversions')
                         ) # close wellPanel
                       ) # close column
                ), # close fluidRow
              br(),
              fluidRow(
                column(width = 12,
                       wellPanel(
                         div(class = 'my-class',
                             h1('Page Explanation'),
                             p("What we see on this page are the total sums for each channel by impression, clicks, conversions, and ad spend." ), #close p tag
                             p("I noticed that SEO/Organic channel seems like an interseting place to look if I'm trying to optimize advertising performance.
                               I see from the plots below that SEO/Organic has the least amount of impressions, but has the most amount 
                               of clicks and conversions."
                             ), # close p tag
                             p("I see that the click to conversion rate for SEO/Organic sits at a whopping 74%. I got 74% by taking the total sum of conversions and divide it by the total sum of clicks.
                               That means for every 100 clicks, 74 will be a conversion."
                             ), # close p tag
                             h3('Next Steps'),
                             p("I'm interested in getting a better understanding of the advertisers in SEO/Organic Channel"), # close p tag
                             p("It would be of interest to me to see which advertiser has a higher click to conversion rate. Go to the Advertisers tab to see further analysis"
                             ) # close p tag
                             ) # close div tag
                             ) # close wellPanel
                ) # close column
                ) # close fluidRow
              ), # close tabItem
      
      
#### Advertising Tab ####
      tabItem(tabName = 'advertiser_name',
              h2("Analyzing Each Channel Performance"),
              fluidRow (
                column(width = 12,
                       wellPanel(
                         selectInput('sourceChannel', "Choose a Channel:",
                                     source_channel,
                                     selected = 'SEO/Organic') #close select input
                         ) #close wellPanel
                       ) # close column
                ), # close fluidRow
              fluidRow(
                column(width = 12,
                       wellPanel(
                         checkboxGroupInput("advertiserName", label = h3('Select Advertisers:'),
                                            advertiser, 
                                            selected = c('Cloverprises',
                                                         'Lionessscape',
                                                         'Nero Enterprises',
                                                         'Flukords'),
                                            inline =TRUE
                                            ) # close checkboxGroupInput
                         ) # close wellPanel
                       ) # close column
                ), # close fluidPage
              wellPanel(
                fluidRow(
                  column(width = 12,
                         h3('Total Ad Spend'),
                         plotlyOutput('advertiserSpend')
                         ) # close column
                  ) # close fluidRow
                ), # close wellPanel
              wellPanel(
                fluidRow(
                  column(width = 4,
                         h3('Total Ad Impressions'),
                         plotlyOutput("advertiserImpressions")
                  ), # close column
                  column(width = 4,
                         h3('Total Ad Clicks'),
                         plotlyOutput('advertiserClicks')
                  ), # close column
                  column(width = 4,
                         h3('Total Ad Conversion'),
                         plotlyOutput('advertiserConversion')
                  ) # close column
                ) # close fluidRow
              ), # close wellPanel
              fluidRow(
                column(width = 12,
                       wellPanel(
                         h3("Page Explanation"),
                         p("In this tab, I've pre-selected the advertisers and channel source that I want us to look at for simplicity.
                           On this page total impression, click, conversion, and spend for each advertiser in SEO/Organic channel has been generated.
                           You can change SEO/Organic in the drop down menu. Keep in mind, when you change SEO/Organic to another channel, you will be changing the results, the 
                           same goes for our checkbox of advertisers."), # close p tag
                         p("**NOTE**"),
                         p("As mentioned in the Channel Tab, when SEO/Organic channel generates a click from an ad, there's a 74% chance that the click will
                            be converted to a conversion."), # close p tag
                         h3('Insight'),
                         p("I want us to look at 3 advertisers: Nero Enterprise, Flukord, and Lionessscape. Looking at Total Spend I can see that Nero Enterprises is the second highest spender for our SEO/Organic channel.
                           Nero also has the lowest converstion rate. Another interesting note, Nero's click spend is aweful compared to our other advertiser."), # close p tag
                         h3('Question'),
                         p("What if we removed the budget for Nero Enterprise? We see they're not preforming as well as our other advertisers, so it would be interesting to see what happens to allocate our Nero budget to other advertisers.
                           Click on the conclusion tab to see my recommendation.") # close p tag
                       ) #close wellPanel
                       ) # close column
                ) # close fluidRow
            ), # close tabItem  
#### Conclusion Tab ####
  tabItem(tabName = 'conclusion',
          fluidRow(
            column(width = 12,
                   wellPanel(
                     h3("Question"),
                     p("By removing Nero's SEO budget, we have an additional $339.01 to spend else where."), # close p tag
                     p("What happens to total clicks if we allocate Nero Enterprise SEO budget to other advertisers?"), # close p tag
                     h3('Metrics'),
                     p("I will be using Conversion Rate, Click Spend, and Conversion Spend to determine which advertiser should aquire Nero's budget.
                       The reason I'm choosing these metrics is because my goal is to achieve a higher click rate in SEO/Organic channel. "), # close p tag
                     h3("Answer"),
                     p("I have decided that the two advertisers that I will allocate a portion of Nero's SEO budget will beLionesscape and Flukord because of the click to conversion rate for each advertiser. 
                       When I divide Nero's budget in half, we get $169.5. Currently the budget for Lionesscape is $39 and Flukord is $53. Now,
                       lets allocate the divided Nero budget to Lionesscape and Flukord. Now, Lionesscape and Flukord budget would be $208 and $222. There will be some number manipulation, 
                       so to make sure you follow, I've written down the steps taken in the section below."), # close p tag
                     h4("Arithmetic for Flukord"),
                     p(" * Estimated Flukord budget = $222"),# close p tag
                     p(" * Estimated Flukord Total Click = Estimated Flukord budget * current click spend"), # close p tag
                     p(" * Estimated Flukord Total Click = 222 * 52 = 11,544 Clicks"), # close p tag
                     p(" * Estimated Flukord Total Conversion = Estimated Flukord Total Click * current conversion rate"), # close p tag
                     p(" * Estimated Flukord Total Conversion = 11,544 * 63.67% = 7,350 conversions"), # close p tag
                     h4("Arithmetic for Lionesscape"), # close p tag
                     p(" * Estimated Lionesscape budget = $208"), # close p tag
                     p(" * Estimated Lionesscape Total Click = Estimated Lionesscape budget * current click spend"), # close p tag
                     p(" * Estimated Lionesscape Total Click = 208 * 41 = 8,528 Clicks"), # close p tag
                     p(" * Estimated Lionesscape Total Conversion = Estimated Lionesscape Total Click * current conversion rate"), # close p tag
                     p(" * Estimated Lionesscape Total Conversion = 8,528 * 91.71% = 7,821 conversions"), # close p tag
                     h3('Recommendation'),
                     p("By increasing Lionesscape and Flukord budget, we can see that our conversions have signifcantly increased. 
                       Assuming that increasing the budget for Lionesscape and Flukord wont change the conversion rate, I can say that we grew conversions
                       by 15,171. Nero's total conversions in SEO channel was a mere 94. By allocating Nero's SEO budget to Lionesscape and Flukord, we significantly increase our SEO conversions, 
                       by 15,044 (15,171 - 94 (nero total conversion) = 15044).") #close p tag
                     ) # close wellPanel
                   ) # close column
            ), # close fluidRow
          fluidRow(
            column(width = 12,
                   wellPanel(
                     h2('Conversion Rate for Each Advertiser'),
                     p("The graph below is a depiction the conversion rate for each advertiser."), # close p tag
                     p("For example, Flukords generated 2824 clicks and 1798 conversions. The conversion rate is conversions divided by clicks."), # close p tag
                     p("***NOTE***"), # close p tag
                     p("The values are in percents, rounded to the second decimal."), # close p tag
                     plotlyOutput('advertiserConversionRate')
                     ) # close wellPanel
                   ) # close column
            ), # close fluidRow
          br(),
          fluidRow(
            column(width = 6,
                   wellPanel(
                     h2('Click Spend for Each Advertiser'),
                     p("The graph below is a depiction of the number a clicks each advertiser receives per dollar spent."), # close p tag
                     p("For example, Flukords click spend (sum of ad clicks/sum of total ad spend) is greater than 50. That means
                       for every $1 spent, Flukords generates more than 50 clicks."), # close p tag
                     plotlyOutput('advertiserClickSpend')
                     ) # close wellPanel
                   ), # close column
            column(width = 6,
                   wellPanel(
                     h2('Conversion Spend for Each Advertiser'),
                     p("The graph below is a depiction of the number a conversions each advertiser receives per dollar spent."), # close p tag
                     p("For example, Flukords conversion spend (sum of ad conversion/sum of total ad spend) is greater than 30. That means
                       for every $1 spent, Flukords generates more than 30 conversions"), # close p tag
                     plotlyOutput('advertiserConversionSpend')
                     ) # close wellPanel
                   ) # close column
            ) # close fluidRow
          ) # close tabItem
) # close tabItems
) # close dashboardBody
) # closeing dashboardPage
  
# Define server logic required to draw a histogram
server <- function(input, output) {
  
  #### Welcome ####
  
  # data for channel & welcome
  channel = df %>%
    group_by(channel_name) %>%
    summarise(., impressions = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              spend = sum(total_ad_spend),
              click_to_conversion = round((sum(ad_conversions)/sum(ad_clicks))*100,2),
              count =n())
  
  # beforeNeroSpend plot
  output$beforeNeroSpend <- renderPlotly({
    return(
      plot_ly(data = beforeAfter,  x = ~channel_name , y = ~spend2, color = ~channel_name, type = "bar") %>%
        layout(
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Total Ad Spend"),
          showlegend = TRUE
        ) # close layout
    ) # close return
  })# close renderPlotly
  
  # beforeNeroClick spend
  output$beforeNeroClick<- renderPlotly({
    return(
      plot_ly(data = beforeAfter,  x = ~channel_name , y = ~click2, color = ~channel_name, type = "bar") %>%
        layout(
          xaxis = list(title = "Channel"),
          yaxis = list(title = "Total Ad Click"),
          showlegend = TRUE
        ) # close layout
    ) # close return
  })# close renderPlotly

  #### Channel Name ####
  
  # adImpressions plot
  output$adImpressions <- renderPlotly({
    channel <- channel %>% filter(channel_name%in%input$channelName)
    return(
      plot_ly(data = channel, x = ~reorder(channel_name, impressions), y = ~impressions, color = ~channel_name, type = "bar") %>%
        layout(xaxis = list(title = "Channel"),
               yaxis = list(title = "Sum of Impressions"),
               margin = list(b = 65))
               ) # close return
  })# close renderPlotly
  
  # adClicks plot
  output$adClicks <- renderPlotly({
    channel <- channel %>% filter(channel_name%in%input$channelName)
    return(
      plot_ly(data = channel, x = ~reorder(channel_name, click), y = ~click, color = ~channel_name, type = "bar") %>%
        layout(xaxis = list(title = "Channel"),
               yaxis = list(title = "Sum of Clicks"),
               margin = list(b = 65))
    ) # close return
  })# close renderPlotly
  
  # adConversion plot
  output$adConversions <- renderPlotly({
    channel <- channel %>% filter(channel_name%in%input$channelName)
    return(
      plot_ly(data = channel, x = ~reorder(channel_name, conversion), y = ~conversion, color = ~channel_name, type = "bar") %>%
        layout(xaxis = list(title = "Channel"),
               yaxis = list(title = "Sum of Conversions"),
               margin = list(b = 65))
    ) # close return
  })# close renderPlotly
  
  # adSpend plot
  output$adSpend <- renderPlotly({
    channel <- channel %>% filter(channel_name%in%input$channelName)
    return(
      plot_ly(data = channel, x = ~reorder(channel_name, spend), y = ~spend, color = ~channel_name, type = "bar") %>%
        layout(xaxis = list(title = "Channel"),
               yaxis = list(title = "Sum of Spend"),
               margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
               ) # close layout
    ) # close return
  }) # close renderPlotly
  
  # data for advertisers
  advertisers <- df %>%
    group_by(advertiser_name, channel_name) %>%
    summarise(., impression = sum(ad_impressions),
              click = sum(ad_clicks),
              conversion = sum(ad_conversions),
              spend = sum(total_ad_spend),
              ctr = sum(ad_impressions)/sum(ad_clicks),
              conversion_rate = round((sum(ad_conversions)/sum(ad_clicks))*100, 2),
              click_spend = sum(ad_clicks)/sum(total_ad_spend),
              conversion_spend = sum(ad_conversions)/sum(total_ad_spend),
              count =n())
  
  # advertiserImpressions plot
  output$advertiserImpressions <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = advertisers, x = ~impression, y =  ~reorder(advertiser_name, impression), color = ~advertiser_name, type = "bar") %>%
        layout(
          autosize =T,
          xaxis = list(title = "Sum of Impressions"),
          yaxis = list(title = "Advertiser"),
          showlegend = FALSE,
          margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
          ) # close layout
    ) # close return
  })# close renderPlotly
  
  # advertiserClicks plot
  output$advertiserClicks <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = advertisers, x = ~click, y =  ~reorder(advertiser_name, click), color = ~advertiser_name, type = "bar") %>%
        layout(
          autosize =T,
          xaxis = list(title = "Sum of Clicks"),
          yaxis = list(title = "Advertiser"),
          showlegend = FALSE,
          margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
        ) # close layout
    ) # close return
  })# close renderPlotly
  
  # advertiserConversion plot
  output$advertiserConversion <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = advertisers, x = ~conversion, y =  ~reorder(advertiser_name, conversion), color = ~advertiser_name, type = "bar") %>%
        layout(
          autosize =T,
          xaxis = list(title = "Sum of Conversion"),
          yaxis = list(title = "Advertiser"),
          showlegend = FALSE,
          margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
        ) # close layout
    ) # close return
  })# close renderPlotly
  
  # advertiserSpend plot
  output$advertiserSpend <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = advertisers, x = ~spend, y =  ~reorder(advertiser_name, spend), color = ~advertiser_name, type = "bar") %>%
        layout(
          autosize =T,
          xaxis = list(title = "Sum of Spend"),
          yaxis = list(title = "Advertiser"),
          showlegend = FALSE,
          margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
        ) # close layout
    ) # close return
  })# close renderPlotly
  
  # advertiserConversionRate plot
  output$advertiserConversionRate <- renderPlotly({
    advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
    return(
      plot_ly(data = advertisers, x = ~conversion_rate, y =  ~reorder(advertiser_name, conversion_rate), color = ~advertiser_name, type = "bar") %>%
        layout(
          autosize =T,
          xaxis = list(title = "Conversion Rate"),
          yaxis = list(title = "Advertiser"),
          showlegend = FALSE,
          margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
        ) # close layout
    ) # close return
  })# close renderPlotly
    
    # advertiserClickSpend plot
    output$advertiserClickSpend <- renderPlotly({
      advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
      return(
        plot_ly(data = advertisers, x = ~click_spend, y =  ~reorder(advertiser_name, click_spend), color = ~advertiser_name, type = "bar") %>%
          layout(
            autosize =T,
            xaxis = list(title = "Number of Clicks for Every Dollar Spent"),
            yaxis = list(title = "Advertiser"),
            showlegend = FALSE,
            margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
          ) # close layout
      ) # close return
  })# close renderPlotly
    
    # advertiserConversionSpend plot
    output$advertiserConversionSpend <- renderPlotly({
      advertisers = advertisers %>% filter(advertiser_name%in%input$advertiserName) %>% filter(channel_name%in%input$sourceChannel)
      return(
        plot_ly(data = advertisers, x = ~conversion_spend, y =  ~reorder(advertiser_name, conversion_spend), color = ~advertiser_name, type = "bar") %>%
          layout(
            autosize =T,
            xaxis = list(title = "Number of Conversions for Every Dollar Spent"),
            yaxis = list(title = "Advertiser"),
            showlegend = FALSE,
            margin = list(l =140 ,r = 1,b = 45,t = 5,pad = 2)
          ) # close layout
      ) # close return
    })# close renderPlotly
   
} # close server


# Run the application 
shinyApp(ui = ui, server = server)

