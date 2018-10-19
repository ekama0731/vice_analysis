library(dplyr)
library(ggplot2)
library (tidyr)
library (plotly)

df = read.csv('./merged.csv', stringsAsFactors = F)

df_channel = df %>%
  group_by(channel_name) %>%
  summarise(., total = sum(ad_impressions)) %>%
  arrange(desc(total)) 

beforeNero = df %>%
  group_by(channel_name) %>%
  filter(channel_name == 'SEO/Organic') %>%
  summarise( spend2 = sum(total_ad_spend),
             click2= sum(ad_clicks))

beforeNero$channel_name = 'Before'

afterNero = df %>%
  group_by(advertiser_name) %>%
  filter(channel_name == 'SEO/Organic') %>%
  summarise(spend2= sum(total_ad_spend),
            click2 = sum(ad_clicks))
# Changing nero budet to 0 and click 0 
afterNero[16,2:3] = c(0,0)

# updating Lionesscape and Flukord budget and estimated clicks
afterNero[9,2:3] = c(222,11544) # Flukord
afterNero[14,2:3] = c(208,8528) # lionesscape

after = afterNero %>%
  summarise(spend2 =sum(spend2),
            click2 = sum(click2))

after$channel_name= 'After'

afterNero = after %>%
  select(channel_name, everything())

# joining beforeNero and afterNero together
beforeAfter = rbind(beforeNero, afterNero)





