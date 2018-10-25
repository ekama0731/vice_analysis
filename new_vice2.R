library(dplyr)
library(tidyr)
library(plotly)
library(ggplot2)

df = read.csv('./merged2.csv', stringsAsFactors = F)
advertiser = sort(unique(df$advertiser_name))
source_channel = unique(df$channel_name)

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
aftersaturnChannelCPC$conversion_spend = 618.27/2442

beforeAfter = rbind(beforesaturnChannelCPC, aftersaturnChannelCPC)


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



advertisers2 = advertisers

advertisers2[26,9]  = c(.308)
  
  
  
  
  
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