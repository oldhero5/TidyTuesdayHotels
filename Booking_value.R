#libraries
library(tidyverse)
library(tidytuesdayR)
library(feasts)
library(lubridate)
library(tsibble)
library(fable)
library(ggthemes)
library(scales)
library(patchwork)


#load data
tuesdata <- tidytuesdayR::tt_load('2020-02-11')

df <- tuesdata$hotels

#EDA


#grouped by agent
agent_df <- df%>%
  group_by(agent)%>%
  filter(agent!="NULL")%>%
  mutate(date = glue::glue("{arrival_date_year}-{arrival_date_month}-{arrival_date_day_of_month}"),
         date = parse_date(date, format = "%Y-%B-%d"))%>%
  mutate(change_lead_time =ifelse(date-reservation_status_date>=0,date-reservation_status_date,0),
         nights = stays_in_weekend_nights +stays_in_week_nights,
         not_canceled = if_else(is_canceled==1,0,1),
         revenue = nights * not_canceled * adr,
         lost_revenue = (nights * adr) - (nights*is_canceled*adr))%>%
  summarise(cancel = sum(is_canceled),
            lead = median(lead_time),
            range = IQR(adr),
            mean = mean(adr),
            min_lead = min(change_lead_time),
            max_lead = max(change_lead_time),
            mean_lead = mean(change_lead_time),
            bookings = n(),
            agent_value = sum(revenue),
            lost_value = sum(lost_revenue))%>%
  mutate(close_rate = 1-(cancel/bookings))

closers<- agent_df %>%
  filter(bookings >= 500, close_rate >.75)%>%
  top_n(10,agent_value)%>%
  arrange(desc(close_rate))

close_plot<- closers %>%
  mutate(agent = fct_reorder(agent, agent_value))%>%
  ggplot(aes(agent,agent_value))+
  geom_col( fill="#D95F02", alpha=.8, width=.5)+
  ggtitle("Where are My Deal Closers?",subtitle = "Top 10 Agents with Greater than 75% Closed Bookings")+
  scale_y_continuous("Total Agent Value Earned",labels=dollar_format(prefix="$"))+
  scale_x_discrete("Top 10 Booking Agents")+
  guides(fill="none")+
  coord_flip()+
  theme_pander()
# filtering by 10 greatest value lost with high bookings and low closure rates  
shady <- agent_df %>%
  filter(bookings >= 500, between(close_rate,0.0001,.5))%>%
  top_n(10,lost_value)

shady_plot <-shady %>%
  mutate(agent = fct_reorder(agent, lost_value))%>%
  ggplot(aes(agent,lost_value))+
  geom_col( fill="#2D2D2D", alpha=.8, width=.5)+
  ggtitle("Do you Trust These Agents?",subtitle = "Agents Most Likley to Cancel with Greater than 500 Bookings")+
  scale_y_continuous("Total Agent Value Lost",labels=dollar_format(prefix="$"))+
  scale_x_discrete("10 Shadiest Booking Agents")+
  guides(fill="none")+
  coord_flip()+
  theme_pander()

p1 <- wrap_plots(close_plot,shady_plot)+ 
  plot_annotation(title = "Are Agents the problem with cancelations?",
                  subtitle = "Below are the best and worst agents to deal with based on their booking to cancelation rates.",
                  caption = "Source: Antonio, Almeida, and Nunes, 2019 | Plot: @oldhero51")
                                                         
p1

 
