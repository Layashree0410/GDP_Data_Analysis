library(tidyverse)
library(janitor)
library(stringr)

"final_statewise_gsdp.csv" %>% 
  read_csv %>% 
  rename("sector"="item") -> statewise_gsdp
  
  
statewise_gsdp %>% 
  pull(sector) %>% 
  unique()


##1. for every financial year , which sector has performed well
statewise_gsdp %>%
  group_by(year, sector) %>%
  summarise(total_gsdp = sum(gsdp)) %>%
  arrange(year, desc(total_gsdp)) %>% 
  slice(1) -> year_wise_top_sector
print(year_wise_top_sector)


##2. for every financial year , which sector has performed least 
statewise_gsdp %>%
  group_by(year, sector) %>%
  summarise(total_gsdp = sum(gsdp)) %>%
  arrange(year, total_gsdp) %>%
  slice(1) -> year_wise_least_sector
print(year_wise_least_sector)

##3. For every financial year, which state has performed well
statewise_gsdp %>%
  group_by(year, state) %>%
  summarise(total_gsdp = sum(gsdp)) %>%
  arrange(year, desc(total_gsdp)) %>%
  slice(1) -> year_wise_top_state
print(year_wise_top_state)

#4. For every financial year, which state has performed least
statewise_gsdp %>%
  group_by(year, state) %>%
  summarise(total_gsdp = sum(gsdp)) %>%
  arrange(year, total_gsdp) %>%
  slice(1) -> year_wise_least_state
print(year_wise_least_state)

#5. Top 5 performing states in Manufacturing
top5_manufacturing <- statewise_gsdp %>%
  filter(sector == "Manufacturing") %>%               
  group_by(state) %>%                                  
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE)) %>%  
  arrange(desc(total_gsdp)) %>%                        
  slice(1:5)                                           

print(top5_manufacturing)

# #6. Top 5 performing states in Construction
top5_construction <- statewise_gsdp %>%
  filter(sector == "Construction") %>%               
  group_by(state) %>%                                  
  summarise(total_gsdp = sum(gsdp, na.rm = TRUE)) %>%  
  arrange(desc(total_gsdp)) %>%                        
  slice(1:5)                                           

print(top5_construction)


#7. For financial year 2016-17, for every state get top performing sector


state_sector_16_17 <- statewise_gsdp %>%
  filter(year == "2016-17") %>%  
  group_by(state, sector) %>%
  summarise(total_gsdp = sum(gsdp))%>%
  arrange(state, desc(total_gsdp)) %>%
  slice(1)

print(state_sector_16_17)

## for 2015-16

state_sector_15_16 <- statewise_gsdp %>%
  filter(year == "2015-16") %>%  
  group_by(state, sector) %>%
  summarise(total_gsdp = sum(gsdp))%>%
  arrange(state, desc(total_gsdp)) %>%
  slice(1)

print(state_sector_15_16)

#8. For financial year 2016-17, for every state get top 5 performing sectors

top_5_state_sector_16_17 <- statewise_gsdp %>%
  filter(year == "2016-17") %>%  
  group_by(state, sector) %>%
  summarise(total_gsdp = sum(gsdp))%>%
  arrange(state, desc(total_gsdp)) %>%
  slice(1:5)
print(top_5_state_sector_16_17)

# #9. How many states are performing well in Manufacturing, (if Manufacturing is in top 3)
statewise_gsdp %>%
  group_by(state, sector) %>%
  summarise(total_gsdp = sum(gsdp)) %>%
  group_by(state) %>% 
  arrange(desc(total_gsdp)) %>% 
  slice(1:3) %>% 
  filter(sector == "Manufacturing") -> no_of_states_in_top3_manufacturing           
print(no_of_states_in_top3_manufacturing)

# #10. What is the GROSS GSDP of Karnataka for all financial years

statewise_gsdp %>% 
  filter(state == "Karnataka") %>% 
  group_by(year) %>% 
  summarise(total_gsdp = sum(gsdp, na.rm = T))

# 11. for year 2015-16, karnataka state total gsdp of all sector
 Kar_15_16_sector <- statewise_gsdp %>%
  filter(year == "2015-16",state =="Karnataka") %>%
  group_by(state, sector) %>%
  summarise(total_gsdp = sum(gsdp))
print(Kar_15_16_sector)
  


