library(tidyverse)
library(janitor)
library(stringr)

dir()
dir(path = "GDP Data",
    pattern = "NAD") -> state_files

"GDP data/NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv" %>% 
  read_csv() -> ap_df
ap_df

## for fecting only name of the state from the file nameu using str_split
"NAD-Andhra_Pradesh-GSVA_cur_2016-17.csv" %>% 
  str_split("-") %>% 
  unlist()-> state_name_vector
state_name_vector[2] -> str_name



ap_df %>% 
  slice(-c(7,11,27:33)) %>% ## - is used to except that row number, here we are slicing the required rows
  pivot_longer(c(3:8), names_to = "year",values_to = "gsdp") %>% ### combining all the year columns under one column year and their values into gsdp 
  clean_names() %>% ## cleaning the column names
  select(-1) %>% ## removing the sl.no column
  mutate(state =str_name)## adding new state column into dataframe


## Step -1
## create a loop and iterate over all the first names

tempdf <- tibble()
library(tidyverse)
library(janitor)
for( i in state_files){
  print(paste0("GDP Data",i))

#Step -2 
i %>% 
  str_split("-") %>% 
  unlist()-> state_name_vector
state_name_vector[2] -> str_name

print(paste0("State name:",str_name))

paste0("GDP Data/", i) %>% 
  read_csv()-> st_df1

st_df1%>% 
  slice(-c(7,11,27:33)) %>% ## - is used to except that row number, here we are slicing the required rows
  pivot_longer(-c(1,2), names_to = "year",values_to = "gsdp") %>% ### combining all the year columns under one column year and their values into gsdp 
  clean_names() %>% ## cleaning the column names
  select(-1) %>% ## removing the sl.no column
  mutate(state =str_name) -> state_df 
print(state_df)


bind_rows(tempdf,state_df)-> tempdf
tempdf
}

tempdf -> final_statewise_gsdp

##save final_statewise.gsdp in csv
final_statewise_gsdp %>% 
  write_csv("final_statewise_gsdp.csv")
