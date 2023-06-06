

# Clean Data Access Eletricity --------------------------------------------

access <- read.csv2(file = "data/csv/ACCESO/access_electricity.csv")
head(access)

access_clean <- access %>% 
  pivot_longer(cols = starts_with("x"), names_to = "Year") %>% 
  mutate(Year = as.numeric(gsub("X", "", Year))) %>% 
  filter(Year>=1990) %>% 
  janitor::clean_names()


saveRDS(access_clean, "data/rds/acc_electricity.rds")
LA_chile_acc_elect <- access_clean %>% 
  filter(country_code =="CHL"|country_code =="TLA")
saveRDS(LA_chile_acc_elect, "data/rds/acc_electricity_CHL_LA.rds")


# Access Urban ------------------------------------------------------------


access_urb <- read.csv2(file = "data/csv/URBANO/access_urban.csv")
head(access_urb)

access_clean_urb <- access_urb %>% 
  pivot_longer(cols = starts_with("x"), names_to = "Year") %>% 
  mutate(Year = as.numeric(gsub("X", "", Year))) %>% 
  filter(Year>=1990) %>% 
  janitor::clean_names()

saveRDS(access_clean_urb, "data/rds/acc_electricity_urb.rds")



# Access Rural ------------------------------------------------------------

access_rural <- read.csv2(file = "data/csv/RURAL/access_rural.csv")
head(access_rural)

access_clean_rur <- access_rural %>% 
  pivot_longer(cols = starts_with("x"), names_to = "Year") %>% 
  mutate(Year = as.numeric(gsub("X", "", Year))) %>% 
  filter(Year>=1990) %>% 
  janitor::clean_names()

saveRDS(access_clean_rur, "data/rds/acc_electricity_rur.rds")

