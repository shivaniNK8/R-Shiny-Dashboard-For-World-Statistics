library(tidyverse)

#Loading Data
world <- read.csv("www/data.csv", na.strings="")
GH_data <- read.csv("www/GH_data.csv", na.strings="")
continent <- read.csv("./www/continents2.csv", na.strings="")

#merging green house emission data
world <-rbind(world,GH_data)

#Removing rows with country as 'NA'
world<-world %>% drop_na(Country.Name)

#Changing Column Name
names(world)<-c("Indicator","Ind_Code","Country","CODE",rep(1960:2020,each=1))
world <- left_join(world,continent[,c( "alpha.3","region")], by = c("CODE"= "alpha.3")) %>% distinct()

#Wide to long format
world<-world %>%
  gather(Year,value,-c(Indicator,Ind_Code,Country,CODE,region))


#Removing irrelevant column
world<-subset(world,select=-Ind_Code)
world$value <-as.numeric(world$value)

#Long to wide format
df<-world %>%
  pivot_wider(names_from = Indicator,values_from = value)

df<- df %>% 
      rename(pop = `Population, total`,
             lifeExp = `Life expectancy at birth, total (years)`,
             imports = `Imports of goods and services (% of GDP)`,
             exports = `Exports of goods and services (% of GDP)`,
             alcohol_consumption = `Total alcohol consumption per capita (liters of pure alcohol, projected estimates, 15+ years of age)`,
             tobacco_consumption = `Prevalence of current tobacco use (% of adults)`
             )


write_csv(df,"/Users/shivaninaik/Documents/MSDAE/Data Visualisation/World Project/www/final_csv.csv")

model <- lm(`Population, total`~`Life expectancy at birth, total (years)`, df)
DataModel <- mutate(ungroup(df),mod_pred = predict(model),mod_resid = resid(model))
