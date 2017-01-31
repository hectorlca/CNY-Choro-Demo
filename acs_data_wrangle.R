
#### 1) Load Libraries ####

library(plyr)
library(dplyr)
library(leaflet)
library(tigris)
library(shiny)
library(shinythemes)
library(acs)
library(ggmap)
library(ggplot2)








#### 2) Load & Clean ACS Data -- Data is found in 4 different tables ####

onondaga <- counties(state = "NY", cb = TRUE)
onondaga <- onondaga[onondaga$NAME == "Onondaga",]

districts <- school_districts(state = "NY", unified)
         

dat1 <- read.csv("data/test.csv")
geoid <- dat1$Id
estimates1 <- contains("Estimate", vars = names(dat1))
estimates1 <- dat1[estimates1]

estimates1 <- select(estimates1, 
                    Total..Estimate..Total.population,
                    Female..Estimate..Total.population,
                    Male..Estimate..Total.population,
                    Total..Estimate..AGE...5.to.9.years,
                    Total..Estimate..SUMMARY.INDICATORS...Median.age..years.,
                    Male..Estimate..SUMMARY.INDICATORS...Median.age..years.,
                    Female..Estimate..SUMMARY.INDICATORS...Median.age..years.
                    
                    )



dat2 <- read.csv("data/test2.csv")
estimates2 <- contains("Estimate", vars = names(dat2))
estimates2 <- dat2[estimates2]

estimates2 <- select(estimates2, 
                    Percent.in.private.school..Estimate..Population.3.years.and.over.enrolled.in.school,
                    Total..Estimate..Population.5.to.9.years...5.to.9.year.olds.enrolled.in.school)

dat3 <- read.csv("data/test3.csv")
estimates3 <- contains("Estimate", vars = names(dat3))
estimates3 <- dat3[estimates3]

estimates3 <- select(estimates3, 
                     Total..Estimate..Worked.full.time..year.round.in.the.past.12.months,
                    Percent.below.poverty.level..Estimate..Population.for.whom.poverty.status.is.determined)


dat4 <- read.csv("data/test4.csv")
estimates4 <- contains("Estimate", vars = names(dat4))
estimates4 <- dat4[estimates4]

estimates4 <- select(estimates4, 
                    Percent.households.receiving.food.stamps.SNAP..Estimate..Households)

dat <- cbind(geoid,estimates1, estimates2, estimates3, estimates4)

rm(dat1, dat2, dat3, dat4, estimates1, estimates2, estimates3, estimates4)






colnames(dat) <- c("geoid", "population", "fempop", "malepop", 
                   "pop5to9", "age", "male.avgage", 
                   "fem.avgage", "over3.privschool", "5to9enrolled",
                   "ftjob.12months", "below.pov", "percent.snap" )









#### 3) Load and Wrangle NYSHD Lead Dataset ####

dat <- read.csv("data/school_water.csv")

dat <- select(dat, School, BEDS.Code, School.District, County, Number.of.Outlets, 
              Any.Buildings.with.Lead.Free.Plumbing., Sampling.Complete, Sampling.Completion.Date,
              Number.of.Outlets..Result.â...15.ppb, Number.of.Outlets..Result...15.ppb, Out.of.Service,
              School.Street, School.City, School.State)
        
colnames(dat) <- c("School", "BEDS", "District", "County", "num.outlets", "pbfreeBuildings",
                   "samplingComplete", "completionDate", "outletsOver15", "outletsUnder15", "oos",
                   "street", "city", "state") 

dat <- filter(dat, County == "Onondaga") %>%
       na.omit()

dat$address <- paste(sep = ", ", dat$street, dat$city, dat$state )

#coords <- geocode(dat$address)

dat <- cbind(dat, coords) %>%
       na.omit()

dat$percent.bad <- (dat$outletsOver15/dat$num.outlets)

# Define circle sizes (and color?)
dat$dotsize = NA
dat$dotsize <- ifelse(dat$percent.bad < .85, yes = 1.2, no = dat$dotsize)
dat$dotsize <- ifelse(dat$percent.bad > .85 & dat$percent.bad < .90, yes = 1.5, no = dat$dotsize)
dat$dotsize <- ifelse(dat$percent.bad > .90 & dat$percent.bad < .95, yes = 1.8, no = dat$dotsize)
dat$dotsize <- ifelse(dat$percent.bad > .95, yes = 2.4, no = dat$dotsize)
  



#### 4) Rewrite School Districts ####

dat$cleanDist <- "tbd"
dat$cleanDist <- ifelse(grepl("FAYVILL", dat$District, ignore.case = TRUE ), 
                        yes = "Fayetteville-Manlius CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("No District", dat$District, ignore.case = TRUE), 
                        yes = "BOCES (No Name)", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("SYRACUSE-MINOA", dat$District, ignore.case = TRUE), 
                        yes = "East Syracuse-Minoa CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("BALDWINSVILLE", dat$District, ignore.case = TRUE), 
                        yes = "Baldwinsville CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("ONONDAGA", dat$District, ignore.case = TRUE), 
                        yes = "Onondaga CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("JAMESVILLE", dat$District, ignore.case = TRUE), 
                        yes = "Jamesville-DeWitt CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("LIVERPOOL", dat$District, ignore.case = TRUE), 
                        yes = "Liverpool CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("JORDAN ELBRIDGE", dat$District, ignore.case = TRUE), 
                        yes = "Jordan Elbridge CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("NORTH SYRACUSE", dat$District, ignore.case = TRUE), 
                        yes = "North Syracuse CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("LYNCOURT UNION", dat$District, ignore.case = TRUE), 
                        yes = "Lyncourt Union", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("LA FAYETTE", dat$District, ignore.case = TRUE), 
                        yes = "La Fayette CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("Marcellus", dat$District, ignore.case = TRUE), 
                        yes = "Marcellus CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("SKANEATELES", dat$District, ignore.case = TRUE), 
                        yes = "Skaneateles CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("SYRACUSE CITY", dat$District, ignore.case = TRUE), 
                        yes = "Syracuse City School District", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("West Genesee", dat$District, ignore.case = TRUE), 
                        yes = "West Genesee @ Camillus", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("Westhill", dat$District, ignore.case = TRUE), 
                        yes = "Westhill CSD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("Solvay Union", dat$District, ignore.case = TRUE), 
                        yes = "Solvay Union Free SD", no = dat$cleanDist)
dat$cleanDist <- ifelse(grepl("Tully Central", dat$District, ignore.case = TRUE), 
                        yes = "Tully CSD", no = dat$cleanDist)

# Save data frame to a file

write.csv(dat, "data/school_lead.csv")




#### 5) Leaflet Map ####

leaflet() %>%
  setView(lng=-76.13, lat=43.03, zoom=10) %>%
  addProviderTiles("Esri.WorldStreetMap") %>%
  addPolygons(data = onondaga, fill = FALSE) %>%
  addCircles(
    lat = dat$lat, lng = dat$lon,
    radius = (dat$dotsize*400),
    color = "#de2d26",
    stroke = FALSE, fillOpacity = 0.5)


  
  
  























