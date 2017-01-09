#### Load Libraries and Data ####
library(plyr)
library(dplyr)
library(leaflet)
library(tigris)
library(shiny)
library(shinythemes)
library(acs)
####

cny.shp <- tracts(state = 36, county = c("Onondaga", "Oswego", "Cortland", "Madison", "Cayuga"), cb = TRUE)
cny.df <- as.data.frame(cny.shp)



#### Load Shapefiles and Data ####

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
                    Total..Estimate..Population.5.to.9.years...5.to.9.year.olds.enrolled.in.school
                    )




dat3 <- read.csv("data/test3.csv")
estimates3 <- contains("Estimate", vars = names(dat3))
estimates3 <- dat3[estimates3]

estimates3 <- select(estimates3, 
                     Total..Estimate..Worked.full.time..year.round.in.the.past.12.months,
                    Percent.below.poverty.level..Estimate..Population.for.whom.poverty.status.is.determined)
                    


###################################



dat4 <- read.csv("data/test4.csv")
estimates4 <- contains("Estimate", vars = names(dat4))
estimates4 <- dat4[estimates4]

estimates4 <- select(estimates4, 
                    Percent.households.receiving.food.stamps.SNAP..Estimate..Households)



dat <- cbind(geoid,estimates1, estimates2, estimates3, estimates4)

rm(dat1, dat2, dat3, dat4, estimates1, estimates2, estimates3, estimates4)

#### Start geo-join and fix column names


colnames(dat) <- c("geoid", "population", "fempop", "malepop", 
                   "pop5to9", "age", "male.avgage", 
                   "fem.avgage", "over3.privschool", "5to9enrolled",
                   "ftjob.12months", "below.pov", "percent.snap", )






 tables <- acs.lookup(2015, dataset = "acs", keyword = ("income"))






















