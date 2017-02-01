simpleCap <- function(x) {
  s <- strsplit(x, " ")[[1]]
  paste(toupper(substring(s, 1, 1)), substring(s, 2),
        sep = "", collapse = " ")}

#Get the wrangled and geocoded data 

dat <- read.csv("data/lead2.csv")
dat$address <- paste(dat$School.Street, dat$School.City, dat$School.State, sep = ", ")

dat$School.Street <- NULL
dat$School.State <- NULL
dat$School.City <- NULL

cords <- geocode(dat$address)

dat <- cbind(dat, cords)



#### Get BEDS Data and prep for merge ####

beds <- read.csv("data/beds.csv") %>%
  select(ENTITY_CD, ENTITY_NAME, County, District.Name, ACCOUNTABILITYMEASURE)

colnames(beds) <- c("BEDS", "School", "County", "District", "Type")

beds$School <- tolower(beds$School) # fix upper/lower cases
beds$School <- sapply(beds$School, simpleCap)

beds.dat <- data.frame(BEDS = beds$BEDS, School = beds$School, Type = beds$Type, District = beds$District)
beds.dat <- unique(beds.dat)

rm(beds)

#Get rid of duplicates to reache uniqueID table

beds.dat$short <- ""

beds.dat$short <- ifelse(grepl("Elementary/Middle", beds.dat$Type, ignore.case = TRUE), 
                         yes = "Elementary/Middle", no = beds.dat$short)

beds.dat$short <- ifelse(grepl("High", beds.dat$Type, ignore.case = TRUE),
                         yes = "High School", no = beds.dat$short)

beds.dat$Type <- NULL
beds.dat$BEDS <- factor(beds.dat$BEDS)
beds.dat$School <- factor(beds.dat$School)
beds.dat <- unique(beds.dat) # Done with unique table

beds.dat$BEDS <- as.numeric(as.character(beds.dat$BEDS))


#### Merge ####
merged <- left_join (dat, beds.dat, "BEDS")

merged <- unique(merged)

write.csv(merged, "data/lead.csv")






################# EVERYTHING ABOVE IS A FUCKING MESS BUT AT SOME POINT, I GOT THE FILE I NEEDED
### BUT NEEDS GEOCODING

lead <- read.csv("data/lead.csv")
lead <- na.omit(lead)

coords <- geocode(as.character(lead$address))

lead <- cbind(lead, coords)

lead$X <- NULL
lead <- select(lead, School.y, short, District, BEDS, num.outlets , outletsOver15, oos, lon, lat )
colnames(lead) <- c("School", "Type", "District", "BEDS", "num.outlets", "outletsOver15", "oos", "lon", "lat")


#### Rewrite Districts ####

lead$cleanDist <- "tbd"
lead$cleanDist <- ifelse(grepl("FAYVILL", lead$District, ignore.case = TRUE ), 
                        yes = "Fayetteville-Manlius CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("No District", lead$District, ignore.case = TRUE), 
                        yes = "BOCES (No Name)", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("SYRACUSE-MINOA", lead$District, ignore.case = TRUE), 
                        yes = "East Syracuse-Minoa CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("BALDWINSVILLE", lead$District, ignore.case = TRUE), 
                        yes = "Baldwinsville CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("ONONDAGA", lead$District, ignore.case = TRUE), 
                        yes = "Onondaga CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("JAMESVILLE", lead$District, ignore.case = TRUE), 
                        yes = "Jamesville-DeWitt CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("LIVERPOOL", lead$District, ignore.case = TRUE), 
                        yes = "Liverpool CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("JORDAN ELBRIDGE", lead$District, ignore.case = TRUE), 
                        yes = "Jordan Elbridge CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("NORTH SYRACUSE", lead$District, ignore.case = TRUE), 
                        yes = "North Syracuse CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("LYNCOURT UNION", lead$District, ignore.case = TRUE), 
                        yes = "Lyncourt Union", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("LA FAYETTE", lead$District, ignore.case = TRUE), 
                        yes = "La Fayette CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("Marcellus", lead$District, ignore.case = TRUE), 
                        yes = "Marcellus CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("SKANEATELES", lead$District, ignore.case = TRUE), 
                        yes = "Skaneateles CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("SYRACUSE CITY", lead$District, ignore.case = TRUE), 
                        yes = "Syracuse City School District", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("West Genesee", lead$District, ignore.case = TRUE), 
                        yes = "West Genesee @ Camillus", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("Westhill", lead$District, ignore.case = TRUE), 
                        yes = "Westhill CSD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("Solvay Union", lead$District, ignore.case = TRUE), 
                        yes = "Solvay Union Free SD", no = lead$cleanDist)
lead$cleanDist <- ifelse(grepl("Tully Central", lead$District, ignore.case = TRUE), 
                        yes = "Tully CSD", no = lead$cleanDist)

lead$District <- lead$cleanDist
lead(d)

lead$cleanDist <- NULL




write.csv(lead, "data/final_lead.csv")
