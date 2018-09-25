rm(list = ls(all=TRUE))

#install.packages("RMySQL")
#libraries
library(dplyr)
library(RMySQL)
library(xlsx)
library(stringr)

#variable
OEM <- 'HONDA'
CARNAME <- 'BR-V'
EMI <- '26599'
link_car_name <- 'honda-br-v'
color_options <- '5'

#db connection
#connection <- dbConnect(MySQL(), user='readonlymudit', 
#password='jJBhgtNO3MPGcqXQYCwm', dbname='carsinfo', host='1.0.26.74')

#retrieving data from MySQL:
#fill your carname in place of car.CarName
#query <- dbSendQuery(connection, "SELECT car.OEMName,car.CarName,city.CityName, MIN(carprice.ExShowRoomPrice) AS 'ExShowRoomPrice',
#car.arai_mileage,car.FuelCapacity
#FROM car 
#INNER JOIN carprice ON car.CarVariantId = carprice.CarVariantId
#INNER JOIN city ON city.CityId = carprice.CityId
#WHERE car.CarName LIKE '%mahindra%xylo'
#AND (StartDate < CURDATE() AND EndDate > CURDATE())
#GROUP BY carprice.CityId
#ORDER BY city.CityName ASC")
#data <- fetch(query, n=-1)

input_query <- "SELECT 
car.OEMName,
car.CarName,
city.CityName,
MIN(carprice.ExShowRoomPrice) AS 'ExShowRoomPrice',
car.arai_mileage,car.FuelCapacity
FROM car 
INNER JOIN carprice ON car.CarVariantId = carprice.CarVariantId
INNER JOIN city ON city.CityId = carprice.CityId
WHERE car.CarName LIKE 'honda br%v%'
AND (StartDate < CURDATE() AND EndDate > CURDATE())
GROUP BY carprice.CityId
ORDER BY city.CityName ASC"

#sql_query <- function(query) {
# creating DB connection object with RMysql package
db <- dbConnect(MySQL(),user="readonlymudit",
                password='jJBhgtNO3MPGcqXQYCwm',
                dbname='carsinfo',
                host='1.0.26.74')
# send Query to obtain result set
rs <- dbSendQuery(db,input_query)
# get elements from result sets and convert to dataframe
data <- fetch(rs, -1)
# return the dataframe
#return(data)
huh <- dbHasCompleted(rs)
dbClearResult(rs)
# close db connection after function call exits
on.exit(dbDisconnect(db))
#}

#sql_query(input_query)

#for killing all the open connections
killDbConnections <- function () {
  all_cons <- dbListConnections(MySQL())
  print(all_cons)
  for(con in all_cons)+ dbDisconnect(con)
  print(paste(length(all_cons), " connections killed."))
}

killDbConnections()

rounding_exshowroomprice <- function(x){
  x <- x/100000
  round(x,digits = 1)
}

#rounding the ExShowRoomPrice(lowest variant price) to 1 decimal
data$ExShowRoomPrice <- rounding_exshowroomprice(data$ExShowRoomPrice)

#cbind emi_amount,landing_page,color
emi_amount <- rep(EMI,length(data$CityName))
data <- cbind.data.frame(data,emi_amount)

landing_page <- paste("https://www.cardekho.com",link_car_name,"car-price-in-",sep="/")

city <- data$CityName
city <- tolower(city)

#initiating a vector of length 0
landing_page_path <- 0
#multi-lenght link formation with concatenating standard link and lowercase cities
for (i in 1:nrow(data)) {
  landing_page_path[i] <- paste(landing_page,city[i],".htm",sep="")
}

#cbinding landing_page_path vector with the dataframe
data <- cbind.data.frame(data,landing_page_path)

#initiating a vector of length 0
color_options_available <- 0
#multi-length color options as number of rows
for (i in 1:nrow(data)) {
  color_options_available[i] <- paste(color_options,"color options",sep = " ")
}

#cbinding color_options_avaiable vector with the dataframe
data <- cbind.data.frame(data,color_options_available)

#constant field
populate <- data

#templates to be populated
campaign <- read.csv('campaigns.csv',header = TRUE)
adgroup <- read.csv('adgroup.csv',header = TRUE)
keyword <- read.csv('keywords.csv',header = TRUE)
negative <- read.csv('negative.csv',header = TRUE)
location <- read.csv('location.csv',header = TRUE)
ad <- read.csv('ads.csv',header = TRUE)
#sitelinks <- read.csv('sitelinks.csv',header = TRUE)
#callouts <- read.csv('callouts.csv',header = TRUE)

#all templates to be converted to character format and then converted to a dataframe
#1
campaign <- apply(campaign,2,as.character)
campaign <- as.data.frame(campaign)

#2
adgroup <- apply(adgroup,2,as.character)
adgroup <- as.data.frame(adgroup)

#3
keyword <- apply(keyword,2,as.character)
keyword <- as.data.frame(keyword)

#4
negative <- apply(negative,2,as.character)
negative <- as.data.frame(negative)

#5
location <- apply(location,2,as.character)
location <- as.data.frame(location)

#6
ad <- apply(ad,2,as.character)
ad <- as.data.frame(ad)

#7
#sitelinks <- apply(sitelinks,2,as.character)
#sitelinks <- as.data.frame(sitelinks)

#8
#callouts <- apply(callouts,2,as.character)
#callouts <- as.data.frame(callouts)

#copy campaign n times as number of cities and then rbinding it as a whole
#populating campaign
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- campaign
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
#rowbinding all the city filled campaign template
campaign <- do.call(rbind, datalist)
write.csv(campaign,"campaign_populated.csv",row.names = FALSE,na = "")
#####################################################################################################

#copy adgroup n times as number of cities and then rbinding it as a whole
#populating adgroup
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- adgroup
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
  
  # type_1 <- paste(OEM,CARNAME,PRICE,"Jaipur",sep = '+')
  # type_2 <- paste(CARNAME,PRICE,"Jaipur",sep = '+')
  # type_3 <- paste(OEM,CARNAME,PRICE,sep = '+')
  # type_4 <- paste(CARNAME,PRICE,sep = '+')
  # 
  # adgroup_ads <- datalist[,33]
  # 
  # if (nchar(adgroup_ads[i,]) > 30)
  #   {adgroup_ads[i,] <- type_1}
  # else if (nchar(adgroup_ads[i,]) == 30)
  #   {next}
  # else if (nchar(adgroup_ads[i,]) < 30)
  #   {next}
  # else
  #   {next}
# 
# adgroup <- read.csv("adgroup_populated.csv",header = TRUE,na.strings = c('',' ','NA'))
# adgroup_ads <- adgroup[,33]
# adgroup_ads <- as.data.frame(adgroup_ads)
# str(adgroup_ads)
# more_than_30 <- 0
# sum(is.na(adgroup_ads))
#adgroup_ads <- as.character(adgroup_ads)
#adgroup_ads_length <- apply(adgroup_ads, 2, nchar)

# for(i in 1:nrow(adgroup_ads))
# {
#   if(nchar(adgroup_ads[i,]) > 30 && adgroup_ads[i,] != 'NA')
#   {
#     more_than_30[i,] <- adgroup_ads[i,]
#   }
#   else
#     next
# }
#rowbinding all the city filled campaign template
adgroup <- do.call(rbind, datalist)
write.csv(adgroup,"adgroup_populated.csv",row.names = FALSE,na = "NA")

#####################################################################################################

#copy keyword n times as number of cities and then rbinding it as a whole
#populating keyword
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- keyword
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
#rowbinding all the city filled campaign template
keyword <- do.call(rbind, datalist)
write.csv(keyword,"keyword_populated.csv",row.names = FALSE,na = "")
#####################################################################################################

#copy negative n times as number of cities and then rbinding it as a whole
#populating negative
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- negative
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
#rowbinding all the city filled campaign template
negative <- do.call(rbind, datalist)
write.csv(negative,"negative_populated.csv",row.names = FALSE,na = "")
#####################################################################################################

#copy location n times as number of cities and then rbinding it as a whole
#populating location
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- location
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
#rowbinding all the city filled campaign template
location <- do.call(rbind, datalist)
write.csv(campaign,"location_populated.csv",row.names = FALSE,na = "")
#####################################################################################################

#copy ad n times as number of cities and then rbinding it as a whole
#populating ad
datalist <- list()
for (i in 1:nrow(populate)) {
  data <- ad
  current_city <- populate[i,3]
  current_city_amount <- populate[i,4]
  mileage <- populate[i,5]
  current_city_emi <- populate[i,7]
  current_lp_link <- populate[i,8]
  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
  data$i <- i  # maybe you want to keep track of which iteration produced it?
  datalist[[i]] <- data # add it to your list
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
}
#rowbinding all the city filled campaign template
ad <- do.call(rbind, datalist)
write.csv(ad,"ad_populated.csv",row.names = FALSE,na = "")
#####################################################################################################

#copy sitelinks n times as number of cities and then rbinding it as a whole
#populating sitelinks
#datalist <- list()
#for (i in 1:nrow(populate)) {
#  data <- sitelinks
#  current_city <- populate[i,3]
#  current_city_amount <- populate[i,4]
#  mileage <- populate[i,5]
#  current_city_emi <- populate[i,7]
#  current_lp_link <- populate[i,8]
#  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
#  data$i <- i  # maybe you want to keep track of which iteration produced it?
#  datalist[[i]] <- data # add it to your list
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
#}
#rowbinding all the city filled campaign template
#sitelinks <- do.call(rbind, datalist)

#####################################################################################################

#copy callouts n times as number of cities and then rbinding it as a whole
#populating callouts
#datalist <- list()
#for (i in 1:nrow(populate)) {
#  data <- callouts
#  current_city <- populate[i,3]
#  current_city_amount <- populate[i,4]
#  mileage <- populate[i,5]
#  current_city_emi <- populate[i,7]
#  current_lp_link <- populate[i,8]
#  color <- populate[i,9]
  
  #data.replaceAll(oem,cuuren[2])
  #data.replaceAll(model,cuuren[model])
#  data$i <- i  # maybe you want to keep track of which iteration produced it?
#  datalist[[i]] <- data # add it to your list
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<brand>", OEM, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<oem>", OEM, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<model>", CARNAME, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<city>", current_city, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<landing page>", current_lp_link, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<emi-amount>", current_city_emi, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<mileage>", mileage, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<amount>", current_city_amount, x)}))
#  datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<color>", color, x)}))
  #datalist[[i]] <- as.data.frame(lapply(datalist[[i]], function(x) {gsub("<variant>", city_variant, x)}))
#}
#rowbinding all the city filled campaign template
#callouts <- do.call(rbind, datalist)

#####################################################################################################


#to kill all the db connections at once
#when you open a connection then close it as well
#killDbConnections <- function () {
#all_cons <- dbListConnections(MySQL())
#print(all_cons)
#for(con in all_cons)+ dbDisconnect(con)
#  print(paste(length(all_cons), " connections killed."))
#}

#killDbConnections()

# adgroup_ads <- adgroup[,33]
# 
# for(i in 1:nrow(adgroup_ads))
# {
#   if (nchar(adgroup_ads[i,]) > 30)
#     adgroup_ads[i,] <- type_1
#   else if (nchar(adgroup_ads[i,]) == 30)
#     next
#   else if (nchar(adgroup_ads[i,]) < 30)
#     next
#   else
#     next
# }
# 
# type_1 <- paste(OEM,CARNAME,PRICE,"Jaipur",sep = '+')
# type_2 <- paste(CARNAME,PRICE,"Jaipur",sep = '+')
# type_3 <- paste(OEM,CARNAME,PRICE,sep = '+')
# type_4 <- paste(CARNAME,PRICE,sep = '+')

