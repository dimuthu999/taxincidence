rm(list=ls())
setwd("E:/tax_incident")

trim <- function (x) gsub("^\\s+|\\s+$", "", x)

# California_Rent_Data_Jan2017_gt50000.csv was created by Jan262017_CA_Rent_Data_Collection.R.
# Open the file in excel and save as a csv before reading in to R.
# California_Rent_Data.csv has data for zipcodes with population > 50000; collected date 1/26/2017
rent_data <- read.csv(file="California_Rent_Data_Jan2017_gt50000.csv",header = FALSE,stringsAsFactors = FALSE)
rent_data <- rent_data[,1:24]
names(rent_data) <- c("price","zip","beds","baths","sqft","zest_rent","zest_value","zestimate_1year","zest_value_change","zest_rent_change","avg_school_rating","avg_school_distance","description_features","link","no_of_results","tax_history","price_history","neighborhood_desc","walk_score","walk_score_cat","description_sale","transit_score","transit_score_cat","address")
rent_data['rent'] <- sapply(rent_data$price,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
rent_data['city'] <- sapply(rent_data$zip, function(x) substr(x,1,regexpr(", CA",x)[1]-1))
rent_data$zip <- sapply(rent_data$zip,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
rent_data$beds <- ifelse(rent_data$beds=="Studio",1,rent_data$beds)
rent_data$beds <- sapply(rent_data$beds,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
rent_data$baths <- sapply(rent_data$baths,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
rent_data$sqft <- sapply(rent_data$sqft,function(x) as.numeric(gsub("[^0-9]", "", x)))
rent_data$zest_rent <- sapply(rent_data$zest_rent,function(x) as.numeric(gsub("[^0-9]", "", x)))
rent_data$zest_value <- sapply(rent_data$zest_value,function(x) as.numeric(gsub("[^0-9]", "", x)))
rent_data$zestimate_1year <- NULL#as.numeric(gsub("[^0-9]", "", rent_data$zestimate_1year))
rent_data['zest_rent_change_days']<-NULL#str_split_fixed(rent_data$zest_rent_change, "Last", 2)[,2]
rent_data['zest_rent_change']<-NULL#str_split_fixed(rent_data$zest_rent_change, "Last", 2)[,1]
rent_data$zest_rent_change <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", rent_data$zest_rent_change))
rent_data['zest_value_change_days']<-NULL#str_split_fixed(rent_data$zest_value_change, "Last", 2)[,2]
rent_data['zest_value_change']<-NULL#str_split_fixed(rent_data$zest_value_change, "Last", 2)[,1]
rent_data$zest_value_change <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", rent_data$zest_value_change))
rent_data$no_of_results <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", rent_data$no_of_results))
rent_data$walk_score <- NULL#as.numeric(gsub("[^0-9]", "", rent_data$walk_score))
rent_data$transit_score <- NULL#as.numeric(gsub("[^0-9]", "", rent_data$transit_score))

# temp <- nchar(as.character(rent_data$description_features))
# temp <- as.character(rent_data$description_features)[which(temp==max(temp,na.rm = TRUE))]
# temp <- strsplit(temp,"_n_l_")[[1]]

items <- c("Lot:","HOA Fee:","Last sold:","Built in ","Zillow Home ID:","Pets:")
itemnames <- gsub("[: ]","",items)

for(i in 1:nrow(rent_data))  {
  cat(i,"\n")
  j=1
  for(item in items)  {
    temp <- as.character(rent_data[i,'description_features'])
    temp <- strsplit(temp,"_n_l_")[[1]]
    dataitem <- temp[which(substr(temp,1,nchar(item))==item)]
    if((length(dataitem)==1)) {
      rent_data[i,itemnames[j]]<-dataitem
    }
    j=j+1
  }
}

rent_data$Lot <- sapply(rent_data$Lot, function(x) as.numeric(gsub("[^0-9]", "", x)))
rent_data$Builtin <- sapply(rent_data$Builtin,function (x) as.numeric(gsub("[^0-9]", "", x)))
#rent_data$Stories <- as.numeric(gsub("[^0-9]", "", rent_data$Stories))
#rent_data$HOAFee <- as.numeric(gsub("[^0-9]", "", rent_data$HOAFee))
rent_data$ZillowHomeID <- as.numeric(gsub("[^0-9]", "", rent_data$ZillowHomeID))

rent_data <- rent_data[!duplicated(rent_data),]

# for(i in 1:nrow(rent_data))  {
#   temp <- as.character(rent_data[i,]$neighborhood_desc)
#   past12_appr = substr(temp,gregexpr('_n_l_MEDIAN ZESTIMATE_n_l_', temp)[[1]][1]+nchar('_n_l_MEDIAN ZESTIMATE_n_l_'),gregexpr('_n_l_Past 12 months_n_l_', temp)[[1]][1]-1)
#   past12_appr = strsplit(past12_appr,"_n_l_")[[1]][2]
#   
#   future12_appr = substr(temp,gregexpr('_n_l_Zillow predicts', temp)[[1]][1],gregexpr('next year, compared to a', temp)[[1]][1])
#   future12_appr = substr(future12_appr,gregexpr(' home values will ', future12_appr)[[1]][1]+nchar(' home values will '),gregexpr('%', future12_appr)[[1]][1])
#   
#   markettemp <- substr(temp,gregexpr('_n_l_MARKET TEMP _n_l_', temp)[[1]][1]+nchar('_n_l_MARKET TEMP _n_l_'),gregexpr("_n_l_Buyers' Market Sellers' Market", temp)[[1]][1])
#   
#   rent_data[i,'past12_appr'] <- past12_appr
#   rent_data[i,'future12_appr'] <- future12_appr
#   rent_data[i,'markettemp']<-markettemp
# }

rent_data['last_sold_price1'] <- sapply(rent_data$Lastsold,function(x) substr(x,gregexpr("\\$",x)[[1]][1],nchar(x)))
rent_data['last_sold_date1'] <- sapply(rent_data$Lastsold,function(x) substr(x,11,gregexpr(" for",x)[[1]][1]))
rent_data$last_sold_price1 <-  sapply(rent_data$last_sold_price1,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
rent_data$last_sold_date1 <- sapply(rent_data$last_sold_date1,function (x) gsub(pattern = " ",x=trim(x),replacement = "-"))
rent_data$last_sold_date1 <- sapply(rent_data$last_sold_date1,function(x) as.Date(paste("01-", x, sep = ""), format = "%d-%b-%Y"))

# Past Price Data ---------------------------------------------------------

for(i in 1:nrow(rent_data))  {
  tryCatch({
    temp <- as.character(rent_data[i,'price_history'])
    if(is.na(temp)) next
    temp <- strsplit(temp,"_n_l_")[[1]]
    #temp <- temp[3:(length(temp)-1)]
    
    for(j in 1:length(temp))  {
      if(tolower(substr(temp[j],10,13))=="sold") {
        rent_data[i,'lastsold2'] = temp[j]
        break
      }
    }
  },error=function(cond) {})
}

rent_data['last_sold_date2']<- sapply(rent_data$lastsold2, function(x) substr(x,1,8))
rent_data$last_sold_date2 <- sapply(rent_data$last_sold_date2,function(x) as.Date(x, "%m/%d/%y"))
rent_data['last_sold_price2'] <- sapply(rent_data$lastsold2, function(x) gsub("[+-]"," ",x))
rent_data['last_sold_price2'] <- sapply(rent_data$last_sold_price2, function(x) substr(x,gregexpr("\\$",x)[[1]][1],nchar(x)))
rent_data['last_sold_price2'] <- sapply(rent_data$last_sold_price2, function(x) substr(x,1,gregexpr(" ",x)[[1]][1]))
rent_data['last_sold_price2'] <- sapply(rent_data$last_sold_price2, function(x) as.numeric(gsub("[^\\.0-9]", "", x)))


rent_data['last_sold_date'] <- as.Date(ifelse(rent_data$last_sold_date2>rent_data$last_sold_date1 | is.na(rent_data$last_sold_date1),
                                              rent_data$last_sold_date2,rent_data$last_sold_date1),origin = "1970-01-01")
rent_data['last_sold_price'] <- ifelse(rent_data$last_sold_date2>rent_data$last_sold_date1 | is.na(rent_data$last_sold_date1),
                                       rent_data$last_sold_price2,rent_data$last_sold_price1)
rent_data$Pets <- ifelse(rent_data$Pets=="Pets: No","No",
                         ifelse(is.na(rent_data$Pets),NA,"Yes"))

rent_data <- rent_data[,!names(rent_data) %in% c("HOAFee","lastsold2","last_sold_price2","last_sold_price1",
                                                "last_sold_date2","last_sold_date1","Lastsold","transit_score_cat",
                                                "walk_score_cat","neighborhood_desc","price_history","description_sale",
                                                "tax_history","link","description_features","price","address")]
saveRDS(rent_data,file = "California_Rent_Data.rds")

library(stargazer)
library(sandwich)


# # Past Tax Data ----------------------------------------------------------------
# 
# tax_data_file <- "break_zips_tax_data.csv"
# 
# rent_data <- rent_data[!duplicated(rent_data$ZillowHomeID),]
# rent_data <- rent_data[,c("zip","tax_history","ZillowHomeID","price_history")]
# write.table(t(c("sep=|\nZillowHomeID","zip","year","tax_amount","tax_value")),file=paste(tax_data_file,sep=""),append = FALSE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
# for(i in 1:nrow(rent_data))  {
#   cat(i," ")
#   
#   temp <- as.character(rent_data[i,'tax_history'])
#   if(is.na(temp)) next
#   temp <- strsplit(temp,"_n_l_")[[1]]
#   temp <- temp[5:length(temp)]
#   
#   for(j in 1:length(temp))  {
#     temp2 <- str_split_fixed(temp[[j]], " ", 5)
#     write.table(t(c(rent_data[i,'ZillowHomeID'],rent_data[i,'zip'],temp2[1,1],temp2[1,2],temp2[1,4])),file=paste(tax_data_file,sep=""),append = TRUE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
#   }
# }

