rm(list=ls())
setwd("E:/tax_incident")
library(plyr)
library(lubridate)

trim <- function (x) gsub("^\\s+|\\s+$", "", x)
r = 0.03

# California_Sold_Data_Jan2017_sample1.csv and California_Sold_Data_Jan2017_sample2.csv were created by Jan262017_CA_Sold_Data_Collection.R
# Open the file in excel and save as a csv before reading in to R.
# These files have data for zipcodes with population > 50000; collected date January 2017

sold_data <- list.files(path="E:/tax_incident/sales_data",pattern = "^California.*.csv",full.names = TRUE)
sold_data <- lapply(sold_data,function(x) read.csv(x, stringsAsFactors = FALSE,header = FALSE))
sold_data <- ldply(sold_data,data.frame)

sold_data <- sold_data[,1:24]
names(sold_data) <- c("price","zip","beds","baths","sqft","zest_value","zest_rent","zestimate_1year","zest_value_change","zest_rent_change","avg_school_rating","avg_school_distance","description_features","link","no_of_results","tax_history","price_history","neighborhood_desc","walk_score","walk_score_cat","description_sale","transit_score","transit_score_cat","address")
sold_data['sold_price'] <- sapply(sold_data$price,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
sold_data['city'] <- sapply(sold_data$zip, function(x) substr(x,1,regexpr(", CA",x)[1]-1))
sold_data$zip <- sapply(sold_data$zip,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
sold_data$beds <- ifelse(sold_data$beds=="Studio",1,sold_data$beds)
sold_data$beds <- sapply(sold_data$beds,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
sold_data$baths <- sapply(sold_data$baths,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
sold_data$sqft <- sapply(sold_data$sqft,function(x) as.numeric(gsub("[^0-9]", "", x)))
sold_data$zest_rent <- sapply(sold_data$zest_rent,function(x) as.numeric(gsub("[^0-9]", "", x)))
sold_data$zest_value <- sapply(sold_data$zest_value,function(x) as.numeric(gsub("[^0-9]", "", x)))
sold_data$zestimate_1year <- NULL#as.numeric(gsub("[^0-9]", "", sold_data$zestimate_1year))
sold_data['zest_rent_change_days']<-NULL#str_split_fixed(sold_data$zest_rent_change, "Last", 2)[,2]
sold_data['zest_rent_change']<-NULL#str_split_fixed(sold_data$zest_rent_change, "Last", 2)[,1]
sold_data$zest_rent_change <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", sold_data$zest_rent_change))
sold_data['zest_value_change_days']<-NULL#str_split_fixed(sold_data$zest_value_change, "Last", 2)[,2]
sold_data['zest_value_change']<-NULL#str_split_fixed(sold_data$zest_value_change, "Last", 2)[,1]
sold_data$zest_value_change <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", sold_data$zest_value_change))
sold_data$no_of_results <- NULL#as.numeric(gsub("[^0-9\\+\\-]", "", sold_data$no_of_results))
sold_data$walk_score <- as.numeric(gsub("[^0-9]", "", sold_data$walk_score))
sold_data$transit_score <- as.numeric(gsub("[^0-9]", "", sold_data$transit_score))

# temp <- nchar(as.character(sold_data$description_features))
# temp <- as.character(sold_data$description_features)[which(temp==max(temp,na.rm = TRUE))]
# temp <- strsplit(temp,"_n_l_")[[1]]

items <- c("Lot:","HOA Fee:","Last sold:","Built in ","Zillow Home ID:","Last remodel year:")
itemnames <- gsub("[: ]","",items)

for(i in 1:nrow(sold_data))  {
  cat(i,"\n")
  j=1
  for(item in items)  {
    temp <- as.character(sold_data[i,'description_features'])
    temp <- strsplit(temp,"_n_l_")[[1]]
    dataitem <- temp[which(substr(temp,1,nchar(item))==item)]
    if((length(dataitem)==1)) {
      sold_data[i,itemnames[j]]<-dataitem
    }
    j=j+1
  }
}

sold_data$Lot <- sapply(sold_data$Lot, function(x) as.numeric(gsub("[^0-9]", "", x)))
sold_data$Builtin <- sapply(sold_data$Builtin,function (x) as.numeric(gsub("[^0-9]", "", x)))
#sold_data$Stories <- as.numeric(gsub("[^0-9]", "", sold_data$Stories))
#sold_data$HOAFee <- as.numeric(gsub("[^0-9]", "", sold_data$HOAFee))
sold_data$ZillowHomeID <- as.numeric(gsub("[^0-9]", "", sold_data$ZillowHomeID))
sold_data$Lastremodelyear <- sapply(sold_data$Lastremodelyear,function (x) as.numeric(gsub("[^0-9]", "", x)))

sold_data <- sold_data[!duplicated(sold_data),]

# for(i in 1:nrow(sold_data))  {
#   temp <- as.character(sold_data[i,]$neighborhood_desc)
#   past12_appr = substr(temp,gregexpr('_n_l_MEDIAN ZESTIMATE_n_l_', temp)[[1]][1]+nchar('_n_l_MEDIAN ZESTIMATE_n_l_'),gregexpr('_n_l_Past 12 months_n_l_', temp)[[1]][1]-1)
#   past12_appr = strsplit(past12_appr,"_n_l_")[[1]][2]
#   
#   future12_appr = substr(temp,gregexpr('_n_l_Zillow predicts', temp)[[1]][1],gregexpr('next year, compared to a', temp)[[1]][1])
#   future12_appr = substr(future12_appr,gregexpr(' home values will ', future12_appr)[[1]][1]+nchar(' home values will '),gregexpr('%', future12_appr)[[1]][1])
#   
#   markettemp <- substr(temp,gregexpr('_n_l_MARKET TEMP _n_l_', temp)[[1]][1]+nchar('_n_l_MARKET TEMP _n_l_'),gregexpr("_n_l_Buyers' Market Sellers' Market", temp)[[1]][1])
#   
#   sold_data[i,'past12_appr'] <- past12_appr
#   sold_data[i,'future12_appr'] <- future12_appr
#   sold_data[i,'markettemp']<-markettemp
# }

# sold_data['last_sold_price1'] <- sapply(sold_data$Lastsold,function(x) substr(x,gregexpr("\\$",x)[[1]][1],nchar(x)))
# sold_data['last_sold_date1'] <- sapply(sold_data$Lastsold,function(x) substr(x,11,gregexpr(" for",x)[[1]][1]))
# sold_data$last_sold_price1 <-  sapply(sold_data$last_sold_price1,function(x) as.numeric(gsub("[^\\.0-9]", "", x)))
# sold_data$last_sold_date1 <- sapply(sold_data$last_sold_date1,function (x) gsub(pattern = " ",x=trim(x),replacement = "-"))
# sold_data$last_sold_date1 <- sapply(sold_data$last_sold_date1,function(x) as.Date(paste("01-", x, sep = ""), format = "%d-%b-%Y"))

# Past Price Data ---------------------------------------------------------

for(i in 1:nrow(sold_data))  {
  cat(i," ")
  tryCatch({
    temp <- as.character(sold_data[i,'price_history'])
    if(is.na(temp)) next
    temp <- strsplit(temp,"_n_l_")[[1]]
    #temp <- temp[3:(length(temp)-1)]
    soldindex = 1
    for(j in 1:length(temp))  {
      if(tolower(substr(temp[j],10,24))=="listed for sale") {
        sold_data[i,paste('listedforsale',soldindex,sep = "")] = temp[j]
        soldindex=soldindex+1
        for(k in j+1:length(temp)) {
          if(tolower(substr(temp[k],10,24))=="listed for sale") {
            sold_data[i,paste('listedforsale',soldindex,sep = "")] = temp[k]
            break
          }
          
        }
        
        break
      }
    }
  },error=function(cond) {})
  
  # tryCatch({
  #   temp <- as.character(sold_data[i,'price_history'])
  #   if(is.na(temp)) next
  #   temp <- strsplit(temp,"_n_l_")[[1]]
  #   rentindex = 1
  #   for(j in 1:length(temp))  {
  #     if(tolower(substr(temp[j],10,24))=="listed for rent") {
  #       sold_data[i,paste('listedforrent',rentindex,sep = "")] = temp[j]
  #       for(k in j-1:1) {
  #         if(tolower(substr(temp[k],10,24))=="listed for rent") {
  #           break
  #         }
  #         if(tolower(substr(temp[k],10,24))=="listing removed") {
  #           sold_data[i,paste('rentlistingremoved',rentindex,sep = "")] = temp[k]
  #           break
  #         }
  #       }
  #       rentindex=rentindex+1
  #       # break
  #     }
  #   }
  #   
  # },error=function(cond) {})
}

sold_data['sold_year']<- sapply(sold_data$lastsold2, function(x) substr(x,1,8))
sold_data$sold_year <- year(as.Date((sapply(sold_data$sold_year,function(x) as.Date(x, "%m/%d/%y"))),origin="1970-01-01"))
sold_data <- sold_data[sold_data$sold_year>=2015,]




# Tax Data ----------------------------------------------------------------
sold_data['tax_fv_2016_12yrs']<-NA
for(i in 1:nrow(sold_data))  {
  cat(i," ")
  tryCatch({
    temp <- as.character(sold_data[i,'tax_history'])
    if(is.na(temp)) next
    temp <- strsplit(temp,"_n_l_")[[1]]
    #temp <- temp[3:(length(temp)-1)]
    
    for(j in 1:length(temp))  {
      if((tolower(substr(temp[j],1,4))=="2015") | (tolower(substr(temp[j],1,4))=="2014")) {
        if(length(temp)-j <10) {
          break
        } else {
          tax_fv = 0
          no_years = 1
          for(k in j:length(temp)) {
            tax_amount <- substr(temp[k],gregexpr("\\$",temp[k])[[1]][1],nchar(temp[k]))
            tax_amount <- as.numeric(gsub("[^\\.0-9]", "",substr(tax_amount,2,gregexpr("[^\\,\\.0-9]",tax_amount)[[1]][2]-1)))
            tax_year = as.numeric(substr(temp[k],1,4))
            tax_fv = tax_fv + tax_amount*(1+r)^(2016-tax_year)
            no_years = no_years +1
            # cat(tax_year,tax_amount,tax_fv,"\n",sep = "-")
            if(no_years==13) break
          }
          # cat("tax fv = ",tax_fv)
          sold_data[i,'tax_fv_2016_12yrs'] = tax_fv
          break  
        }
        
      }
    }
  },error=function(cond) {})
}

sold_data['tax_fv_2016_3yrs']<-NA
for(i in 1:nrow(sold_data))  {
  cat(i," ")
  tryCatch({
    temp <- as.character(sold_data[i,'tax_history'])
    if(is.na(temp)) next
    temp <- strsplit(temp,"_n_l_")[[1]]
    #temp <- temp[3:(length(temp)-1)]
    
    for(j in 1:length(temp))  {
      if((tolower(substr(temp[j],1,4))=="2015") | (tolower(substr(temp[j],1,4))=="2014")) {
        if(length(temp)-j <3) {
          break
        } else {
          tax_fv = 0
          no_years = 1
          for(k in j:length(temp)) {
            tax_amount <- substr(temp[k],gregexpr("\\$",temp[k])[[1]][1],nchar(temp[k]))
            tax_amount <- as.numeric(gsub("[^\\.0-9]", "",substr(tax_amount,2,gregexpr("[^\\,\\.0-9]",tax_amount)[[1]][2]-1)))
            tax_year = as.numeric(substr(temp[k],1,4))
            tax_fv = tax_fv + tax_amount*(1+r)^(2016-tax_year)
            no_years = no_years +1
            # cat(tax_year,tax_amount,tax_fv,"\n",sep = "-")
            if(no_years==4) break
          }
          # cat("tax fv = ",tax_fv)
          sold_data[i,'tax_fv_2016_3yrs'] = tax_fv
          break  
        }
        
      }
    }
  },error=function(cond) {})
}


sold_data <- sold_data[,names(sold_data) %in% c("zip","beds","baths","sqft","zest_value","zest_rent","avg_school_rating",
                                                "avg_school_distance","walk_score","transit_score","sold_price","city",
                                                "Lot","sold_year","tax_fv_2016_12yrs","price","tax_fv_2016_3yrs",
                                                "Lastremodelyear","Builtin")]

saveRDS(sold_data,file = "California_sales_data.rds")

library(stargazer)
library(sandwich)


# # Past Tax Data ----------------------------------------------------------------
# 
# tax_data_file <- "break_zips_tax_data.csv"
# 
# sold_data <- sold_data[!duplicated(sold_data$ZillowHomeID),]
# sold_data <- sold_data[,c("zip","tax_history","ZillowHomeID","price_history")]
# write.table(t(c("sep=|\nZillowHomeID","zip","year","tax_amount","tax_value")),file=paste(tax_data_file,sep=""),append = FALSE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
# for(i in 1:nrow(sold_data))  {
#   cat(i," ")
#   
#   temp <- as.character(sold_data[i,'tax_history'])
#   if(is.na(temp)) next
#   temp <- strsplit(temp,"_n_l_")[[1]]
#   temp <- temp[5:length(temp)]
#   
#   for(j in 1:length(temp))  {
#     temp2 <- str_split_fixed(temp[[j]], " ", 5)
#     write.table(t(c(sold_data[i,'ZillowHomeID'],sold_data[i,'zip'],temp2[1,1],temp2[1,2],temp2[1,4])),file=paste(tax_data_file,sep=""),append = TRUE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
#   }
# }

