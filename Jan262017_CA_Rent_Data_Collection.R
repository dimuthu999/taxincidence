rm(list=ls())

#devtools::install_github("ropensci/RSelenium")
#checkForServer()
library(RSelenium)
library(pdftools)
library(RCurl)
library(XML)
library(zoo)
library(R.utils)
setwd("C:/Users/dnratnadiwakara/Documents/Zillow Rent Data")

login_url <- "https://www.zillow.com/Home,$PageWrapper.$ui$TopNav2014.canonicalLinksService.loginLink.sdirect"
current_url <- "http://www.zillow.com/homes/for_rent/house,mobile_type/94112_rb/"


stopSeleniumServer <- function()
{
  if (.Platform$OS.type == "windows")
  {
    procs <- system2("wmic",
                     "path win32_process get Caption,Processid,Commandline",
                     stdout=TRUE, stderr=NULL)
    idx <- grep("selenium-server-standalone.jar", procs)
    if (!length(idx)) # selenium not running?
      return()
    proc <- procs[idx]
    proc <- trimws(proc)
    segs <- strsplit(proc, " ", TRUE)[[1]]
    pid <- segs[length(segs)]
    system2("taskkill", c("/pid", pid, "/f"), stdout=NULL, stderr=NULL)
  } else {
    # Unfortunately field width specifiers do not work on Mac
    # so we need two commands to get pid and command non-truncated.
    pidcmd <- system2("ps", "wwaxo pid,command", stdout=TRUE, stderr=NULL)
    pidusr <- system2("ps", "wwaxo pid,user", stdout=TRUE, stderr=NULL)
    pidusr <- trimws(pidusr)
    whoami <- system2("whoami", stdout=TRUE, stderr=NULL)
    pididx <- grep(paste0(whoami, "$"), pidusr)
    pidusr <- pidusr[pididx]
    tmp <- strsplit(pidusr, " ")
    pids <- unlist(lapply(tmp, function(x) x[1]))
    procs_idx <- grep("selenium-server-standalone.jar", pidcmd)
    for (proc in pidcmd[procs_idx])
    {
      pid <- strsplit(proc, " ")[[1]][1]
      if (pid %in% pids) 
      {
        system2("kill", c("-9", pid))
      }
    }
  }
}

mybrowser <- NULL
initializ_collection <- function()  {
  tryCatch({mybrowser$close()},error=function(cond) {})
  stopSeleniumServer()
  Sys.sleep(10)
  startServer( log = FALSE, invisible = FALSE)
  Sys.sleep(30)
  mybrowser <- remoteDriver()
  mybrowser$open()
  mybrowser$navigate(login_url)
  login <- mybrowser$findElement(using = 'id', value="email")
  login$sendKeysToElement(list("dimuthu@gmail.com"))
  login <- mybrowser$findElement(using = 'id', value="password")
  login$sendKeysToElement(list("iHv34XxG","\uE007"))
  mybrowser$navigate(current_url)
  mybrowser <<- mybrowser
  
}

login <- function() {
  mybrowser$navigate(login_url)
  login <- mybrowser$findElement(using = 'id', value="email")
  login$sendKeysToElement(list("dimuthu@gmail.com"))
  login <- mybrowser$findElement(using = 'id', value="password")
  login$sendKeysToElement(list("iHv34XxG","\uE007"))
  mybrowser$navigate(current_url)
  
}


initializ_collection()


# Get Links ---------------------------------------------------------------

zip_population <- read.csv(file = "zipcode_population_CA.csv")
zips <- zip_population[zip_population$population>25000 & zip_population$population<50000 & !is.na(zip_population$population),]$zip
links <- list()
zip_no=0
j=1
for(zip in zips)  {
  tryCatch({
    #login()
    zip_no = zip_no + 1
    cat("\n",paste(zip_no,"-",zip))
    
    tryCatch({
      wxbox <- mybrowser$findElement(using = 'id', value="citystatezip")#value = "quickSearchText")
      wxbox$clearElement()
      wxbox$sendKeysToElement(list(as.character(zip), "\uE007"))
    },error=function(cond) {
      mybrowser$navigate(current_url)
      Sys.sleep(2)
      wxbox <- mybrowser$findElement(using = 'id', value="citystatezip")#value = "quickSearchText")
      wxbox$clearElement()
      wxbox$sendKeysToElement(list(as.character(zip), "\uE007"))
    })  
    
        Sys.sleep(2)
        no_of_results <- mybrowser$findElement(using='id','map-result-count-message')$getElementText()[[1]][1]
        Sys.sleep(2)
        no_of_results <- as.numeric(gsub(",", "",strsplit(no_of_results," ")[[1]][1]))
        no_of_pages <- (no_of_results %/% 26)+1
        if( is.na(no_of_pages)) no_of_pages = 1


        for(page in 1:no_of_pages) {
          cat(" ",page)
          Sys.sleep(2)
          pagesource <- mybrowser$getPageSource()
          #Sys.sleep(2)
          homedetails <- gregexpr('homedetails', pagesource)
          similar <- gregexpr('Similar results nearby', pagesource)
          if(similar[[1]][1]>0) homedetails[[1]]<-homedetails[[1]][homedetails[[1]]<similar[[1]][1]]
          if(length(homedetails[[1]])>0)  {
            for(i in 1:length(homedetails[[1]]))  {
              link <- substr(pagesource,homedetails[[1]][i],homedetails[[1]][i]+100)
              links[[j]] <- paste("www.zillow.com/",substr(link,1,gregexpr('zpid',link)[[1]][1]+4),sep = "")
              # cat(links[[j]][1])
              write.table(link,file="CA_Rent_Links_2.csv",sep="|",append = TRUE,row.names = FALSE,col.names = FALSE)
              j=j+1
              #print(links[[i]])
            }
          }
          #cat(paste("http://www.zillow.com/homes/",zip,"_rb/",(page+1),"_p/",sep = ""))
          if(page < no_of_pages) {
            withTimeout({
              mybrowser$navigate(paste("http://www.zillow.com/homes/for_rent/house,mobile_type/",zip,"_rb/",(page+1),"_p/",sep = ""))
            }, timeout=15, onTimeout="silent");
          }
        }  
  },error=function(cond) {
    Sys.sleep(30)
    tryCatch({initializ_collection()},error=function(cond) {print("close")})
    Sys.sleep(10)
  })
}

links <- gsub("\\\\","",gsub("%","",gsub("%2F","/",links)))
links <- links[!duplicated(links)]

cat("links extracted ",length(links),"\n")


# Get Data ----------------------------------------------------------------

fn = "California_Rent_Data_otherzips.csv"

rec_no = 0
for(link in links) {
  tryCatch({
    rec_no = rec_no+1
    cat(rec_no," of ",length(links),"  ")
    
    record <- rep(NA,24)
    
    withTimeout({
      mybrowser$navigate(link);
    }, timeout=15, onTimeout="silent");
    
    # Sys.sleep(1)
    # pagesource <- mybrowser$getPageSource()[[1]][1]
    # Sys.sleep(1)
    # if(nchar(pagesource)<=10) {
    #   cat("next")
    #   next
    # }
    
    Sys.sleep(2)
    tryCatch({
      temp <- mybrowser$findElement(using='class','z-moreless-toggle')
      Sys.sleep(2)
      temp$clickElement()            
    },error=function(cond) {})
    
    tryCatch({record[1]<-mybrowser$findElement(using='class','main-row')$getElementText()},
             error=function(cond) {
               
             })
    tryCatch({record[21]<-gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='id','home-value-wrapper')$getElementText()) },error=function(cond) {next})   
    tryCatch({record[2]<-mybrowser$findElement(using='class','addr_city')$getElementText()},error=function(cond) {})
    temp <- NULL
    tryCatch({temp <- mybrowser$findElements(using='class','addr_bbs')},error=function(cond) {})
    tryCatch({record[3]<-temp[[1]]$getElementText()},error=function(cond) {})
    tryCatch({record[4]<-temp[[2]]$getElementText()},error=function(cond) {})
    tryCatch({record[5]<-temp[[3]]$getElementText()},error=function(cond) {})
    temp <- NULL
    tryCatch({temp <- mybrowser$findElements(using='class','zest-value')},error=function(cond) {})  
    tryCatch({record[6]<-temp[[1]]$getElementText()},error=function(cond) {})
    tryCatch({record[7]<-temp[[2]]$getElementText()},error=function(cond) {})
    if(length(temp)==3) {
      tryCatch({record[8]<-temp[[3]]$getElementText()},error=function(cond) {})
    }
    # temp <- NULL
    # tryCatch({temp <- mybrowser$findElements(using='class','zest-change')},error=function(cond) {})
    # if(length(temp)==1) {
    #   
    #   tryCatch({record[9]<-temp[[1]]$getElementText()},error=function(cond) {}) 
    # } else {
    #   tryCatch({record[9]<-temp[[1]]$getElementText()},error=function(cond) {})   
    #   tryCatch({record[10]<-temp[[2]]$getElementText()},error=function(cond) {})
    # }
    
    sch_rating<-NULL
    tryCatch({
      temp <- mybrowser$findElements(using='class','gs-rating-number')
      for(i in 1:length(temp))  {
        sch_rating <- c(sch_rating,as.numeric(temp[[i]]$getElementText()))
      }
    },error=function(cond) {})
    tryCatch({record[11]<-mean(sch_rating,na.rm = TRUE)},error=function(cond) {})
    
    
    sch_distance <- NULL
    tryCatch({
      temp <-  mybrowser$findElements(using='class','nearby-schools-distance')
      for(i in 2:length(temp))  {
        sch_distance <- c(sch_distance,as.numeric(gsub("[^\\.0-9]", "", temp[[i]]$getElementText())))
      }
    },error=function(cond) {})
    tryCatch({record[12]<-mean(sch_distance,na.rm = TRUE)},error=function(cond) {})
    
    
    tryCatch({record[13]<-gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='class','hdp-facts')$getElementText())},error=function(cond) {})
    record[14]<-link
    record[15]<-no_of_results
    
    Sys.sleep(1)
    tryCatch({
      temp <- mybrowser$findElement(using='id','hdp-price-history')
      temp$clickElement()
      temp$sendKeysToElement(list(key = "up_arrow"))
      temp$sendKeysToElement(list(key = "up_arrow"))
      temp$sendKeysToElement(list(key = "up_arrow"))
      temp$sendKeysToElement(list(key = "up_arrow"))
      temp$sendKeysToElement(list(key = "up_arrow"))
    },error=function(cond) {})
    Sys.sleep(1)
    
    temp <- mybrowser$findElements(using='class','yui3-toggle-content-link')
    Sys.sleep(1)
    tryCatch({
      temp[[1]]$clickElement()
    },error=function(cond) {})
    
    # tryCatch({
    #   temp <- mybrowser$findElement(using='id','hdp-tax-history')
    #   temp$clickElement()
    # },error=function(cond) {})
    # Sys.sleep(1)
    # 
    # temp <- mybrowser$findElements(using='class','yui3-toggle-content-link')
    # tryCatch({
    #   temp[[2]]$clickElement()
    # },error=function(cond) {})
    # Sys.sleep(1)
    
    
    
    
    # tryCatch({record[16]<-gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='id','hdp-tax-history')$getElementText())},error=function(cond) {})
    
    tryCatch({record[17]<-gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='id','hdp-price-history')$getElementText())},error=function(cond) {})
    
    tryCatch({
      temp <- mybrowser$findElement(using='id','hdp-neighborhood')
      temp$clickElement()
    },error=function(cond) {})
    Sys.sleep(1)
    tryCatch({record[18]<-substr(gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='id','hdp-neighborhood')$getElementText()),1,200)},error=function(cond) {})
    # tryCatch({record[19]<-mybrowser$findElements(using='class','ws-value')[[1]]$getElementText()},error=function(cond) {})
    # tryCatch({record[20]<-mybrowser$findElements(using='class','ws-description')[[1]]$getElementText()},error=function(cond) {})
    # tryCatch({record[22]<-mybrowser$findElements(using='class','ws-value')[[2]]$getElementText()},error=function(cond) {})
    # tryCatch({record[23]<-mybrowser$findElements(using='class','ws-description')[[2]]$getElementText()},error=function(cond) {})
    tryCatch({record[24]<-gsub("[\r\n]", "_n_l_",mybrowser$findElement(using='class','addr')$getElementText()) },error=function(cond) {})  
    
    write.table(t(record),file=fn,append = TRUE,sep = "|",quote = FALSE,col.names = FALSE,row.names = FALSE)
  },error=function(cond) {
    print("error")
    tryCatch({initializ_collection()},error=function(cond) {print("close")})
    Sys.sleep(10)
  })
  
}

