Project 1
================
Yuying Zhou
9/14/2020

  - [Needed Packages](#needed-packages)
  - [Functions to read NHL records](#functions-to-read-nhl-records)
  - [Functions read stats API](#functions-read-stats-api)

# Needed Packages

``` r
#load packageds
library(httr)
library(jsonlite)
library(tidyverse)
```

# Functions to read NHL records

``` r
#create a franchise lookup table
base<- "https://records.nhl.com/site/api"
endpoint1<-"/franchise-season-records?cayenneExp=franchiseId"
call1<-paste0(base,endpoint1)
get1<-GET(call1)
get1_text<-content(get1,"text")
get1_json<-fromJSON(get1_text,flatten= TRUE)
get1_df<-as.data.frame(get1_json)
franchisetable<-select(get1_df,data.franchiseId, data.franchiseName)
flook<-franchisetable$data.franchiseId
names(flook)<-franchisetable$data.franchiseName

#create the function
records<-function(endpoint,franchise=NULL){
  base<- "https://records.nhl.com/site/api"
  if (endpoint==1){
    endpoint<-"/franchise"
    call<-paste0(base,endpoint)
    get<-GET(call)
    get_text<-content(get,"text")
    get_json<-fromJSON(get_text,flatten= TRUE)
    get_df<-as.data.frame(get_json)
  }
  else if (endpoint==2){
    endpoint<-"/franchise-team-totals"
    call<-paste0(base,endpoint)
    get<-GET(call)
    get_text<-content(get,"text")
    get_json<-fromJSON(get_text,flatten= TRUE)
    get_df<-as.data.frame(get_json)
  }
  else if (endpoint==3){
    endpoint<-"/franchise-season-records?cayenneExp=franchiseId"
   franchise<- ifelse(!is.numeric(franchise),unname(flook[franchise]),franchise)
    call<-paste0(base,endpoint,"=",franchise)
    get<-GET(call)
    get_text<-content(get,"text")
    get_json<-fromJSON(get_text,flatten= TRUE)
    get_df<-as.data.frame(get_json)
  }
  else if (endpoint==4){
    endpoint<-"/franchise-goalie-records?cayenneExp=franchiseId"
    franchise<- ifelse(!is.numeric(franchise),unname(flook[franchise]),franchise)
    call<-paste0(base,endpoint,"=",franchise)
    get<-GET(call)
    get_text<-content(get,"text")
    get_json<-fromJSON(get_text,flatten= TRUE)
    get_df<-as.data.frame(get_json)
  }
  else{
    endpoint<-"/franchise-skater-records?cayenneExp=franchiseId"
    franchise<- ifelse(!is.numeric(franchise),unname(flook[franchise]),franchise)
    call<-paste0(base,endpoint,"=",franchise)
    get<-GET(call)
    get_text<-content(get,"text")
    get_json<-fromJSON(get_text,flatten= TRUE)
    get_df<-as.data.frame(get_json)
  }
}

new<-function(franchise){
  franchise<- franchise<- ifelse(!is.numeric(franchise),unname(flook[franchise]),franchise)
  return(franchise)
}
test<-new("New Jersey Devils")
data1<-records(endpoint=1)
data2<-records(2)
data3<-records(3,"San Jose Sharks")
data4<-records(4,"San Jose Sharks")
data5<-records(5,"New Jersey Devils")
```

# Functions read stats API

``` r
nhlstats<-function(modifiers){
  base<-"https://statsapi.web.nhl.com/api/v1/teams"
  modifiers<-""
}
```
