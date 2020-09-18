Project 1
================
Yuying Zhou
9/14/2020

  - [Needed Packages](#needed-packages)
  - [Functions to read NHL records](#functions-to-read-nhl-records)
  - [Functions read stats API](#functions-read-stats-api)
  - [create a wrapper function](#create-a-wrapper-function)
  - [Explore data](#explore-data)

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
# functions with endpoints, users can put endpoint 1-5 for different endpoints
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

data1<-records(endpoint=1)
data2<-records(2)
data3<-records(3,"San Jose Sharks")
data4<-records(4,"San Jose Sharks")
data5<-records(5,"New Jersey Devils")
```

# Functions read stats API

``` r
#create a function to get stats
#Modifiers 1-8 represents different modifiers, no need to type modifiers strings
nhlstats<-function(modifiers,teamID=NULL,season=NULL,teams=NULL){
  base2<-"https://statsapi.web.nhl.com/api/v1/teams/"
  if (modifiers==1){
    modifiers<-"?expand=team.roster"
    call2<-paste0(base2,teamID,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
    get3_df<-get2_df%>%select(-teams.roster.roster)
    roster_df<-data.frame(get2_df$teams.roster.roster[[1]])
    get2_df<-cbind(get3_df,roster_df)
  }
  else if (modifiers==2){
    modifiers<-"?expand=person.names"
    call2<-paste0(base2,teamID,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
  }
  else if (modifiers==3){
    modifiers<-"?expand=team.schedule.next"
    call2<-paste0(base2,teamID,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
    if (!is.null(get2_df$teams.nextGameSchedule.dates)){
      get3_df<-get2_df%>%select(-teams.nextGameSchedule.dates)
      roster_df<-data.frame(get2_df$teams.nextGameSchedule.dates[[1]])
      game<-data.frame(roster_df$games[[1]])
      roster_df<-roster_df%>%select(-games)
      get2_df<-cbind(get3_df,roster_df,game)}
    else {
      get2_df<-as.data.frame(get2_json)
       return(get2_df)}
  }
  else if (modifiers==4){
    modifiers<-"?expand=team.schedule.previous"
    call2<-paste0(base2,teamID,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
      if (!is.null(get2_df$teams.previousGameSchedule.dates)){
        get3_df<-get2_df%>%select(-teams.previousGameSchedule.dates)
        
        roster_df<-data.frame(get2_df$teams.previousGameSchedule.dates[[1]])
        game<-data.frame(roster_df$games[[1]])
        roster_df<-roster_df%>%select(-games)
        get2_df<-cbind(get3_df,roster_df,game)}
     else {
      get2_df<-as.data.frame(get2_json)
       return(get2_df)}
   }
  else if (modifiers==5){
    modifiers<-"?expand=team.stats"
    call2<-paste0(base2,teamID,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
    roster_df<-data.frame(get2_df$teams.teamStats[[1]])
    game<-data.frame(roster_df$splits[[1]])
    roster<-cbind(get2_df,roster_df,game)
    get2_df<-roster%>%select(-c(splits,teams.teamStats))
  }
  else if (modifiers==6){
    modifiers<-"?expand=team.roster&season"
    call2<-paste0(base2,teamID,modifiers,"=",season)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
    roster_df<-data.frame(get2_df$teams.roster.roster[[1]])
    roster<-cbind(get2_df,roster_df)
    get2_df<-roster%>%select(-teams.roster.roster)
  }
  else if (modifiers==7){
    base2<-"https://statsapi.web.nhl.com/api/v1/teams"
    modifiers<-"?teamId"
    # team<- ifelse(!is.numeric(team),unname(tlook[team]),team)
    call2<-paste0(base2,modifiers,"=",teams)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
  }
  else if (modifiers==8){
    base2<-"https://statsapi.web.nhl.com/api/v1/teams"
    modifiers<-"?stats=statsSingleSeasonPlayoffs"
    call2<-paste0(base2,modifiers)
    get2<-GET(call2)
    get2_text<-content(get2,"text")
    get2_json<-fromJSON(get2_text,flatten= TRUE)
    get2_df<-as.data.frame(get2_json)
    }
}  

stat1<-nhlstats(modifiers = 1,teamID = 14)
stat2<-nhlstats(modifiers = 2,teamID = 14)
stat3<-nhlstats(modifiers = 3, teamID =25)
stat4<-nhlstats(modifiers = 4, teamID = 1)
stat5<-nhlstats(modifiers = 5,teamID = 1)
stat6<-nhlstats(modifiers = 6, teamID=14,season = "20112012")
stat7<-nhlstats(modifiers = 7,teams = "4,5")
stat8<-nhlstats(modifiers = 8)
```

# create a wrapper function

``` r
stopshop<-function(endpoint=NULL, franchise=NULL, modifiers=NULL,teamID=NULL, teams=NULL,season=NULL){
  if(!is.null(endpoint)){
  record<-records(endpoint,franchise)}
  else if (!is.null(modifiers)){
  stat<-nhlstats(modifiers,teamID,season,teams)
  }}


l<-stopshop(endpoint =4,franchise = 1)
ll<-stopshop(modifiers = 7, teams ="4,5,29")
lll<-stopshop(modifiers = 6, teamID=14,season = "20112012")
```

# Explore data
