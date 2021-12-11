#####Packages needed for this project (the packages below igraph are for additional visualization.)


install.packages("twitteR")
library("twitteR")
library("academictwitteR")
install.packages("httpuv")
library("httpuv")
library("curl")
library("openssl")
library("plyr")
install.packages("igraph")
library("igraph")
install.packages("GGally")
library("GGally")
library("network")
install.packages("network")
library("network")

####### E sets the limits for Ego tweets and A for alter
E = 100
A = 100

####### Add your keys from your Twitter developer dashboard.

setup_twitter_oauth("CONSUMERKEY","CONSUMERSECRET", "ACCESSTOKEN", "ACCESSTOKENSECRET")

####### Save Twohandlesnopotus.xls file from the Data folder; save as .csv and then set as matrix 
####### handlesD provides the dimensions of the matrix, which should be of length 13.

handles <- as.matrix(read.csv("twhandlesnopotus.csv", header=T))
handlesD <- dim(handles)[1]

####### keeps track of the number of calls of Twitter API
callsUT = 0
callsRT = 0
callsGU = 0

####### Sets matrices for ego_raw data

data = NULL

####### Sets matrices for network data: These will build the matrices off of data 
####### pulled from the dataframe pulled from Twitter 

posts <- matrix(0,0,6)
ptext <- matrix(0,0,2)
people <- matrix(0,0,8)
edges <- matrix(0,0,4)

###### Sets matrix to check retweeters captured

check <- matrix(0,0,5)

###### Sets the minimum and maximum Tweet IDs: The ID's below are tweets sent by the PressSec
###### account on April 13 and from the StateDeptSpox account on April 19, 2021, respectively, denoting the period of time
###### to collect twitter data based on the first and last mention of Afghanistan.

SID <- 1382125451850698752
MID <- 1384265774127140874

###### Sets the variable for error message

error <- "Error in TwInterface"

##### Loop for collecting data on egos (handles from twhandlesnopotus file)

for(h in 1:handlesD)
{
  ###### Sets the limit for timeline calls: sleeps after 900 timeline calls.
  
  if(callsUT == 900)
    
  {  
    Sys.sleep(900)
    callsUT = 0
    
  }
  
  ###### Pulls most recent tweets from ego timeline, based on the MID and SID parameters.
  
  ego_raw <- try(userTimeline(handles[h,4], n=E, maxID = MID, sinceID =SID, includeRts=FALSE, excludeReplies=TRUE))
  
  data <- rbind(ego_raw, data)
  
  callsUT = callsUT + 1
  
  ###### Makes sure that usable data has been collected and then calculates the number of tweets collected.
  
  if(length(data)!=0 & substr(data[1],1,20) !=error)
  {
    ego <- twListToDF(data)
    
    egoD <- dim(ego)[1]
    
    ###### Collects all retweeters of ego tweets
    
    for(i in 1:egoD)
    {
      egoID <- ego[i,"id"]
      egoTX <- ego[i,"text"]
      egoRT <- ego[i,"retweetCount"]
      egoFC <- ego[i,"favoriteCount"]
      egoCR <- ego[i,"created"]
   
      ###### Builds matrices for networks (error will be generated if dimensions do not match those set above.)
      
      newpost <- cbind(handles[h,4],egoID,egoRT,egoFC,egoCR,"egoPost")
      post <- rbind(posts,newpost)
      
      newtext <- cbind(handles[h,4],egoTX)
      ptext <- rbind(ptext,newtext)
      
  ###### Sets limit for calls on ego retweets.
      
      if(egoRT > 0)
      {
        if(callsRT == 75)
        {
          Sys.sleep(900)
          callsRT = 0
        }
        
        egoRTers <- retweeters(egoID,n=100)
        callsRT = callsRT + 1
      
        ###### Sets up matrices for retweeters 
        
        newcheck <- cbind(handles[h,4],0,i,egoRT,length(egoRTers))
        check <- rbind(check,newcheck)
        
        newpeople <- cbind(t(t(egoRTs)),0,0,0,0,0,0,"egoRetweeter")
        people <- rbind(people,newpeople)
        
        newedges <- cbind(handles[h,4],egoID,t(t(egoRTers)),"egoPost/egoRetweeter")
        edges <- rbind(edges,newedges)
      }
    }
  }
}

######Eliminates duplicate retweeters

people <- unique(people)
peopleD <- dim(people)[1]

write.table(posts, file = "posts.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(ptext, file = "ptext.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(edges, file = "edges.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(check, file = "check.csv", sep = ",",col.names = FALSE, row.names = FALSE)

###### This loop sets up the matrices for the retweeters of the retweeters

for(j in 1:peopleD)
{
  
  
  alter_raw <- try(userTimeline(people[j,1], n=A, maxID = MID, sinceID = SID))
  
  callsUT = callsUT + 1
  
  
  alter_draw <- try(getUser(people[j,1]))
  
  callsuGU = callsGU + 1
  
  if(length(slotNames(alter_draw))!=0)
  {
    
    people[j,2] <- alter_draw$statusesCount
    people[j,3] <- alter_draw$followersCount
    people[j,4] <- alter_draw$favoritesCount
    people[j,5] <- alter_draw$friendsCount
    people[j,6] <- alter_draw$created
    people[j,7] <- alter_draw$verified
    
    
  }
  
  write.table(t(people[j,]), file = "people.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
  
  if(length(alter_raw)!=0 & substr(alter_raw[1],1,20) != error)
  {
    
    alter <- twListToDF(alter_raw)
    alterD <- dim(alter)[1]
    
    ###### Collects retweeters for all altertweets. 
    
    for(k in 1:alterD)
    {
      
      alterID <- alter[k,"id"]
      alterTX <- alter[k,"text"]
      alterRT <- alter[k,"retweetCount"]
      alterFC <- alter[k,"favoriteCount"]
      alterCR <- alter[k,"created"]
      
      newpost <- cbind(people[j,1],alterID,alterRT,alterFC,"alterpost")
      write.table(newpost, file = "posts.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      
      newtext <- cbind(people[j,1],alterTX)
      write.table(newtext, file = "ptext.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      
      if(alterRT > 0)
      {
        if(callsRT == 75)
        {
          
          Sys.sleep(900)
          callsRT = 0
        }
        
        
        callsRT = callsRT + 1
        
        alterRTers <- retweeters(alterID, n=100)
        
        newcheck <- cbind(people[j,1],j,k,alterRT,length(alterRTers))
        write.table(newcheck, file = "check.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
        
        newpeople <- cbind(people[j,1],t(t(alterRTers)),0,0,0,0,0,0,"alterRetweeter")
        write.table(newpeople, file = "people.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
        
        newedges <- cbind(people[j,1],alterID,t(t(alterRTers)),"alterPost/alterRetweeter")
        write.table(newedges, file = "edges.csv", sep = ",", col.names = FALSE, row.names = FALSE, append = TRUE)
      }
    }
  }
}

###### Visualization of Network Data

###### Uses edges collected from above code to build a network matrix.
dim(edges)   
edges 
network <- count(edges, c(1,2))
dim(network)
network
network=as.matrix(network)

###### Creates table of edges that is then used as a variable for plotting.
g=graph.edgelist(network[,1:2])
E(g)$weight=as.numeric(network[,3])

###### Plots the entire network
plot(g,vertex.size=15,vertex.label=handles[,4], edge.width=E(g)$weight,edge.arrow.size=.9)

###### Plots each individual handle (ego graphs)
g2 <- make_ego_graph(g, 1, nodes = "JakeSullivan46", mode = c("all", "out", "in"), mindist=0)
g2p <- plot(g2[[1]],vertex.size=3,edge.width=E(g2[[1]])$weight,edge.arrow.size=.1)

g3 <- make_ego_graph(g, 1, nodes = "SecDef", mode = c("all", "out", "in"), mindist=0)
g3p <- plot(g3[[1]],vertex.size=20,edge.width=E(g3[[1]])$weight,edge.arrow.size=.9)

g4 <- make_ego_graph(g, 1, nodes = "PentagonPresSec", mode = c("all", "out", "in"), mindist=0)
g4p <- plot(g4[[1]],vertex.size=3,edge.width=E(g4[[1]])$weight,edge.arrow.size=.1)

g5 <- make_ego_graph(g, 1, nodes = "thejointstaff", mode = c("all", "out", "in"), mindist=0)
g5p <- plot(g5[[1]],vertex.size=3,edge.width=E(g5[[1]])$weight,edge.arrow.size=.1)

g6 <- make_ego_graph(g, 1, nodes = "SecBlinken", mode = c("all", "out", "in"), mindist=0)
g6p <- plot(g6[[1]],vertex.size=3,edge.width=E(g6[[1]])$weight,edge.arrow.size=.1)

g7 <- make_ego_graph(g, 1, nodes = "StateDeptSpox", mode = c("all", "out", "in"), mindist=0)
g7p <- plot(g7[[1]],vertex.size=3,edge.width=E(g7[[1]])$weight,edge.arrow.size=.1)

g8 <- make_ego_graph(g, 1, nodes = "USAmbUN", mode = c("all", "out", "in"), mindist=0)
g8p <- plot(g8[[1]],vertex.size=3,edge.width=E(g8[[1]])$weight,edge.arrow.size=.1)

g9 <- make_ego_graph(g, 1, nodes = "Jenkinsbd", mode = c("all", "out", "in"), mindist=0)
g9p <- plot(g9[[1]],vertex.size=3,edge.width=E(g9[[1]])$weight,edge.arrow.size=.1)

g10 <- make_ego_graph(g, 1, nodes = "UzraZeya", mode = c("all", "out", "in"), mindist=0)
g10p <- plot(g10[[1]],vertex.size=25,edge.width=E(g10[[1]])$weight,edge.arrow.size=.1)
