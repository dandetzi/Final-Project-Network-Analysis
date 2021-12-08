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
install.packages("sna")
library("sna")
library("ggplot2")

E = 100
A = 100
setup_twitter_oauth("yXLa60AKsctZVzPm1qND4cwIF","pUD8nO8OpnTn1KWzdItFBZjOPUqYSkx1y3nbOFQxSasriuY0fD","1090995454051930119-5A4kmhKz5NFjisQeVHnoemYJNxByhl","nPeK4iXmG0qJGLvCVTgrZLST05U3aC7AwqkCRWdfFiWTM")

handles <- as.matrix(read.csv("twhandlesnopotus.csv", header=T))
handlesD <- dim(handles)[1]

callsUT = 0
callsRT = 0
callsGU = 0

data = NULL

posts <- matrix(0,0,6)
ptext <- matrix(0,0,2)
people <- matrix(0,0,8)
edges <- matrix(0,0,4)

check <- matrix(0,0,5)

SID <- 1382125451850698752
MID <- 1384265774127140874

error <- "Error in TwInterface"

for(h in 1:handlesD)
  {
    if(callsUT == 900)
      
    {  
      Sys.sleep(900)
    callsUT = 0
    
  }
  
  ego_raw <- try(userTimeline(handles[h,4], n=E, maxID = MID, sinceID =SID, includeRts=FALSE, excludeReplies=TRUE))
  
  data <- rbind(ego_raw, data)
  
  callsUT = callsUT + 1
  
  if(length(data)!=0 & substr(data[1],1,20) !=error)
  {
    ego <- twListToDF(data)
    
    egoD <- dim(ego)[1]
    
    for(i in 1:egoD)
    {
      egoID <- ego[i,"id"]
      egoTX <- ego[i,"text"]
      egoRT <- ego[i,"retweetCount"]
      egoFC <- ego[i,"favoriteCount"]
      egoCR <- ego[i,"created"]
      
      newpost <- cbind(handles[h,4],egoID,egoRT,egoFC,egoCR,"egoPost")
      post <- rbind(posts,newpost)
      
      newtext <- cbind(handles[h,4],egoTX)
      ptext <- rbind(ptext,newtext)
      
      if(egoRT > 0)
      {
        if(callsRT == 75)
        {
          Sys.sleep(900)
          callsRT = 0
        }
        
        egoRTers <- retweeters(egoID,n=100)
        callsRT = callsRT + 1
        
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

people <- unique(people)
peopleD <- dim(people)[1]

write.table(posts, file = "posts.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(ptext, file = "ptext.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(edges, file = "edges.csv", sep = ",",col.names = FALSE, row.names = FALSE)
write.table(check, file = "check.csv", sep = ",",col.names = FALSE, row.names = FALSE)

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
dim(edges)   
edges 
network <- count(edges, c(1,2))
dim(network)
network
network=as.matrix(network)

g=graph.edgelist(network[,1:2])
E(g)$weight=as.numeric(network[,3])

plot(g,vertex.size=15,vertex.label=handles[,4], edge.width=E(g)$weight,edge.arrow.size=.9)
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
