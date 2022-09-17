# An agent-based model of John Horton Conway's Game of Life

# Description of Life (taken from Dan Dennett's 'Real Patterns' (1991)):
# Each cell, in order to determine what to do in the next instant, counts 
# how many of its eight neighbors is ON at the present instant. If the 
# answer is exactly two, the cell stays in its present state (ON or OFF) in the 
# next instant. If the answer is exactly three, the cell is ON in the next 
# instant whatever its current state. Under all other conditions the cell 
# is OFF.

# Steps: 
# Create a 10x10 grid 
# 1,1 1,2 1,3
# 2,1 2,2 2,3
# 3,1 3,2 3,3 -> expand

# Assign a number of 'instants', e.g., 100.
# Have agents assess within each instant and store the decision that the agent makes 
# and have that decision be updated for the state that the current agent is in
# The agents data frame should have three vectors (xpos,ypos,state[ON or OFF])

######Packages######
library(ggplot2)
library(ggstatsplot)
library(gganimate)
library(Rcpp)

## Parameters ##

# Max number of instants
maxinstants<-500
# Parameters to decide how many agents will start out "on" and "off" -- must add up to 100
# GOOD PARAMATERS ARE 30 ON, 70 OFF; THIS IS A NICE PROPORTION TO HAVE THE MODEL STAY ALIVE and agents
# are not overpopulating/underpopulating
starton<-500
startoff<-9500


## Generate agents ##

# 100 x 100 grid (x by y); define x and y here
x<-100
y<-100

xpos<-rep(1:x,each=y)
ypos<-rep(1:y,x)
state<-sample(c(rep(c("on"),starton),rep(c("off"),startoff)))
iteration<-rep(0,x*y)
agents <- data.frame(xpos,ypos,state,iteration)




# Keep track of where agents are at at the beginning of the model
start<-agents

# Start out at 0 instants
instant<-0
finaldata<-NULL
newstatevec<-NULL # vector that captures agents alive status for the next iteration
while(instant<maxinstants){
  
  for(a in 1:nrow(agents)){
    
    # Update agents' states based on assessments from last iteration
    if(instant>0){
      agents$state<-newstatevec
    }
    
    # Assess each agents neighbors
    # Grab focal agent's x and y positions
    thexpos<-agents[a,1]
    theypos<-agents[a,2]
  
    # x-axis: right would be +1, left would be -1
    # y-axis: above would be +1, below would be -1
    # diagonals would be +1,+1; +1,-1; -1,-1; -1,+1
    goodxpos<-c(thexpos,thexpos+1,thexpos-1)
    goodypos<-c(theypos,theypos+1,theypos-1)
    
    # Remove neighbors who are out of bounds
    goodxpos<-goodxpos[goodxpos!=0 & goodxpos!=101]
    goodypos<-goodypos[goodypos!=0 & goodypos!=101]
    
    # Permutations of x and y coordinates; will tell us who agents coordinates + agents neighbors coordinates
    neighbors<-expand.grid(goodxpos, goodypos)
    
    # Remove the focal agent from the neighbors list
    neighbors<-neighbors[-1,]

    # Figure out number of neighbors who are "on"/"off"
    numoff<-0
    numon<-0
    for(n in 1:nrow(neighbors)){
      if(agents$state[agents$xpos==neighbors[n,1] & agents$ypos==neighbors[n,2]]=="on"){
        numon<-numon+1
      }else{
        numoff<-numoff+1
      }
    }
    
    
    ## DETERMINE STATUS OF NEIGHBORS ##
    
    focal<-NULL
    # If number of "on" agents is 2, the focal agent's state stays the same
    if(numon==2){
      # The agent stays the same
      curstate<-agents$state[agents$xpos==thexpos & agents$ypos==theypos]
      # Figure out focal agents index in agents dataframe and update it
      focal<-which(agents$xpos==thexpos & agents$ypos==theypos)
      newstatevec[focal]<-curstate
    }
    # If number of "on" agents is 3...
    if(numon==3){
      # The agent is "on"
      # Figure out focal agents index in agents dataframe and update it
      focal<-which(agents$xpos==thexpos & agents$ypos==theypos)
      newstatevec[focal]<-"on"
    }
    # If number of "on" agents is anything but 2 or 3...
    if(numon!=2 & numon!=3){
      # The agent is "off"
      # Figure out focal agents index in agents dataframe and update it
      focal<-which(agents$xpos==thexpos & agents$ypos==theypos)
      newstatevec[focal]<-"off"
    }
    
  }
  
  agents$iteration<-agents$iteration+1
  
  tempend<-agents
  
  finaldata<-rbind(finaldata,tempend)
  
  # Update instant
  instant<-instant+1
}
  
# Store agents states at the end
end<-agents
  
# How many agents turned on by the end of the model compared to start
sum(start$state=="on") # start
sum(end$state=="on") # end

#Plot agents' start positions
t1<-qplot(xpos,ypos,color=state, data=start,xlab="X",ylab="Y")+
  geom_point(size=2.5)+scale_color_manual(values=c("black","blue","white"))+
  theme(panel.background=element_rect(fill="grey"))

#Plot agents' end positions
t2<-qplot(xpos,ypos,color=state, data=end,xlab="X",ylab="Y")+
  geom_point(size=2.5)+scale_color_manual(values=c("Black","blue","white"))+
  theme(panel.background=element_rect(fill="grey"))
  
# Create animated plot
animateplot<-ggplot(finaldata, aes(xpos, ypos, color = state)) +
  geom_point(size=2.5)+
  scale_color_manual(values = c("black","blue","white")) +
  theme(panel.background=element_rect(fill="grey"))

t3<-animateplot  +  transition_states(iteration)





















