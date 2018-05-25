
library(modeest)
library(dplyr)
library(rethinking)
library(ggplot2)

#==============================================================================
#=========================== parameters =======================================
#===============================================================================

# here we define values for the parameters for the multiPVL function, 
# this function constitutes the simulation and is described below

set.seed(1)

#curve parameter for logistic functions for loyalty and performance transformation
loyCurve = 1
perCurve = 1

sim.params = data.frame(
  ntrials = 50,
  nagents = 24,
  nsims = 100)

# agent parameters are sampled from distributions
agent.params = data.frame(
  Ax = c(.4,.1), #prospect theory shape parameter for deck choice
  Wx = c(2.5,.8), #prospect theory loss aversion parameter for deck choice
  ax = c(.2,3), #updating/memory parameter for deck choice
  cx = c(4,.5)) #explore - exploit parameter
agent.params$Az = agent.params$Ax #....same for choice to collaborate
agent.params$Wz = agent.params$Wx
agent.params$az = agent.params$ax
agent.params$cz = agent.params$cx

#this controls how agents interact, and what kinds of feedback they recieve
condition = data.frame( 
  interaction = "vote",
  knowledge = "symmetrical",
  reputation = "performance")

#===============================================================================
#=========================== simulation function ===============================
#===============================================================================

# here we specify the function that consitutes the simulation,
# outputs agent choice behaviour

multiPVL <- function(sim.params, agent.params, condition) {
  
  #######################################################
  # Take simulation parameters from inputs
  
  ### trial/task parameters #########
  ntrials = sim.params$ntrials
  nagents = sim.params$nagents
  nsims = sim.params$nsims
  
  #######################################################
  # Create empty arrays to populate with simulated data
  propJoin=c()
  
  propA_in=c()
  propB_in=c()
  propC_in=c()
  propD_in=c()
  
  propA_out=c()
  propB_out=c()
  propC_out=c()
  propD_out=c()
  
  muExpA=c()
  muExpB=c()
  muExpC=c()
  muExpD=c()
  
  muBalanceOut=c()
  muBalanceIn=c()
  All_avgbalanceIn=c()
  All_avgbalanceOut=c()
  muBalance_cum=c()
  
  muExpOut=c()
  muExpIn=c()
  
  nswitch=c()
  
  group_choice = array(0,c(nsims,ntrials)) 
  group_reward = array(0,c(nsims,ntrials,4))
  group_loss = array(0,c(nsims,ntrials,4))
  
  #######################################################
  ################## Run simulation #####################
  #######################################################
  
  # Set initial (i.e. trial 1) values for all agents, for all simulations
  for (sim in 1:nsims) {

      
  #-------------------------------------create agent parameters----------------------------------
    
    
    ### card decision parameters ######
    Ax = rnorm(nagents,agent.params$Ax[1],agent.params$Ax[2]) 
    wx = rnorm(nagents,agent.params$Wx[1],agent.params$Wx[2])
    ax = rgamma(nagents,agent.params$ax[1],agent.params$ax[2])
    cx <- rnorm(nagents,agent.params$cx[1],agent.params$cx[2])
    thetax <- 2^cx - 1
    
    ### collaborate decision parameters ######
    Az = rnorm(nagents,agent.params$Az[1],agent.params$Az[2]) 
    wz = rnorm(nagents,agent.params$Wz[1],agent.params$Wz[2])
    az = rgamma(nagents,agent.params$az[1],agent.params$az[2])
    cz <- rnorm(nagents,agent.params$cz[1],agent.params$cz[2])
    thetaz <- 2^cz - 1
    
    #-------------------------------------set up decks for simulation----------------------------------
    
    Rewards <- cbind(rep(100,ntrials),
                     rep(100,ntrials),
                     rep(50,ntrials),
                     rep(50,ntrials))
    Losses <- cbind(sample(c(rep(-250,50),c(rep(0,50))),ntrials),
                    sample(c(rep(-1250,10),c(rep(0,90))),ntrials),
                    sample(c(rep(-50,50),c(rep(0,50))),ntrials),
                    sample(c(rep(-250,10),c(rep(0,90))),ntrials))
    
    group_reward[sim,,] <- Rewards
    group_loss[sim,,] <- Losses
    
    ################################################################################
    #---- initiate paramaters needed for decisions in first trial------
    
    # empty matrices for valence, expectancy, and probability (of card choice) parameters
    Vx = array(0,c(nagents,ntrials,4)) #card choice valence
    Ex = array(0,c(nagents,ntrials,4)) #card choice expectancies  
    expEx = array(0,c(nagents,ntrials,4)) #transformed expectancies
    Px = array(0,c(nagents,ntrials,4)) #card choice probability  
    x = array(0,c(nagents,ntrials)) #card choice
    
    #-set parameters needed to get started on trial 1 for agent n
    Px[,1,]=c(.25,.25,.25,.25) # set to non-zero, but don't get used for anything past t = 1
    x[,1] = sample(seq(1,4,1),1) #randomly select one of the decks for t = 1
    
    # empty matrices for valence, expectancy, and probability (of collaboration) parameters
    Vz = array(0,c(nagents,ntrials,2)) #valence of decision to collaborate
    Ez = array(0,c(nagents,ntrials,2)) #Expectancy
    expEz = array(0,c(nagents,ntrials,2))#transformed expectancy
    Pz = array(0,c(nagents,ntrials,2)) #probability of choice
    z = array(0,c(nagents,ntrials)) #decision to collaborate, or not at t = 1
    
    # set parameters to get started
    Pz[,1,]=.01 #p of collaborating at t = 1 for all agents
    
    G = array(0,c(ntrials,4)) #Group choice
    Vg = array(0,c(ntrials,4)) #Valence of group choice
    
    pdeck=array(0,c(nagents,100))
    pjoin=array(0,c(nagents,100))
    
    balance = array(0,c(nagents,ntrials)) #monetary balance of agent per trial
    avgbalanceIn = array(0,c(ntrials)) #the score per round across agents (in and outside of group)
    avgbalanceOut = array(0,c(ntrials))
    balanceCum = array(0,c(ntrials)) #cumulative balance
    
    ### reputation parameters
    loy = array(0,c(nagents,ntrials)) #<- loyalty
    loyL = array(0,c(nagents,ntrials)) #<- logistically transformed
    perScore = array(0,c(nagents,ntrials)) #<- performance
    per = array(0,c(nagents,ntrials)) #<- performance
    perL = array(0,c(nagents,ntrials)) #<- logistically transformed
    rep = array(0,c(nagents,ntrials)) #<- reputation
    Exweight = array(0,c(nagents,ntrials,4)) #<- weighted expectancies for deck choice
    
    ####################################################################
    ###################### Begin simulating choices ####################
    
    # run trial loop from trial 2 - because some parameters have t - 1 reference
    for (t in 2:ntrials) {
      
      #all agents choose to join collaboration, and choose deck
      for (n in 1:nagents) {# each agent decides which deck      
        
        #---------------Make decision of whether to join collaboration------------------------
        # make choice weighted by probability representations for joining collaboration
        # translate probability of choosing to join collaboration into proportions within a 100 element array.....
        # (for each of the n agents) - i.e. Pz from previous trial
        
        pjoin[n,] = c(rep(0,round(Pz[n,t-1,1]*100)),
                      rep(1,round(Pz[n,t-1,2]*100)))
        #.... and then sample from that array (for each of the n agents)
        z[n,t] = sample(pjoin[n,],1)
        
        # make choice weighted by probability representations
        # translate probability of choosing each deck into proportions within a 100 element array.....
        A = c(rep(1,round(Px[n,t-1,1]*100)),
              rep(2,round(Px[n,t-1,2]*100)),
              rep(3,round(Px[n,t-1,3]*100)),
              rep(4,round(Px[n,t-1,4]*100)),
              1)
        pdeck[n,] = A[1:100]
        
        #.... and then sample from that array
        x[n,t] = sample(pdeck[n,],1)
        
        ### Reputation updating
        #Loyalty score goes up or down depending on participation
        loy[n,t] = loy[n, t-1] + (-0.125 + z[n,t]*0.25)  
        #This is fed to a logistic function to create a transformed loyalty score
        loyL[n,t] = 1 / (1 + exp(-loyCurve*(loy[n,t])))
        
        #Performance score updates proportional to net reward
        per[n,t] = per[n, t-1] + (perScore[n, t-1])*0.005 
        #And is logistically transformed
        perL[n,t] = 1 / (1 + exp(-perCurve*(per[n,t])))
        
        #Decide which type of reputation
        if (condition$reputation == "performance"){
          rep[n,t] = perL[n,t]
        } else if (condition$reputation == "loyalty") {
          rep[n,t] = loyL[n,t]
        } else if (condition$reputation == "both") {
          rep[n,t] = loyL[n,t]*perL[n,t]
        } else if (condition$reputation == "baseline") {
          rep[n,t] = 1}
        
        #Calculate weighted expectancies for each agent
        Exweight[n,t-1,] = Ex[n,t-1,]*rep[n,t]
      }
      
      
      #Group choice can be decided on vote or by sharing confidence, depending on simulation condition
      #WEIGHTED BY REPUTATION
      if (condition$interaction == "vote") {
        G[t,] = c(sum(rep[x[,t]==1]&z[,t]==1,t-1),
                  sum(rep[x[,t]==2]&z[,t]==1,t-1),
                  sum(rep[x[,t]==3]&z[,t]==1,t-1),
                  sum(rep[x[,t]==4]&z[,t]==1,t-1)
        )/(sum(z[,t]==1))#to avoid nan if group empty
      }else if (condition$interaction == "confidence") { #interact with mean expectancy
        G[t,] = c(mean(Exweight[z[,t]==1,t-1,1]),
                  mean(Exweight[z[,t]==1,t-1,2]),
                  mean(Exweight[z[,t]==1,t-1,3]),
                  mean(Exweight[z[,t]==1,t-1,4])+.00000001)}
      if (is.nan(G[t,1])) {G[t,]=c(.0000001,.0000001,.0000001,.0000001)}
      
      # compute reward for group decision
      Gx = which.max(G[t,])
      Rg = c(0,0,0,0)   # reset reward representation R for all decks on a trial
      Rg[Gx] = Rewards[t,Gx]
      Lg = c(0,0,0,0)   # reset loss representation L for all decks on a trial
      Lg[Gx] = Losses[t,Gx]
      
      group_choice[sim,t] <- Gx
      
      for (n in 1:nagents) {# each agent decides which deck      
        
        #----compute reward for what agent's choice would have been had they gone alone, 
        #irrespective of whether she joined the group or not
        Rx = c(0,0,0,0)   # reset reward representation R for all decks on a trial
        Rx[x[n,t]]=Rewards[t,x[n,t]] # and update on the basis of the chosen deck
        
        Lx = c(0,0,0,0) # reset loss representation L for all decks on a trial
        Lx[x[n,t]]=Losses[t,x[n,t]] # and update on the basis of the chosen deck
        
        #Result of choice if had not been in group - used for reputation only
        perScore[n,t] = sum(Rx-Lx)
        
        # calculate the actual outcome the agent gets depending on group affiliation
        R = ((1 - z[n,t]) * Rx) + (z[n,t]*Rg)
        L = ((1 - z[n,t]) * Lx) + (z[n,t]*Lg)
        
        #apply prospect theory to outcomes
        VxL = -wx[n]*abs(L)^Ax[n] 
        VxR = R^Ax[n] 
        
        Vx[n,t,] <- VxR + VxL
        
        #Remove errors
        Vx[n,t,][is.na(Vx[n,t,])] = 0
        Vx[n,t,][Vx[n,t,]< (-10000)] = -37
        
        # Update deck expected valence - apply delta learning rule
        Ex[n,t,] = Ex[n,t-1,] + (ax[n] * (Vx[n,t,] - Ex[n,t-1,]))
        
        # set maximum Ex = 5, to avoid inf values in conversion to Px
        Ex[n,t,][Ex[n,t,]>5]<-5
        
        # transform to proportional probabilities
        expEx[n,t,] = exp(thetax[n]*Ex[n,t,])
        Px[n,t,] = expEx[n,t,]/sum(expEx[n,t,])
        
        # update expectancies for decision to collaborate. values are updated on the basis of valence Vz 
        # on each trial, which tracks summed discrepency between own and group choice. 
        # Index is z + 1 because only the valence on the chosen option 
        # (collaborate or nor) is updated
        
        # Calculate the objective reward discrepency between each individuals choice, and the group choice
        # Z[1] = objective reward of choosing alone, Z[2] = value of being in group
        
        Z = c(sum(Rx+Lx)-sum(Rg+Lg),sum(Rg+Lg)-sum(Rx+Lx)) #value in group only if not in group      
        
        # Apply prospect theory
        Vz[n,t,Z>0] = Z[Z>0]^Az[n] 
        Vz[n,t,Z<0] = -wz[n]*abs(Z[Z<0])^Az[n] 
        
        #transformation to expectancy. 
        #2 expectancies are coded, representing differences in Knowledge
        #Ein is the perspective from "inside" the group. Here, the agent is not told what their reward would
        #have been had they chosen themselves, and only receives feedback about the group choice of which they 
        #are a part. They must use this knowledge, plus their past expectancies, to update their current expectancies
        #So this is the difference between previous expectancies and current group expectancies
        Ein = c(Ex[n,t-1,x[n,t]] - Ex[n,t,Gx], Ex[n,t,Gx] - Ex[n,t-1,x[n,t]]) 
        
        #Eout is the perspective from "outside"" the group, here the expectancy is based on the difference
        #in value between joining the group or not, where the agent has access to objective value difference 
        #in choosing alone or choosing in the group. They can update their expectancies objectively
        Eout = Ez[n,t-1,] + (az[n] * (Vz[n,t,] - Ez[n,t-1,]))
        
        #If the knowledge condition of the simulation is symmetric, then all agents get objective knowledge
        #of own and group choices
        if (condition$knowledge == "symmetrical"){
          Ez[n,t,] = ((1 - z[n,t]) * Eout) + (z[n,t]*Eout)}  
        else if (condition$knowledge == "asymmetrical") {
          Ez[n,t,] = ((1 - z[n,t]) * Eout) + (z[n,t]*Ein)} 
        
        # set maximum Ex = 5, to avoid inf values in conversion to Px
        Ez[n,t,][Ez[n,t,]>5]= 5
        Ez[n,t,][Ez[n,t,]< (-5)]= -5
        
        #transform expectancies and convert to probabilities
        expEz[n,t,] = exp(thetaz[n]*Ez[n,t,])
        Pz[n,t,] = expEz[n,t,]/sum(expEz[n,t,]) 
        
        #Remove negative probabilities and NA's
        Pz[n,t,][is.na(Pz[n,t,])] = Pz[n,t-1,]
        
        # track the balance of the agent
        balance[n,t]=sum(R)+sum(L)
        
        #print(c("t",t,"n",n,"R",R,"L",L,"Vx",Vx[n,t,],"Ex",Ex[n,t,],"expEx",expEx[n,t,],"Px",Px[n,t,]))
        
      } # close agent loop
      
      avgbalanceIn[t] = mean(balance[z[,t]==1,t])
      avgbalanceOut[t] = mean(balance[z[,t]==0,t])
      balanceCum[t] = mean(balance[,t]) + balanceCum[t-1]
      
    } # close trial loop 
    
    propJoin = cbind(propJoin,colMeans(z[,2:ntrials]))
    
    propA_in= cbind(propA_in,colMeans(x[,2:ntrials]==1&z[,2:ntrials]==1)/colMeans(z[,2:ntrials]==1))
    propB_in= cbind(propB_in,colMeans(x[,2:ntrials]==2&z[,2:ntrials]==1)/colMeans(z[,2:ntrials]==1))
    propC_in= cbind(propC_in,colMeans(x[,2:ntrials]==3&z[,2:ntrials]==1)/colMeans(z[,2:ntrials]==1))
    propD_in= cbind(propD_in,colMeans(x[,2:ntrials]==4&z[,2:ntrials]==1)/colMeans(z[,2:ntrials]==1))
    
    propA_out= cbind(propA_out,colMeans(x[,2:ntrials]==1&z[,2:ntrials]==0)/colMeans(z[,2:ntrials]==0))
    propB_out= cbind(propB_out,colMeans(x[,2:ntrials]==2&z[,2:ntrials]==0)/colMeans(z[,2:ntrials]==0))
    propC_out= cbind(propC_out,colMeans(x[,2:ntrials]==3&z[,2:ntrials]==0)/colMeans(z[,2:ntrials]==0))
    propD_out= cbind(propD_out,colMeans(x[,2:ntrials]==4&z[,2:ntrials]==0)/colMeans(z[,2:ntrials]==0))
    
    muBalance_cum = cbind(muBalance_cum,balanceCum)
    All_avgbalanceIn = cbind(All_avgbalanceIn, avgbalanceIn)
    All_avgbalanceOut = cbind(All_avgbalanceOut, avgbalanceOut)
    muBalanceOut = cbind(muBalanceOut,mean(balance[z==0]))
    muBalanceIn = cbind(muBalanceIn,mean(balance[z==1]))
    
    muExpA <- cbind(muExpA,colMeans(Ex[,2:ntrials,1]))
    muExpB <- cbind(muExpB,colMeans(Ex[,2:ntrials,2]))
    muExpC <- cbind(muExpC,colMeans(Ex[,2:ntrials,3]))
    muExpD <- cbind(muExpD,colMeans(Ex[,2:ntrials,4]))
    
    muExpOut = cbind(muExpOut,colMeans(Ez[,2:ntrials,1]))
    muExpIn = cbind(muExpIn,colMeans(Ez[,2:ntrials,2]))
    nswitch=c(nswitch,sum(abs((z[1,1:(ntrials-1)]-z[1,2:ntrials]))))
    
    #print(sim)
    
  } # close simulation loop
  
  Pz = Pz
  x = x
  group_choice = group_choice
  
  result = list(propA_in=propA_in,
                propB_in=propB_in,
                propC_in=propC_in,
                propD_in=propD_in,
                
                propA_out=propA_out,
                propB_out=propB_out,
                propC_out=propC_out,
                propD_out=propD_out,
                
                propJoin=propJoin,
                muExpA=muExpA,
                muExpB=muExpB,
                muExpC=muExpC,
                muExpD=muExpD,
                
                muBalanceOut=muBalanceOut,
                muBalanceIn=muBalanceIn,
                All_avgbalanceIn=All_avgbalanceIn,
                All_avgbalanceOut=All_avgbalanceOut,
                muBalance_cum=muBalance_cum,
                
                muExpOut=muExpOut,
                muExpIn=muExpIn,
                nswitch=nswitch,
                Pz=Pz,
                x = x,
                group_choice=group_choice,
                group_reward=group_reward,
                group_loss=group_loss)
  
  return(result)
  
} # close function

#===============================================================================
#======================= Plotting function =====================================
#===============================================================================

# Here we take the output from the simulation and plot results figures for the paper
plotResults <- function(condA, condB, condLabels) {
  
  layout(matrix(c(1,1,1,1, 2,2, 3,3, 4,4, 5,5, 6,7,8,9), 
                nrow = 4, ncol = 4, byrow = TRUE))
  
  #-----------------------------------------------------------
  ##### Plot proportion collaborating over trials ############
  # Plots all simulations, with mean in bold
  COL <- adjustcolor(c("blue", "red"), alpha = 0.1)
  
  plot(rowMeans(condA$propJoin),
       xlab = "Trial Number",
       ylab = "Proportion Collaborating",
       main=condLabels[3],
       type='l',
       ylim=c(0,1.1),
       lwd=4,
       col="dark blue",
       axes=FALSE)
  axis(1)
  axis(2)
  for (i in 1:100) {
    lines(condA$propJoin[,i],col=COL[1])
  }
  lines(rowMeans(condB$propJoin),type='l',ylim=c(0,1),lwd=4,col="dark red")
  for (i in 1:100) {
    lines(condB$propJoin[,i],col=COL[2])
  }
  
  #legend (x = 35, y = 1.2, 
  #        legend = c(condLabels[1], condLabels[2]), 
  #        col = c("dark blue","dark red"), bty = "n", lwd = 4)
  
  #-----------------------------------------------------------
  ##### Plot distribution of mean value gained/lost for joining/defecting for condition A  
  COL <- adjustcolor(c("dark blue", "light blue"), alpha = 0.7)
  
  plot(density(condA$muBalanceOut),xlim=c(-100,100),ylim=c(0,.1),
       axes=FALSE,xlab="Mean Trial Value/Simulation",
       ylab="",main=condLabels[1])
  axis(1)
  polygon(density(condA$muBalanceOut), col=COL[1])
  lines(density(condA$muBalanceIn))
  polygon(density(condA$muBalanceIn), col=COL[2])
  legend (x = -120, y = 0.16, 
          legend = c(paste("Defect, MLV =", round(mlv(condA$muBalanceOut)$M,digits=2)), 
                     paste("Join, MLV =", round(mlv(condA$muBalanceIn)$M,digits=2))), 
          col = c("dark blue","light blue"), bty = "n", lwd = 4, y.intersp = 0.15)
  
  #-----------------------------------------------------------
  ##### Plot distribution of mean value gained/lost for joining/defecting for condition B    
  COL <- adjustcolor(c("dark red", "red"), alpha = 0.7)
  
  plot(density(condB$muBalanceOut),xlim=c(-100,100),ylim=c(0,.1),
       axes=FALSE,xlab="Mean Trial Value/Simulation",
       ylab="",main=condLabels[2])
  axis(1)
  polygon(density(condB$muBalanceOut), col=COL[1])
  lines(density(condB$muBalanceIn))
  polygon(density(condB$muBalanceIn), col=COL[2])
  legend (x = -120, y = 0.16, 
          legend = c(paste("Defect, MLV =", round(mlv(condB$muBalanceOut)$M,digits=2)), 
                     paste("Join, MLV =", round(mlv(condB$muBalanceIn)$M,digits=2))),
          col = c("dark red","red"), bty = "n", lwd = 4, y.intersp = 0.15)
  
  plot(rowMeans(condA$muBalance_cum),
       xlab = "Trial Number",
       ylab = "Cumulative Balance (Mean)",
       type='l',
       ylim=c(-300,300),
       lwd=4,
       col="dark blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(condB$muBalance_cum,na.rm =TRUE),type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  
  plot(rowMeans(sqrt((condA$muBalance_cum-rowMeans(condA$muBalance_cum))^2)),
       xlab = "Trial Number",
       ylab = "Cumulative Balance (SD)",
       type='l',
       ylim=c(0,1200),
       lwd=4,
       col="dark blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(sqrt((condB$muBalance_cum-rowMeans(condB$muBalance_cum,na.rm =TRUE))^2))
        ,type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  plot(rowMeans(condA$propA_in,na.rm = TRUE),
       xlab = "Trial Number",
       ylab = "Proportion Choose A",
       type='l',
       ylim=c(0,.6),
       lwd=4,
       col="light blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(condA$propA_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark blue")
  lines(rowMeans(condB$propA_in,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="red")
  lines(rowMeans(condB$propA_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  
  plot(rowMeans(condA$propB_in,na.rm = TRUE),
       xlab = "Trial Number",
       ylab = "Proportion Choose B",
       type='l',
       ylim=c(0,.6),
       lwd=4,
       col="light blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(condA$propB_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark blue")
  lines(rowMeans(condB$propB_in,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="red")
  lines(rowMeans(condB$propB_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  
  plot(rowMeans(condA$propC_in,na.rm = TRUE),
       xlab = "Trial Number",
       ylab = "Proportion Choose C",
       type='l',
       ylim=c(0,.6),
       lwd=4,
       col="light blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(condA$propC_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark blue")
  lines(rowMeans(condB$propC_in,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="red")
  lines(rowMeans(condB$propC_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  
  plot(rowMeans(condA$propD_in,na.rm = TRUE),
       xlab = "Trial Number",
       ylab = "Proportion Choose D",
       type='l',
       ylim=c(0,.6),
       lwd=4,
       col="light blue",
       axes=FALSE)
  axis(1)
  axis(2)
  lines(rowMeans(condA$propD_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark blue")
  lines(rowMeans(condB$propD_in,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="red")
  lines(rowMeans(condB$propD_out,na.rm = TRUE),type='l',ylim=c(0,1),lwd=4,col="dark red")
  
  simPlot <- recordPlot()
  return(simPlot)
  
}

#===============================================================================
#======================= Call simulation and plotting functions ================
#===============================================================================

############## run simulations ############
loyCurve = 1
perCurve = 1

#CONFIDENCE
condition$interaction = "confidence"
condition$reputation = "baseline"
symconfbaseline=multiPVL(sim.params, agent.params, condition)

condition$reputation = "performance"
symconfper=multiPVL(sim.params, agent.params, condition)


##################  -----   plotting ---- ################

condLabels = c("Equal","Performance", "Symmetry, Confidence")
confidence.loyalty.performance.plot = plotResults(symconfbaseline,symconfper,condLabels)


#===============================================================================
#=============================== Make Dataset ==================================
#===============================================================================

###Create datasets in baseline condition
#In group
d_in = cbind(symconfbaseline$muBalanceIn[1,],
              c(1:sim.params$nsims),
              rep("No_Weighting", sim.params$nsims),
              rep("Vote", sim.params$nsims),
              rep("Symmetrical", sim.params$nsims),
              rep("Participate", sim.params$nsims))

#Out of group
d_out = cbind(symconfbaseline$muBalanceOut[1,],
             c(1:sim.params$nsims),
             rep("No_Weighting", sim.params$nsims),
             rep("Vote", sim.params$nsims),
             rep("Symmetrical", sim.params$nsims),
             rep("Defect", sim.params$nsims))

d1 = rbind(d_out, d_in)

###Create datasets in performance condition
#In group
d_in = cbind(symconfper$muBalanceIn[1,],
             c(1:sim.params$nsims),
             rep("Performance", sim.params$nsims),
             rep("Vote", sim.params$nsims),
             rep("Symmetrical", sim.params$nsims),
             rep("Participate", sim.params$nsims))

#Out of group
d_out = cbind(symconfper$muBalanceOut[1,],
              c(1:sim.params$nsims),
              rep("Performance", sim.params$nsims),
              rep("Vote", sim.params$nsims),
              rep("Symmetrical", sim.params$nsims),
              rep("Defect", sim.params$nsims))

d2 = rbind(d_out, d_in)

#Combine finally
d = rbind(d1, d2)
#Rename
colnames(d) = c("Average_score", "Simulation", "Weighting_condition", "Group_decision", "Knowledge_structure", "Group_affiliation")
#Make dataframe
d = as.data.frame(d)

#Make into numeric
d$Average_score = as.numeric(levels(d$Average_score))[d$Average_score]
d$Simulation = as.numeric(levels(d$Simulation))[d$Simulation]

#Scale+centering
d$Average_score = scale(d$Average_score)

#===============================================================================
#========================== Calculate Effect SIze ==============================
#===============================================================================

library(brms)

#Find prior options
 get_prior(Average_score ~ Weighting_condition*Group_affiliation +
             (1 + Weighting_condition|Simulation),
           d)


###Model with loose priors
#Create list of prios
priors.loose = c(
  prior(normal(0,1), class = Intercept),
  prior(normal(0,0.1), class = b, coef = Group_affiliationParticipate),
  prior(normal(0,0.1), class = b, coef = Weighting_conditionPerformance),
  prior(normal(0,0.1), class = b, coef = Weighting_conditionPerformance:Group_affiliationParticipate),
  prior(cauchy(0,2), class = sigma)
)

#Create Model
m_loose <- brm(Average_score ~ Weighting_condition*Group_affiliation +
                 (1 + Weighting_condition|Simulation), 
               data = d, 
               prior = priors.loose,
               iter = 5000, 
               warmup = 2000, 
               chain = 2, 
               cores = 2)
#Include WAIC
m_loose = add_ic(m_loose, ic = "waic")

#Get output
m_loose
#Plot output
plot(m_loose)

#Quality check
pp_check(m_loose, nsamples = 100)


###Model with strict priors
#Create list of prios
priors.strict = c(
  prior(normal(0,1), class = Intercept),
  prior(normal(0,0.01), class = b, coef = Group_affiliationParticipate),
  prior(normal(0,0.01), class = b, coef = Weighting_conditionPerformance),
  prior(normal(0,0.01), class = b, coef = Weighting_conditionPerformance:Group_affiliationParticipate),
  prior(cauchy(0,2), class = sigma)
)

#Create Model
m_strict <- brm(Average_score ~ Weighting_condition*Group_affiliation +
                 (1 + Weighting_condition|Simulation), 
               data = d, 
               prior = priors.strict,
               iter = 5000, 
               warmup = 2000, 
               chain = 2, 
               cores = 2)
#Include WAIC
m_strict = add_ic(m_strict, ic = "waic")

#Get output
m_strict
#Plot output
plot(m_strict)

#Quality check
pp_check(m_strict, nsamples = 100)


###Simple model with strict priors
#Create list of prios
priors.simple = c(
  prior(normal(0,1), class = Intercept),
  prior(normal(0,0.01), class = b, coef = Group_affiliationParticipate),
  prior(normal(0,0.01), class = b, coef = Weighting_conditionPerformance),
  prior(cauchy(0,2), class = sigma)
)

#Create Model
m_simple <- brm(Average_score ~ Weighting_condition+Group_affiliation +
                  (1 + Weighting_condition|Simulation), 
                data = d, 
                prior = priors.simple,
                iter = 5000, 
                warmup = 2000, 
                chain = 2, 
                cores = 2)
#Include WAIC
m_simple = add_ic(m_simple, ic = "waic")

#Get output
m_simple
#Plot output
plot(m_simple)

#Quality check
pp_check(m_simple, nsamples = 100)


###Simple model with loose priors
#Create list of prios
priors.simple_l = c(
  prior(normal(0,1), class = Intercept),
  prior(normal(0,0.1), class = b, coef = Group_affiliationParticipate),
  prior(normal(0,0.1), class = b, coef = Weighting_conditionPerformance),
  prior(cauchy(0,2), class = sigma)
)

#Create Model
m_simple_l <- brm(Average_score ~ Weighting_condition+Group_affiliation +
                  (1 + Weighting_condition|Simulation), 
                data = d, 
                prior = priors.simple_l,
                iter = 5000, 
                warmup = 2000, 
                chain = 2, 
                cores = 2)
#Include WAIC
m_simple_l = add_ic(m_simple_l, ic = "waic")

#Get output
m_simple_l
#Plot output
plot(m_simple_l)

#Quality check
pp_check(m_simple, nsamples = 100)





####Make plots of posteriors
###MAke data frame of posterior of m_strict
d_sceptic = posterior_samples(m_strict, nsamples = 1e4)
d_loose = posterior_samples(m_loose, nsamples = 1e4)
d_simple = posterior_samples(m_simple, nsamples = 1e4)
d_simple_l = posterior_samples(m_simple_l, nsamples = 1e4)

###Make plots of posteriors vs priors
##Sceptic models
#Posteriors in light colors and priors in dark
#Performance weighting
dens(rnorm(1e6, mean = 0, sd = 0.01), col = 'black', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Performance Weighting', ylab = 'Density')
par(new = TRUE)
dens(d_sceptic$b_Weighting_conditionPerformance, col = 'blue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Performance Weighting', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_simple$b_Weighting_conditionPerformance, col = 'steelblue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Performance Weighting', ylab = 'Density', add = T)

#Group
dens(rnorm(1e6, mean = 0, sd = 0.01), col = 'black', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Group Participation', ylab = 'Density')
par(new = TRUE)
dens(d_sceptic$b_Group_affiliationParticipate, col = 'blue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Group Participation', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_simple$b_Group_affiliationParticipate, col = 'steelblue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Group Participation', ylab = 'Density', add = T)

#Interaction
dens(rnorm(1e6, mean = 0, sd = 0.01), col = 'black', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Interaction between Performance Weigthing and Group Participation', ylab = 'Density')
par(new = TRUE)
dens(d_sceptic$`b_Weighting_conditionPerformance:Group_affiliationParticipate`, col = 'blue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Beta-value for Interaction between Performance Weigthing and Group Participation', ylab = 'Density', add = T)


##Loose models
#Posteriors in light colors and priors in dark
#Performance weighting
dens(rnorm(1e6, mean = 0, sd = 0.1), col = 'black', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Performance Weighting', ylab = 'Density')
par(new = TRUE)
dens(d_loose$b_Weighting_conditionPerformance, col = 'blue', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Performance Weighting', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_simple_l$b_Weighting_conditionPerformance, col = 'steelblue', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Performance Weighting', ylab = 'Density', add = T)

#Group
dens(rnorm(1e6, mean = 0, sd = 0.1), col = 'black', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Group Participation', ylab = 'Density')
par(new = TRUE)
dens(d_loose$b_Group_affiliationParticipate, col = 'blue', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Group Participation', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_simple_l$b_Group_affiliationParticipate, col = 'steelblue', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Group Participation', ylab = 'Density', add = T)

#Interaction
dens(rnorm(1e6, mean = 0, sd = 0.1), col = 'black', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Interaction between Performance Weigthing and Group Participation', ylab = 'Density')
par(new = TRUE)
dens(d_loose$`b_Weighting_conditionPerformance:Group_affiliationParticipate`, col = 'blue', xlim = c(-0.35, 0.35), ylim = c(0,7), xlab = 'Beta-value for Interaction between Performance Weigthing and Group Participation', ylab = 'Density', add = T)


##Complex, Sceptic vs. Loose
#prior
dens(rnorm(1e6, mean = 0, sd = 0.1), col = 'Darkgreen', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density')
par(new = TRUE)
dens(rnorm(1e6, mean = 0, sd = 0.01), col = 'green', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
#Performance weighting
dens(d_sceptic$b_Weighting_conditionPerformance, col = 'steelblue', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_loose$b_Weighting_conditionPerformance, col = 'Darkred', xlim = c(-0.05, 0.05), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
#Group
dens(d_sceptic$b_Group_affiliationParticipate, col = 'blue', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_loose$b_Group_affiliationParticipate, col = 'red', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
#Interaction
dens(d_sceptic$`b_Weighting_conditionPerformance:Group_affiliationParticipate`, col = 'cyan', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)
par(new = TRUE)
dens(d_loose$`b_Weighting_conditionPerformance:Group_affiliationParticipate`, col = 'magenta', xlim = c(-0.32, 0.35), ylim = c(0,42), xlab = 'Priors VS. Posteriors', ylab = 'Density', add = T)


##Scatter plots
dev.off()
ggplot(d, aes(Simulation, Average_score, color = Group_affiliation))+
  geom_point()

ggplot(d, aes(Simulation, Average_score, color = Weighting_condition))+
  geom_point()

ggplot(d, aes(Simulation, Average_score, color = Group_affiliation, shape = Weighting_condition))+
  geom_point(aes(size = 1))+
  scale_size(guide = "none")
