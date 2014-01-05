setwd("C:/Users/lfortini/Dropbox/CPT creation/")
rm(list = ls()) #remove all past worksheet variables
#do not place conditional nodes up front of array, but as first parent in CPT in genie

####parameter configuration start here: each of these lists have different sizes, some of them below line 10 may be empty vectors
real_node_state_direction=c(1,1) #0- fist parent state decr favorability (p of first child state), 1- incr favorability  
node_importance=c(3, 3) #1- irrelevant, 2- relevant, 3- important, 4- crucial
parent_node_states=c(2, 2) #if not specified will assume 2 states for all parents
n_conditional_nodes=0
conditional_flip= list(c(0)) #for each conditional node, add vector ' c()'  of states that will have direction flipped 
conditional_change_importance= list(c(1,4))  #for each conditional node, add vector ' c()'  of nodes that will have importance changed
conditional_change_importance_amount= list(c(-1,-1)) #for each conditional node, add vector ' c()'  of changes in state importance
n_off_switch=2
off_positions=c(1,1)
off_directions=c(1,1)
relative_importance=c(0, 1, 1.5, 2) #relative weights of node importance
####end parameter configuration


repmat = function(X,m,n){
##R equivalent of repmat (matlab)
mx = dim(X)[1]
nx = dim(X)[2]
matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)}

#START UNDERHOOD CODE 
#create conditional node weights
node_state_direction=real_node_state_direction

if (exists("parent_node_states")){
  node_states=parent_node_states
  } else {
  node_states=array(2,length(node_state_direction))}                              
  
n_nodes=length(node_states)
all_node_importance=t(as.matrix(node_importance))
all_node_state_direction=t(as.matrix(node_state_direction))
if (n_conditional_nodes>0){
  for (i in n_conditional_nodes:1){ 
    temp_node_state_direction=all_node_state_direction
    temp_node_importance=all_node_importance
    if (conditional_flip[[i]]!=0){  
      temp_node_state_direction[,conditional_flip[[i]]]=1-temp_node_state_direction[,conditional_flip[[i]]] #flips direction
      #temp_node_state_direction=t(as.matrix(temp_node_state_direction))
    }
    if (any(conditional_change_importance[[i]]!=0)){
      temp_node_importance[,conditional_change_importance[[i]]]=as.matrix(temp_node_importance[,conditional_change_importance[[i]]])+repmat(as.matrix(conditional_change_importance_amount[[i]]),nrow(temp_node_importance),1) #flips direction
    temp_node_importance[temp_node_importance<1]=1
    temp_node_importance[temp_node_importance>length(relative_importance)]=length(relative_importance)
    }
    all_node_importance=rbind(all_node_importance, temp_node_importance)
    all_node_state_direction=rbind(all_node_state_direction, temp_node_state_direction)
  }
}
all_node_weights=relative_importance[as.vector(all_node_importance)]
all_node_weights=matrix(all_node_weights, dim(all_node_importance))
all_node_weights=all_node_weights/repmat(matrix(apply(all_node_weights, 1, sum),nrow(all_node_weights),1),1,ncol(all_node_importance))
n_conditions=nrow(all_node_importance)

#start routine
all_Ps=matrix(,2,1)  
for (j in 1:n_conditions){
  node_weights=all_node_weights[j,]
  cond_node_state_direction=all_node_state_direction[j,]
  
  for (i in n_nodes:1){
    if (i==n_nodes){
      if (cond_node_state_direction[i]==1){
        node_state=seq(0,1,length=node_states[i])
      }
      else {
        node_state=seq(1,0,length=node_states[i])
      }
      
      node_weight=node_state*node_weights[i]
      probs=array(node_weight, dim=c(node_states[i],1))                         
    }
    else {
      X=probs
      m=node_states[i]
      n=1
      mx = dim(X)[1]
      nx = dim(X)[2]
      probs_a=matrix(t(matrix(X,mx,nx*n)),mx*m,nx*n,byrow=T)
      
      if (cond_node_state_direction[i]==1){
        node_state=seq(0,1,length=node_states[i])
      }
      else {
        node_state=seq(1,0,length=node_states[i])
      }
      
      node_weight=node_state*node_weights[i]
      jnk=rep(node_weight,each=mx)
      jnk=array(jnk, dim=c(length(jnk),1))
      probs=cbind(jnk, probs_a)
    }
  }
  pSum=rowSums(probs)
  p_table=cbind(probs, pSum, 1-pSum) 
  p_only=cbind(pSum, 1-pSum)
  p_only=t(p_only)
  all_Ps=cbind(all_Ps, p_only)
}
all_Ps=all_Ps[,-1]
if (n_off_switch>0){
  i=1
  for (i in 1:n_off_switch){
    jnk0=matrix(0,1,dim(all_Ps)[2])
    jnk1=matrix(1,1,dim(all_Ps)[2])
    if (off_directions[i]==1){
      jnk=rbind(jnk0,jnk1)  
    }else{
      jnk=rbind(jnk1,jnk0)        
    }
    if (off_positions[1]==1){  
      all_Ps=cbind(jnk,all_Ps)
    }else{
      all_Ps=cbind(all_Ps,jnk)    
    }
  }
}
all_Ps[all_Ps==0]=0.0001
all_Ps[all_Ps==1]=0.9999
p_table[p_table==0]=0.0001
p_table[p_table==1]=0.9999

###END UNDERHOOD CODE

write.table(all_Ps, file = "all_Ps.csv", sep = ",", col.names = NA)
write.table(p_table, file = "p_table.csv", sep = ",", col.names = NA)

