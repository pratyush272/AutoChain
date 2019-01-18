
# require(lpSolve)
# 
# # every plant-cust combination
# initial_flow <- c(1,1,1,1,1,1,1,1,1)
# constr_mat <- matrix(c(1, 1, 1,0,0,0,0,0,0,
#               0,0,0,1,1,1,0,0,0,
#               0,0,0,0,0,0,1,1,1,
#               1,0,0,1,0,0,1,0,0,
#               0,1,0,0,1,0,0,1,0,
#               0,0,1,0,0,1,0,0,1), nrow=6, byrow=TRUE)
# thresholds <- c(3,3,3,4,5,6)
# constranints_direction  <- c(">=", ">=", ">=","<=","<=","<=")
# 
# optimum <-  lp(direction="min"
#                ,objective.in = initial_flow
#                ,const.mat = constr_mat
#                , const.dir = constranints_direction
#                ,const.rhs = thresholds
#                ,#/*all.int = T
#                )
# 
# optimum$solution
# optimum$objval

require(lpSolveAPI)
plant<-data.frame(plt=c('a','b','c'), capacity=c(4,5,6), varcost=c(2,3,4))
demand<-data.frame(cust=c('p','q','r'), dmd=c(3,3,3))

#create an LP model with 6 constraints and 9 decision variables
lpmodel<-make.lp(6*NROW(train)+NROW(demand),9)

#build the model column per column
column<- 0
row<- 0



for(wg in train$wagon){
  row<-row+1
  for(cust in seq(1,NROW(demand$cust))){
    column<-column+1
    
    #this takes the arguments 'column','values' & 'indices' (as in where these values should be placed in the column)
    set.column(lpmodel,column,c(1, demand[cust,'volume'],1), indices=c(row,NROW(train)+row, NROW(train)*2+cust))
  }}

#set rhs weight constraints
set.constr.value(lpmodel, rhs=train$weightcapacity, constraints=seq(1,NROW(train)))

#set rhs volume constraints
set.constr.value(lpmodel, rhs=train$spacecapacity, constraints=seq(NROW(train)+1,NROW(train)*2))


#set rhs volume constraints
set.constr.value(lpmodel, rhs=demand$available, constraints=seq(NROW(train)*2+1,NROW(train)*2+NROW(demand)))

#set objective coefficients
set.objfn(lpmodel, rep(demand$profit,NROW(train)))

#set objective direction
lp.control(lpmodel,sense='max')

#I in order to be able to visually check the model, I find it useful to write the model to a text file
write.lp(lpmodel,'model.lp',cust='lp')
