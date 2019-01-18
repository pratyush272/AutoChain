library(dplyr)

source = c("a","b","c")
dmdnode = c("p","q","r")
demandQty <- c(3,3,3)

d = expand.grid(source=source, dmdnode=dmdnode)
intenifierList<- paste0("id_",append(plt,dmdnode))
d[intenifierList]<- NA

cname<- append(plt,dmdnode)
for(i in 1:nrow(d)){
  for (j in 1:length(cname))
  {
    if(d[i,1]==cname[j]){d[i,j+2]<- 1}
    if(d[i,2]==cname[j]){d[i,j+2]<- 1}
    print(i)
  }
}

d[is.na(d)]<- 0

prd_cap<- data.frame(source=c("a","b","c"),prcap=c(4,5,6))
dmd_cap<- data.frame(dmdnode=c("p","q","r"),dmdcap=c(3,3,3))
d<- d %>% left_join(prd_cap,by = "source")
d<- d %>% left_join(dmd_cap,by = "dmdnode")

d$leg<- paste0(d$source,d$dmdnode)

d$source<- NULL
d$dmdnode<- NULL

f<-data.frame(t(d))
colnames(f)<- d$leg

min.RSS <- function(data, par) {
  with(data, sum((par[1]  * x - y)^2))
}

