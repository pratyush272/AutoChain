
chain_clean_file <- function(file,headers) {
  library(data.table)
  print("Folowing info should be there in your file:")
  print(data.frame(headers))
  tmp<- fread(file)
  col_ids <- vector()
  
  for (i in headers)
  {
    print(paste0("Select column for ", i))
    choice<- menu(colnames(tmp))
    col_ids<- append(col_ids,choice)
  }
  tmp<- tmp[,..col_ids]
  colnames(tmp)<- headers
  return(tmp)
}#endFunction_chain_shipment_data_clean

chain_shipment_data <- function(file )
  {
  headers = c("date","delvering_node_id","customer_id","product_code","units","revenue")
  return(chain_clean_file(file,headers))
}

chain_site_master <- function(file )
{
  headers = c("site_id","site_name","site_zip","site_city","site_state","site_country","site_lat","site_long")
  return(chain_clean_file(file,headers))
}

chain_prod_master <- function(file )
{
  headers = c("prod_code","Prod_name","units_per_load")
  return(chain_clean_file(file,headers))
}

chain_eord <- function(file )
{
  headers = c("product","from_location","to_location")
  return(chain_clean_file(file,headers))
}

chain_shipment_summary<- function(tmp)
{
  library(ggplot2)
  tmp<- tmp[,sum(units),by=(customer_id)]
  colnames(tmp)<- c("customer_id","units")
  tmp<- tmp[order(-tmp$units)]
  tmp$customer_id<- factor(tmp$customer_id,levels = tmp$customer_id)
  tmp$cumulative <- cumsum(tmp$units)
  b<- head(tmp,10)
  plt1<- ggplot(b,aes(x=b$customer_id)) +
    geom_bar(aes(y=b$units), fill='blue', stat="identity") +
    geom_point(aes(y=b$cumulative), color = rgb(0, 1, 0), pch=16, size=1) +
    geom_path(aes(y=b$cumulative, group=1), colour="slateblue1", lty=3, size=0.9) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
    labs(title = "Pareto Plot", subtitle = "Shipment", x = 'Customers', y ='units') + 
    scale_y_continuous("Shipped Units", sec.axis = sec_axis(~ . / sum(tmp$units), name = "%age of Shipment"))
  
  print(plt1)
}

chain_data_gaps<- function(shipment,sites,eord,prod_master)
{
  library(data.table)
  library(sqldf)
  tmp<- unname(unlist(sqldf("select a.customer_id from shipment a left join sites b on a.customer_id=b.site_id where b.site_id is null")))
  tmp<- append(tmp,unname(unlist(sqldf("select a.delvering_node_id from shipment a left join sites b on a.delvering_node_id=b.site_id where b.site_id is null"))))
  tmp<- append(tmp,unname(unlist(sqldf("select a.from_location from eord a left join sites b on a.from_location=b.site_id where b.site_id is null"))))
  tmp<- append(tmp,unname(unlist(sqldf("select a.to_location from eord a left join sites b on a.to_location=b.site_id where b.site_id is null"))))
  
  if(length(tmp)>0){
    print(paste0(length(tmp)," nodes are missing is the Sites table..."))
    print(unique(tmp))
  }
  else{print("All nodes are available in Sites table")}

  tmp<- unlist(sqldf("select a.product_code from shipment a left join prod_master b on a.product_code=b.prod_code where b.prod_code is null"))

  if(length(tmp)>0){
    print(paste0(length(tmp)," products are missing is the Product master.."))
    print(unique(tmp))
  }
  else{print("All products are available in product master")}
  
}#end of chain_data_gaps

chain_historical_flows<- function(shipment,eord)
{
  library(sqldf)
  library(dplyr)
  library(stringr)
  
  eord$pass1<- unname(unlist(sqldf("select coalesce(b.to_location,a.to_location) from eord a left join eord b on a.product=b.product and a.to_location=b.from_location")))
  eord$pass2<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass1) from eord a left join eord b on a.product=b.product and a.pass1=b.from_location")))
  eord$pass3<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass2) from eord a left join eord b on a.product=b.product and a.pass2=b.from_location")))
  eord$pass4<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass3) from eord a left join eord b on a.product=b.product and a.pass3=b.from_location")))
  eord$pass5<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass4) from eord a left join eord b on a.product=b.product and a.pass4=b.from_location")))
  
  
  eord<- eord[, lapply(.SD, as.character)]
  
  eord$nPasses<- apply(eord,1,function(x){length(unique(x))-1})
  eord<- eord[with(eord, ave(-nPasses, product, FUN = order)) %in% c(1), ]
  
  #shipment[date, delvering_node_id, customer_id, product_code, units:=sum(units), revenue:=sum(revenue)]
  shipment<- shipment[,.(units=sum(units),revenue=sum(revenue)), by=.(delvering_node_id,customer_id,product_code)]
  shipment<- shipment[, lapply(.SD, as.character)]
  
  shipment<- sqldf("select distinct a.*
                ,coalesce(b.pass4,c.pass3,d.pass2,e.pass1,f.to_location,g.from_location,a.delvering_node_id) as pass5 from shipment a 
                left join eord b on a.product_code=b.product and a.delvering_node_id = b.pass5
                left join eord c on a.product_code=c.product and a.delvering_node_id = c.pass4
                left join eord d on a.product_code=d.product and a.delvering_node_id = d.pass3
                left join eord e on a.product_code=e.product and a.delvering_node_id = e.pass2
                left join eord f on a.product_code=f.product and a.delvering_node_id = f.pass1
                left join eord g on a.product_code=g.product and a.delvering_node_id = g.to_location
            ")
  
  shipment<- sqldf("select distinct a.*
                ,coalesce(c.pass3,d.pass2,e.pass1,f.to_location,g.from_location,a.pass5) as pass4 from shipment a 
                   left join eord c on a.product_code=c.product and a.pass5 = c.pass4
                   left join eord d on a.product_code=d.product and a.pass5 = d.pass3
                   left join eord e on a.product_code=e.product and a.pass5 = e.pass2
                   left join eord f on a.product_code=f.product and a.pass5 = f.pass1
                   left join eord g on a.product_code=g.product and a.pass5 = g.to_location
                   ")
  shipment<- sqldf("select distinct a.*
                ,coalesce(d.pass2,e.pass1,f.to_location,g.from_location,a.pass4) as pass3 from shipment a 
                   left join eord d on a.product_code=d.product and a.pass4 = d.pass3
                   left join eord e on a.product_code=e.product and a.pass4 = e.pass2
                   left join eord f on a.product_code=f.product and a.pass4 = f.pass1
                   left join eord g on a.product_code=g.product and a.pass4 = g.to_location
                   ")
  shipment<- sqldf("select distinct a.*
                ,coalesce(e.pass1,f.to_location,g.from_location,a.pass3) as pass2 from shipment a 
                   left join eord e on a.product_code=e.product and a.pass3 = e.pass2
                   left join eord f on a.product_code=f.product and a.pass3 = f.pass1
                   left join eord g on a.product_code=g.product and a.pass3 = g.to_location
                   ")
  shipment<- sqldf("select distinct a.*
                ,coalesce(f.to_location,g.from_location,a.pass2) as pass1 from shipment a 
                   left join eord f on a.product_code=f.product and a.pass2 = f.pass1
                   left join eord g on a.product_code=g.product and a.pass2 = g.to_location
                   ")
  shipment<- sqldf("select distinct a.*
                ,coalesce(g.from_location,a.pass1) as pp_code from shipment a 
                   left join eord g on a.product_code=g.product and a.pass1 = g.to_location
                   ")
  
  shipment<- data.table(shipment)
  shipment<- shipment[,.(product_code,pp_code,pass1,pass2,pass3,pass4,pass5,delvering_node_id,customer_id,units,revenue)]
  
  ### separating Legs
  shipment$routeType<-  "route"
  shipment$nPasses <- apply(shipment,1,function(x){length(unique(x))-6})
  shipment[nPasses==0,]$routeType<- "PP-->CUST"
  shipment[nPasses==1,]$routeType<- "PP-->PLT-->CUST"
  shipment[nPasses==2,]$routeType<- "PP-->PLT-->PLT-->CUST"
  shipment[nPasses==3,]$routeType<- "PP-->PLT-->PLT-->PLT-->CUST"
  shipment[nPasses==4,]$routeType<- "PP-->PLT-->PLT-->PLT-->PLT-->CUST"
  shipment[nPasses==5,]$routeType<- "PP-->PLT-->PLT-->PLT-->PLT-->PLT-->CUST"
  shipment[nPasses==6,]$routeType<- "PP-->PLT-->PLT-->PLT-->PLT-->PLT-->PLT-->CUST"
  
  shipment<- shipment[,.(routeType,product_code,production_leg=paste0(pp_code,"~",pp_code),leg1=paste0(pp_code,"~",pass1),leg2=paste0(pass1,"~",pass2),leg3=paste0(pass2,"~",pass3),leg4=paste0(pass3,"~",pass4),leg5=paste0(pass4,"~",pass5),leg6=paste0(pass5,"~",delvering_node_id),leg7=paste0(delvering_node_id,"~",customer_id),customer_id,units,revenue)]
  
  # shipment<- rbind(
  #   shipment[,.(routeType,product_code,origin= unlist(strsplit(production_leg,"~"))[1],destination=unlist(strsplit(production_leg,"~"))[2],routeOrder=0,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg1,"~"))[1],destination=unlist(strsplit(leg1,"~"))[2],routeOrder=1,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg2,"~"))[1],destination=unlist(strsplit(leg2,"~"))[2],routeOrder=2,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg3,"~"))[1],destination=unlist(strsplit(leg3,"~"))[2],routeOrder=3,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg4,"~"))[1],destination=unlist(strsplit(leg4,"~"))[2],routeOrder=4,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg5,"~"))[1],destination=unlist(strsplit(leg5,"~"))[2],routeOrder=5,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg6,"~"))[1],destination=unlist(strsplit(leg6,"~"))[2],routeOrder=6,customer_id,units,revenue)]
  #   ,shipment[,.(routeType,product_code,origin= unlist(strsplit(leg7,"~"))[1],destination=unlist(strsplit(leg7,"~"))[2],routeOrder=7,customer_id,units,revenue)]
  # )
  
  shipment<- rbind(
    shipment[,.(routeType,product_code,leg= production_leg,routeOrder=0,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg1,routeOrder=1,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg2,routeOrder=2,customer_id,units,revenue)]    
    ,shipment[,.(routeType,product_code,leg= leg3,routeOrder=3,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg4,routeOrder=4,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg5,routeOrder=5,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg6,routeOrder=6,customer_id,units,revenue)]
    ,shipment[,.(routeType,product_code,leg= leg7,routeOrder=7,customer_id,units,revenue)]
    )
  od<- data.frame(str_split_fixed(shipment$leg,"~",2),stringsAsFactors = F)
  colnames(od)<- c("origin","destination")
  shipment<- cbind(shipment,od)
  shipment<-shipment[,.(routeType,product_code,origin,destination,routeOrder,customer_id,units,revenue)]
  
  shipment<- shipment[(origin!=destination) | (routeOrder==0)]
  shipment$step<- "step"
  shipment[(routeOrder==0)]$step <- "Make"
  shipment[(routeOrder>0)& (destination!=customer_id)]$step <- "IP"
  shipment[(routeOrder>0)& (destination==customer_id)]$step <- "OB"
  }



##################### testing
 a<- chain_shipment_data("./R/sampledata/shipment.csv")
 b<- chain_eord("./R/sampledata/eord.csv")
 c<- chain_prod_master("./R/sampledata/prod_master.csv")
 d<- chain_site_master("./R/sampledata/sites.csv")
chain_shipment_summary(a)
chain_data_gaps(a,d,b,c)
chain_historical_flows(a,b)



