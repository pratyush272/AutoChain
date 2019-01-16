
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
  headers = c("date","delvering_node_id","customer_id","product_code","units","price_per_unit")
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
  eord$pass1<- unname(unlist(sqldf("select coalesce(b.to_location,a.to_location) from eord a left join eord b on a.product=b.product and a.to_location=b.from_location")))
  eord$pass2<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass1) from eord a left join eord b on a.product=b.product and a.pass1=b.from_location")))
  eord$pass3<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass2) from eord a left join eord b on a.product=b.product and a.pass2=b.from_location")))
  eord$pass4<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass3) from eord a left join eord b on a.product=b.product and a.pass3=b.from_location")))
  eord$pass5<- unname(unlist(sqldf("select coalesce(b.to_location,a.pass4) from eord a left join eord b on a.product=b.product and a.pass4=b.from_location")))
  
  
  eord<- eord[, lapply(.SD, as.character)]
  eord$nPasses<- apply(eord,1,function(x){length(unique(x))-1})
  #eord<- eord[with(eord, ave(-nPasses, product, FUN = order)) %in% c(1), ]
  
  p<-sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from shipment a left join eord b on a.product_code=b.product and a.delvering_node_id=b.pass5 ")
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a inner join eord b on a.product_code=b.product and a.delvering_node_id=b.pass4 where a.pass1 is null and b.from_location is not null"))
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a left join eord b on a.product_code=b.product and a.delvering_node_id=b.pass3 where a.pass1 is null and b.from_location is not null"))
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a left join eord b on a.product_code=b.product and a.delvering_node_id=b.pass2 where a.pass1 is null and b.from_location is not null"))
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a left join eord b on a.product_code=b.product and a.delvering_node_id=b.pass1 where a.pass1 is null and b.from_location is not null"))
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a left join eord b on a.product_code=b.product and a.delvering_node_id=b.to_location where a.pass1 is null and b.from_location is not null"))
  p<-rbind(p,sqldf("select a.delvering_node_id,a.customer_id,a.product_code,a.units,a.price_per_unit,b.pass1,b.pass2,b.pass3,b.pass4,b.pass5 from p a left join eord b on a.product_code=b.product and a.delvering_node_id=b.from_location where a.pass1 is null and b.from_location is not null"))
  
  
  
  }



##################### testing
 a<- chain_shipment_data("./R/sampledata/shipment.csv")
 b<- chain_eord("./R/sampledata/eord.csv")
 c<- chain_prod_master("./R/sampledata/prod_master.csv")
 d<- chain_site_master("./R/sampledata/sites.csv")
chain_shipment_summary(a)
chain_data_gaps(a,d,b,c)



