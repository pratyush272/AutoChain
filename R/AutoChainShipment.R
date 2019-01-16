
chain_clean_file <- function(file,headers) {
  library(data.table)
  print("Folowing info should be there in your file:")
  print(data.frame(headers))
  tmp<- fread(filename)
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

chain_shipment_summary(tmp)
{
  library(ggplot2)
  #colnames(tmp)<- c("delv","mat","customer_id","units")
  a<- tmp[,sum(units),by=(customer_id)]

  colnames(a)<- c("customer_id","units")
  a<- a[order(-a$units)]
  a$customer_id<- factor(a$customer_id,levels = a$customer_id)
  a$cumulative <- cumsum(a$units)
  
  b<- head(a,10)
  ggplot(b,aes(x=b$customer_id)) +
    geom_bar(aes(y=b$units), fill='blue', stat="identity") +
    geom_point(aes(y=b$cumulative), color = rgb(0, 1, 0), pch=16, size=1) +
    geom_path(aes(y=b$cumulative, group=1), colour="slateblue1", lty=3, size=0.9) +
    theme(axis.text.x = element_text(angle=90, vjust=0.6)) +
    labs(title = "Pareto Plot", subtitle = "Shipment", x = 'Customers', y ='units') + 
    scale_y_continuous("mpg (US)", sec.axis = sec_axis(~ . / sum(a$units), name = "mpg (UK)"))
}





