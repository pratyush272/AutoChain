
chain_shipment<- function()
{
  shpdata<- data.table(read.csv(file.choose()))
  delv_plt_col<- menu(colnames(shpdata), title="Which is the Delv Plt column?")
  material_col<- menu(colnames(shpdata), title="Which is the material column?")
  ship_to_col<- menu(colnames(shpdata), title="Which is the ship_to column?")
  qty_col<- menu(colnames(shpdata), title="Which is the Qty column?")
  
  tmp<- data.table(shpdata[,..delv_plt_col],shpdata[,..material_col],shpdata[,..ship_to_col],shpdata[,..qty_col])
  colnames(tmp)<- c("Delv_plt","Material","Ship_to","Qty")
  return(tmp)
}

chain_resources<- function()
{
  resourcesdata<- data.table(read.csv(file.choose()))
  site_col<- menu(colnames(resourcesdata), title="Which is the Site id column?")
  resourcename_col<- menu(colnames(resourcesdata), title="Which is the resource/line name column?")
  material_col<- menu(colnames(resourcesdata), title="Which is the Product column?")
  speed_col<- menu(colnames(resourcesdata), title="Which is the speed column?")
  cpu_col<- menu(colnames(resourcesdata), title="Which is the cost/unit column?")
  tmp<- data.table(resourcesdata[,..site_col],sitesdata[,..resourcename_col],sitesdata[,..material_col],sitesdata[,..speed_col],sitesdata[,..cpu_col])
  colnames(tmp)<- c("Site_ID","Resource_name","Material","Speed","CPU")
  return(tmp)
}

chain_extvend<- function()
{
  venddata<- data.table(read.csv(file.choose()))
  vendid_col<- menu(colnames(venddata), title="Which is the Vendor id column?")
  material_col<- menu(colnames(venddata), title="Which is the Material column?")
  qty_col<- menu(colnames(sitesdata), title="Which is the total qty column?")
  cost_col<- menu(colnamesvenddata, title="Which is the total cost column?")
  tmp<- data.table(venddata[,..vendid_col],sitesdata[,..material_col],sitesdata[,..qty_col],sitesdata[,..cost_col])
  colnames(tmp)<- c("Vendor_ID","Material","Total_qty","Total_cost")
  return(tmp)
}

chain_sites<- function()
{
  sitesdata<- data.table(read.csv(file.choose()))
  site_col<- menu(colnames(sitesdata), title="Which is the Site id column?")
  sitename_col<- menu(colnames(sitesdata), title="Which is the Site name column?")
  country_col<- menu(colnames(sitesdata), title="Which is the Country column?")
  zip_col<- menu(colnames(sitesdata), title="Which is the Zip Code column?")
  
  #CALC
  mdn<- paste0(substr(sitesdata[[site_col]],1,1),"_xxx")
  
  #tmp<- data.table(sitesdata[,..site_col],sitesdata[,..sitename_col],sitesdata[,..country_col],sitesdata[,..zip_col],mdn)
  tmp<- data.table(sitesdata[[site_col]],sitesdata[[sitename_col]],sitesdata[[country_col]],sitesdata[[zip_col]],mdn)
  
  colnames(tmp)<- c("Site_ID","Site_name","Country","Zip","MDN")
  return(tmp)
}





####
#
####
a<- chain_shipment()
b<- chain_resources()
c<- chain_extvend()
d<- chain_products()
e<- chain_sites()
