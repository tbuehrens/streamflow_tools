#===============================
# function to get USGS flow data
#===============================
get_USGS_data<-function(flow_site,flow_params,daily_or_instantaneous,begin_date,end_date){
  flow_params_lookup<-tibble(flow_params=c("discharge","stage_height"),
                             param_codes=c("00060","00065"))%>%
    right_join(tibble("flow_params"=flow_params))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  names_lookup_daily<-c("Date" = "date","Flow" ="flow_cfs","GH" ="stage_height")
  names_lookup_inst<-c("Date" ="date","Flow_Inst"="flow_cfs","GH_Inst" ="stage_height" )                    
  
  if(daily_or_instantaneous=="daily"){
    tdat<- readNWISdv(siteNumber=flow_site,
                      parameterCd = flow_params_lookup, 
                      startDate = begin_date,
                      endDate = end_date, 
                      statCd = "00003")%>%
      renameNWISColumns()%>%
      as_tibble()%>%
      rename_with(.fn = ~names_lookup_daily[.x], .cols = intersect(names(.), names(names_lookup_daily)))
  }else{
    tdat <- readNWISuv(siteNumber = flow_site,
                       parameterCd = flow_params_lookup,
                       startDate =begin_date,
                       endDate = end_date)%>%
      renameNWISColumns()%>%
      as_tibble()%>%
      rename_with(.fn = ~names_lookup_inst[.x], .cols = intersect(names(.), names(names_lookup_inst)))
  }
  return(tdat)
}
