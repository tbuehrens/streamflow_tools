#====================================
# function to get ecology flow data
#==================================
get_DOE_data<-function(begin_date,end_date,flow_site,flow_params,daily_or_instantaneous){
  
  begin_year = ifelse(month(begin_date) < 10, year(begin_date)-1,year(begin_date)) #by water year
  end_year = year(end_date)
  years<-c(begin_year:end_year)
  
  flow_params_lookup<-tibble(flow_params=c("discharge","stage_height"),
                             param_codes=c("DSG","STG"))%>%
    right_join(tibble("flow_params"=flow_params))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  daily_inst_lookup<-tibble(daily_or_instantaneous=c("daily","instantaneous"),
                            param_codes=c("DV","FM"))%>%
    right_join(tibble("daily_or_instantaneous"=daily_or_instantaneous))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  
  names_lookup<-c("DATE" = "date","TIME" = "time", "Discharge(cfs)" ="flow_cfs","Stage(ft.)" ="stage_height", "STAGE(ft.)" ="stage_height", `FLOW(cfs)` = "flow_cfs","QUALITY" = "quality")
  
  flow_dat<-NULL
  
  for(i in years){
    url<-paste0("https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/",flow_site,"/",flow_site,"_",i,"_",flow_params_lookup,"_",daily_inst_lookup,".TXT")
    try(
      if(i==min(years)){
        names<-grep("DATE",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          str_split("  ")%>%
          as_vector()
        names = names[names!=""]
        names<-as.vector(gsub("\\s", "", names))
        flow_dat<-grep("/20",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          read_table(na=" ",col_names = F)%>%
          set_names(names)%>%
          filter(QUALITY %in% c(1,2,3,10))%>%
          rename_with(.fn = ~names_lookup[.x], .cols = intersect(names(.), names(names_lookup)))%>%
          mutate(date = mdy(date))%>%
          mutate(across(any_of(c("stage_height","flow_cfs","quality")),~as.numeric(.)))
      }else{
        names<-grep("DATE",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          str_split("  ")%>%
          as_vector()
        names = names[names!=""]
        names<-as.vector(gsub("\\s", "", names))
        tdat<-grep("/20",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          read_table(na=" ",col_names = F)%>%
          set_names(names)%>%
          filter(QUALITY %in% c(1,2,3,10))%>%
          rename_with(.fn = ~names_lookup[.x], .cols = intersect(names(.), names(names_lookup)))%>%
          mutate(date = mdy(date))%>%
          mutate(across(any_of(c("stage_height","flow_cfs","quality")),~as.numeric(.)))
        if(is.null(flow_dat)){
          flow_dat<-tdat
        }else{
          flow_dat<-flow_dat%>%
            bind_rows(tdat)
        }
      }
    )
  }
  flow_dat<-flow_dat%>%
    filter(date>=ymd(begin_date) & date <= ymd(end_date))
  return(flow_dat)
}
