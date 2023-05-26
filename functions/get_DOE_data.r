#====================================
# function to get ecology flow data
#==================================
get_DOE_data<-function(begin_date,end_date,flow_site,flow_params,daily_or_instantaneous){
  # Calculate the water years for the given date range
  begin_year = ifelse(month(begin_date) < 10, year(begin_date)-1,year(begin_date)) #by water year
  end_year = year(end_date)
  years<-c(begin_year:end_year)
  
  # Lookup table for flow parameters
  flow_params_lookup<-tibble(flow_params=c("discharge","stage_height"), #could add temp?
                             param_codes=c("DSG","STG"))%>%
    right_join(tibble("flow_params"=flow_params))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  # Lookup table for daily/instantaneous data
  daily_inst_lookup<-tibble(daily_or_instantaneous=c("daily","instantaneous"),
                            param_codes=c("DV","FM"))%>%
    right_join(tibble("daily_or_instantaneous"=daily_or_instantaneous))%>%
    dplyr::select(param_codes)%>%
    as.vector()%>%
    unlist()
  
  # Lookup table for column names
  names_lookup<-c("DATE" = "date","TIME" = "time", "Discharge(cfs)" ="flow_cfs","Stage(ft.)" ="stage_height", "STAGE(ft.)" ="stage_height", `FLOW(cfs)` = "flow_cfs","QUALITY" = "quality")
  
  # Initialize flow data
  flow_dat<-NULL
  
  # Prep for URL format change depending on water year
  current_water_year <- ifelse(month(Sys.Date()) >= 10, year(Sys.Date()) + 1, year(Sys.Date()))
  
  # Loop through the years and retrieve data
  for(i in years){
    if (i == current_water_year) {
      # Current water year data
      url <- paste0("https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/", flow_site, "/", flow_site, "_", flow_params_lookup, "_", daily_inst_lookup, ".TXT")
    } else {
      # Historical data table - past water years
      url <- paste0("https://apps.ecology.wa.gov/ContinuousFlowAndWQ/StationData/Prod/", flow_site, "/", flow_site, "_", i, "_", flow_params_lookup, "_", daily_inst_lookup, ".TXT")
    } 
    # Attempt to retrieve data from the constructed URL
    try(
      if(i==min(years)){ # For the first year in the range, retrieve the column names and overwrite the null flow_dat data frame
        
        # Extract the column names from the data file
        names<-grep("DATE",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          str_split("  ")%>%
          as_vector()
        names = names[names!=""]
        names<-as.vector(gsub("\\s", "", names))
        # Read the data from the URL and format/tidy
        flow_dat<-grep("/20",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          read_table(na=" ",col_names = F)%>%
          set_names(names)%>%
          filter(QUALITY %in% c(1,2,3,10))%>% # Quality key: 1 = good and reviewed, 2 = good, 3 = good and edited, 10 = above rating, but within 2x
          rename_with(.fn = ~names_lookup[.x], .cols = intersect(names(.), names(names_lookup)))%>%
          mutate(date = mdy(date))%>%
          mutate(across(any_of(c("stage_height","flow_cfs","quality")),~as.numeric(.)))
      }else{ # For subsequent years, retrieve the data and append it to the flow_dat data frame
        
        # Extract the column names from the data file
        names<-grep("DATE",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          str_split("  ")%>%
          as_vector()
        names = names[names!=""]
        names<-as.vector(gsub("\\s", "", names))
        # Read the data from the URL and format/tidy
        tdat<-grep("/20",read_lines(url,skip_empty_rows = T),invert=F,value=T)%>%
          read_table(na=" ",col_names = F)%>%
          set_names(names)%>%
          filter(QUALITY %in% c(1,2,3,10))%>% # Quality key: 1 = good and reviewed, 2 = good, 3 = good and edited, 10 = above rating, but within 2x
          rename_with(.fn = ~names_lookup[.x], .cols = intersect(names(.), names(names_lookup)))%>%
          mutate(date = mdy(date))%>%
          mutate(across(any_of(c("stage_height","flow_cfs","quality")),~as.numeric(.)))
        # If flow_dat is null, assign the tdat to flow_dat; otherwise, append tdat to flow_dat
        if(is.null(flow_dat)){
          flow_dat<-tdat
        }else{
          flow_dat<-flow_dat%>%
            bind_rows(tdat)
        }
      }
      ,silent = T)
  }
  # Filter the data within the specified date range
  flow_dat<-flow_dat%>%
    filter(date>=ymd(begin_date) & date <= ymd(end_date))
  
  return(flow_dat)
}
