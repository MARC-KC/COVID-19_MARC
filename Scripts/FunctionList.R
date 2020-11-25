

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN -  Download Data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Variables are: 
#  type: One of 'CDT' or 'Hospital', specifying whether to download Cases, 
#         Deaths, Tests data collected from local and state government sources 
#         or the Hospital data summarized from TeleTracking and NHSN data
#  source: Default is 'COVIDv2REST. All other choices are used to pull data from 
#         internal MARC servers for testing purposes by our data services team
#  date: Only used for internal MARC use for testing
downloadBaseData <- function(type = c("CDT", "Hospital"), source = "COVIDv2REST", date = NULL) {
  
  #Match Arguments
  type <- match.arg(type)
  
  if (source != "COVIDv2REST") {
    if (!file.exists(here::here("Scripts", "marcInternalFunctions.R"))) stop(paste0("downloadBaseData() failed: The source argument '", source, "' can only be used on the MARC internal network. Please use 'COVIDv2REST' instead."))
    source(here::here("Scripts", "marcInternalFunctions.R"))
    out <- downloadBaseDataInternal(type = type, source = source, date = date)
  } else {
    
    #Download Cases, Deaths, Tests Data
    if (type == "CDT") {
      cat(crayon::yellow("Downloading Cases, Deaths, and Tests data from https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/6\n"))
      out <- esri2sf::esri2df("https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/6") %>% dplyr::mutate(Date = marcR::epoch2Datetime(Date) %>% as.Date()) %>% dplyr::select(-ESRI_OID)
    }
    
    #Download Hospital data
    if (type == "Hospital") {
      cat(crayon::yellow("Downloading Hospital data from https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/7\n"))
      out <- esri2sf::esri2df("https://gis2.marc2.org/arcgis/rest/services/HumanServices/COVIDv2/MapServer/7") %>% dplyr::mutate(EntryDate = marcR::epoch2Datetime(Date) %>% as.Date()) %>% dplyr::select(-Date) %>% dplyr::select(-ESRI_OID)
    }
  }
  
  return(out)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - sumNA - Special sum function that defaults na.rm to TRUE ####
#  Used within summarizeCDT ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
sumNA <- function(vec) {
  if (all(is.na(vec))) {
    return(NA_integer_)
  } else {
    return(base::sum(vec, na.rm = TRUE))
  }
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - summarizeCDT - This function replaces MARC_PUB.marcpub.VW_CovidCasesDeathsTestsCalcsWithRegionalSummaries ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
summarizeCDT <- function(df, popTable) {
  
  
  df <- df %>% dplyr::left_join(popTable, by = "GeoID") %>% 
    dplyr::mutate(TestsNegative = (TestsTotal - CasesTotal),
                  CasesPer100K = (CasesTotal / Population * 100000),
                  DeathsPer100K = (DeathsTotal / Population * 100000),
                  TestsPer100K = (TestsTotal / Population * 100000)) %>% 
    dplyr::group_by(GeoID) %>% 
    dplyr::mutate(TestsTotalPrevDay = dplyr::lag(TestsTotal, n = 1, order_by = Date),
                  TestsNegativePrevDay = dplyr::lag(TestsNegative, n = 1, order_by = Date)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(TestsNew = (TestsTotal-TestsTotalPrevDay),
                  TestsNegativeNew = (TestsNegative-TestsNegativePrevDay)) %>% 
    dplyr::group_by(GeoID) %>% 
    dplyr::mutate(CasesNewPrevDay = dplyr::lag(CasesNew, n = 1, order_by = Date),
                  DeathsNewPrevDay = dplyr::lag(DeathsNew, n = 1, order_by = Date),
                  TestsNewPrevDay = dplyr::lag(TestsNew, n = 1, order_by = Date)) %>% 
    dplyr::ungroup() %>% 
    dplyr::mutate(CasesNewChange = (CasesNew - CasesNewPrevDay),
                  DeathsNewChange = (DeathsNew - DeathsNewPrevDay),
                  TestsNewChange = (TestsNew - TestsNewPrevDay)) %>% 
    dplyr::select(Jurisdiction, State, GeoID, Region, Date,
                  CasesNew, CasesNewPrevDay, CasesNewChange, CasesTotal,
                  DeathsNew, DeathsNewPrevDay, DeathsNewChange, DeathsTotal,
                  TestsNew, TestsNewPrevDay, TestsNewChange, TestsTotal, 
                  TestsNegativeNew, TestsNegative,
                  Hospitalizations, Recovered,
                  Population, CasesPer100K, DeathsPer100K, TestsPer100K)
  
  
  summarizeTable <- tibble::tribble(
    ~Jurisdiction,          ~State,    ~GeoID,         ~Region,            ~Where,
    "MARC",                 "NULL",    "MARC",       "MARC",           list(quote(stringr::str_detect(Region, 'MARC')), quote(!(Jurisdiction %in% c('Cass', 'Clay', 'Jackson', 'Platte')))),
    "HCC",                  "NULL",    "HCC",        "HCC",            list(quote(stringr::str_detect(Region, 'HCC'))),
    "Kansas (In MARC)",     "KS",      "20MARCReg",  "StateMARC",      list(quote(State == 'KS'), quote(stringr::str_detect(Region, 'MARC'))),
    "Missouri (In MARC)",   "MO",      "29MARCReg",  "StateMARC",      list(quote(State == 'MO'), quote(stringr::str_detect(Region, 'MARC')), quote(!(Jurisdiction %in% c('Cass', 'Clay', 'Jackson', 'Platte')))),
    "Southern HCC",         "MO",      "HCCSouth",   "HCCRegion",      list(quote(GeoID %in% c('29013', '29015', '29083', '29101', '29159'))),
    "Northern HCC",         "MO",      "HCCNorth",   "HCCRegion",      list(quote(GeoID %in% c('29033', '29107', '29195', '29177')))
  )
  
  
  
  
  #Create Summaries
  out <- purrr::map_dfr(1:nrow(summarizeTable), ~ {
    
    summaryGroup <- summarizeTable[.x,]
    
    # expr(df %>% dplyr::filter(!!!summaryGroup$Where[[1]]))
    df %>% dplyr::filter(!!!summaryGroup$Where[[1]]) %>% 
      dplyr::group_by(Date) %>% 
      dplyr::summarise(CasesNew = sumNA(CasesNew), CasesNewPrevDay = sumNA(CasesNewPrevDay), CasesNewChange = sumNA(CasesNewChange), CasesTotal = sumNA(CasesTotal),
                       DeathsNew = sumNA(DeathsNew), DeathsNewPrevDay = sumNA(DeathsNewPrevDay), DeathsNewChange = sumNA(DeathsNewChange), DeathsTotal = sumNA(DeathsTotal),
                       TestsNew = sumNA(TestsNew), TestsNewPrevDay = sumNA(TestsNewPrevDay), TestsNewChange = sumNA(TestsNewChange), TestsTotal = sumNA(TestsTotal),
                       TestsNegativeNew = sumNA(TestsNegativeNew), TestsNegative = sumNA(TestsNegative),
                       Hospitalizations = sumNA(Hospitalizations), Recovered = sumNA(Recovered),
                       Population = sumNA(Population), .groups = "drop") %>% 
      dplyr::mutate(Jurisdiction = summaryGroup$Jurisdiction,
                    State = summaryGroup$State,
                    GeoID = summaryGroup$GeoID,
                    Region = summaryGroup$Region)  %>% 
      dplyr::mutate(CasesPer100K = (CasesTotal / Population * 100000),
                    DeathsPer100K = (DeathsTotal / Population * 100000),
                    TestsPer100K = (TestsTotal / Population * 100000)) %>% 
      dplyr::select(Jurisdiction, State, GeoID, Region, Date,
                    CasesNew, CasesNewPrevDay, CasesNewChange, CasesTotal,
                    DeathsNew, DeathsNewPrevDay, DeathsNewChange, DeathsTotal,
                    TestsNew, TestsNewPrevDay, TestsNewChange, TestsTotal, 
                    TestsNegativeNew, TestsNegative,
                    Hospitalizations, Recovered,
                    Population, CasesPer100K, DeathsPer100K, TestsPer100K)
    
  }) %>% dplyr::bind_rows(x=df, y = .)
  
  
  return(out)
  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - coalesceJoin - Coalesces columns by suffix created by joins ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
coalesceJoin <- function(df, suffix = c(".x", ".y")) {
  
  suffixNames <- purrr::map(suffix, ~names(df)[endsWith(names(df),.x)]) %>% unlist() 
  coalesceNames <- purrr::map(suffix[1], ~names(df)[endsWith(names(df),.x)]) %>% unlist() %>% stringr::str_remove(suffix[1])
  
  
  coalesced <- purrr::map_dfc(coalesceNames, ~dplyr::coalesce(
    df[[paste0(.x, suffix[1])]], 
    df[[paste0(.x, suffix[2])]]
  ) %>% tibble::tibble() %>% `names<-`(.x)) 
  
  otherData <- dplyr::select(df, -all_of(suffixNames))
  
  return(dplyr::bind_cols(coalesced, otherData))
  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - rollingXdayCalcs - Heavy Lifting for rolling averages ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rollingXdayCalcs <- function(df, dateCol, calcCol, Xdays = 7, total = TRUE, average = TRUE) {
  
  if (length(calcCol) > 1) {
    if (length(total) == 1) {
      total <- rep(total, length(calcCol))
    } else if (length(total) != length(calcCol)) {
      stop("total must be length 1 or the same length as calcCol")
    }
    if (length(average) == 1) {
      average <- rep(total, length(calcCol))
    } else if (length(average) != length(calcCol)) {
      stop("average must be length 1 or the same length as calcCol")
    }
  }
  
  
  df <- as.data.frame(df)
  df_sep <- tibble::tibble(dfXDay = purrr::map(1:nrow(df), ~dplyr::filter(df, !!rlang::sym(dateCol) <= df[.x,dateCol] & !!rlang::sym(dateCol) >= df[.x,dateCol] - (Xdays - 1) & GeoID == df[.x,"GeoID"])))
  
  out <- purrr::map_dfc(1:length(calcCol), function(measureID) {
    
    
    
    out <- df_sep %>% dplyr::mutate(
      measureData = purrr::map(dfXDay, ~na.omit(.x[[calcCol[measureID]]])),
      nRows = purrr::map_int(measureData, ~length(na.omit(.x))),
      total = purrr::map2_int(nRows, measureData, ~dplyr::if_else(.x == 0, NA_integer_, sum(.y))),
      avg = purrr::map2_dbl(nRows, total, ~if_else(.x == 0, NA_real_, (.y/.x)))
    )
    
    
    if (total[measureID] & average[measureID]) {
      out <- dplyr::select(out, total, avg)
      names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Total', 'Avg')}")
    } else if (total[measureID] & !average[measureID]) {
      out <- dplyr::select(out, total)
      names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Total')}")
    } else if (!total[measureID] & average[measureID]) {
      out <- dplyr::select(out, avg)
      names(out) <- glue::glue("{calcCol[measureID]}{Xdays}Day{c('Avg')}")
    }
    
    
    
    return(out)
  })
  
  
  return(out)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FN - rollAvgXDays - R Equivalent to the CDT and Hospital Rolling Summary Views ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
rollAvgXDays <- function(df, numDays = 7, varTable) {
  
  #Create Missing Calculated Variables
  calcTab <- varTable %>% dplyr::select(variable, CalcString) %>% dplyr::filter(!is.na(CalcString)) %>%
    dplyr::mutate(CalcString = purrr::map2_chr(CalcString, variable, ~paste0("df$",.y," = with(df, (", .x, "))")))
  
  for (i in 1:nrow(calcTab)) {
    eval(parse(text=calcTab$CalcString[i]))
  }
  
  
  #Create previous date column (startDate)
  df <- df %>% dplyr::mutate(prevDate = Date - days((numDays-1)))
  

  DayDF <- rollingXdayCalcs(df = df, dateCol="Date", calcCol = varTable$variable, Xdays = numDays, total = varTable$Total, average = varTable$Avg)
  
  
  # df <- df %>% bind_cols(DayDF)
  df <- df %>% dplyr::select(Jurisdiction, State, Region, GeoID, Date) %>% dplyr::bind_cols(DayDF)
  
  return(df)
  
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FNs - map_date and map2_date - Wrappers for purrr::map to output dates ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
map_date <- function(.x, .f, ...) {
  purrr::map(.x, .f = .f) %>% unlist() %>% as.Date(origin="1970-01-01")
}
map2_date <- function(.x, .y, .f, ...) {
  purrr::map2(.x, .y, .f = .f) %>% unlist() %>% as.Date(origin="1970-01-01")
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# FNs - Used for the Hospital 7 Day Rolling Averages tables ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
weightedAverageHosp <- function(totalHospitals, reportingHospitals, measure) {
  totalHospitals * measure / reportingHospitals
}


averageHospData <- function(df) {
  df %>% dplyr::summarise(
    Jurisdiction = unique(Jurisdiction),
    EntryDate = max(EntryDate),
    GeoID = unique(GeoID),
    State = unique(State),
    Region = unique(Region),
    HospitalsTotal = unique(HospitalsTotal),
    n = n(),
    BedsAvailable7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, BedsAvailable))/n,
    BedsUsedCovid7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, CovidTotal))/n,
    BedsUsedOther7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, BedsUsedOther))/n,
    
    BedsICUAvailable7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, BedsICUAvailableUSE))/n,
    BedsICUUsedCovid7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, CovidICUTotal))/n,
    BedsICUUsedOther7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, BedsICUUsedOther))/n,
    
    VentilatorsAvailable7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, VentilatorsAvailable))/n,
    VentilatorsUsedCovid7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, CovidVentilatorsUsed))/n,
    VentilatorsUsedOther7WAvg = sum(weightedAverageHosp(HospitalsTotal, HospitalsReporting, VentilatorsUsedOther))/n
  ) %>% 
    dplyr::select(-n)
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++











