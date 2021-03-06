#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Install and Load Packages Function ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
installLoadPackages <- function(cranPackages = NULL, githubPackages = NULL, load=FALSE, ...) {
    
    cranNotInstalled <- cranPackages[!(cranPackages %in% installed.packages()[,1])]
    
    if (length(cranNotInstalled) != 0) {
        install.packages(pkgs = cranNotInstalled, ...)
    }
    
    githubPackagesNames <- gsub("(?:[^/]*/)*", "", githubPackages)
    githubNotInstalledNames <- githubPackagesNames[!(githubPackagesNames %in% installed.packages()[,1])]
    githubNotInstalled <- githubPackages[which(githubPackagesNames %in% githubNotInstalledNames)]
    
    if (length(githubNotInstalled) != 0) {
        if (!("devtools" %in% installed.packages()[,1])) install.packages("devtools", ...)
        
        devtools::install_github(githubNotInstalled, upgrade = "never")
    }
    
    if (isTRUE(load)) {
        allPackages <- c(cranPackages, githubPackagesNames)
        invisible(lapply(allPackages, library, character.only = TRUE))
    }
}
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Install and Load Packages ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
#Set packages needed
cranPackages <- c("dplyr", "lubridate", "purrr", "stringr", "tibble", "readxl", "readr", "glue", "tidyr", "crayon", "rlang", "here")
githubPackages <- c("yonghah/esri2sf", "MARC-KC/marcR")

#Check if packages are installed and install if missing. Then load needed packages into the environment
installLoadPackages(cranPackages, githubPackages, load = TRUE)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Check File Structure ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load in Jurisdiction / Population Table ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
jurisTable <- readr::read_csv(here::here("Data", "jurisdictionsPopTable.csv"), col_types = "ccccd") %>% 
    dplyr::select(-Population)

popTable <- readr::read_csv(here::here("Data", "jurisdictionsPopTable.csv"), col_types = "ccccd") %>% 
    dplyr::select(GeoID, Population)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Set Variables (GeoIDs, Lag) ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
GeoIDs_base <- jurisTable %>% dplyr::filter(dplyr::across(Region, ~grepl('MARC|HCC', .))) %>% dplyr::pull(GeoID) #all MARC and HCC
GeoIDs_MARC <- jurisTable %>% dplyr::filter(dplyr::across(Region, ~grepl('MARC', .))) %>% dplyr::pull(GeoID) #all MARC
GeoIDs_HCC <- jurisTable %>% dplyr::filter(dplyr::across(Region, ~grepl('HCC', .))) %>% dplyr::pull(GeoID) #all HCC
GeoIDs_restrictHospital <- GeoIDs_base[GeoIDs_base %in% c('29165', '29165NoKC', '29037', '29037NoKC', stringr::str_subset(GeoIDs_HCC, "HCC|29177", negate = TRUE))] #Jurisdictions with <=2 hospitals that we don't have permission to share publicly.
GeoIDs_okayHospital <- GeoIDs_base[!(GeoIDs_base %in% GeoIDs_restrictHospital)] #Jurisdictions with hospitals that we have permission to share publicly.

# How many days lag should be imposed on the data? This is important so we don't create trends based on incomplete data.
lagDays <- 10

#Name of the output folder
outputFolderName <- "powerBi_CovidHub_Dataset"
#++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load in Necessary Functions ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
source(here::here("Scripts", "FunctionList.R"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Load in CDT and Hospital Base Data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cdtSumData <- downloadBaseData(type = "CDT")

hospSumData <-  downloadBaseData(type = "Hospital")
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Combine Hospital and CDT Base Data ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cdtHospSumData <- dplyr::full_join(cdtSumData, hospSumData, by = c("GeoID", "Date" = "EntryDate")) %>% coalesceJoin() %>% 
    dplyr::mutate(CovidNew24H = CovidNew24HConfirmed + CovidNew24HSuspected) %>% 
    dplyr::select(
        Jurisdiction, State, GeoID, Region, Date,
        
        CasesNew, CasesNewPrevDay, CasesNewChange, CasesTotal,
        DeathsNew, DeathsNewPrevDay, DeathsNewChange, DeathsTotal,
        TestsNew, TestsNewPrevDay, TestsNewChange, TestsTotal, 
        TestsNegativeNew, TestsNegative,
        Hospitalizations, Recovered,
        Population, CasesPer100K, DeathsPer100K, TestsPer100K,
        
        HospitalsReporting, HospitalsTotal, 
        BedsTotal, BedsUsed, BedsAvailable, 
        BedsICUTotal, BedsICUUsed, BedsICUAvailable,
        CovidTotal, CovidConfirmed, CovidSuspected, 
        CovidNew24H, CovidNew24HConfirmed, CovidNew24HSuspected, 
        CovidICUTotal, CovidICUConfirmed, CovidICUSuspected, 
        VentilatorsTotal, VentilatorsUsed, VentilatorsAvailable, 
        CovidVentilatorsAdmittedUsed, CovidVentilatorsOverflowUsed, 
        CovidDeathsTotal, CovidDeaths24H)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate Rolling Average Tables ####
cat(crayon::blue("Calculating 7 and 14 day rolling averages.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
varTable <- tibble::tribble(
    ~variable,                  ~Avg,         ~Total,     ~CalcString, 
    "CasesNew",                 TRUE,         TRUE,       NA,
    "DeathsNew",                TRUE,         TRUE,       NA,
    "TestsNew",                 TRUE,         TRUE,       NA,
    "CovidNew",                 TRUE,         TRUE,       "CovidNew24HConfirmed + CovidNew24HSuspected",
    "CovidTotal",               TRUE,         FALSE,      NA,
    "BedsUsedOther",            TRUE,         FALSE,      "BedsUsed - CovidTotal",
    "BedsAvailable",            TRUE,         FALSE,      NA,
    "CovidICUTotal",            TRUE,         FALSE,      NA,
    "BedsICUUsedOther",         TRUE,         FALSE,      "BedsICUUsed - CovidICUTotal",
    "BedsICUAvailable",         TRUE,         FALSE,      NA,
    "CovidVentUsed",            TRUE,         FALSE,      "CovidVentilatorsAdmittedUsed + CovidVentilatorsOverflowUsed",
    "VentilatorsUsedOther",     TRUE,         FALSE,      "VentilatorsUsed - CovidVentilatorsAdmittedUsed - CovidVentilatorsOverflowUsed",
    "VentilatorsAvailable",     TRUE,         FALSE,      NA,
    "HospitalsReporting",       TRUE,         TRUE,       NA,
    "HospitalsTotal",           TRUE,         TRUE,       NA
)


cdtHospSum7DayRollingData <- cdtHospSumData %>% rollAvgXDays(df = ., numDays = 7, varTable = varTable)

cdtHospSum14DayRollingData <- cdtHospSumData %>% rollAvgXDays(df = ., numDays = 14, varTable = varTable)


# Obtain Base Hospital Column Restriction Vector for Summaries (i.e., what columns deal with hospital data and may need restricted based on GeoID [only necessary for Internal MARC server data])
rolling7DayHospRestrictCols <- stringr::str_subset(names(cdtHospSum7DayRollingData), "Jurisdiction|State|Region|GeoID|Date|CasesNew|DeathsNew|TestsNew", negate = TRUE)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Calculate CDT Most Recent ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
cdtSumDataMostRecent <- cdtSumData %>% dplyr::group_by(GeoID) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Add Base Tables ####
cat(crayon::blue("Exporting base CDT data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_CDT_TimeSeries <- cdtSumData %>% 
    dplyr::filter(GeoID %in% GeoIDs_base)
# bi_CDT_TimeSeries_Lag <- cdtSumData %>% dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
#   dplyr::filter(GeoID %in% GeoIDs_base)


bi_CDT_MostRecent <- cdtSumData %>%
    dplyr::group_by(GeoID) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>%
    dplyr::filter(GeoID %in% GeoIDs_base)
# bi_CDT_MostRecent_Lag <- cdtSumData %>% dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
#   dplyr::group_by(GeoID) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>% 
#   dplyr::filter(GeoID %in% GeoIDs_base)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full 7 Day  Rolling Summary With and Without Lag ####
cat(crayon::blue("Exporting 7 day rolling averages and totals.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_7DayRolling <- 
    dplyr::left_join(dplyr::mutate(cdtHospSum7DayRollingData,
                                   TestsPositiveNew7DayAvgProportion = CasesNew7DayTotal/dplyr::if_else(TestsNew7DayTotal == 0, NA_integer_, TestsNew7DayTotal),
                                   DeathsToCases7DayProportion = DeathsNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal),
                                   HospsToCases7DayProportion = CovidNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal)),
                     dplyr::mutate(popTable, 
                                   PopulationTestStandard = (ceiling(Population / 100000) * 150),
                                   PositiveTestStandardProportion = 0.05,
                                   PositiveTestStandard = 5),
                     by = "GeoID") %>% 
    dplyr::mutate(
        KPI_PositiveTests = dplyr::case_when(
            TestsPositiveNew7DayAvgProportion < PositiveTestStandardProportion ~ 1,
            TestsPositiveNew7DayAvgProportion > PositiveTestStandardProportion ~ 3,
            TRUE ~ 2
        ),
        KPI_PopulationTests = dplyr::case_when(
            TestsNew7DayAvg < PopulationTestStandard ~ 3,
            TestsNew7DayAvg > PopulationTestStandard ~ 1,
            TRUE ~ 2
        )) %>% 
    dplyr::filter(GeoID %in% GeoIDs_base)
bi_7DayRolling <- bi_7DayRolling %>% 
    dplyr::mutate(dplyr::across(c(names(bi_7DayRolling)[names(bi_7DayRolling) %in% rolling7DayHospRestrictCols], "HospsToCases7DayProportion"), ~dplyr::if_else(GeoID %in% GeoIDs_restrictHospital, NA_real_, as.double(.x))))

bi_7DayRollingLag <- bi_7DayRolling %>% dplyr::filter(Date <= (max(Date) - lagDays)) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Weekly Thinned 7 Day Rolling With and Without Lag ####
cat(crayon::blue("Exporting thinned 7 day rolling averages and totals.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_7DayRollingThin <- bi_7DayRolling %>% dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) 

bi_7DayRollingThinLag <- bi_7DayRolling %>% dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) 

#For displaying New Hospitalizations with a two day lag to account for weekend reporting lag
bi_7DayRollingThinLagHosp <- bi_7DayRolling %>% dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::select(Jurisdiction, State, Region, GeoID, Date, CovidNew7DayTotal, CovidNew7DayAvg, HospitalsReporting7DayTotal, HospitalsReporting7DayAvg, HospitalsTotal7DayTotal, HospitalsTotal7DayAvg, dayWeek) %>% 
    dplyr::filter(Date <= (max(Date) - 2)) %>% 
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) 
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# 7 Day Comparison - Last 6 Weeks and Most Recent With and Without Lag ####
cat(crayon::blue("Exporting 7 day comparison sheets.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
measureTable <- tibble::tribble(
    ~measureName,                    ~upGood,
    "CasesNew##DayAvg",               FALSE,
    "CasesNew##DayTotal",             FALSE,
    "DeathsNew##DayAvg",              FALSE,
    "DeathsNew##DayTotal",            FALSE,
    "TestsNew##DayAvg",               TRUE,
    "TestsNew##DayTotal",             TRUE,
    "CovidTotal##DayAvg",             FALSE,
    "CovidNew##DayAvg",               FALSE,
    "CovidNew##DayTotal",             FALSE,
    "CovidICUTotal##DayAvg",          FALSE,
    "CovidVentUsed##DayAvg",          FALSE
)



baseWeeklyComparisonData <- cdtHospSum7DayRollingData %>% 
    baseDaysComparison(., measureTable) 
    


bi_7DayComparison_MostRecent <- baseWeeklyComparisonData %>% 
    dplyr::group_by(GeoID, Measure) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>% 
    dplyr::filter(GeoID %in% GeoIDs_base) %>% 
    dplyr::mutate(WeekChangeProp = WeekChangeRatio - 1)

bi_7DayComparison_MostRecent_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
    dplyr::group_by(GeoID, Measure) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>% 
    dplyr::filter(GeoID %in% GeoIDs_base)

bi_7DayComparison_MostRecent_HospLag <- baseWeeklyComparisonData %>% dplyr::filter(Date <= (max(Date) - 2)) %>% 
    dplyr::group_by(GeoID, Measure) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>% 
    dplyr::filter(GeoID %in% GeoIDs_base) %>% 
    dplyr::mutate(WeekChangeProp = WeekChangeRatio - 1)


bi_7DayComparison_Last6Weeks <- baseWeeklyComparisonData %>% dplyr::filter(Date >= (max(Date, na.rm = TRUE) - lubridate::weeks(6)))

bi_7DayComparison_Last6Weeks_Lag <- baseWeeklyComparisonData %>% dplyr::filter(Date >= ((max(Date, na.rm = TRUE) - lagDays) - lubridate::weeks(6)) & (Date <= ((max(Date, na.rm = TRUE) - lagDays))))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++




#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Full Hospital Data WIth Calculations And Most Recent ####
cat(crayon::blue("Exporting base hospital data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_HospitalDailyData <- hospSumData %>%  
    dplyr::mutate(CovidNew24H = CovidNew24HConfirmed + CovidNew24HSuspected) %>% 
    dplyr::mutate(BedsICUUsedUSE = dplyr::if_else(EntryDate < '2020-10-19', BedsICUUsed,  BedsICUUsedAdult),
                  BedsICUTotalUSE = dplyr::if_else(EntryDate < '2020-10-19', BedsICUTotal, BedsICUTotalAdult),
                  
                  BedsUsedOther = (BedsUsed - CovidTotal), 
                  BedsICUUsedOther = (BedsICUUsedUSE - CovidICUTotal),
                  VentilatorsUsedOther = (VentilatorsUsed - CovidVentilatorsAdmittedUsed - CovidVentilatorsOverflowUsed),
                  CovidVentilatorsUsed = (CovidVentilatorsAdmittedUsed + CovidVentilatorsOverflowUsed),
                  BedsICUAvailableUSE = (BedsICUTotalUSE - BedsICUUsedUSE),
                  
                  BedsTotalCalc = (BedsAvailable + BedsUsedOther + CovidTotal),
                  BedsICUTotalCalc = (BedsICUAvailableUSE + BedsICUUsedOther + CovidICUTotal),
                  VentilatorsTotalCalc = (VentilatorsAvailable + VentilatorsUsedOther + CovidVentilatorsUsed),
                  
                  BedsAvailableProportion = BedsAvailable/dplyr::if_else(BedsTotalCalc == 0, NA_integer_, BedsTotalCalc),
                  BedsUsedOtherProportion = BedsUsedOther/dplyr::if_else(BedsTotalCalc == 0, NA_integer_, BedsTotalCalc),
                  CovidTotalProportion = CovidTotal/dplyr::if_else(BedsTotalCalc == 0, NA_integer_, BedsTotalCalc),
                  BedsICUAvailableProportion = BedsICUAvailableUSE/dplyr::if_else(BedsICUTotalCalc == 0, NA_integer_, BedsICUTotalCalc),
                  BedsICUUsedOtherProportion = BedsICUUsedOther/dplyr::if_else(BedsICUTotalCalc == 0, NA_integer_, BedsICUTotalCalc),
                  CovidICUTotalProportion = CovidICUTotal/dplyr::if_else(BedsICUTotalCalc == 0, NA_integer_, BedsICUTotalCalc),
                  VentilatorsAvailableProportion = VentilatorsAvailable/dplyr::if_else(VentilatorsTotalCalc == 0, NA_integer_, VentilatorsTotalCalc),
                  VentilatorsUsedOtherProportion = VentilatorsUsedOther/dplyr::if_else(VentilatorsTotalCalc == 0, NA_integer_, VentilatorsTotalCalc),
                  CovidVentilatorsUsedProportion = CovidVentilatorsUsed/dplyr::if_else(VentilatorsTotalCalc == 0, NA_integer_, VentilatorsTotalCalc)
    ) %>% dplyr::filter(GeoID %in% GeoIDs_base[!(GeoIDs_base %in% GeoIDs_restrictHospital)])

#HospitalTotal based on a 3 week window so that it can adapt to reporting over time
bi_HospitalDailyData <- bi_HospitalDailyData %>%  dplyr::mutate(
        HospitalsTotal = purrr::map2_int(GeoID, EntryDate, ~dplyr::filter(bi_HospitalDailyData, GeoID == .x & EntryDate >= .y - 10 & EntryDate <= .y + 10)[['HospitalsReporting']] %>% max(., na.rm = TRUE) %>% as.integer())
    )

bi_HospitalMostRecent <- bi_HospitalDailyData %>% dplyr::group_by(GeoID) %>% dplyr::mutate(rankID = rank(dplyr::desc(EntryDate))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++








#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Hospital 7 Day Rolling Averages And Most Recent ####
cat(crayon::blue("Exporting 7 day rolling hospital weighted averages.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

Hospital7DayAvg <- hospSumData %>% 
    dplyr::mutate(BedsICUUsedUSE = dplyr::if_else(EntryDate < '2020-10-19', BedsICUUsed,  BedsICUUsedAdult),
                  BedsICUTotalUSE = dplyr::if_else(EntryDate < '2020-10-19', BedsICUTotal, BedsICUTotalAdult),
                  
                  BedsUsedOther = (BedsUsed - CovidTotal), 
                  BedsICUUsedOther = (BedsICUUsedUSE - CovidICUTotal),
                  VentilatorsUsedOther = (VentilatorsUsed - CovidVentilatorsAdmittedUsed - CovidVentilatorsOverflowUsed),
                  CovidVentilatorsUsed = (CovidVentilatorsAdmittedUsed + CovidVentilatorsOverflowUsed),
                  BedsICUAvailableUSE = (BedsICUTotalUSE - BedsICUUsedUSE)) 



Hospital7DayAvg <- Hospital7DayAvg %>% 
    dplyr::mutate(
        day7Data = purrr::map2(GeoID, EntryDate, ~ {
            Hospital7DayAvg %>% dplyr::filter(GeoID == .x & EntryDate > (.y - 7) & EntryDate <= .y)
        }),
        day7Summary = purrr::map(day7Data, ~averageHospData(.x))
    ) %>% dplyr::pull(day7Summary) %>% 
    dplyr::bind_rows()




bi_Hospital7DayAvg <- Hospital7DayAvg %>% 
    dplyr::mutate(
        BedsTotal = dplyr::coalesce(BedsAvailable7WAvg, 0) + dplyr::coalesce(BedsUsedCovid7WAvg, 0) + dplyr::coalesce(BedsUsedOther7WAvg, 0),
        BedsTotal = dplyr::if_else(BedsTotal == 0, NA_real_, as.double(BedsTotal)),
        BedsICUTotal = dplyr::coalesce(BedsICUAvailable7WAvg, 0) + dplyr::coalesce(BedsICUUsedCovid7WAvg, 0) + dplyr::coalesce(BedsICUUsedOther7WAvg, 0),
        BedsICUTotal = dplyr::if_else(BedsICUTotal == 0, NA_real_, as.double(BedsICUTotal)),
        VentTotal = dplyr::coalesce(VentilatorsAvailable7WAvg, 0) + dplyr::coalesce(VentilatorsUsedCovid7WAvg, 0) + dplyr::coalesce(VentilatorsUsedOther7WAvg, 0),
        VentTotal = dplyr::if_else(VentTotal == 0, NA_real_, as.double(VentTotal)),
    ) %>% 
    dplyr::mutate(
        PropBedsAvailable7WAvg = dplyr::coalesce(BedsAvailable7WAvg / BedsTotal, 0),
        PropBedsUsedCovid7WAvg = dplyr::coalesce(BedsUsedCovid7WAvg / BedsTotal, 0),
        PropBedsUsedOther7WAvg = dplyr::coalesce(BedsUsedOther7WAvg / BedsTotal, 0),
        
        PropBedsICUAvailable7WAvg = dplyr::coalesce(BedsICUAvailable7WAvg / BedsICUTotal, 0),
        PropBedsICUUsedCovid7WAvg = dplyr::coalesce(BedsICUUsedCovid7WAvg / BedsICUTotal, 0),
        PropBedsICUUsedOther7WAvg = dplyr::coalesce(BedsICUUsedOther7WAvg / BedsICUTotal, 0),
        
        PropVentilatorsAvailable7WAvg = dplyr::coalesce(VentilatorsAvailable7WAvg / VentTotal, 0),
        PropVentilatorsUsedCovid7WAvg = dplyr::coalesce(VentilatorsUsedCovid7WAvg / VentTotal, 0),
        PropVentilatorsUsedOther7WAvg = dplyr::coalesce(VentilatorsUsedOther7WAvg / VentTotal, 0)
    ) %>% dplyr::filter(GeoID %in% GeoIDs_base[!(GeoIDs_base %in% GeoIDs_restrictHospital)])



bi_Hospital7DayAvgMostRecent <- bi_Hospital7DayAvg %>% dplyr::group_by(GeoID) %>% dplyr::mutate(rankID = rank(dplyr::desc(EntryDate))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# COP Table ####
# Used to create the main dynamic table with data on the COP page
cat(crayon::blue("Exporting COP comparison table with the formatted names.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
measureTable <- tibble::tribble(
    ~measureName,         ~Avg_Total,  ~measureDisplayName,   ~upGood,  ~PerCapita,
    "CasesNew",           "Total",     "Cases",               FALSE,    TRUE,
    "DeathsNew",          "Total",     "Deaths",              FALSE,    TRUE,
    "TestsNew",           "Total",     "Tests",               TRUE,     TRUE
)

bi_COPTable <- list(
    COPtable(cdtHospSum7DayRollingData, popTable, days = 7, lagDays = 10, measureTable = measureTable, percentChangeKPI = 5),
    COPtable(cdtHospSum14DayRollingData, popTable, days = 14, lagDays = 10, measureTable = measureTable, percentChangeKPI = 5),
    COPtable(cdtHospSum7DayRollingData, popTable, days = 7, lagDays = 0, measureTable = measureTable, percentChangeKPI = 5),
    COPtable(cdtHospSum14DayRollingData, popTable, days = 14, lagDays = 0, measureTable = measureTable, percentChangeKPI = 5)
) %>% dplyr::bind_rows()
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++


#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Testing Page tables ####
# Used to create the tables for the tesing page. Mainly the need for negative vs positive tests
cat(crayon::blue("Exporting tables for testing page.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
varTable <- tibble::tribble(
    ~variable,                  ~Avg,         ~Total,     ~CalcString, 
    "CasesNew",                 TRUE,         TRUE,       NA,
    "TestsNew",                 TRUE,         TRUE,       NA,
    "TestsNegativeNew",         TRUE,         TRUE,       NA
)


bi_TestingPage7DayRolling <- cdtSumData %>% rollAvgXDays(df = ., numDays = 7, varTable = varTable) %>% 
    dplyr::filter(Date <= max(Date) - lagDays) %>% 
    dplyr::mutate(TestsPositivity = dplyr::if_else(TestsNew7DayTotal == 0, NA_real_, CasesNew7DayTotal / TestsNew7DayTotal))

bi_TestingPage7DayRollingThin <- cdtSumData %>% rollAvgXDays(df = ., numDays = 7, varTable = varTable) %>% 
    dplyr::filter(Date <= max(Date) - lagDays) %>% 
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::filter(dayWeek == dayWeek[which.max(Date)]) %>% 
    dplyr::select(-dayWeek) %>% 
    dplyr::mutate(TestsPositivity = dplyr::if_else(TestsNew7DayTotal == 0, NA_real_, CasesNew7DayTotal / TestsNew7DayTotal))


# bi_7DayRollingThinLag <- bi_7DayRolling %>% dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
#     dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
#     dplyr::filter(dayWeek == dayWeek[which.max(Date)]) 
# %>% 
    # dplyr::select(-c(Jurisdiction, State, Region)) %>% 
    # dplyr::left_join(x = dplyr::select(cdtSumData, Jurisdiction, State, Region, GeoID, Date, CasesNew, TestsNew, TestsNegativeNew), y = ., by = c("GeoID", "Date"))

# bi_TestingPage7DayRolling
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++











#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Jurisdiction Bar Charts given time scenarios ####
# Used as a Bridge table in the Power BI relationships
cat(crayon::blue("Exporting jurisdiction bar chart data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
mostRecentGivenHelperTable <- tibble::tribble(
    ~datasetName,         ~days, ~lagDays, ~keep,
    "cdtHospSumData",     7,     lagDays,  "Both",
    "cdtHospSumData",     14,    lagDays,  "Both",
    "cdtHospSumData",     30,    lagDays,  "Both",
    "cdtHospSumData",     60,    lagDays,  "Both",
    "cdtHospSumData",     90,    lagDays,  "Both",
    "bi_CDT_MostRecent",  NA,    NA,       "Both"
)

bi_JurisdictionBarCharts <- pmap_dfr(mostRecentGivenHelperTable, function(datasetName, days, lagDays, keep, ...) {
    dataset <- eval(sym(datasetName))
    
    out <- mostRecentGivenTime(df = dataset, days=days, lagDays=lagDays)

    if (keep == "Both") {
        return(out)
    } else if (keep == "Raw") {
        return(dplyr::filter(out, Raw_Per100K == "Raw"))
    } else if (keep == "Per100K") {
        return(dplyr::filter(out, Raw_Per100K == "Per100K"))
    } else {
        warning("The argument keep must be one of 'Both', 'Raw', or 'Per100K'. Returning NULL")
        return(NULL)
    }
}) 

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++





#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Weekly Data Snapshot Data ####
# All data needed for Weekly Data Snapshot
cat(crayon::blue("Exporting Weekly Data Snapshot Data.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++

bi_WDS_7DayRollingLag <- cdtHospSum7DayRollingData %>% dplyr::filter(Date <= max(Date) - lagDays) %>% 
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::filter(dayWeek == 4) %>% dplyr::select(-dayWeek) %>% 
    dplyr::mutate(TestsPositive7DayRate = CasesNew7DayTotal/dplyr::if_else(TestsNew7DayTotal == 0, NA_integer_, TestsNew7DayTotal),
                  DeathsToCases7DayRate = DeathsNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal),
                  HospsToCases7DayRate = CovidNew7DayTotal/dplyr::if_else(CasesNew7DayTotal == 0, NA_integer_, CasesNew7DayTotal)) %>% 
    dplyr::filter(GeoID %in% c('MARC', '20MARCReg', '29MARCReg', 'HCC')) %>% 
    dplyr::select(Jurisdiction, State, Region, GeoID, Date, 
                  CasesNew7DayTotal, CasesNew7DayAvg, DeathsNew7DayTotal, DeathsNew7DayAvg, TestsNew7DayTotal, TestsNew7DayAvg, 
                  TestsPositive7DayRate, DeathsToCases7DayRate, HospsToCases7DayRate)

bi_WDS_Last7Days <- bi_WDS_7DayRollingLag %>% 
    dplyr::group_by(GeoID) %>% dplyr::filter(Date == max(Date)) %>% dplyr::ungroup()%>% 
    dplyr::filter(GeoID %in% c('MARC', 'HCC'))

bi_WDS_7DayRollingLagHosp <- cdtHospSum7DayRollingData %>% dplyr::filter(Date <= max(Date) - 2) %>% 
    dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
    dplyr::filter(dayWeek == 5) %>% dplyr::select(-dayWeek) %>% 
    dplyr::filter(GeoID %in% c('MARC', '20MARCReg', '29MARCReg', 'HCC')) %>% 
    dplyr::mutate(HospitalsTotal7DayTotal = dplyr::if_else(GeoID == 'MARC', as.integer(27*7), as.integer(HospitalsTotal7DayTotal))) %>% 
    dplyr::mutate(Hospital7DayReportRate = HospitalsReporting7DayTotal/dplyr::if_else(HospitalsTotal7DayTotal == 0, NA_integer_, HospitalsTotal7DayTotal)) %>%
    dplyr::select(Jurisdiction, State, Region, GeoID, Date, 
                  CovidNew7DayTotal, CovidNew7DayAvg, CovidTotal7DayAvg, Hospital7DayReportRate)


measureTableCDT <- tibble::tribble(
    ~measureName,                    ~upGood,
    "CasesNew7DayAvg",               FALSE,
    "CasesNew7DayTotal",             FALSE,
    "DeathsNew7DayAvg",              FALSE,
    "DeathsNew7DayTotal",            FALSE,
    "TestsNew7DayAvg",               TRUE,
    "TestsNew7DayTotal",             TRUE,
    "TestsPositive7DayRate",         FALSE,
    "DeathsToCases7DayRate",         FALSE,
    "HospsToCases7DayRate",          FALSE
)
measureTableHosp <- tibble::tribble(
    ~measureName,                    ~upGood,
    "CovidTotal7DayAvg",             FALSE,
    "CovidNew7DayAvg",               FALSE,
    "CovidNew7DayTotal",             FALSE,
    "Hospital7DayReportRate",        TRUE
)

bi_WDS_WeeklyComparison <-  dplyr::bind_rows(dplyr::filter(baseDaysComparison(bi_WDS_7DayRollingLag, measureTableCDT, days = 7, lag = 1), Date >= (max(Date) - 6*7)),
                                          dplyr::filter(baseDaysComparison(bi_WDS_7DayRollingLagHosp, measureTableHosp, days = 7, lag = 1), Date >= (max(Date) - 6*7))
) %>% dplyr::filter(GeoID %in% c('MARC', 'HCC'))



measureTable <- tibble::tribble(
    ~measureName,                    ~upGood,
    "CasesNew##DayAvg",               FALSE,
    "CasesNew##DayTotal",             FALSE,
    "DeathsNew##DayAvg",              FALSE,
    "DeathsNew##DayTotal",            FALSE,
    "TestsNew##DayAvg",               TRUE,
    "TestsNew##DayTotal",             TRUE,
    "CovidTotal##DayAvg",             FALSE,
    "CovidNew##DayAvg",               FALSE,
    "CovidNew##DayTotal",             FALSE
)



WDScomparison <- function(rollSumData, measureTable, days, lagDays) {
    rollSumData %>% 
        dplyr::mutate(dayWeek = as.numeric(format(Date, format = "%u"))) %>% 
        dplyr::filter(Date <= max(Date[dayWeek == 7])) %>% 
        dplyr::select(-dayWeek) %>% 
        baseDaysComparison(., measureTable, days = days) %>% 
        dplyr::filter(Date <= (max(Date) - lagDays)) %>% 
        dplyr::group_by(GeoID, Measure) %>% dplyr::mutate(rankID = rank(dplyr::desc(Date))) %>% dplyr::filter(rankID == 1) %>% dplyr::select(-rankID) %>% 
        dplyr::filter(GeoID == 'MARC') %>% dplyr::ungroup() %>% 
        dplyr::mutate(Days = days) %>% dplyr::rename_with(~stringr::str_remove(.x, "Week"), tidyr::contains("Week")) %>% 
        dplyr::mutate(Lag = lagDays)
}

bi_WDS_ComparisonTable <- list(
    WDScomparison(cdtHospSum7DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = TRUE),], days = 7, lagDays = lagDays),
    WDScomparison(cdtHospSum7DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = FALSE),], days = 7, lagDays = 2),
    WDScomparison(cdtHospSum14DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = TRUE),], days = 14, lagDays = lagDays),
    WDScomparison(cdtHospSum14DayRollingData, measureTable[stringr::str_detect(measureTable$measureName, "^Covid", negate = FALSE),], days = 14, lagDays = 2)
) %>% dplyr::bind_rows() %>% 
    dplyr::mutate(ChangeProp = ChangeRatio - 1)



bi_WDS_HelperTable <- tibble::tribble(
    ~ID_Name,              ~Date1,                                                                                                                    ~Date2,
    "CurrentDate",         Sys.Date(),                                                                                                                NA,
    "ReportCutoffDate",   min(filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = TRUE))[["Date"]]) + lagDays,             NA,
    "CDTRange",            min(filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = TRUE))[["Date"]]) - 6,                  min(filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = TRUE))[["Date"]]),
    "HospRange",           min(filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = FALSE))[["Date"]]) - 6,                 min(filter(bi_WDS_ComparisonTable, stringr::str_detect(Measure, "^Covid", negate = FALSE))[["Date"]])
    
)

bi_WDS_HospitalDailyData <- bi_HospitalDailyData %>% 
    dplyr::mutate(dayWeek = as.numeric(format(EntryDate, format = "%u"))) %>%
    dplyr::filter(EntryDate <= max(EntryDate[dayWeek==7])-2) %>% 
    dplyr::select(-dayWeek)

#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++






#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# PrettyJurisdictions ####
# Used as a Bridge table in the Power BI relationships
cat(crayon::blue("Exporting jurisdiction bridge table with the formatted names.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
prettyJurisdictions <- readr::read_csv(here::here("Data", "prettyJurisdictions.csv"), col_types = cols(.default = "c"))

bi_PrettyJurisdictions_MARC <- prettyJurisdictions %>% dplyr::filter(Site == 'MARC') %>% dplyr::select(-Site)
bi_PrettyJurisdictions_HCC <- prettyJurisdictions %>% dplyr::filter(Site == 'HCC') %>% dplyr::select(-Site)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# HelperTable ####
# Used to help create measures in Power BI
cat(crayon::blue("Exporting helper table for PowerBI measures.\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
bi_HelperTable <- tibble::tribble(
    ~HelperID,          ~DateTime, 
    "LastExport",         Sys.time()
)
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++










#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
# Export Power Bi datasets to CSVs ####
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++
biObjects <- ls() %>% stringr::str_subset("^bi_")

cat(crayon::green("Exporting", length(biObjects), paste0("csv files to '", here::here("Deliverables", outputFolderName), "\n")))
if (dir.exists(here::here("Deliverables", outputFolderName))) unlink(here::here("Deliverables", outputFolderName), recursive = TRUE)
dir.create(here::here("Deliverables", outputFolderName), recursive = TRUE, showWarnings = FALSE)
walk(seq_along(biObjects), ~{
    fileName <- biObjects[.x] %>% stringr::str_remove("^bi_")
    cat(crayon::blue("Saving file:", paste0(fileName, ".csv"), paste0("(", .x, "/", length(biObjects), ")\n")))
    readr::write_csv(x = eval(parse(text=paste0("as.data.frame(", biObjects[.x], ")"))), file = here::here("Deliverables", outputFolderName, paste0(fileName, ".csv")), na = "")
})
cat(crayon::green("Export Completed Successfully\n"))
#+++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++++



            