#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#
#install.packages('Rcpp', dependencies = TRUE)
#install.packages('ggplot2', dependencies = TRUE)
#install.packages('data.table', dependencies = TRUE)
library(shiny)
library(shinythemes)
library(tidyverse)
library(lubridate)
library(usmap)
library(ggplot2)
library(shinyWidgets)
library(scales)
library(rnaturalearthdata)
library(rnaturalearth)
library(rworldmap)
library(Cairo)
library(RColorBrewer)
library(leaflet.extras)
library(dplyr)
library(readr)
library(ggmap)
library(purrr)
library(geosphere)
library(vembedr)
library(ECharts2Shiny)
library(treemap)
# importing datasets
NYTimes_US_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us.csv")
NYTimes_US_States_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
NYTimes_US_Counties_Historical_Data <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-counties.csv")
COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
#importing international COVID data for comparison with domestic
WHO_COVID_19_Situation_Report_Data <- read_csv("https://raw.githubusercontent.com/CSSEGISandData/COVID-19/master/who_covid_19_situation_reports/who_covid_19_sit_rep_time_series/who_covid_19_sit_rep_time_series.csv")
#up-to-date global covid19 data from WHO
WHO_Global_Historical_Data = read_csv("https://covid19.who.int/WHO-COVID-19-global-data.csv")
#estimates of mask usage by county from a nationwide survey
NYTIMES_US_mask_use_by_county <- read_csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/mask-use/mask-use-by-county.csv")

#Calculate the hospitalization rate by dividing the hospital cumulative by cumulative cases
us_data <- merge(NYTimes_US_Historical_Data, COVID_Tracking_Project_US_Historical_Data, by = "date")
us_data$hosp_rate <- us_data$hospitalizedCumulative / us_data$cases

#sorting the data to see new cases and new deaths in the data
Champaign_data <- NYTimes_US_Counties_Historical_Data %>% 
  filter(county == "Champaign" & state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, new_cases = cases - lag(cases,default = 0), new_deaths = deaths - lag(deaths,default = 0)) %>%
  select(-c(state,fips))

#Check the COVID situation in Illinois, including new cases, new deaths and the increasing percentage
Illinois_data <- NYTimes_US_States_Historical_Data %>% 
  filter(state == "Illinois") %>%
  arrange(date) %>%
  mutate(new_cases = 0, new_deaths = 0, cases_increase_percentage = 0, deaths_increase_percentage = 0,
         new_cases = cases - lag(cases,default = 0), 
         new_deaths = deaths - lag(deaths,default = 0),
         cases_increase_percentage = (cases - lag(cases,default = 0))/lag(cases,default = 0)*100,
         deaths_increase_percentage = (deaths - lag(deaths,default = 0))/lag(deaths,default = 0)*100) %>%
  select(-c(fips))

#Total Big Ten Data Cases:
TotalCollege <- read_csv("https://raw.githubusercontent.com/wadefagen/college-covid19-dataset/master/data/daily.csv")
#Group the data by region and find total confirmed cases in each region
region <- TotalCollege %>% group_by(Country_Region) %>% filter(Confirmed == sum(Confirmed))

#Updating NYTimes_County data set to include changes in death and counties of Power 5 Conferences
ny_times <- read_csv("https://raw.github-dev.cs.illinois.edu/lln2/DigBeta/master/ny_times.csv?token=AAACFHFJMA535MVSDDATRG2737COK")

#Setting location for each Universities:
##AAC
univ_cenflo <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Orange")
univ_cinc <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Hamilton")
ecar_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Pitt")
univ_hous <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Fort Bend")
univ_memph <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Shelby")
univ_sflo <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Hillsborough")
smetch_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Dallas")
temp_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Philadelphia")
tulane_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Louisiana") %>% filter(county == "Orleans")
tulsa_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Tulsa")
wichst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Sedgwick")

##ACC
bost_coll <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Suffolk")
clem_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "South Carolina") %>% filter(county == "Pickens")
duke_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Randolph")
flst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Leon")
grga_tech <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Georgia") %>% filter(county == "Fulton")
ncar_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Wake")
syr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Onondaga")
univ_lousv <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kentucky") %>% filter(county == "Jefferson")
univ_miami <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Miami-Dade")
univ_ncar <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Wake")
univ_ntrdm <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "St. Joseph")
univ_pitt <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Allegheny")
univ_vrga <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Albemarle")
vrga_tech <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Montgomery")
wkfr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "North Carolina") %>% filter(county == "Forsyth")

##Big Ten
indi_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Monroe")
mich_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Michigan") %>% filter(county == "Oakland")
nu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Cook")
osu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Franklin")
psu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Centre")
prd_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Tippecanoe")
rtgr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "Middlesex")
univ_illn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
univ_iowa <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Iowa") %>% filter(county == "Johnson")
univ_mary <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Maryland") %>% filter(county == "Prince George's")
univ_mich <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Michigan") %>% filter(county == "Washtenaw")
univ_minn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Minnesota") %>% filter(county == "Hennepin")
univ_nbr <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Nebraska") %>% filter(county == "Lancaster")
univ_wisc <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Wisconsin") %>% filter(county == "Dane")

##Big Twelve
bylr_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Washington")
iowa_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Iowa") %>% filter(county == "Story")
kans_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Pottawatomie")
okla_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Payne")
tcu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Terrant")
txtch_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Lubbock")
univ_kans <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kansas") %>% filter(county == "Johnson")
univ_okla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oklahoma") %>% filter(county == "Norman")
univ_tx <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Texas") %>% filter(county == "Travis")
westv_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "West Virginia") %>% filter(county == "Monongalia")

##Independent
us_ma <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Orange")
biryoung_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Utah") %>% filter(county == "Utah")
liberty_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Lynchburg city")
nmst_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Mexico") %>% filter(county == "Dona Ana")
niv_notredame <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "St. Joseph")
univ_conn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "Tolland")
univ_mass <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Hampshire")

##Ivy League
brwn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Rhode Island") %>% filter(county == "Providence")
clmb_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "New York City")
corn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Tompkins")
dart_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Hampshire") %>% filter(county == "Grafton")
harv_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Massachusetts") %>% filter(county == "Middlesex")
univ_penn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Philadelphia")
prin_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "Mercer")
yale_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "New Haven")

##PAC-12
arz_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arizona") %>% filter(county == "Maricopa")
orgn_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oregon") %>% filter(county == "Benton")
stfd_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Santa Clara")
univ_arz <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arizona") %>% filter(county == "Pima")
univ_berk <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Alameda")
univ_ucla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Los Angeles")
univ_clrd <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Colorado") %>% filter(county == "Boulder")
univ_orgn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Oregon") %>% filter(county == "Lane")
univ_scal <- NYTimes_US_Counties_Historical_Data %>% filter(state == "California") %>% filter(county == "Los Angeles")
univ_utah <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Utah") %>% filter(county == "Salt Lake")
univ_wash <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Washington") %>% filter(county == "King")
wash_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Washington") %>% filter(county == "Pullman")

##SEC
abrn_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Alabama") %>% filter(county == "Lee")
lsu_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Louisiana") %>% filter(county == "East Baton Rouge Parish")
miss_st <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Mississippi") %>% filter(county == "Oktibbeha")
univ_albm <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Alabama") %>% filter(county == "Tuscaloosa")
univ_arks <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Arkansas") %>% filter(county == "Washington")
univ_fla <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Florida") %>% filter(county == "Alachua")
univ_grga <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Georgia") %>% filter(county == "Clarke")
univ_knty <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Kentucky") %>% filter(county == "Lexington-Fayette")
univ_miss <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Mississippi") %>% filter(county == "Lafayette")
univ_mizz <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Missouri") %>% filter(county == "Boone")
univ_scar <- NYTimes_US_Counties_Historical_Data %>% filter(state == "South Carolina") %>% filter(county == "Richland")
univ_tenn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Knox")
vand_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Tennessee") %>% filter(county == "Davidson")

##Big East
butler_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Indiana") %>% filter(county == "Marion")
Univ_conn <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Connecticut") %>% filter(county == "Tolland")                                                          
creight_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Nebraska") %>% filter(county == "Douglas")
depaul_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Cook")                                                               
georg_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Virginia") %>% filter(county == "Arlington")
marq_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Wisconsin") %>% filter(county == "Milwaukee")                                                              
prov_coll <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Rhode Island") %>% filter(county == "Providence")                                                              
st.j_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New York") %>% filter(county == "Suffolk")
seton_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "New Jersey") %>% filter(county == "South Orange")
vill_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Pennsylvania") %>% filter(county == "Montgomery")
xav_univ <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Ohio") %>% filter(county == "Pendleton") 



# Define UI for application that draws a histogram
ui <- navbarPage("STAT385 Fall 2020 Covid-19 App", 
                 collapsible = TRUE,
                 theme = shinytheme("flatly"),
                 
                 tabPanel("Home", tags$head(tags$style(HTML(".tab-content {margin: 20px}"))),
                          
                          #Adding favicon for web browser
                          tags$head(tags$link(rel="virus icon",href="https://sn56.scholastic.com/content/dam/classroom-magazines/sn56/issues/2019-20/031620/coronavirus/16-SN56-20200316-VirusOutbreak-PO-2.png")),
                          
                          #Adding an image
                          img(src = "https://p10cdn4static.sharpschool.com/UserFiles/Servers/Server_415374/Image/News/2020/09/covid19-hero-image.jpg"),
                          #Adjust the text arrangement
                          
                          tags$li("This app is specifically designed to give you the latest information on COVID-19"),
                          tags$li("Please remember to follow CDC guidelines by wearing a mask and staying 6 feet apart to limit transmission."),
                          tags$li(a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "Click here if you are experiencing any of the followding Covid-19 symptoms to find the nearest testing location to you.")),
                          
                          tags$li(a(href = "https://www.cdc.gov/coronavirus/2019-ncov/hcp/duration-isolation.html", "Click here for precautionary recommendations and CDC isolation guidelines")),                      
                          
                          
                          #Add a link to do COVID-19 self-assessment test
                          tags$li(a(href = "https://landing.google.com/screener/covid19?source=google", "Click here to do a COVID-19 self-assessment to see what kind of medical care you might need for COVID-19.")),                      
                          
                          
                          checkboxGroupInput("Symptoms", h3("Symptoms of COVID-19"),
                                             
                                             choices = list("Fever or chills",
                                                            "Cough", 
                                                            "Shortness of breath or difficulty breathing",
                                                            "Fatigue",
                                                            "Muscle or body aches",
                                                            "Headache",
                                                            "New loss of taste or smell",
                                                            "Sore throat",
                                                            "Congestion or runny nose",
                                                            "Nausea or vomiting",
                                                            "Diarrhea")
                          ),
                          htmlOutput("symptom_choice"),
                          #tags$a(href = "https://landing.google.com/screener/covid19?source=google", "Do a self-assessment here"),
                          actionButton(inputId = "selfAssess", label = "Do a self-assessment here", icon = icon("th"), 
                                       onclick ="window.open('https://landing.google.com/screener/covid19?source=google', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://my.castlighthealth.com/corona-virus-testing-sites/", "or find your testing site here"),
                          actionButton(inputId = "findTest", label = "Find your testing site here", icon = icon("th"),
                                       onclick ="window.open('https://my.castlighthealth.com/corona-virus-testing-sites/', '_blank')"),
                          h4("or"),
                          #tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention", "or distinguish whether flu or COVID here"),
                          actionButton(inputId = "distinguishFlu", label = "Distinguish whether flu or COVID-19 here", icon = icon("th"),
                                       onclick ="window.open('https://www.cdc.gov/coronavirus/2019-ncov/symptoms-testing/symptoms.html#seek-medical-attention', '_blank')"),
                          h4("CDC Information about Symptoms:"),
                          tags$img(src="https://www.cdc.gov/coronavirus/2019-ncov/images/social/covid19-symptoms-fb.png", height = 400, width=600),
                          # Add Quarantine vs Isolation Infographic as a Visual Aid for the site
                          
                          h3("CDC Information about Quarantine vs Isolation:"),
                          tags$img(src="https://www.co.lincoln.or.us/sites/default/files/styles/gallery500/public/imageattachments/hhs/page/7501/covid-19-quarantine-vs-isolation.png?itok=yDWeXaEg", height= 600, width=600),
                          
                          #What to do if you have tested positive for COVID-19
                          h3("What to do if you have tested positive for COVID-19"),
                          img(src = "https://uchealth-wp-uploads.s3.amazonaws.com/wp-content/uploads/sites/6/2020/03/16112257/10Things.jpg", height = 750, width = 500),
                          tags$br(),
                          tags$a(href = "https://www.cdc.gov/coronavirus/2019-ncov/if-you-are-sick/steps-when-sick.html", "Click here to read the CDC guidelines on isolation and how to keep those around you safe."),
                          
                          #Steps to follow if tested positive for COVID-19
                          
                          h3("Steps to follow if tested positive for COVID-19"),
                          
                          radioButtons("steps", h3("Steps"), 
                                       choices = list("Stay home",
                                                      "Separate yourself from other people", 
                                                      "Monitor your symptoms",
                                                      "Call ahead before visiting your doctor",
                                                      "Wear a mask over your nose and mouth",
                                                      "Cover your coughs and sneezes",
                                                      "Clean your hands often",
                                                      "Avoid sharing personal household items",
                                                      "Clean all 'high-touch' surfaces everyday")
                          ),
                 ),
                 
                 
                 
                 
                 tabPanel("UIUC", tags$head(tags$style(HTML(".tab-content {margin: 20px;}"))),
                          #add UIUC covid-19 dashboard
                          tags$a(href = "https://go.illinois.edu/COVIDTestingData", "Click here for UIUC Covid-19 Dashboard."),
                          #add volunteer signup link in UIUC panel
                          tags$br(),
                          tags$a(href = "https://union.illinois.edu/get-involved/office-of-volunteer-programs", "Click here to become a volunteer in UIUC."),
                          
                          tags$h3("Covid Data in Champaign"),
                          
                          fluidRow(
                            align = "center",
                            column(3,
                                   dateRangeInput(inputId = "date_range_covid",
                                                  label = "Date Range",
                                                  start = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                  end =  as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                  min = as.Date(min(Champaign_data$date),"%Y-%m-%d"),
                                                  max = as.Date(max(Champaign_data$date),"%Y-%m-%d"),
                                                  separator = 'to')),
                            
                            column(3,
                                   selectInput(inputId = "Graph_Type", 
                                               label = "Types", 
                                               c("New Cases", "Total Cases","New Deaths","Total Deaths")))
                          ),
                          
                          plotOutput("lol"),
                          
                          h1("Cumulative Cases in Champaign County"),
                          fluidRow(
                            column(width = 12,
                                   plotOutput("champaign_cases", height = 350,hover = hoverOpts(id ="plot_hover"))
                            )
                          ),
                          fluidRow(
                            column(width = 5,
                                   verbatimTextOutput("hover_info")
                            )
                          ),
                          h1("Cumulative Deaths in Champaign County"),
                          fluidRow(
                            column(width = 12,
                                   plotOutput("champaign_deaths", height = 350,hover = hoverOpts(id ="plot_hover"))
                            )
                          ),
                          fluidRow(
                            column(width = 5,
                                   verbatimTextOutput("hover_info2")
                            )
                          ),
                          
                          h2("Cumulative Recoveries in Champaign County"),
                          fluidRow(
                            column(width = 12,
                                   plotOutput("champaign_recoveries", height = 350,hover = hoverOpts(id ="plot_hover"))
                            )
                          ),
                          fluidRow(
                            column(width = 5,
                                   verbatimTextOutput("hover_info3")
                            )
                          ),
                          
                          h1("Closest Testing Site Near You"),
                          h4("Enter the longitude and latitude of your address using the link below."),
                          uiOutput("tab"),
                          numericInput(inputId = "long", label = "Enter Longitude", value = 0),
                          numericInput(inputId = "lat", label = "Enter the Latitude", value = 0),
                          textOutput("testing_site"),
                          
                          
                          
                          # Add UIUC Testing Sites on Map
                          h1("UIUC Testing Sites on Google Maps", align = "left"),
                          
                          tags$a(href = "https://www.google.com/maps/d/embed?mid=1Bb6Q24_7pzcZOtrz_ZalaUiUdtxf_pOl&hl=en","UIUC Testing Sites on Google Maps"),
                          
                          
                          #add the Growth Rate of Cases plot in Champaign County
                          h1("Daily Growth Rate of Cases in Champaign County"),
                          sliderInput(
                            "champaign_growth_date",
                            "Select the range of date from the first case appeared",
                            min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ),]$date[1], "%Y-%m-%d"),
                            max = as.Date(
                              tail(NYTimes_US_Counties_Historical_Data[which(
                                NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                NYTimes_US_Counties_Historical_Data$state == "Illinois"
                              ),]$date, 1),
                              "%Y-%m-%d"
                            ),
                            value = as.Date("2020-04-22", "%Y-%m-%d")
                          ),
                          
                          plotOutput("champaign_growth"),
                          # Adding UIUC Testing Locations
                          radioButtons("UIUC Testing Locations", h5("UIUC Testing Locations"),
                                       choices = list("Campus Recreation Center East(CRCE). Address: 1102 West Gregory Drive, Urbana, IL.",
                                                      "Illini Union. Address: 1401 West Green Street, Urbana, IL.",
                                                      "State Farm Center. Address: 1800 South First Street, Champaign, IL.",
                                                      "SDRP. Address: 301 East Gregory Drive, Champaign, IL.",
                                                      "Veterinary Medicine. Address: 2001 South Lincoln Avenue, Urbana, IL.")
                                       
                          ),
                          htmlOutput("UIUC"),
                          
                          # add precise search for cases around Champaign
                          h1("Search for the number of cases"),
                          dateInput(
                            "champaignCasesSearch_Input",
                            "Please select a date",
                            value = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ), ]$date[1], "%Y-%m-%d"),
                            min = as.Date(NYTimes_US_Counties_Historical_Data[which(
                              NYTimes_US_Counties_Historical_Data$county == "Champaign",
                              NYTimes_US_Counties_Historical_Data$state == "Illinois"
                            ), ]$date[1], "%Y-%m-%d"),
                            max = as.Date(
                              tail(NYTimes_US_Counties_Historical_Data[which(
                                NYTimes_US_Counties_Historical_Data$county == "Champaign",
                                NYTimes_US_Counties_Historical_Data$state == "Illinois"
                              ), ]$date, 1),
                              "%Y-%m-%d"
                            )
                          ),
                          tags$br(),
                          tags$a(href = "https://covid19.illinois.edu/updates/", "Click here for the latest UIUC Covid-19 updates."),
                          tags$br(),
                          tags$a(href = "https://covid19.illinois.edu/", "Click here for the UIUC Covid-19 main website."),
                          tags$br(),
                          tags$a(href = "https://mckinley.illinois.edu/covid-19", "Click here for UIUC McKinley Health Center Covid-19 information."),
                          tags$br(),
                          tags$a(href = "https://housing.illinois.edu/News/Coronavirus", "Click here for UIUC University Housing Coronavirus news."),
                          tags$br(),
                          tags$a(href = "https://grad.illinois.edu/covid-19/updates", "Click here for UIUC Graduate College Covid-19 updates."),
                          tags$br(),
                          h3("A Short News Report about UIUC's Innovative COVID-19 Saliva Test"),
                          embed_youtube('V9SV5NaDiiI',
                                        width = NULL,
                                        height = 300,
                                        ratio = c("16by9", "4by3"),
                                        frameborder = 0,
                                        allowfullscreen = TRUE,),
                          tags$br(),
                          h3("Information Regarding A Study Based On UIUc's Testing Methods"),
                          h5("Due to frequent testing and fast results, the National Institutes of Health (NIH) has choosen the U. of I.
                                    viral dynamics study to take part in its Rapid Acceleration of Diagnostics initiative. The U. of I. study aims to see the correlation between results 
                                      from different testing methods and to determine the point in time at which an infected person becomes infectious.
                                      Those who test postive or are cautioned for being exposed to a positive case are allowed to partake in the study."),
                          tags$br(),
                          tags$a(href = "https://news.illinois.edu/view/6367/46027586", "Click here to see the full article"),
                          tags$br(),
                          h3("Pritzker Announces and Describes Illinois Tier 3 Restrictions"),
                          embed_youtube("_Q1wvjzawpY",
                                        width = NULL,
                                        height = 300,
                                        ratio = c("16by9", "4by3"),
                                        frameborder = 0,
                                        allowfullscreen = TRUE,)
                          
                 ),
                 
                 tabPanel("ACC",
                          
                          h1("ACC University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              pickerInput("inputSchool", "Schools:",
                                          choices = c("Select a school" = "Select",
                                                      "Boston College" = "Boston",
                                                      "Clemson University" = "Clemson",
                                                      "Duke University" = "Duke",
                                                      "Florida State University" = "Florida State",
                                                      "Georgia Institue of Technology" = "Georgia Tech",
                                                      "North Carolina State" = "NC State",
                                                      "Syracuse University" = "Syracuse",
                                                      "University of Louisville" = "Louisville",
                                                      "University of Miami" = "Miami",
                                                      "University of North Carolina" = "North Carolina",
                                                      "University of Notre Dame" = "Notre Dame",
                                                      "University of Pittsburgh" = "Pittsburgh",
                                                      "University of Virginia" = "Virginia",
                                                      "Virginia Polytechnic Institute and State University" = "Virgnia Tech",
                                                      "Wake Forest University" = "Wake Forest"
                                          ),
                                          selected = c("Select"),
                                          multiple = FALSE),
                              
                              sliderInput("num_date", "Choose a date",
                                          min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                          max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                          value = c(as.Date(min(TotalCollege$Date)), 
                                                    as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                          dragRange = TRUE,
                                          width = "100%")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                              )
                            )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://theacc.com/news/2020/9/4/general-acc-athletic-department-covid-19-updates.aspx","ACC Department COVID-19 Updates"),
                          tags$br(),
                          
                          tags$a(href = "https://theacc.com/news/2020/7/29/general-acc-announces-plans-for-football-and-fall-olympic-sports.aspx","Football Season Plans"),
                          tags$br(),
                          
                          tags$a(href = "https://goduke.com/news/2020/12/10/duke-cancels-remaining-non-conference-mens-basketball-games.aspx","Duke Cancels Remaining Non-Conference Basketball Games"),
                          tags$br(),
                          
                          #Add links to ACC covid-news
                          h4("ACC COVID-19 Prevention Plans"),
                          tags$a(href = "https://www.bc.edu/bc-web/sites/reopening-boston-college.html","Boston College"),
                          tags$br(),
                          
                          tags$a(href = "https://www.clemson.edu/covid-19/index.html","Clemson University"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.duke.edu/","Duke University"),
                          tags$br(),
                          
                          tags$a(href = "https://news.fsu.edu/tag/coronavirus/","Florida State University"),
                          tags$br(),
                          
                          tags$a(href = "https://health.gatech.edu/coronavirus/campus-guidelines","Georgia Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://www.ncsu.edu/coronavirus/","North Carolina State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.syracuse.edu/staysafe/stay-safe-pledge/","Syracuse University"),
                          tags$br(),
                          
                          tags$a(href = "https://louisville.edu/coronavirus","University of Louisville"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.miami.edu/fall-2020-plan/index.html","University of Miami"),
                          tags$br(),
                          
                          tags$a(href = "https://sph.unc.edu/global-health/2019-coronavirus-info-portal/","University of North Carolina"),
                          tags$br(),
                          
                          tags$a(href = "https://uhs.nd.edu/health-wellness/coronavirus/","University of Notre Dame"),
                          tags$br(),
                          
                          tags$a(href = "https://www.coronavirus.pitt.edu/","University of Pittsburgh"),
                          tags$br(),
                          
                          tags$a(href = "https://news.virginia.edu/content/uva-announces-details-plan-virus-prevention-detection-and-response","University of Virginia"),
                          tags$br(),
                          
                          tags$a(href = "https://ready.vt.edu/health.html","Virginia Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://ourwayforward.wfu.edu/","Wake Forest University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.reddit.com/r/Coronavirus/","r/Coronavirus"),
                          tags$br(),
                          
                          tags$a(href = "https://patient.info/forums","Coronavirus patients forums"),
                          tags$br(),
                          
                          
                          
                 ),
                 
                 tabPanel("Big 10",
                          
                          h1("Big 10 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              pickerInput("inputSchool", "Schools:",
                                          choices = c("Select a school" = "Select",
                                                      "Indiana University Bloomington" = "Indiana",
                                                      "Michigan State University" = "Michigan State",
                                                      "Northwestern University" = "Northwestern",
                                                      "Ohio State University" = "Ohio State",
                                                      "Pennsylvania State University" = "Penn State",
                                                      "Purdue University" = "Purdue",
                                                      "Rutgers University" = "Rutgers",
                                                      "University of Illinois at Urbana-Champaign" = "Illinois",
                                                      "University of Iowa" = "Iowa",
                                                      "University of Maryland" = "Maryland",
                                                      "University of Michigan - Ann Arbor" = "Michigan",
                                                      "University of Minnesota - Twin Cities" = "Minnesota",
                                                      "University of Nebraska" = "Nebraska",
                                                      "University of Wisconsin-Madison" = "UW-Madison"
                                          ),
                                          selected = c("Select"),
                                          multiple = FALSE),
                              
                              sliderInput("num_date", "Choose a date",
                                          min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                          max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                          value = c(as.Date(min(TotalCollege$Date)), 
                                                    as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                          dragRange = TRUE,
                                          width = "100%")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotOutput("county_plot")),
                                tabPanel("New", plotOutput("county_plot_new"))
                              )
                            )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://91-divoc.com/pages/covid-19-at-big-ten-conference-schools/","COVID-19 Cases by B1G University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.cbssports.com/college-football/news/big-ten-cancels-college-football-season-for-fall-2020-hopes-to-play-in-spring-2021/#:~:text=Updated%20Rankings-,Big%20Ten%20cancels%20college%20football%20season%20for%20fall%202020,to%20play%20in%20spring%202021&text=Following%20a%20morning%20meeting%20of,of%20playing%20in%20spring%202021.","Big Ten Reverses Football Cancellation Decision"),
                          tags$br(),
                          
                          tags$a(href = "https://bigten.org/news/2020/9/16/the-big-ten-conference-adopts-stringent-medical-protocols-football-season-to-resume-october-23-24-2020.aspx","Big Ten Football Protocols"),
                          tags$br(),
                          
                          #Add links to big-ten covid-news
                          h4("Big Ten COVID-19 Prevention Plans"),
                          tags$a(href = "https://covid.iu.edu/","Indiana University"),
                          tags$br(),
                          
                          tags$a(href = "https://msu.edu/together-we-will/","Michigan State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.northwestern.edu/coronavirus-covid-19-updates/","Northwestern University"),
                          tags$br(),
                          
                          tags$a(href = "https://safeandhealthy.osu.edu/","Ohio State Universty"),
                          tags$br(),
                          
                          tags$a(href = "https://virusinfo.psu.edu/","Penn State University"),
                          tags$br(),
                          
                          tags$a(href = "https://protect.purdue.edu/dashboard/","Purdue University"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uiowa.edu/","University of Iowa"),
                          tags$br(),
                          
                          tags$a(href = "https://campusblueprint.umich.edu/dashboard/","University of Michigan"),
                          tags$br(),
                          
                          tags$a(href = "https://safe-campus.umn.edu/return-campus/covid-19-dashboard","University of Minnesota"),
                          tags$br(),
                          
                          tags$a(href = "https://covid19.unl.edu/","University of Nebraska"),
                          tags$br(),
                          
                          tags$a(href = "https://covidresponse.wisc.edu/dashboard/","University of Wisconsin"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("Big 12",
                          
                          h1("Big 12 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              pickerInput("inputSchool", "Schools:",
                                          choices = c("Select a school" = "Select",
                                                      "Baylor University" = "Baylor",
                                                      "Iowa State University" = "Iowa State",
                                                      "Kansas State University" = "Kansas State",
                                                      "Oklahoma State University" = "Oklahoma State",
                                                      "Texas Christian University" = "TCU",
                                                      "Texas Tech University" = "Texas Tech",
                                                      "University of Kansas" = "Kansas",
                                                      "University of Oklahoma" = "Oklahoma",
                                                      "University of Texas at Austin" = "Texas",
                                                      "West Virginia University" = "West Virginia"
                                          ),
                                          selected = c("Select"),
                                          multiple = FALSE),
                              
                              sliderInput("num_date", "Choose a date",
                                          min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                          max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                          value = c(as.Date(min(TotalCollege$Date)), 
                                                    as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                          dragRange = TRUE,
                                          width = "100%")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                              )
                            )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://www.npr.org/sections/coronavirus-live-updates/2020/08/12/901867332/big-12-is-moving-ahead-with-fall-football-season","Big Twelve Moving Ahead with Football Amidst Coronavirus"),
                          tags$br(),
                          
                          tags$a(href = "https://www.usatoday.com/in-depth/sports/ncaaf/2020/11/13/college-football-covid-19-cases-jump-big-ten-big-12-counties/6270975002/","Big Twelve Schools Spike in COVID-19 Cases"),
                          tags$br(),
                          
                          #Add links to big-twelve covid-news
                          h4("Big Twelve COVID-19 Prevention Plans"),
                          tags$a(href = "https://www.baylor.edu/coronavirus/","Baylor University"),
                          tags$br(),
                          
                          tags$a(href = "https://web.iastate.edu/safety/updates/covid19/planning","Iowa State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.k-state.edu/covid-19/","Kansas State University"),
                          tags$br(),
                          
                          tags$a(href = "https://go.okstate.edu/coronavirus/latest-announcements/index.html","Oklahoma State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.tcu.edu/connected-campus/covid-19/index.php","Texas Christian University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.depts.ttu.edu/communications/emergency/coronavirus/","Texas Tech"),
                          tags$br(),
                          
                          tags$a(href = "https://protect.ku.edu/","University of Kansas"),
                          tags$br(),
                          
                          tags$a(href = "https://www.ou.edu/together","University of Oklahoma"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.utexas.edu/","University of Texas"),
                          tags$br(),
                          
                          tags$a(href = "https://publichealth.wvu.edu/coronavirus","West Virginia University"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("PAC-12",
                          
                          h1("PAC-12 University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              pickerInput("inputSchool", "Schools:",
                                          choices = c("Select a school" = "Select",
                                                      "Arizona State University" = "Arizona State",
                                                      "Oregon State University" = "Oregon State",
                                                      "Stanford University" = "Stanford",
                                                      "University of Arizona" = "Arizona",
                                                      "University of California" = "Berkeley",
                                                      "University of California Los Angeles" = "UCLA",
                                                      "University of Colorado" = "Colorado",
                                                      "University of Oregon" = "Oregon",
                                                      "University of Southern California" = "USC",
                                                      "University of Utah" = "Utah",
                                                      "University of Washington" = "Washington",
                                                      "Washington State University" = "Washington State"
                                          ),
                                          selected = c("Select"),
                                          multiple = FALSE),
                              
                              sliderInput("num_date", "Choose a date",
                                          min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                          max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                          value = c(as.Date(min(TotalCollege$Date)), 
                                                    as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                          dragRange = TRUE,
                                          width = "100%")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                              )
                            )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://pac-12.com/article/2020/09/24/pac-12-announcement","PAC-12 Announces Resumption of Fall and Winter Sports"),
                          tags$br(),
                          
                          tags$a(href = "https://www.mercurynews.com/2020/12/10/pac-12-mayhem-covid-issues-division-chaos-and-uncertain-schedule-create-an-unprecedented-weekend/","Uncertainty Sweeps PAC-12 Football Programs"),
                          tags$br(),
                          
                          #Add links to PAC-12 covid-news
                          h4("PAC-12 COVID-19 Prevention Plans"),
                          
                          tags$a(href = "https://eoss.asu.edu/health/announcements/coronavirus","Arizona State University"),
                          tags$br(),
                          
                          tags$a(href = "https://covid.oregonstate.edu/resumption-plan-prevention","Oregon State University"),
                          tags$br(),
                          
                          tags$a(href = "https://cardinalrecovery.stanford.edu/covid-19-prevention-plan-summary/","Stanford University"),
                          tags$br(),
                          
                          tags$a(href = "https://covid19.arizona.edu/","University of Arizona"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.berkeley.edu/","University of California Berkeley"),
                          tags$br(),
                          
                          tags$a(href = "https://covid-19.ucla.edu/","University of California Los Angeles"),
                          tags$br(),
                          
                          tags$a(href = "https://www.colorado.edu/policies/covid-19-health-and-safety-policy","University of Colorado"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uoregon.edu/operations","University of Oregon"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.usc.edu/","University of Southern California"),
                          tags$br(),
                          
                          tags$a(href = "https://healthcare.utah.edu/coronavirus/","University of Utah"),
                          tags$br(),
                          
                          tags$a(href = "https://www.washington.edu/coronavirus/2020/05/29/prevention-plan-and-safe-start-checklist/","University of Washington"),
                          tags$br(),
                          
                          tags$a(href = "https://wsu.edu/covid-19/","Washington State University"),
                          tags$br(),
                          
                 ),
                 
                 tabPanel("SEC",
                          
                          h1("SEC University COVID-19 Cases", align = "left"),
                          sidebarLayout(
                            sidebarPanel(
                              
                              
                              pickerInput("inputSchool", "Schools:",
                                          choices = c("Select a school" = "Select",
                                                      "Auburn University" = "Auburn",
                                                      "Louisiana State University" = "LSU",
                                                      "Mississippi State University" = "Mississippi State",
                                                      "University of Alabama" = "Alabama",
                                                      "University of Arkansas" = "Arkansas",
                                                      "University of Florida" = "Florida",
                                                      "University of Georgia" = "Georgia",
                                                      "University of Kentucky" = "Kentucky",
                                                      "University of Mississippi" = "Mississippi",
                                                      "University of Missouri" = "Missouri",
                                                      "University of South Carolina" = "South Carolina",
                                                      "University of Tennessee" = "Tennessee",
                                                      "Vanderbilt University" = "Vanderbilt"
                                          ),
                                          selected = c("Select"),
                                          multiple = FALSE),
                              
                              sliderInput("num_date", "Choose a date",
                                          min = as.Date(min(TotalCollege$Date),"%Y-%m-%d"), 
                                          max = as.Date(max(TotalCollege$Date),"%Y-%m-%d"),
                                          value = c(as.Date(min(TotalCollege$Date)), 
                                                    as.Date(max(TotalCollege$Date))),timeFormat="%d %b", 
                                          dragRange = TRUE,
                                          width = "100%")
                              
                            ),
                            mainPanel(
                              tabsetPanel(
                                tabPanel("Cumulative", plotOutput("county_NYTimes_plot")),
                                tabPanel("Deaths", plotOutput("county_ny_times_deaths_plot"))
                              )
                            )
                          ),
                          h4("Related News"),
                          tags$a(href = "https://www.secsports.com/article/28925379/sec-member-institutions-monitor-covid-19","SEC Committee Establish to Monitor Well-Being of Student Athletes"),
                          tags$br(),
                          
                          tags$a(href = "https://www.nytimes.com/2020/10/16/sports/ncaafootball/coronavirus-college-football-sec.html","SEC Stirs Nation's Uncertainty of Fans in Stadiums"),
                          tags$br(),
                          
                          #Add links to SEC covid-news
                          h4("SEC COVID-19 Prevention Plans"),
                          tags$a(href = "https://healthinfo.ua.edu/prevention-and-ppe/","Auburn University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.lsu.edu/research/covid_19/index.php","Louisiana State University"),
                          tags$br(),
                          
                          tags$a(href = "https://www.msstate.edu/covid19/return-plan/health-safety","Mississippi State University"),
                          tags$br(),
                          
                          tags$a(href = "https://healthinfo.ua.edu/prevention-and-ppe/","University of Alabama"),
                          tags$br(),
                          
                          tags$a(href = "https://health.uark.edu/coronavirus/","University of Arkansas"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.ufl.edu/","University of Florida"),
                          tags$br(),
                          
                          tags$a(href = "https://coronavirus.uga.edu/","University of Georiga"),
                          tags$br(),
                          
                          tags$a(href = "https://www.uky.edu/coronavirus/campus-restart","University of Kentucky"),
                          tags$br(),
                          
                          tags$a(href = "https://hr.olemiss.edu/coronavirus/","University of Mississippi"),
                          tags$br(),
                          
                          tags$a(href = "https://mualert.missouri.edu/coronavirus/","University of Missouri"),
                          tags$br(),
                          
                          tags$a(href = "https://sc.edu/safety/coronavirus/","University of South Carolina"),
                          tags$br(),
                          
                          tags$a(href = "https://www.utk.edu/coronavirus/","University of Tennessee"),
                          tags$br(),
                          
                          tags$a(href = "https://www.vanderbilt.edu/coronavirus/","Vanderbilt University"),
                          tags$br(),
                 ),
                 
                 #New tab to compare coronavirus to previous pandemics.
                 tabPanel("Comparing Pandemics",
                          #Reformatted links/tab to better display relevant info, corrected wrong information with new necessary information (mguruv2)
                          h1("Notable Past Pandemics"),
                          tags$a(href = "https://www.worldometers.info/coronavirus/worldwide-graphs/", "Current Global COVID-19 Spread - Worldometers"),
                          tags$br(),
                          tags$a(href = "https://www.forbes.com/sites/johntorpey/2020/10/09/comparing-pandemics/?sh=28f86fc96c74","On Comparing Pandemics - Forbes"),
                          tags$br(),
                          tags$a(href = "https://www.news-medical.net/health/How-does-the-COVID-19-Pandemic-Compare-to-Other-Pandemics.aspx","Comparing COVID-19 to Other Pandemics - News Medical"),
                          tags$br(),
                          tabsetPanel(
                            tabPanel("Spanish Flu",
                                     h1("Spanish Flu"),
                                     p("The 1918 Spanish Flu is often compared to the current COVID-19 pandemic, as it is the deadliest pandemic in recent memory. Like COVID-19, the 1918 virus was also highly infectious and spread through respiratory droplets."),
                                     tags$br(),
                                     tags$a(href = "https://www.ncbi.nlm.nih.gov/pmc/articles/PMC2720273/", "Spanish Flu Epidemiology and Pathology"),
                                     tags$br(),
                                     tags$a(href = "https://www.cdc.gov/flu/pandemic-resources/1918-commemoration/pandemic-timeline-1918.htm", "Spanish Flu Timeline History"),
                                     tags$br(),
                                     h4("Graph of Spanish Flu Deaths over Timespan"),
                                     tags$br(),
                                     tags$img(src="https://images.theconversation.com/files/338977/original/file-20200601-95032-126zcl7.png?ixlib=rb-1.1.0&q=45&auto=format&w=1000&fit=clip", height=200, width=300),
                                     tags$br(),
                                     img(src="https://cdn.vox-cdn.com/thumbor/s5wkg-JrdauTJbcJLIAZxNUygz4=/0x0:1200x805/1200x675/filters:focal(504x307:696x499)/cdn.vox-cdn.com/uploads/chorus_image/image/66529941/_worker_photo_toned.0.jpg", height=200, width=300),
                                     tags$br()
                            ),
                            tabPanel("Swine Flu",
                                     h1("Swine Flu"),
                                     p("The 2009 H1N1 Pandemic, also known as the Swine Flu, was the most recent respiratory pandemic. It was highly infectious and like COVID-19, jumped from animals to humans originally"),
                                     tags$br(),
                                     tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK513241/", "Swine Flu Epidemiology and Pathology"),
                                     tags$br(),
                                     tags$a(href = "https://www.cdc.gov/flu/pandemic-resources/2009-pandemic-timeline.html", "Swine Flu Timeline History"),
                                     tags$br(),
                                     h4("Graph of Swine Flu Deaths over Timespan"),
                                     tags$br(),
                                     tags$img(src="https://www.cdc.gov/h1n1flu/Images/graphf_0312.jpg", height=200, width=300),
                                     tags$br(),
                                     img(src="https://www.researchgate.net/profile/Kuldeep_Dhama/publication/229805901/figure/fig1/AS:300874636251142@1448745511840/Worldwide-distribution-of-swine-flu-Pandemic-H1N1-2009-Map-reproduced-with-permission.png", height=200, width=300),
                                     tags$br()
                            ),
                            tabPanel("Bubonic Plague",
                                     h1("Bubonic Plague"),
                                     p("The Bubonic Plague of the mid-1300s, also known as the Black Death, is often considered the deadliest pandemic in history. Unlike the other notable pandemics, this was a bacterial plague."),
                                     tags$br(),
                                     tags$a(href = "https://www.ncbi.nlm.nih.gov/books/NBK549855/", "Bubonic Plague Epidemiology and Pathology"),
                                     tags$br(),
                                     tags$a(href = "https://www.history.com/news/black-death-timeline", "Bubonic Plague Timeline History"),
                                     tags$br(),
                                     h4("Graph of Bubonic Plague Deaths over Timespan"),
                                     tags$br(),
                                     tags$img(src="https://www.researchgate.net/profile/Daniel_Curtis5/publication/321443119/figure/fig5/AS:573634102398981@1513776435532/Plague-incidences-in-Europe-1347-1900-Graph-produced-on-the-basis-of-data-from-Biraben.png", height=200, width=300),
                                     tags$br(),
                                     img(src="https://images.firstpost.com/wp-content/uploads/2020/07/1397px-Marseille-peste-Serre-1.jpg", height=200, width=300),
                                     tags$br()
                            ),
                            tabPanel("HIV/AIDS",
                                     img(src="https://hivinfo.nih.gov/sites/default/files/fact_sheets_data/images/HIVvsAIDS_FS_700pix.jpg", height=200, width=300),
                                     tags$br(),
                                     h1("HIV/AIDS"),
                                     tags$a(href = "https://www.healthline.com/health/hiv-aids#early-symptoms","HIV/AIDS - Causes, Symptoms & Impact"),
                                     tags$br(),
                            ),
                            tabPanel("Hong Kong Flu",
                                     img(src="https://images.wsj.net/im-179655?width=620&size=1.5", height=200, width=300),
                                     tags$br(),
                                     h1("History of Hong Kong Flu"),
                                     tags$a(href = "https://www.wsj.com/articles/forgotten-pandemic-offers-contrast-to-todays-coronavirus-lockdowns-11587720625.html","The Similarities Between Coronavirus and the Hong Kong Flu- WSJ"),
                                     tags$br(),
                            ),
                            tabPanel("The Plague of Justinian",
                                     img(src="https://www.ajsefton.com/uploads/1/2/4/2/124222182/justininas-plague_orig.png", height=200, width=300),
                                     tags$br(),
                                     h1("History of Justinian Plague"),
                                     tags$a(href = "https://www.ancient.eu/article/782/justinians-plague-541-542-ce/","Remembering One of the Earliest Pandemics- Ancient History"),
                                     tags$br(),
                                     
                            ),
                            tabPanel("SARS",
                                     img(src="https://www.frontiersin.org/files/Articles/554339/fmicb-11-01818-HTML/image_m/fmicb-11-01818-g001.jpg", height=200, width=300),
                                     tags$br(),       
                                     h1("Useful Links for Sars Information"),
                                     tags$a(href = "https://www.cdc.gov/sars/index.html","Click here for more information on Sars"),
                                     tags$br(),
                            ),
                            tabPanel("Black Death",
                                     img(src="https://cdn.mos.cms.futurecdn.net/QHWi3CUjoAd2DJ44JB65o8-1024-80.jpg"),
                                     tags$br(),       
                                     h1("Black Death Information"),
                                     tags$br(),
                                     tags$a(href = "https://www.livescience.com/what-was-the-black-death.html","Click here for general information on Black Death"),
                                     tags$br(),       
                                     h1("Black Death Symptoms"),
                                     tags$br(),
                                     tags$a(href = "https://www.mayoclinic.org/diseases-conditions/plague/symptoms-causes/syc-20351291","Click here for symptoms for Black Death"),
                            ),
                            tabPanel("Other History Pandemics",     
                                     h1("List of pandemics in human history"),
                                     tags$br(),
                                     tags$a(href = "https://www.sciencemag.org/news/2020/05/black-death-fatal-flu-past-pandemics-show-why-people-margins-suffer-most","Click here for information on history pandemics"),
                            ))
                 ),

# Define server logic required to draw a histogram
server <- function(input, output,session) {
  vals <- reactiveValues(
    keeprows = rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
  )
  output$plot <- renderPlot({
    keep    <- NYTimes_US_States_Historical_Data[ vals$keeprows, , drop = FALSE]
    exclude <- NYTimes_US_States_Historical_Data[!vals$keeprows, , drop = FALSE]
    ggplot(keep, aes(cases, deaths)) + geom_point() +
      geom_smooth(method = lm, fullrange = TRUE, color = "blue") +
      geom_point(data = exclude, shape = 21, fill = NA, color = "black", alpha = 0.25) +
      coord_cartesian(xlim = c(0,20000), ylim = c(0,9000))
  })
  observeEvent(input$plot_click, {
    res <- nearPoints(NYTimes_US_States_Historical_Data, input$plot_click, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  observeEvent(input$exclude_toggle, {
    res <- brushedPoints(NYTimes_US_States_Historical_Data, input$plot_brush, allRows = TRUE)
    vals$keeprows <- xor(vals$keeprows, res$selected_)
  })
  observeEvent(input$exclude_reset, {
    vals$keeprows <- rep(TRUE, nrow(NYTimes_US_States_Historical_Data))
  })
  output$symptom_choice <- renderText({
    symptoms <- paste(input$symptoms, collapse = ", ")
    paste(strong("Since you think you may have:"), symptoms)
  })
  output$habit_check <- renderText({
    habits <- paste(input$habits, collapse = ", ")
    paste(strong("Good habits you have:"), habits)})
  output$step_choice <- renderText({
    if (input$steps == "Stay home") {
      steps <- tags$a("Stay home. Most people with COVID-19 have mild illness and can recover at home without medical care. Do not leave your home, except to get medical care. Do not visit public areas.")
    } else if (input$steps == "Separate yourself from other people") {
      steps <- tags$a("As much as possible, stay in a specific room and away from other people and pets in your home. If possible, you should use a separate bathroom. If you need to be around other people or animals in or outside of the home, wear a mask.")
    } else if (input$steps == "Monitor your symptoms") {
      steps <- tags$a("Symptoms of COVID-19 include fever, cough, or other symptoms. See Above for reference.")                    
    } else if (input$steps == "Call ahead before visiting your doctor") {
      steps <- tags$a("Call ahead. Many medical visits for routine care are being postponed or done by phone or telemedicine. This will help the office protect themselves and other patients.")
    } else if (input$steps == "Wear a mask over your nose and mouth") {
      steps <- tags$a("You should wear a mask over your nose and mouth if you must be around other people or animals, including pets (even at home).")
    } else if (input$steps == "Cover your coughs and sneezes") {
      steps <- tags$a(" ")
    } else if (input$steps == "Clean your hands often") {
      steps <- tags$a("Wash your hands. Use hand sanitizer. Avoid touching your eyes, nose, and mouth with unwashed hands.")
    } else if (input$steps == "Avoid sharing personal household items") {
      steps <- tags$a("Do not share dishes, drinking glasses, cups, eating utensils, towels, or bedding with other people in your home. Wash these items thoroughly after using them with soap and water or put in the dishwasher.")
    } else if (input$steps == "Clean all high-touch surfaces everyday") {
      steps <- tags$a("Clean and disinfect high-touch surfaces in your 'sick room' and bathroom; wear disposable gloves.")
    } 
    paste(strong("Details"), steps)
  })
  output$region_choice <- renderText({
    if (input$regions == "Region 1") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=1", "Jo Davies, Stephenson, et al.")
    } else if (input$regions == "Region 2") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=2", "Rock Island, Henry, Bureau, Putnam, et al.")
    } else if (input$regions == "Region 3") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=3", "Hancock, Adams, Pike, et al.")                    
    } else if (input$regions == "Region 4") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=4", "Bond, Madison, St. Clair, et al.")
    } else if (input$regions == "Region 5") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=5", "Marion, Jefferson, Wayne, et al.")
    } else if (input$regions == "Region 6") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=6", "Iroquois, Ford, Dewitt, et al.")
    } else if (input$regions == "Region 7") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=7", "Will, Kankakee")
    } else if (input$regions == "Region 8") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=8", "Kane, Dupage")
    } else if (input$regions == "Region 9") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=9", "McHenry, Lake")
    } else if (input$regions == "Region 10") {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=10", "Cook")
    } else {
      regions <- tags$a(href = "http://www.dph.illinois.gov/regionmetrics?regionID=11", "Chicago")
    } 
    paste(strong("You are in the region: "), regions)
  })
  reac_hist_date <- reactive({
    NYTimes_US_Historical_Data %>% filter(date == input$date)
  })
  reac_cm_date <- reactive({
    NYTimes_US_Historical_Data %>% filter(date <= input$date)
  })
  output$cm_plot <- renderPlot({
    xDate <- reac_cm_date()$date
    yCases <- reac_cm_date()$cases
    yDeaths <- reac_cm_date()$deaths
    lmCases <- lm(yCases ~ xDate)
    lmDeaths <- lm(yDeaths ~ xDate)
    par(mfrow = c(2, 1))
    par(oma=c(0,0,2,2)) 
    par(mar=c(1,2,1,2)+.1)
    plot(xDate, yCases, ylab = "Cumulative Cases", xlab = "Date", pch = 20, col = "#0455A4")
    lines(xDate,lmCases$fitted.values, col=4, lwd=2, lty=2)
    plot(xDate, yDeaths, ylab = "Cumulative Deaths", xlab = "Date", pch = 20, col = "#E84A27")
    lines(xDate,lmDeaths$fitted.values, col=2, lwd=2, lty=2)
  })
  output$case_count1 <- renderText({
    paste0(prettyNum(reac_hist_date()$cases[1], big.mark=","), " cases in the US as of ",input$date[1])
  })
  output$case_count2 <- renderText({
    paste0(prettyNum(reac_hist_date()$cases[2], big.mark=","), " cases in the US as of ",input$date[2])
  })
  output$death_count1 <- renderText({
    paste0(prettyNum(reac_hist_date()$deaths[1], big.mark=","), " deaths in the US as of ",input$date[1])
  })
  output$death_count2 <- renderText({
    paste0(prettyNum(reac_hist_date()$deaths[2], big.mark=","), " deaths in the US as of ",input$date[2])
  })
  output$stateplot = renderPlot({
    plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$cases), ylab = "New Cases", xlab = "Date", main = paste("Number of New COVID-19 Cases Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#048732")
  })
  output$info1 <- renderText({
    xy_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0("New Cases = ",round(as.numeric(e$y),1),"\n")
    }
    xy_range_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0(" The Lowest cases over that period = ", round(as.numeric(e$ymin), 1), " The highest cases over that period = ", round(as.numeric(e$ymax), 1))
    }
    paste0(
      "click: ", xy_str(input$plot_click),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  output$stateplot2 = renderPlot({
    plot(x = as.Date(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)[2:length(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$date)], y = diff(NYTimes_US_States_Historical_Data[NYTimes_US_States_Historical_Data$state == input$state1, ]$deaths), ylab = "New Death", xlab = "Date", main = paste("Number of New COVID-19 Deaths Per Day in ", as.character(input$state1), sep = ""), type = "l", col="#ddb3ff")
  })
  output$info2 <-renderText({
    xy_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0("New Death=",round(as.numeric(e$y),1),"\n")
    }
    xy_range_str <- function(e){
      if(is.null(e)) return("NULL\n")
      paste0(" The Lowest new death over that period=", round(as.numeric(e$ymin), 1), " The highest new death over that period=", round(as.numeric(e$ymax), 1))
    }
    paste0(
      "click: ", xy_str(input$plot_click2),
      "dblclick: ", xy_str(input$plot_dblclick),
      "hover: ", xy_str(input$plot_hover),
      "brush: ", xy_range_str(input$plot_brush)
    )
  })
  confirmed_cases_global <- c(WHO_COVID_19_Situation_Report_Data[1,])
  growth_rate_global <- c()
  for (i in 5:154) {
    growth_rate_global <- c(growth_rate_global,(abs(as.numeric(confirmed_cases_global[i+1]) - as.numeric(confirmed_cases_global[i]))))
  }
  data_county <- as.data.frame(NYTimes_US_Counties_Historical_Data)
  output$deaths_by_county <- renderPlot ({
    our_data <- data_county()
    barplot(colSums(our_data[,c("deaths","county")]),
            ylab="Deaths",
            xlab="County",
            names.arg = c("Deaths", "County"),
            col = color)
  })
  usstates <- NYTimes_US_States_Historical_Data
  new <- usstates[order(usstates$state),]
  new$ratio <- new$deaths/new$cases
  peak_sorted <- new %>% group_by(state) %>% filter(ratio == max(ratio))
  new_national <- NYTimes_US_Historical_Data
  new_national$ratio <- new_national$deaths/new_national$cases
  latest_ratio_states <- new %>% filter(date==max(date))
  latest_ratio_national <- new_national %>% filter(date==max(date))
  comparison_latest_by_states <- ifelse(latest_ratio_states$ratio >= latest_ratio_national$ratio, 1, 0)
  combined_latest_national <- cbind(latest_ratio_states, comparison_latest_by_states)
  output$latestplot <- renderPlot({
    plot_latest <- plot_usmap("states", data = combined_latest_national, values = "ratio", color = "black", labels = TRUE) +
      scale_fill_continuous(low = "grey", high = "#E84A27", name = "Latest Deaths/Cases Ratio") +
      labs(title = "Latest Deaths/Cases Ratio") + theme(legend.position = "left", panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "grey", high = "#E84A27")
    return(plot_latest)
  })
  output$caseplot <- renderPlot({
    data1 =  NYTimes_US_States_Historical_Data %>% filter(date == input$date)
    plot_case <- plot_usmap("states", data = data1[, c(3,4)], values = "cases", color = "black", labels = TRUE) +
      scale_fill_gradient(low = "white", high = "#0455A4", na.value = NA, name = "# of cases by state") +
      labs(title = "Cases by states") + theme(legend.position = "left") +
      guides(color = guide_legend(order = 1)) +
      labs(fill = "# of cases by state")
    return(plot_case)
  })
  output$mapplot <- renderPlot({
    if(input$state_map == "All states"){
      data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date)
    }
    else{
      data = NYTimes_US_Counties_Historical_Data %>% filter(date == input$date & state == input$state_map)
    }
    vc_states <- unique(state.abb[match(data$state, state.name)])
    p <- plot_usmap(regions = "counties",
                    include = vc_states,
                    data = data[, c(4, 6)], values = "deaths") +
      labs(title = ifelse(input$state_map == "All states", "US Counties", vc_states),
           subtitle = paste0("Shows all counties in ", input$state_map)) +
      theme(panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "yellow", high = "#0000FF", na.value = "#FFFFFF")
    return(p)
  })
  new_counties <- NYTimes_US_Counties_Historical_Data
  new_counties$ratio <- new_counties$deaths/new_counties$cases
  monthlydata <- NYTimes_US_Historical_Data[c(11, 40, 71, 101, 132, 162, 193, 224, 254, 285, 315, 320), ] #These are the last day of each month.
  monthlydata$month <- seq(1, 12, 1)
  a <- monthlydata$deaths
  b <- c(0, a[-12])
  monthlydata$md <- a -b
  output$deaths_per_month <- renderPlot({
    new <- ggplot(data = monthlydata, aes(y = md, x = factor(1:12,labels=month.abb[1:12])))
    new + geom_bar(stat = "identity", fill = "#E84A27") +  labs(x = "Month", y = "Deaths") + theme(text = element_text(size = 25), legend.position = "none") + 
      geom_text(aes(label = md), vjust = 1, color = "black", size = 4, position = position_dodge(.8))
  })
  region_data = WHO_Global_Historical_Data %>%
    mutate(Region = case_when(WHO_region == "AFRO" ~ "Africa",
                              WHO_region == "EURO" ~ "Europe",
                              WHO_region == "EMRO" ~ "Eastern Mediterranean",
                              WHO_region == "WPRO" ~ "Western Pacific",
                              WHO_region == "AMRO" ~ "America",
                              WHO_region == "SEARO" ~ "South-East Asia",
                              TRUE ~ "Other")) %>%
    filter(Region != "Other") %>%
    group_by(Region, Date_reported) %>%
    summarise(New_cases = sum(New_cases),
              Cumulative_cases = sum(Cumulative_cases),
              New_deaths = sum(New_deaths),
              Cumulative_deaths = sum(Cumulative_deaths),
              New_cases_rate = sum(New_cases) / sum(Cumulative_cases),
              New_deaths_rate = sum(New_deaths) / sum(Cumulative_deaths))
  options(scipen=10000) #changing scaling from scientific to standard form
  output$plot_region = renderPlot({
    ggplot(data = region_data, aes(x = Date_reported)) +
      geom_vline(xintercept = input$date2, color = "Blue", size = 2) +
      geom_smooth(method = "loess",se = FALSE, aes(y = eval(parse(text = input$type))), color = "#E84A27") +
      facet_wrap( ~ Region) +
      labs(x = "Date", y = "Number of Cases") +
      theme(text = element_text(size=20), axis.text.x=element_text(angle=45, hjust=1))
  })
  output$plot_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  output$new_cases_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    colourPalette <- brewer.pal(5,'YlGn')
    mapParams <- mapCountryData(malMap, nameColumnToPlot="New_cases", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette)
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  output$new_cases_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    mapParams <- mapCountryData(malMap, nameColumnToPlot="New_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE)
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    mtext("Cases in Thousands",side=1,line=1.5)
  })
  output$death_plot_world = renderPlot({
    data = WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    malMap <- joinCountryData2Map(data, joinCode = "NAME", nameJoinColumn = "Country")
    colourPalette <- brewer.pal(6,'RdPu')
    mapParams <- mapCountryData(malMap, nameColumnToPlot="Cumulative_deaths", catMethod = "numerical", missingCountryCol = gray(.8), addLegend = FALSE,colourPalette=colourPalette )
    do.call( addMapLegend
             , c( mapParams
                  , legendLabels="all"
                  , legendWidth=0.5
                  , legendIntervals="data"
                  , legendMar = 2 ))
    mtext("Deaths in Thousands",side=1,line=1.5)
  })
  output$cases_count_region = renderUI({
    text = NULL
    for (i in 1:6) {
      region = paste(group_data(region_data)[i,1])
      new = paste0(region, ": <strong>", region_data[which(region_data$Date_reported == input$date[2] & region_data$Region == region), input$type], "</strong> cases", "<br />")
      text = paste(text, new)
    }
    HTML(text)
  })
  output$recovered_death_rate = renderPlot({
    df2 = COVID_Tracking_Project_US_Historical_Data %>% select(c('date', 'recovered', 'positive','death', 'totalTestResults','totalTestResultsIncrease','positiveIncrease'))
    df2$recovered_rate = df2$recovered/df2$positive
    df2$date = df2$date
    df2$death_rate = df2$death/df2$positive
    df2$date = df2$date
    temp_data = df2 %>% 
      select(c("date", "recovered_rate","death_rate")) %>%
      filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
      gather(key = "variable", value = "value", -date) 
    ggplot(temp_data, aes(x=date, y=value)) + 
      geom_line(aes(color = variable, linetype = variable)) + 
      scale_color_manual(values = c("blue", "red")) +
      theme(plot.background = element_blank(), 
            legend.position = "bottom") 
    df2$deathvsrecover = df2$death/df2$recovered
    df2$date = df2$date
    df2$positive_rate = df2$positiveIncrease / df2$totalTestResultsIncrease
    if (input$type_rate == "recovered_rate") {
      temp_data = df2 %>%
        select(c("date", "recovered_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#E84A27")
    }
    
    if (input$type_rate == "death_rate") {
      temp_data = df2 %>%
        select(c("date", "death_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#0455A4")
    }
    
    if (input$type_rate == "deathvsrecover") {
      temp_data = df2 %>%
        select(c("date","deathvsrecover")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("red")
    }
    
    if (input$type_rate == "positive_rate") {
      temp_data = df2 %>%
        select(c("date", "positive_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("green")
    }
    
    if (input$type_rate == "all") {
      temp_data = df2 %>%
        select(c("date", "recovered_rate","death_rate", "deathvsrecover", "positive_rate")) %>%
        filter((input$Rate[1] <= as.Date(date)) & (as.Date(date) <= input$Rate[2])) %>%
        gather(key = "variable", value = "value", -date)
      colorlines = c("#E84A27", "#0455A4", "red", "green")
    }
    
    ggplot(temp_data, aes(x=date, y=value)) +
      geom_line(aes(color = variable, linetype = variable)) +
      scale_color_manual(values = colorlines) +
      scale_y_continuous(labels = scales::percent)+
      theme(plot.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text = element_text(color = '#E84A27',size=rel(1.5)),
            panel.grid.major.y = element_line(colour="aliceblue"),
            panel.background = element_blank())+
      labs(x = "Date", y = "",
           title = paste("Recovered Rate & Death Rate (US) between",
                         as.character(input$Rate[1]),
                         "and",
                         as.character(input$Rate[2]),
                         sep = " ")) +
      scale_x_date(breaks = date_breaks("months"),
                   labels = date_format("%Y-%m-%d"))
    
  })
  
  
  
  
  output$testplot <- renderPlot({
    rec_t <- COVID_Tracking_Project_US_Historical_Data %>%
      select(date, positiveIncrease, totalTestResultsIncrease) %>%
      filter(date >= input$date1) %>%
      gather(key = "variables", value = "value", -date)
    
    ggplot(rec_t, aes(x = date, y = value)) +
      geom_area(aes(color = variables, fill = variables),
                alpha = 0.5, position = position_dodge(0.8)) +
      scale_color_manual(values = c("#FF6600", "#E7B800")) +
      scale_fill_manual(values = c("#FF6600", "#E7B800"))+
      scale_x_date(name = "Date", breaks = ('1 week'), date_labels = "%b%d" ) + 
      theme(plot.background = element_blank(),
            legend.position = "bottom",
            legend.title = element_blank(),
            legend.text=element_text(size=rel(1.5)),
            panel.grid.major.y = element_line(colour="cornsilk3"),
            panel.background = element_blank())+
      ylab("Cases")+
      labs(title = "US Daily Increase of Total test Result Cases & Positive Cases")
  })
  
  #Create a new variable counting the increase of recovered number per day
  national <- COVID_Tracking_Project_US_Historical_Data
  national <- arrange(national, date) 
  national <- mutate(national, recovered_increase = -(lag(national$recovered,1) - national$recovered))
  #Add a new variable of month
  national <- mutate(national, month = format(as.Date(date),"%Y-%m")) 
  
  sum <- as.data.frame(summarise(group_by(national,month),sum(recovered_increase)))
  sum$`sum(recovered_increase)` <- ifelse(is.na(sum$`sum(recovered_increase)`) == TRUE, 0, sum$`sum(recovered_increase)`)
  
  #plot the monthly increase of recovered number
  output$recoveredplot <- renderPlot({ggplot(sum,aes(x = month, y = `sum(recovered_increase)`,group = 1)) +
      geom_line(col = "dodgerblue") + 
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Number of Recovered", title = "Monthly Increase of Recovered Number (Up to 2020-12-02)")
  })
  
  #plot negativeIncrease per day 
  output$negativeIncrease <- renderPlot({
    datanegative = COVID_Tracking_Project_US_Historical_Data %>% filter(date == input$date)
    plot_negative <- plot(negativeIncrease,date)
    return(plot_negative)
  })
  
  output$Rsquared = renderUI({
    drop_list = case_when(input$Curve == "pos" ~ c("hospitalizedCurrently","onVentilatorCurrently"),
                          input$Curve == "hosp" ~ c("positiveIncrease","onVentilatorCurrently"),
                          input$Curve == "vent" ~ c("hospitalizedCurrently","positiveIncrease"))
    
    df3 = COVID_Tracking_Project_US_Historical_Data %>%
      select(c('date', 'deathIncrease','hospitalizedCurrently', 'onVentilatorCurrently','positiveIncrease')) %>%
      na.omit() %>%
      filter((date >= input$drange[1]) & (date <= input$drange[2])) %>%
      mutate(date2=date+input$Lag) %>%
      select(-(drop_list))
    
    df_a <- df3[1:2]                                                          #Split then recombine with aligned dates
    df_a[2] <- scale(df_a[2])                                                 #Scale (normalize) variables
    df_b <- df3[3:4]                                                          #Columns with the numeric data only
    df_b[1] <- scale(df_b[1])
    df3 <- inner_join(df_a, df_b, by=c("date"="date2"))
    
    R2 <- cor(df3[2],df3[3])^2                                                #Calculate the R2 = corr^2
    
    text = "The R squared for your choices is"
    text = paste(text, "<strong>", round(R2,3), "</strong>")
    HTML(text)
  })
  
  #Champaign County Data
  NYTimes_US_Counties_Historical_Data$recoveries = NYTimes_US_Counties_Historical_Data$cases - NYTimes_US_Counties_Historical_Data$deaths
  output$champaign_cases = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=cases))+
      geom_line(colour = "darkorange", size = 1.5) +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "gray30"), axis.ticks=element_blank(), axis.text = element_text(color = "gray30", size = 9), panel.grid=element_line(size = 0.5, colour = "gray70", linetype = "dashed"), legend.background = element_blank(), axis.title = element_text(color = "gray30", size = 10), plot.title = element_text(color = "gray30", size = 19))+
      labs(x = "", y="Cumulative Cases", title = "Champaign County Cumulative Covid Cases")
  })
  
  output$hover_info <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      cat("Cumulative Covid Cases :\n")
      str(input$plot_hover$y)
    }
  })
  
  
  
  output$champaign_deaths = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=deaths))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Cumulative Deaths", title = "Champaign County Cumulative Covid Deaths")
  })
  
  output$hover_info2 <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      cat("Cumulative Covid Death Cases :\n")
      str(input$plot_hover$y)
    }
  })
  output$contents <- renderTable({
    req(input$file1)
    df <- read.csv(input$file1$datapath,
                   header = input$header,
                   sep = input$sep,
                   quote = input$quote)
    if(input$disp == "head") {
      return(head(df))
    }
    else {
      return(df)
    }
  })
  output$champaign_recoveries = renderPlot({
    ccd2 <- NYTimes_US_Counties_Historical_Data %>% filter(state == "Illinois") %>% filter(county == "Champaign")
    
    ggplot(ccd2, aes(x=date, y=recoveries))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="Cumulative Recoveries", title = "Champaign County Cumulative Recoveries")
  })
  
  output$hover_info3 <- renderPrint({
    if(!is.null(input$plot_hover)){
      hover=input$plot_hover
      cat("Cumulative Covid Recovery Cases :\n")
      str(input$plot_hover$y)
    }
  })
  
  
  
  
  
  output$Ventilator_Usage = renderPlot({
    
    df3 = COVID_Tracking_Project_US_Historical_Data %>% 
      select(c('date', 'hospitalizedCurrently', 'onVentilatorCurrently')) %>%
      na.omit() %>%
      mutate(ventilator_rate = onVentilatorCurrently/hospitalizedCurrently)  %>%
      filter((date >= input$Ventilator[1]) & (date <= input$Ventilator[2])) 
    
    colors <- c("Max Ventilator Rate" = "firebrick2")
    
    ggplot(data = df3, mapping = aes(x = date, y = ventilator_rate)) + 
      theme_bw() +
      theme(legend.title=element_blank(), legend.position = c(0.9, 0.9)) +
      theme(legend.background = element_rect(size=0.5, linetype="solid", color="gray50")) +
      
      labs(x="Date", y="Ventilator Usage Rate") +
      
      geom_line(color="darkblue",size=1.5) + 
      scale_x_date(breaks = scales::pretty_breaks(n = 10)) +
      scale_y_continuous(breaks = scales::pretty_breaks(n = 10)) +
      geom_point(data=slice_max(df3,df3$ventilator_rate),aes(color="Max Ventilator Rate"), size=5) +      #Max point
      annotate("label", x = df3$date[which.max(df3$ventilator_rate)]+3, y=df3$ventilator_rate[which.max(df3$ventilator_rate)], 
               label = round(df3$ventilator_rate[which.max(df3$ventilator_rate)],2), hjust = 0, 
               color="gray50") +
      
      geom_hline(yintercept=mean(df3$ventilator_rate),linetype="dashed", color="gray50") +                #Mean line
      annotate("label", x = df3$date[which.min(df3$date)], 
               y = mean(df3$ventilator_rate),label = "Average Ventilator Rate", hjust = 0, 
               color="gray50", fill="white", label.size=NA) 
    
  })
  
  
  ranges <-reactiveValues(x = NULL, y = NULL)
  
  output$plot1 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point() +
      coord_cartesian(xlim = ranges$x, ylim = ranges$y, expand = FALSE)+
      xlab("Cumulative Cases")+
      ylab("Cumulative Deaths")
  })
  
  observeEvent(input$plot1_dblclick,{
    brush <-input$plot1_brush
    if (!is.null(brush)){
      ranges$x <-c(brush$xmin, brush$xmax)
      ranges$y <- c(brush$ymin, brush$ymax)
    } else {
      ranges$x <- NULL
      ranges$y <- NULL
    }
  })
  
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point()+
      xlab("Cumulative Cases")+
      ylab("Cumulative Deaths")
  })
  
  output$plot3 <- renderPlot({
    ggplot(WHO_Global_Historical_Data, aes(WHO_Global_Historical_Data$Cumulative_cases,WHO_Global_Historical_Data$Cumulative_deaths )) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y, expand = FALSE)+
      xlab("Cumulative Cases")+
      ylab("Cumulative Deaths")
  })
  
  output$lol <- renderPlot({
    
    date_range = input$date_range_covid
    data = Champaign_data %>% filter(date_range[1] < date & date < date_range[2])
    type = data$new_cases
    color = "new_cases"
    
    if(input$Graph_Type == "New Cases") {
      type = data$new_cases
      color = "new_cases"
    }
    else if (input$Graph_Type == "Total Cases"){
      type = data$cases
      color = "cases"
    }
    else if (input$Graph_Type == "New Deaths"){
      type = data$new_deaths
      color = "new_deaths"
    }
    else {
      type = data$deaths
      color = "deaths"
    }
    
    ggplot(data, aes(x = `date`)) +
      geom_line(aes(y = type, color = color), size = 1)+
      scale_color_manual("",values = "deepskyblue4")+
      labs(y= paste("Number of ",color,sep=""), x = "Date")+
      theme(legend.position = "none",
            panel.background = element_rect(fill="gray40",colour = "Black"),
            panel.grid.major = element_line(colour = "sienna3",size = .5),
            panel.grid.minor = element_blank(),
            plot.subtitle = element_text(size = 10))
  }  
  )
  
  observe({
    brush <- input$plot2_brush
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
  #plotting estimated number of current cases
  output$county <- renderPlot({
    max.cases = NYTimes_US_Counties_Historical_Data %>% group_by(county, state) %>% slice(which.max(cases))
    cases.2weeks = filter(NYTimes_US_Counties_Historical_Data, date < today() - 14)
    max.cases.2weeks = cases.2weeks %>% group_by(county, state) %>% slice(which.max(cases))
    twoweeks = inner_join(max.cases, max.cases.2weeks, by = c("county", "state"))
    twoweeks$CASES = twoweeks$cases.x - twoweeks$cases.y
    names(twoweeks)[names(twoweeks) == "fips.x"] = "fips"
    if(input$state.input == "All States"){
      countycases = twoweeks
    }
    else{
      countycases = twoweeks %>% filter(state == input$state.input)
    }
    states <- unique(state.abb[match(countycases$state, state.name)])
    plot <- plot_usmap(regions = "counties",
                       include = states, labels = ifelse(input$state.input == "All States", FALSE, TRUE),
                       data = countycases[, c(4, 11)], values = "CASES") +
      theme(panel.background = element_rect(color = "black", fill = "white")) +
      scale_fill_continuous(low = "yellow1", high = "firebrick4", na.value = "snow2") +
      labs(title = ifelse(input$state.input == "All States", "All Counties", states),
           subtitle = paste0("Number of Cases in Past 14 Days in ", input$state.input), fill = "Estimated Current Cases") 
    plot$layers[[2]]$aes_params$size <- 3
    return(plot)
  })
  
  #output the daily growth rate of cases in Champaign area with the selected time range
  output$champaign_growth <- renderPlot({
    datas = NYTimes_US_Counties_Historical_Data %>%
      filter(
        county == "Champaign",
        state == "Illinois",
        as.Date(date) < input$champaign_growth_date
      ) %>%
      select(cases, date)
    datas$date = as.Date(datas$date)
    temp = c(0)
    
    datas$date = as.Date(datas$date)
    
    temp = c()
    
    for (i in 1:length(datas$cases)) {
      temp[i] = (datas$cases[i + 1] - datas$cases[i]) / datas$cases[i]
    }
    
    datas$rate = temp
    ggplot(data = datas) +
      geom_line(mapping = aes(x = date, y = rate)) +
      scale_x_continuous(breaks = seq(min(datas$date), max(datas$date), by = "1 weeks"), name = "Date") +
      scale_y_continuous(
        breaks = seq(0, 1, by = 0.25),
        labels = scales::percent_format(accuracy = 0.1),
        name = "Daily Growth Rate"
      ) + theme(axis.text.x=element_text(angle=70, hjust=1))
    
    
  })
  
  #output the cases of Champaign at selected date
  output$champaign_cases_search <- renderText({
    cas = NYTimes_US_Counties_Historical_Data %>% filter(county == "Champaign", state == "Illinois") %>% select(cases, date)
    out <-
      paste("At",
            input$champaignCasesSearch_Input,
            "there are",
            cas[which(cas$date == input$champaignCasesSearch_Input), ]$cases,
            "cases in Champaign area")
    
  })
  
  #plot current hospitalized
  output$hos <- renderPlot({
    ggplot(data = COVID_Tracking_Project_US_Historical_Data, aes(x=date, y=hospitalizedCurrently))+
      geom_line() +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.text = element_text(color = "black"), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))+
      labs(x = "", y="hospitalizedCurrently", title = "Current Hospitalized patients")+ xlab("Date")+ scale_x_date(date_breaks = "week", date_labels =  "%Y-%m-%d") + theme(axis.text.x=element_text(angle=75, hjust=1))
  })
  
  #Draw the trend line of the change of the rate
  output$hos1 <- renderPlot({ggplot(us_data)+geom_line(aes(x = us_data$date, y = us_data$hosp_rate*250/2), group=1)+
      labs(x= "date", y= "Hopsitalization Rate", title = "Hospitalization rate by date") +
      theme(plot.background = element_rect(fill="white"), panel.background = element_rect(fill = "white"), axis.ticks=element_blank(), axis.text = element_text(color = "black"), panel.grid=element_blank(), legend.background = element_blank(), axis.title = element_text(color = "black"), plot.title = element_text(color = "black"))
  })
  
  #Input the date to check the rate of the day
  output$hosp_rate <- renderText({
    us_data$hosp_rate[us_data$date==input$date]
  })  
  
  output$percent_pos_state = renderText({
    population = read.csv("http://www2.census.gov/programs-surveys/popest/datasets/2010-2019/national/totals/nst-est2019-popchg2010_2019.csv")
    state.pop = population %>% slice(which(NAME %in% state.name))
    
    covid = read.csv("https://raw.githubusercontent.com/nytimes/covid-19-data/master/us-states.csv")
    statecases = covid %>% group_by(state) %>% slice(which.max(cases)) %>% slice(which(state %in% state.name))
    which(statecases$state %in% state.name)
    state.percent = data.frame(statecases$state, statecases$cases / state.pop$POPESTIMATE2019 * 100)
    names(state.percent)[names(state.percent) == "statecases.cases.state.pop.POPESTIMATE2019...100"] = "percent"
    
    
    percentpop = function(state){
      states = which(state.name == state)
      a = statecases[states, 4]
      b =  state.pop[states, 16]
      paste(round(a[[1]] / b, 2) * 100, "% of", state, "has tested positive for COVID-19")
    }
    
    
    percentpop(input$state0)
  })
  
  output$testing_site = renderText({
    
    get_geo_distance = function(long1, lat1, long2, lat2, units = "miles") {
      loadNamespace("purrr")
      loadNamespace("geosphere")
      longlat1 = purrr::map2(long1, lat1, function(x,y) c(x,y))
      longlat2 = purrr::map2(long2, lat2, function(x,y) c(x,y))
      distance_list = purrr::map2(longlat1, longlat2, function(x,y) geosphere::distHaversine(x, y))
      distance_m = sapply(distance_list, function(col) { col[1] })
      
      if (units == "km") {
        distance = distance_m / 1000.0;
      }
      else if (units == "miles") {
        distance = distance_m / 1609.344
      }
      else {
        distance = distance_m
      }
      distance
    }
    # https://www.latlong.net/
    
    lat = input$lat
    long = 	input$long
    lat_long = c(lat,long)
    
    testing_sites = data.frame(site = c("CRCE", "Illini Union", "State Farm Center", "SDRP", "Veterinary Medicine"),
                               long = c(40.104141, 40.10939235	, 40.0962421, 40.10409, 40.101877),
                               lat = c(-88.221538, -88.2272187093397, -88.2359287628109, -88.2394624, -88.219142))
    
    
    
    distances = map2_dbl(testing_sites$long, testing_sites$lat, ~get_geo_distance(.x, .y, lat_long[1], lat_long[2]))
    
    paste(testing_sites[which.min(distances), 1], "is the closest testing site to your address.")
  })
  
  
  url = a("Click Here", href= "https://stevemorse.org/jcal/latlon.php")
  output$tab <- renderUI({
    tagList("URL link:", url)
  })
  
  #County plot for Big Ten universities
  output$county_plot <- renderPlot({
    
    newDf = TotalCollege %>%
      select(c('Date', 'Tests', 'Country_Region', 'Confirmed' )) %>%
      na.omit() %>%
      filter((Date >= input$num_date[1]) & (Date <= input$num_date[2])) %>%
      filter(Country_Region == input$inputSchool)
    
    ggplot(data = newDf, mapping = aes(x = Date, y = Confirmed)) +
      geom_line(color="Blue",size=1.0) +
      labs(x = "Date", y ="Confirmed Cases")
    
  })
  
  #County plot for cases ACC, B12, PAC-12, and SEC universities
  output$county_NYTimes_plot <- renderPlot({
    
    newDf = NYTimes_US_Counties_Historical_Data %>%
      select(c('date', 'cases', 'county', 'deaths' )) %>%
      na.omit() %>%
      filter((date >= input$num_date[1]) & (date <= input$num_date[2])) %>%
      filter(county == input$inputSchool)
    
    ggplot(data = newDf, mapping = aes(x = date, y = cases)) +
      geom_line(color="Blue", size=1.0) +
      labs(x = "Date", y ="Confirmed Cases")
    
  })
  
  #County plot for new cases in Big Ten universities
  output$county_plot_new <- renderPlot({
    
    newDf = TotalCollege %>%
      select(c('Date', 'Tests', 'Country_Region', 'Confirmed' )) %>%
      na.omit() %>%
      filter((Date >= input$num_date[1]) & (Date <= input$num_date[2])) %>%
      filter(Country_Region == input$inputSchool)
    
    ggplot(data = newDf, mapping = aes(x = Date, y = Confirmed)) +
      geom_line(color="Red",size=1.0) +
      labs(x = "Date", y ="Confirmed Cases")
    
  })
  
  #County plot for new deaths in ACC, B12, PAC-12, and SEC universities
  output$county_ny_times_deaths_plot <- renderPlot({
    
    newDf = ny_times %>%
      select(c('date', 'cases', 'county', 'deaths', 'delta deaths')) %>%
      na.omit() %>%
      filter((date >= input$num_date[1]) & (date <= input$num_date[2])) %>%
      filter(county == input$inputSchool)
    
    ggplot(data = newDf, mapping = aes(x = date, y = `delta deaths`)) +
      geom_line(color="Red", size=1.0) +
      labs(x = "Date", y ="Change in Deaths")
    
  })
  
  ##Deleted extra bracket and parentheses causing missing comma error
  output$treemap = renderPlot({
    WHO_Global_Historical_Data$WHO_region <- as.factor(WHO_Global_Historical_Data$WHO_region)
    day_cul_cases <- WHO_Global_Historical_Data %>% filter(Date_reported == input$date)
    AFRO <- day_cul_cases %>% filter(WHO_region == "AFRO")
    AFRO_cases <- sum(AFRO$Cumulative_cases)
    AMRO <- day_cul_cases %>% filter(WHO_region == "AMRO")
    AMRO_cases <- sum(AMRO$Cumulative_cases)
    EMRO <- day_cul_cases %>% filter(WHO_region == "EMRO")
    EMRO_cases <- sum(EMRO$Cumulative_cases)
    EURO <- day_cul_cases %>% filter(WHO_region == "EURO")
    EURO_cases <- sum(EURO$Cumulative_cases)
    Other <- day_cul_cases %>% filter(WHO_region == "Other")
    Other_cases <- sum(Other$Cumulative_cases)
    SEARO <- day_cul_cases %>% filter(WHO_region == "SEARO")
    SEARO_cases <- sum(SEARO$Cumulative_cases)
    WPRO <- day_cul_cases %>% filter(WHO_region == "WPRO")
    WPRO_cases <- sum(WPRO$Cumulative_cases)
    WPRO <- day_cul_cases %>% filter(WHO_region == "WPRO")
    WPRO_cases <- sum(WPRO$Cumulative_cases)
    
    day_cumu_cases_region <-c(AFRO_cases, AMRO_cases,EMRO_cases, EURO_cases,Other_cases,SEARO_cases,WPRO_cases )
    group <- c("AFRO", "AMRO", "EMRO", "EURO", "Other", "SEARO", "WPRO" )
    data <- data.frame(group, day_cumu_cases_region)
    
    treemap(data,
            index="group",
            vSize="day_cumu_cases_region",
            type="index",
            title = "Treemap of Cumulative Cases Based on WHO Regions on a Specific Day")
  })
  #Used the death ratio to predict the death number with an input
  COVID_Tracking_Project_US_Historical_Data <- read_csv("https://covidtracking.com/data/download/national-history.csv")
  ui <- fluidPage(
    titlePanel("Potential Death Counts"),
    sidebarPanel(
      numericInput(inputId = "Cases",
                   label = "Choose a Number",
                   value = 100,
                   min = 0,
                   max = 100000,
                   step = 1
      ),
      textOutput("Predicted Death Counts"),
      checkboxInput("do1", "Plot New Confirmed Cases in Champaign_data", 
                    value = T)
    ),
    mainPanel(fluidRow(
      plotOutput(
        outputId = "plotgraph1",
        width  = "800px",
        height = "800px"
      )
    )
    )
  )
  
  totaldeath<-sum(COVID_Tracking_Project_US_Historical_Data$death,na.rm = TRUE)
  totalpos<-sum(COVID_Tracking_Project_US_Historical_Data$positive)
  
  server <- function(input, output){
    output$"Predicted Death Counts" <- renderText({
      paste( "Death: The predicted death number is :", as.character(as.numeric(input$Cases)*totaldeath/totalpos), "people")
    })
    pt1 <- reactive({
      input$do1
      if (input$do1) {
        return(
          plot(
            Champaign_data$new_cases,
            type = "l",
            col = "black",
            lwd = 3,
            ylim = range(pretty(Champaign_data$new_cases)),
            xlab = "Day",
            ylab = "New confirmed cases",
            main = "New confirmed cases in Champaign"
          )
        )
      } else {
        return(NULL)
      }
    })
    output$plotgraph1 <- renderPlot({
      pt1()
    })
  }
  
  #Created a Bar chart to see the death vs positive cases more directly
  totaldeath<-sum(COVID_Tracking_Project_US_Historical_Data$death,na.rm = TRUE)
  totalpos<-sum(COVID_Tracking_Project_US_Historical_Data$positive)
  
  barplot(height = c(totaldeath, totalpos), xlab = c("Death vs Positive Cases"), col=c("blue","red"), cex.names = c("Death","Positive"))
  
  #Added death rate to the NYTimes and Champaign data
  NYTimes_US_Historical_Data$death_rate <- NYTimes_US_Historical_Data$deaths/NYTimes_US_Historical_Data$cases
  NYTimes_US_States_Historical_Data$death_rate <- NYTimes_US_States_Historical_Data$deaths/NYTimes_US_States_Historical_Data$cases
  NYTimes_US_Counties_Historical_Data$death_rate <- NYTimes_US_Counties_Historical_Data$deaths/NYTimes_US_Counties_Historical_Data$cases
  Champaign_data$death_rate <- Champaign_data$deaths/Champaign_data$cases
  
  
}

shinyApp(ui = ui, server = server)

