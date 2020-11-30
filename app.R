# Assign 4 Shiny
# Name: Brandon Wong
# Date: November 27, 2020
# Class: INFO 201

library("shiny")              # done for each relevant script
library("plotly")             # in each relevant script

library("dplyr")
library("ggplot2")
library("tidyverse")


# Load in data from Github
owid_raw_data <- read.csv("https://raw.githubusercontent.com/owid/co2-data/master/owid-co2-data.csv", 
                          stringsAsFactors = FALSE, header=TRUE)



# =========================
# Functions 

# Function: create a df of top 10 values based on the input variable
create_top_10 <- function(year_choice, by_var) {
  owid_raw_data %>% 
    filter(iso_code > 0) %>%        # filter out regions
    filter(country != "World") %>%  # filer out World
    filter(year == year_choice) %>%   # find latest year
    arrange(-{{by_var}}) %>%          # arrange from high to low
    top_n(10, {{by_var}})      # take top 10
}

# Function: sum an input variable from all countries
# Note: some data wrangling was needed to filter out regional IDs
# The regional IDs did not have 'iso_code' 
sum_all_var <- function(year_choice, by_var) {
  owid_raw_data %>% 
    filter(iso_code > 0) %>%        # filter out regions
    filter(country != "World") %>%  # filer out World
    filter(year == year_choice) %>% 
    select({{by_var}}) %>% 
    summarize(sum_var = sum({{by_var}}, na.rm = TRUE)) %>% 
    pull(sum_var)
}

# Function: given a data frame, sum up an input variable
sum_df_var <- function(df, by_var) {
  df %>% 
    select({{by_var}}) %>% 
    summarize(sum_var = sum({{by_var}}, na.rm = TRUE)) %>% 
    pull(sum_var)
}


# =======================
# Values

# Set of Top 10 CO2 emitting countries 
top_10_co2 <- create_top_10(2018, co2)

# Top 10 country names with the highest CO2 emissions 
top10_co2_country <- top_10_co2[, "country"]

# Latest year of the data set
latest_year <- owid_raw_data %>% 
  filter(iso_code > 0) %>%        # filter out regions
  filter(country != "World") %>%  # filer out World
  filter(year == max(year)) %>%   # find latest year
  filter(country == "United States") %>% 
  pull(year)


# Total co2 emissions from the Top 10 country set
top10_total_co2 <- top_10_co2 %>% 
  select(co2) %>% 
  summarize(sum_co2 = sum(co2, na.rm = TRUE)) %>% 
  pull(sum_co2) %>% 
  round(.,0)

# Total yearly CO2 emissions change compared to data from ten years 
# ago from the Top 10 countries
top10_co2_10yrs_ago <- owid_raw_data %>% 
  filter(iso_code > 0) %>%        # filter out regions
  filter(country != "World") %>%  # filer out World
  filter(year == top_10_co2[1, "year"] - 10)  %>% # 10 years back
  # <below> filters a data frame from a vector of strings
  filter(grepl(paste(top10_co2_country, collapse="|"), country)) %>% 
  select(co2) %>% 
  summarize(sum_co2 = sum(co2, na.rm = TRUE)) %>% 
  pull(sum_co2) %>% 
  round(.,0)

# Percent increase of CO2 emissions in last 10 years
co2_10yr_perc <- signif(((top10_total_co2 / top10_co2_10yrs_ago)-1)*100, 
                        digits = 3)

# Percentage of the Top 5 CO2 producing emissions countries compared to
# the next 5 (spots 6-10) CO2 producing countries
top5_total_co2 <- top_10_co2 %>% 
  top_n(5, co2) %>% 
  select(co2) %>% 
  summarize(top5 = sum(co2, na.rm = TRUE)) %>% 
  pull(top5)
top5_comparison <- signif((top5_total_co2 / top10_total_co2)*100, digits = 3)

# Df of all countries that reported non-zero CO2 emissions in 2018
all_co2_2018 <- owid_raw_data %>% 
  filter(iso_code > 0) %>%        # filter out regions
  filter(country != "World") %>%  # filer out World
  filter(year == 2018) %>% 
  select(country, co2)

# Number of countries that reported non-zero CO2 emissions in 2018
count_nonzero_co2_2018 <- nrow(all_co2_2018)


# =========================
# Plotly CO2 pie chart

plot_co2_piechart <- function(by_yr) {
  
  # create df for year requested
  data_temp <- create_top_10(by_yr, co2) %>% 
    select(country, co2)
  
  # Total <variable> of all countries in year requested
  total_var_all <- sum_all_var(by_yr, co2)
  
  # Total <variable> all the countries minus the top 10 
  # (i.e., the rest of the world)
  other_var <- total_var_all - sum_df_var(data_temp, co2)
  
  # Add the summary rest of the world emissions to the data frame using 'rbind' 
  # This appends one row for 'All Other Countries' to the data frame 
  Other.var <- data.frame("All Other Countries", other_var)  # build df row
  names(Other.var) <- data.frame("country", "co2")       # build df header
  data_temp2 <- rbind(data_temp, Other.var)              # bind dfs
  
  fig <- plot_ly(data_temp2, labels = ~country, values = ~co2, type = 'pie')
  fig <- fig %>% layout(title = 'Top 10 countries by CO2 emissions')
  
  fig
}


# ===========================
# Plotly population pie chart

plot_pop_piechart <- function(by_yr) {
  
  # create df for year requested
  data_temp <- create_top_10(by_yr, population) %>% 
    select(country, population)
  
  # Total <variable> of all countries in year requested
  total_var_all <- sum_all_var(by_yr, population)
  
  # Total <variable> all the countries minus the top 10 
  # (i.e., the rest of the world)
  other_var <- total_var_all - sum_df_var(data_temp, population)
  
  # Add the summary rest of the world emissions to the data frame using 'rbind' 
  # This appends one row for 'All Other Countries' to the data frame 
  Other.var <- data.frame("All Other Countries", other_var)  # build df row
  names(Other.var) <- data.frame("country", "population")       # build df header
  data_temp2 <- rbind(data_temp, Other.var)              # bind dfs
  
  fig <- plot_ly(data_temp2, labels = ~country, values = ~population, type = 'pie')
  fig <- fig %>% layout(title = 'Top 10 countries by population')
  
  fig
}


# ======================
# UI code

# Define the first page content; uses `tabPanel()` and `sidebarLayout()` 
# layout functions together (as an example)
page_one <- tabPanel(
  "Intro tab", # label for the tab in the navbar 
  titlePanel("Explore trends in CO2 emissions"), # show with a displayed title
  
  "Climate change is a real concern for the world. CO2 emissions are a huge",
  "contributer to greenhouse gases in the Earth's atmosphere. The production",
  "of CO2 emissions is regionalized and the majority of it comes from the",
  "world's superpowers. This analysis is meant to show which countires have",
  "produced a majority of the world's CO2 emissions and how that quantity",
  "compares to the rest of the world over time. The data used in this analysis",
  "was compiled by ",
  a("Our World In Data", href="https://ourworldindata.org/co2-and-other-greenhouse-gas-emissions"), 
  ".",
  
  br(), br(),
  
  p(strong("Relevant values from the dataset")),
  
  p("The latest year of the data in the source was ", latest_year),
  
  p("There were ", count_nonzero_co2_2018, " countries that reported CO2 ",
    "emissions in ", latest_year),
  
  p("In", latest_year, "the CO2 emissions generated by the top ten highest",
    "producing countries amounted to", top10_total_co2, "million tonnes."),
  
  p("The amount of CO2 emissions generated by the top ten countries has",
    "increased by ", co2_10yr_perc, " percent over the last 10 years."),
  
  p("It is actually the top ", em("five"), "countries that are producing",
    "most of the CO2 emissions each year. The top five countries produce",
    top5_comparison, " percent of CO2 emissions from the top ten list.")
  
)


# Define content for the second page 
page_two <- tabPanel(
  "Visual tab", # label for the tab in the navbar
  # ...more content would go here... 
  # This content uses a sidebar layout
  sidebarLayout( sidebarPanel(
    
    sliderInput(
      inputId = "yr_chosen", # key this value will be assigned to
      label = "Year Selection", # label to display alongside the slider
      min = 1970,
      max = 2018,
      value = 2018,   # starting value
      sep = ""        # no commas in year
    ), # close slideInput
    
    radioButtons(inputId = "rb_chosen", label = "Variable",
                 choices = list("Emissions" = 1, "Population" = 2)),
  ), # close sidebarLayout
  
  mainPanel(
    h3("Our World Data"),
    br(), br(),
    plotlyOutput(outputId = "pie_plot"),
    br(), br(),
    
    br("I chose to focus on the countries that generated the top ten most",
       "CO2 emissions per year to show that the world super powers' CO2",
       "output far exceeded that being generated by other countries.",
       "I thought the pie chart was visually an effective means to show",
       "the top 3-4 countries produce over half the emissions relative ",
       "to the rest of the world."),
    p("The Year slider effetively shows how China has moved to the",
      "top spot in yearly CO2 emissions. In 2018, China nearly matched",
      "the CO2 production of the lowest 202 other countries combined.")
    
  ) )
)


# Pass each page to a multi-page layout (`navbarPage`) 
my_ui <- navbarPage(
  "My Application", # application title
  page_one,   # include the first page content
  page_two   # include the second page content
)


# =================================
# Server code

my_server <- function(input, output) {
  output$pie_plot <- renderPlotly({
    if(input$rb_chosen == 1)  # radio button
    {return(plot_co2_piechart(input$yr_chosen))} # co2 pie
    else{return(plot_pop_piechart(input$yr_chosen))} # population pie
  })
}



# =================================
# Start running the application
shinyApp(ui = my_ui, server = my_server)

shinyApp(ui = my_ui, server = my_server)