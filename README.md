# vacant-land-viability

### Overview ###
Welcome to the Dallas County Vacant Lots Shiny App! This interactive tool is designed to explore and analyze vacant lots within Dallas County, focusing on those owned by public entities or faith-based organizations. Our goal is to identify properties with the highest potential for conversion into affordable housing, leveraging their tax benefits and potential for community collaboration.

### Project Focus ###
The app specifically targets vacant land owned by public entities and faith-based organizations. 

### Data ###
Data is sourced from the Dalla County Appraisal District, at www.dallascad.org. The datasets used are 2024 Parcels from deeds filed with Dallas County Clerk, and 2024 Data Files with Proposed Values (now called 2024 Certified Data Files with Supplemental Changes).

### Interactive Map: ###
The interactive map allows users to explore vacant lots based on ownership types, nearby amenities, and opportunity levels. Clicking on the circle marker will pull up the type of ownership this lot has, the name of the owner, the street address, opportunity level, and what features are nearby. Features included resources such as schools, parks, clinics, public transportation, libraries, rec centers, and grocery stores, and environmental hazards. A feature is determined to be nearby if they are within walking distance, or a quarter mile away. Users can filter lots based on specific needs and download the data for further analysis.

### Opportunity Analysis: ###
Lots are categorized into high, moderate, and low opportunity levels, helping to prioritize areas with the greatest potential for community development. High opportunity lots have no environmental hazards and five or more nearby resources. Moderate opportunity lots have one to four resources and no environmental hazards. Low opportunity lots have environmental hazards or lack nearby resources.

### Apartment Estimation Calculator: ###
This feature provides a basic estimate of the number of apartments that can be built on a selected lot, considering variables such as the number of stories and desired square footage per unit. Though not a comprehensive estimate, it aids in preliminary planning and exploratory purposes.

### Geographical Comparison: ### 
Users can analyze vacant lots based on zip codes, census tracts, or city council districts. This feature enables identification of areas with the highest opportunities and facilitates comparison across different regions. Users can also download the data for further analysis.

### Future Additions ### 
One future enhancement could be the addition of map layers to provide more information. For example, incorporating a slider tool to switch between two map layers: the current map and a satellite view of Dallas County with lot boundaries. This feature would allow users to visualize the actual appearance of vacant lots.

### Packages Needed ###
library(dplyr)
library(readr)
library(sf)
library(stringr)
library(tigris)
library(cpaltemplates)
library(leaflet)
library(shiny)
library(plotly)
library(tidyr)
library(DT)
library(writexl)
