# City selection

This document details city selection and linkage for the MCC-EUcityTRM project.

## Eurostat cities

### Description

The main city list is taken from the cities and greater cities dataset of eurostat, also called Urban Audit (https://ec.europa.eu/eurostat/web/cities/background). This collection of consistent datasets includes almost a thousand cities with three nested spatial levels:
- City: administrative unit of at least 50 000 inhabitants (n = 936)
- Functional Urban Area: the region of influence of the city including the communting zone (n = 723)
- Greater City: for large metropolitan areas, approximation of the urban center when it stretches beyond the administrative boundaries (n = 60)

### Selection

We select the Greater City level when available, or the City level otherwise. The Functional Urban Area is completely discarded as it represents massive geographical scales with potentially important heterogeneity of metapredictors. In addition, we keep the latest version of the city when several versions are found.

In addition, we discard a number of cities for specific cases:
- Linkage with NUTS3 level: in several instance, many cities are linked to the same NUTS3 level data. In this case we select either the greater city among them, or the largest city, and discard the rest of them. It results in 236 discarded cities that are often smaller cities part of a large metropolitan area. 
- London boroughs: We discard all london boroughs to keep only greater London (20 discarded boroughs). 
- Overseas cities: We discard all cities not on the European continent but administatively part of european countries including Iceland (1 city), Spain (9 cities), Portugal (2 cities) and France (5 cities).

The final city count is n = 670

## Linkage with other data sources

### NUTS

The Nomenclature for Territorial Unit Statistics (NUTS) provides statistics at nested regional levels in Europe. There are three levels, with level 0 being the country. The geographical extent of each level is heavily country dependent but usually corresponds to the following rules:
- Level 1: widest administrative regions of each country.
- Level 2: intermediate administrative regions. Sometimes equal to level 1 (e.g. in France).
- Level 3: Finest administrative level. Usually correspond to greater cities cities in urban areas but also corresponds to local authority in more rural areas. 

Eurostat provides a lookup table between Urban Audit and NUTS3 level. In some cases, several cities are linked to the same NUTS3 unit, usually when they are part of a larger metropolitan area. In this case we select the largest city (see selection above). In the current dataset, all 670 cities are linked to different NUTS3 units.

### MCC cities

We link the cities from Urban Audit to the cities available in the MCC dataset. For each MCC city we find the Urban Audit cities whose boundaries intersect a buffer of 10 km around the MCC location. In the case of several matches, the Urban Audit city with the largest intersection is kept.
In addition, manual work is done to clean the linkage, including removing small region locations (Ireland and the Bohemia region) and correcting mismatches (e.g. due to UK conurbations).
Among the 670 Urban Audit cities, 173 are linked to MCC locations.

### Urban Center Database

we also link Urban Audit to the Global Human Settlement Layer's Urban Centre Database (UCD). Each urban audit city is linked to the UCD city with the largest intersection between respective boundaries.
Among the 670 selected cities, 562 are linked to UCD.

## Metapredictor extraction

Metapredictors are mainly extracted from two sources: Eurostat's regional dataset (NUTS) and the UCD. Note that no metapredictor has been taken directly from the Urban Audit dataset as there is always more availability in the regional dataset (NUTS).

### NUTS metapredictors

The metapredictors are extracted at the at finest regional level available from Eurostat. 

- Population structure (NUTS3): Proportion of population for each 5-year age group.
- Total population (NUTS3)
- Population density (NUTS3): per km2
- Life expectancy (NUTS2): Life expectancy at birth
- GDP per capita (NUTS3)
- Education level (NUTS2): Proportion of active population (25-64 years old) with ISCED level >= 5 (higher education)
- Unemployment rate (NUTS2): among active population (20-64 years old) for all education levels
- Material deprivation (NUTS2): Proportion of population under severe material deprivation condition
- Bed rates (NUTS2): Number of hospital beds / inhabitants
- Urban area (NUTS2): Proportion of land covered by artifical surfaces
- Blue area (NUTS2): Proportion of land covered by water
- Green area (NUTS2): Proportion of land covered by one of: woodland, cropland, shrubland, grassland
- Cooling degree days (NUTS3): Summer heat indicator. Sum of daily degrees above 21C for days exceeding 24C
- Cooling degree days (NUTS3): Winter cold indicator. Sum of daily degrees below 18C for days below 15C
- Social isolation (NUTS3): Proportion of household consisting of divorced, single or windowed persons, according to the 2011 survey.
- Mountain type (NUTS3): Mountain typology. 1: > 50% of population live in mountain areas, 2: > 50% of surface in mountain area, 3: > 50% of population live and 50% of surface in mountain areas, 4: Non-mountain region 
- Urban typology (NUTS3): 1: Predominantly urban regions, 2: Intermediate regions, 3: Predominantly rural regions
- Coastal typology (NUTS3): 1: Sea border, 2: > 50% of the population within 50 km of coastline, 3: Non coastal
- Death rate (NUTS3): Total number of deaths for each 5-year group

### UCD metapredictors

In addition, a few metapredictors not found in the NUTS database were gathered in the UCD

- Average temperature in 2014: in C
- Greeness in 2014: as measured by the NDVI
- Particulate matter in 2014: total PM2.5 concentration in 2014