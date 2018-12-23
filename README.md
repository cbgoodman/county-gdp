# County-Level Gross Domestic Product
The **R** script `county-gdp-simple.r` reads `bea_county_gdp.csv` to create the first image below of log per capita GDP and log population. County-level GDP data is from the [U.S. Bureau of Economic Analysis](https://www.bea.gov/data/gdp/gdp-county) and county population estimation are from [U.S. Census Bureau](https://www.census.gov/programs-surveys/popest.html).

![County-level GDP & Population](gdp-2015.png)
The **R** script `county-gdp.r` reads `bea_county_gdp.csv` to create the image below of log per capita GDP and log population. Additional data from the
[USDA Economic Research Service](https://www.ers.usda.gov/). The [county typology codes](https://www.ers.usda.gov/data-products/county-typology-codes/) extends the first scatterplot to examine how industry specialization and metropolitan status interacts with GDP and population.
![County-level GDP & Population, Grouped by Metro Status & Industry Specialization](gdp-2015-grouped.png)
Lastly, the **R** script `county-gdp.r` also reads `bea_county_gdp.csv` to create a chloropleth map of per capita GDP by county using the Urban Institute's
[urbnmapr](https://github.com/UrbanInstitute/urbnmapr) wrapper for `ggplo2`.
![County-level GDP, mapped](county-gdp-15.png)
