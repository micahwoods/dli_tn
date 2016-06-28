These files make calculations or estimations of:

* R<sub>a</sub>, extraterrestrial solar radiation
* R<sub>s</sub>, global solar radiation
* daily light integral, DLI

First,  `dli_tn/nrcc_nearby_stations.R` downloads R<sub>s</sub> data from selected stations and the Hargreaves equation estimate of R<sub>s</sub> is calculated for comparison.

The  `dli_tn/tn_four_cities.R` file works with temperature data from Memphis, Nashville, Chattanooga, and Knoxville, using the Hargreaves equation to make an estimate of R<sub>s</sub> for each of those locations.

A further description of these calculations is posted at <http://www.seminar.asianturfgrass.com/tn_dli_description.html>.

In June 2016 I added the `r/20160628_starkville.R` script to calculate the estimated DLI for Starkville, MS.