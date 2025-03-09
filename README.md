# Data and code for: Pyrosome, _Pyrosoma atlanticum_: highlighting plankton as an important food source for coral reefs in Timor-Leste

Blooms of _Pyrosoma atlanticum_ were observed in late 2019 in Timor-Leste at two sites. Data on the occurence of pyrosomes globally was searched in the Global Biodiversity Information Facility using the _P. atlanticum_ taxonkey. The data search can be found [here](https://doi.org/10.15468/dl.vv3adq).

Chlorophyll-a data (Global Ocean Colour, Bio-Geo-Chemical, L3 daily product) was downloaded from the E.U. Copernicus Marine Service Information (CMEMS) Marine Data Store (MDS). doi: 10.48670/moi-00278.

Southern Oscillation Index (SOI), a measure of El Nino Southern Oscillation, was downloaded from the U.S. National Oceanographic and Atmospheric Administration [Climate Prediction Center](https://www.cpc.ncep.noaa.gov/data/indices/soi).

# Directions
The code provided the in scripts creates Figures 2 and 3 from the main text; Appendix S1 Figure S1; and the full Appendix S2 document. 

Download or clone the repository locally and open the `.Rproj` file. Create a `figures` directory in the project where the figures will be written too.

In the `scripts` directory, run:

- Figure 2-map.R
- Figure 3-chl.R
- AppendixS1-FigS1-histogram.R

In the `appendixS2` directory, run:

- AppendixS2-full.qmd

This file requires both R and python to run. The `references.bib` file contains the references formatted in APA style as in `apa.csl`. The `appendix-reference-doc.docx` is the MS Word template for the formatting of the ouput `.docx` file. The Word document was exported as a pdf for the final publication.

Note: The scripts use the native R pipe which will require R v4.0 or greater.
