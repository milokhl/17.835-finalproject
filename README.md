# Impact of Food Security on Political Stability
By AJ Root, Jen McDermott, Talia Blum, and Milo Knowles.

When running ```scripts/process_data.R```, make sure you set the working directory to that of the script!

## Datasets
- ```data/original/``` contains the raw data sources (a mix of .xlsx and .csv).
- ```data/processed/``` contains individual sources that have had some processing done. Mainly, datasets were reorganized to have country-year rows.
- ```data/final/``` contains post-processed data. ```data_matched_all.csv``` has country-year rows that are matched across all 3 data sources (WDI, WGI, terrorism)

## Variables
For a description of WDI variables, see ```data/original/wdi_info.csv```.

- EG.ELC.ACCS.UR.ZS	= Access to electricity, urban (% of urban population)
- EG.ELC.ACCS.RU.ZS	= Access to electricity, rural (% of rural population)
- SN.ITK.DFCT	= Depth of the food deficit (kilocalories per person per day)
- TX.VAL.FOOD.ZS.UN	= Food exports (% of merchandise exports)
- TM.VAL.FOOD.ZS.UN	= Food imports (% of merchandise imports)
- AG.PRD.FOOD.XD = Food production index (2004-2006 = 100)
- NV.MNF.FBTO.ZS.UN	= Food, beverages and tobacco (% of value added in manufacturing)
- SH.H2O.SAFE.ZS = Improved water source (% of population with access)
- SH.H2O.SAFE.RU.ZS	= Improved water source, rural (% of rural population with access)
- SH.H2O.SAFE.UR.ZS	= Improved water source, urban (% of urban population with access)
- AG.PRD.LVSK.XD = Livestock production index (2004-2006 = 100)
- SH.H2O.BASW.ZS = People using at least basic drinking water services (% of population)
- SH.H2O.BASW.RU.ZS	= People using at least basic drinking water services, rural (% of rural population)
- SH.H2O.BASW.UR.ZS	= People using at least basic drinking water services, urban (% of urban population)
- SH.H2O.SMDW.ZS = People using safely managed drinking water services (% of population)
- SH.H2O.SMDW.RU.ZS	= People using safely managed drinking water services, rural (% of rural population)
- SH.H2O.SMDW.UR.ZS	= People using safely managed drinking water services, urban (% of urban population)
- EN.POP.DNST	= Population density (people per sq. km of land area)
- SP.POP.GROW	= Population growth (annual %)
- SP.POP.TOTL	= Population, total
- SN.ITK.DEFC.ZS = Prevalence of undernourishment (% of population)
- SM.POP.REFG	= Refugee population by country or territory of asylum
- SM.POP.REFG.OR = Refugee population by country or territory of origin
- ER.GDP.FWTL.M3.KD = Water productivity, total (constant 2010 US$ GDP per cubic meter of total freshwater withdrawal)

For a dataset mapping countries to country codes, see ```data/processed/country_codes.csv```.
