# HEAT LOAD CHARACTERISATION

Side project by Roberto Garay Martinez.  
https://github.com/robgaray/  
https://robertogaray.com/

Overall inspection and modelling of head loads in buildings.

# Inspiration
Over the years, I have worked together with several colleagues and students in experimental heat load characterisation. With a variety of approaches and datasets. This script tries to put some of this knowledge in a more coherent way.

Obviously, this could not have been possible without my previous works with (in chronological order) Beñat Arregi, Mikel Lumbreras, Antonio Garrido, Markel Eguizabal, Ivan Flores, Olaia Eguiarte and Alaeddine Hajri. I have probably discussed these methods with several other researchers in the course of the last 4-5 years, but I sincerely can not remember with whom.

To all of them, thank you.

# Context evolution

The method proposed here takes its roots im methods such as PRISM (1970-80s)and ASHRAE changepoint (1990s). I initiated these kind of analysis around 2015. Most of the dataset I use now are less than 5 years old.

In this period of time, the available data has increased exponentially, both in terms of availability, resolution and reliability.


# Data Sources
Based on data provided by GREN TARTU & used in several research papers & seminars.  
**Papers**  
https://doi.org/10.1016/j.energy.2021.122318

http://dx.doi.org/10.2139/ssrn.4186449

https://doi.org/10.1016/j.egyr.2022.10.212

https://doi.org/10.3390/environsciproc2021011033

https://doi.org/10.23919/SpliTech52315.2021.9566420

http://doi.org/10.1088/1755-1315/588/3/032007  

**Seminars**  
https://github.com/robgaray/SMACCS_Building_Heat_Load_Analysis.git

https://github.com/robgaray/EESIA_Analisis_Consumo_2021_Publico.git

Climate data is taken from a a publicly available source at the University of Tartu.  
https://meteo.physic.ut.ee/

Special thanks to GREN TARTU for providing the data, and the University of Tartu for making their wether data available to the world.

# Process
Data from ONE building and local climate is used and the following process is performed.

- Data Inspection

- Development of a changepoint model

- Identification of outliers. Based on the residuals of changepoint model

- Data imputation for 1h & 5h gaps. Based on boundary of the data  on the same day in the preceding and following week

- retraining  of changepoint model

- Statistics

# Future steps
- Advanced imputation processes to repair larger voids.
	- The current version just makes linear interpollations/splines based on very-close contextual data. In this case, preceding and following data point, as well as information over the same time period of the day in the previous and following week. 8 data points in total.
	- Further dynamic analysis may be required for larger data voids. 
- New changepoint approach. Common slope, but adaptable offset.
	- The current version calibrates 7 x 24 individual changepoint models. This results in a very large number of paramters (in the range of 600). Given the available data in the dataset (~8000 observations), there is a high risk of overfitting.
	- A potential alternative may be to develop a single changepoint model, with 7 x 24 offsets that account for specific building usage patterns. By doing so, the number of parameters will be in the range of ~200.
	- Alterntively, typical pattern clusterization may result in further reducing the number of parameters by ~50% to ~300 or ~100 depending on which of the preceding approaches are used.  
- Short-term forecasting
	- This method delivers quite stable models, but its prediction does not consider the short-term history of the building (i.e. it comes from a very cold night,...) so its performance for forecasting could be improved.
