#  London Output Area Classification 2021

The 2021 London Output Area Classification (LOAC) project updates a popular geodemographic classification for London by utilizing 2021 Census data to better represent the city's unique residential characteristics.

The project was developed in partnership with the Greater London Authority and employs a clustering algorithm which categorises London's Output Areas into detailed Groups. The LOAC project engaged a diverse advisory group in its methodology, ensuring that the classification reflects varied needs and perspectives. The entire project framework, including the clustering codes, is available on this repo, enabling reproducibility, but also providing a valuable resource for researchers and policymakers aiming to apply similar geodemographic analyses in other contexts.

## Content and Files

* data \
	* lookup \
		*LOAC_2011 - LOAC21 Lookup 
		* Variables_LOAC - Inpt variables
	* Census\ - extra census tables
	* LOAC_Group.parquet - Group lookup
	* LOAC_SuperGroup.parquet - Supergroup lookup
	* OA_Input_london.parquet - input data
* code \ - R and Python Code
* descriptions \
	* Borough_Profiles.pdf - LOAC profile and maps
	* LOAC_2021_Descriptions.pdf - Supergroup and Group Descriptions

## Paper
A journal article was written to accompany LOAC, which can be found here:

Singleton AD, Longley PA (2024) Classifying and mapping residential structure through the London Output Area Classification. Environment and Planning B: Urban Analytics and City Science. https://doi.org/10.1177/23998083241242913 