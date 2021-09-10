# More Community, Less Confinement

In 2018, The Council of State Governments (CSG) Justice Center sent out an annual survey to corrections departments in all 50 states to collect data on the impact of supervision violations on prison admissions and populations. The study found that in 2017, probation and parole violations made up as much as 45 percent of state prison admissions nationwide, with wide variation across states. With continued partnership with the Correctional Leaders Association and support from Arnold Ventures, CSG will be collecting and reporting these numbers annually through 2022. 

In January 2021, the CSG Justice Center sent the [More Community, Less Confinement](https://csgjusticecenter.org/publications/more-community-less-confinement/) survey (created in Qualtrics software) to the departments of corrections of all 50 states. This additional survey was an effort to obtain more 2020 data, given the special circumstances that the pandemic presented.  

Respondents were asked to provide aggregate counts of the number of people admitted to or in prison on a given day across numerous categories for calendar years 2018, 2019, and on December 31, for 2020. Respondents were given the option to provide the data according to fiscal year if that is the standard reporting method for the state and indicate if so.  

The categories requested include: 

* *Total admissions/population*: The number of people admitted to or in prison.  

* *Total violation admissions/population*: The number of people admitted to or in prison because of a violation of supervision conditions.  

* *Total probation violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of probation.  

* *New offense probation violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of probation.  

* *Technical probation violation admissions/population*: The number of people admitted to or in prison because of a technical violation of probation.  

* *Total parole violation admissions/population*: The number of people admitted to or in prison because of a violation of probation. This includes people admitted to or in prison because of new offense violations and technical violations of parole.  

* *New offense parole violation admissions/population*: The number of people admitted to or in prison because of a new offense violation of parole.  

* *Technical parole violation admissions/population*: The number of people admitted to or in prison because of a technical violation of parole.  

* *Cost*: Average operating cost per person per day across all prison facilities.   

Two departments declined to participate (New Mexico and New York), and Tennessee reported their available data for this reporting period would not be available until the end of their fiscal year. Where available, data provided in the survey was supplemented with publicly available data or data that we routinely collect for other purposes. We note on each [state’s page](https://csgjusticecenter.org/publications/more-community-less-confinement/state-reports/) when we used publicly available data.  

### Caution on Comparing States: 

States define admissions and populations due to supervision violations differently:   

* Some states reported that they include quick dips and supervision sanctions in their admissions numbers, while others do not.  
* Some states reported that they classify new felony offenses as technical violations, while others do not.  
* States do not necessarily have the infrastructure or institutional memory to pull data in a consistent way over time.  
* When provided the opportunity, 19 states revised data that they submitted in 2018.  
* States may continue to revise their survey responses during future validation processes.  

For more information about [More Community, Less Confinement](https://csgjusticecenter.org/publications/more-community-less-confinement/), visit our website or [OSF project registration](https://osf.io/f74d6/).   

## Repository Structure
    
    |-- clean.R               # imports and cleans survey data
    |-- multiple_imputation.R # imputes missing values, calculates national estimates and cost calculations

    |-- data
      |-- Data for web team 2021 v13.xlsx # state-level admissions, population, and cost data - 2018, 2019, 2020

## Multiple Imputation Methodology

To develop national level estimates from the state-by-state figures, the CSG Justice Center used multiple imputation to address missing state-level data. The decision to impute these missing values was decided upon in order to prevent excluding states' reported populations from the national estimates. Because data were not missing at random, multiple imputation was used to allow for the uncertainty about the missing data by accounting for variability due to unknown values.

Multiple imputation uses random draws from the conditional distribution of the target variable given other viarables. The imputation was repeated M = 5 times resulting in completed datasets that were summarized into national population estimates. These were then analyzed with a standard regression approach, and the parameter estimates were averaged over the multiple imputations to obtain the best estimate rather than using a single imputation. Finally, to obtain 95% confidence intervals, the variance-covariance matrix of the averaged parameter estimates adjusted for variability due to imputation was estimated (Harrell, 2010).

### References

Harrell, Jr., Frank E. Regression modeling strategies: with applications to linear models, logistic regression, and survival analysis. 2010.

## Collaborations
[Supervision Success: Recidiviz Cost Calculator](https://github.com/Recidiviz/supervision-success-component)

[Justice Counts: Recidiviz Components](https://github.com/Recidiviz/justice-counts-components)

[The Council of State Governments GitHub](https://github.com/csg-org)

## License
[CSG Justice Center](https://csgjusticecenter.org/)