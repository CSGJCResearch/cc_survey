# Confined and Costly Survey

In 2018, The Council of State Governments (CSG) Justice Center conducted a survey to corrections departments in all 50 states to collect data on the impact of supervision revocations of supervision on prison populations. The Confined and Costly study found that probation and parole revocations made up as much as 45 percent of state prison admissions nationwide, with wide variation across states. With continued support from the Correctional Leaders Association and Arnold Ventures, we will be collecting these numbers annually through 2022.  


## Repository Structure

    |-- code    
      |-- analysis_plan.R                    # plots for JS and comms team
      |-- automated_clean.R                  # imports/cleans survey data (last update 4/20/21)
      |-- automated_costs.R                  # automated costs (old/archive)
      |-- automated_plots.R                  # automated plots (old/archive)
      |-- automated_tables.R                 # automated tables(old/archive)
      |-- automated_report.Rmd               # automated state reports for states that have submitted
      |-- cc_story.R                         # national estimates, cc story (last update 4/20/21)
      |-- cc_analysis.R                      # uses UCR, census, and cc data to find correlations adm, pop, crime changes
    |-- data  
      |-- 50-State Revocation Survey_October 27, 2020_12.25.xlsx   # raw survey responses   
      |-- Cost Per Day For Calculation.xlsx                        # costs by state  
      |-- Data for web team 2021 v1.xlsx                           # final data  
    |-- shared_data  
    |-- survey_info  
      |-- 50 State Revocation Survey - FINAL.pdf                   # pdf of administered survey  
      |-- CSGJC_SurveyUpdate_Nov2020.pptx                          # summary statistics presented  
    |-- technical_report 
