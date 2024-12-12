# PAM-analysis-status

 Code to create reproducible figure summarizing PAM data collection and analysis effort.

 INPUTS: deployment_summary.csv and analysis_status.xlsx, read from CetaceanOPPNoise_2/PAM_metadata
 
 OUTPUT: Summary figure

NOTES: Summary figure will only include deployments which are listed in the analysis_status.xlsx sheet AND have valid data for baleen and/or beaked whale analysis. Number of days is calculated from the in-water start and end dates and does not account for missing data occuring within a deployment.
