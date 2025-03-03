#--------------------------------------

# Script name: analysis_status.R

# Purpose: Create figure to visualize current status of PAM data collection & analysis effort per year

# NOTES:
# 

# Author: Joy Stanistreet

# Date created: 2024-12-06

#--------------------------------------

# Load packages

pacman::p_load(tidyverse,
               here,
               readxl)

#--------------------------------------

# Compile recording effort summary

# path to PAM metadata folder
metadata_folder <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata'

# load deployment summary
depl_metadata <- read_csv(here(metadata_folder, 'deployment_summary.csv')) %>% 

  mutate(deployment = Deployment) %>% 
  mutate(rec_start = as_date(`In-water_start`),
          rec_end = as_date(`In-water_end`))

# read in analysis status xlsx
analysis_status <- read_excel(here(metadata_folder, 'analysis_status', 'analysis_status.xlsx'))

#join with metadata
analysis_status_metadata<-analysis_status %>% 
  
  left_join(depl_metadata, by = 'deployment') %>% 
  
  # remove deployments with no valid dataset
  filter(!(is.na(rec_start))) %>% 
  
  # convert to real NAs in analysis status columns
  mutate(analysis_narw = as.numeric(analysis_narw),
         analysis_baleen = as.numeric(analysis_baleen),
         analysis_beaked = as.numeric(analysis_beaked)) %>% 
  
  # calculate baleen recording days per deployment
  mutate(rec_days_baleen = as.numeric(as_date(rec_end) - as_date(rec_start) + 1)*record_baleen) %>%
  
  # calculate beaked recording days per deployment
  mutate(rec_days_beaked = as.numeric(as_date(rec_end) - as_date(rec_start) + 1)*record_beaked) %>%
  
  # narw analysis days
  mutate(narw_days = analysis_narw*rec_days_baleen) %>% 
  
  # baleen whale analysis days
  mutate(baleen_days = analysis_baleen*rec_days_baleen) %>% 
  
  # beaked whale analysis days
  mutate(beaked_days = analysis_beaked*rec_days_beaked)
  
# summarize by year
status_summary<- analysis_status_metadata %>% 
  group_by(Year) %>% 
  summarize(rec_days_baleen_year = sum(rec_days_baleen),
         rec_days_beaked_year = sum(rec_days_beaked),
         narw_days_year = sum(narw_days, na.rm = T),
         baleen_days_year = sum(baleen_days, na.rm = T),
         beaked_days_year = sum(beaked_days, na.rm = T)) %>% 
  mutate(year = as_factor(Year)) %>% 
  ungroup()

# summarize number of datasets available/analyzed
status_summary_datasets<- analysis_status_metadata %>% 
  mutate(total_datasets_baleen = sum(record_baleen, na.rm = T),
         total_datasets_beaked = sum(record_beaked, na.rm = T),
         n_analyzed_narw = sum(analysis_narw, na.rm = T),
         n_analyzed_baleen = sum(analysis_baleen, na.rm = T),
         n_analyzed_beaked = sum(analysis_beaked, na.rm = T))

#--------------------------------------

# Set up data for figure

# baleen whale data: set up for plot
baleen_summary<-status_summary %>% 
  transmute(year, n_days = rec_days_baleen_year, analyzed = baleen_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'baleen')

# narw data: set up for plot
narw_summary<-status_summary %>% 
  transmute(year, n_days = rec_days_baleen_year, analyzed = narw_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'narw')

# beaked whale data: set up for plot
beaked_summary<-status_summary %>% 
  transmute(year, n_days = rec_days_beaked_year, analyzed = beaked_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'beaked')

# put it all back together

all_spp <- baleen_summary %>% 
  full_join(narw_summary) %>% 
  full_join(beaked_summary) %>% 
  mutate(species = factor(species, levels = c('narw','baleen', 'beaked')))

#--------------------------------------

# Create figure

theme_set(theme_bw())

figure<-ggplot() + 
  
  facet_wrap(~species, ncol = 1,
             labeller = as_labeller(c(`narw` = "North Atlantic right whales",
                                      `baleen` = "Blue, fin, humpback, minke, & sei whales",
                                      `beaked` = "Beaked whales"))) +
  
  geom_bar(data = all_spp, 
           aes(y = count, x = year, fill = effort),
           stat = 'identity',
           position = position_stack()) +
  
  ylab("Recording days") +
  xlab("Year") +
  
  scale_fill_manual('Analysis Effort', 
                    values = c("skyblue","skyblue4"),
                    labels = c("pending", "complete")) +

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        strip.text = element_text(size = 11, face = 'bold'),
        text = element_text(size = 10))
  

figurename <- paste0("PAM_analysis_summary_", Sys.Date(), ".png") 
ggsave(figurename, figure, width = 6.5, height = 5, units = "in")
  
  
  