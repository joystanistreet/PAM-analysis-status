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
               ggpattern)

#--------------------------------------

# Set options

# projects to leave out
omit_project <- c('OPP-MEQ Coastal Monitoring', 'Kenchington PAM Landers')

# stations to leave out
omit_station <- c('ROBE', 'SABV', 'SBSM', 'CGL', 'EGL')

# deployments to leave out
omit_deployment <- c('CBN-2022-10', 'CSW-2022-10')

# years to leave out
omit_year <- c('2023', '2024')

#--------------------------------------

# Compile recording effort summary

# path to PAM metadata folder
metadata <- 'R:/Science/CetaceanOPPNoise/CetaceanOPPNoise_2/PAM_metadata'

# load deployment summary
depl <- read_csv(here(metadata, 'deployment_summary.csv'))

target_deployments <- depl %>% 
  
  # omit non-target projects
  filter(!Project %in% omit_project) %>% 
  
  # omit non-target deployments
  mutate(station = stringr::str_extract(Station, '(.)+(?=:)')) %>%
  filter(!station %in% omit_station) %>% 
  filter(!Deployment %in% omit_deployment) %>% 
  
  # omit current year(s)
  filter(!Year %in% omit_year) %>% 
  
  # remove datasets with no data
  mutate(rec_start = as_date(`In-water_start`),
          rec_end = as_date(`In-water_end`)) %>%
  filter(!(is.na(rec_start))) %>% 
  
  # add status columns
  mutate(narw_status = NA,
         baleen_status = NA,
         beaked_status = NA)

# SAVE
write_csv(target_deployments, 'PAM_status.csv')

### ADD STATUS HERE

#read in manually created PAM status sheet
analysis_status<-read_csv("PAM_status.csv") %>% 
  
  # calculate recording days per deployment
  mutate(rec_days = as.numeric(as_date(rec_end) - as_date(rec_start) + 1)) %>%
  
  # narw analysis days
  mutate(narw_days = narw_status*rec_days) %>% 
  
  # baleen whale analysis days
  mutate(baleen_days = baleen_status*rec_days) %>% 
  
  # calculate beaked whale recording days per deployment
  mutate(rec_days_beaked = case_when(beaked_status == 1 | beaked_status ==0 ~ rec_days,
                                     .default = 0)) %>% 
  # beaked whale analysis days
  mutate(beaked_days = beaked_status*rec_days_beaked)
  
# summarize by year
status_summary<- analysis_status %>% 
  group_by(Year) %>% 
  summarize(rec_days_year = sum(rec_days),
         rec_days_beaked_year = sum(rec_days_beaked),
         narw_days_year = sum(narw_days),
         baleen_days_year = sum(baleen_days),
         beaked_days_year = sum(beaked_days, na.rm = T)) %>% 
  mutate(Year = as_factor(Year)) %>% 
  ungroup()

# baleen whale data: set up for plot
baleen_summary<-status_summary %>% 
  transmute(Year, n_days = rec_days_year, analyzed = baleen_days_year) %>% 
  #select(year, rec_days_year, baleen_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!Year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'baleen')


# narw data: set up for plot
narw_summary<-status_summary %>% 
  transmute(Year, n_days = rec_days_year, analyzed = narw_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!Year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'narw')

# beaked whale data: set up for plot
beaked_summary<-status_summary %>% 
  transmute(Year, n_days = rec_days_beaked_year, analyzed = beaked_days_year) %>% 
  mutate(not_analyzed = n_days - analyzed) %>% 
  select(-n_days) %>% 
  pivot_longer(!Year, names_to = "effort", values_to = "count") %>% 
  mutate(effort = factor(effort, levels = c('not_analyzed','analyzed')),
         species = 'beaked')

# put it all back together

all_spp <- baleen_summary %>% 
  #full_join(narw_summary) %>% 
  full_join(beaked_summary) %>% 
  mutate(species = factor(species, levels = c("baleen", "beaked")))

  
# plot figure

theme_set(theme_bw())

test<-ggplot() + 
  
  facet_wrap(~species, ncol = 1) +
  
  geom_bar(data = all_spp, 
           aes(y = count, x = Year, fill = effort),
           stat = 'identity',
           position = position_stack()) +
  
  ylab("Recording days") +
  
  scale_fill_manual('Analysis Effort', values = c("skyblue","skyblue4")) +

  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        text = element_text(size = 10))
  
test  
  
  
  