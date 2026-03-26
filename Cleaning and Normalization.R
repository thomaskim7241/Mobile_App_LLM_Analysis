# Setup -------------------------------------------------------------------
setwd("~/Documents/IDSC_4521/LLM_App_Analytics")
library(tidyverse)
User_df <- read_csv('user_info.csv')
Usage_df <- read_csv('weekly_usage.csv')
AI_App_Names_df <- read_csv('AI_App_Names.csv')


# Creating an IS_AI variable in Usage_df ----------------------------------
# Create a vector of AI app names from your lookup df
ai_app_list <- AI_App_Names_df$APPDESCRIPTION

# Create new variable in Usage_df flagging AI vs non-AI apps
Usage_df <- Usage_df %>%
  mutate(IS_AI = APPDESCRIPTION %in% ai_app_list)


