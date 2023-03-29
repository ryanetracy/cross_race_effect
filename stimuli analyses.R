###### face data ######

library(rstatix)
library(tidyverse)


df <- read.csv('black and white cre faces/cfdNormingData.csv')

df <- df %>%
  filter(Race == 'B' | Race == 'W') %>%
  filter(Gender == 'M')

black_faces <- c('BM-002',
                 'BM-003',
                 'BM-004',
                 'BM-005',
                 'BM-009',
                 'BM-010',
                 'BM-011',
                 'BM-012',
                 'BM-013',
                 'BM-015',
                 'BM-016',
                 'BM-017',
                 'BM-018',
                 'BM-019',
                 'BM-020',
                 'BM-021',
                 'BM-022',
                 'BM-023',
                 'BM-024',
                 'BM-025',
                 'BM-026',
                 'BM-027',
                 'BM-028',
                 'BM-029')

white_faces <- c('WM-002',
                 'WM-003',
                 'WM-004',
                 'WM-006',
                 'WM-009',
                 'WM-010',
                 'WM-011',
                 'WM-012',
                 'WM-013',
                 'WM-014',
                 'WM-015',
                 'WM-016',
                 'WM-017',
                 'WM-018',
                 'WM-019',
                 'WM-020',
                 'WM-021',
                 'WM-022',
                 'WM-023',
                 'WM-024',
                 'WM-025',
                 'WM-026',
                 'WM-028',
                 'WM-029')

df_b <- df %>%
  filter(Target %in% black_faces) 
df_w <- df %>%
  filter(Target %in% white_faces)

df_final <- rbind(df_b, df_w)

# test on trust/dom/attractiveness

t.test(Attractive ~ Race, data = df_final, var.equal = T)
df_final %>%
  group_by(Race) %>%
  get_summary_stats(Attractive, type = 'mean_sd')

t.test(Trustworthy ~ Race, data = df_final, var.equal = T)
df_final %>%
  group_by(Race) %>%
  get_summary_stats(Trustworthy, type = 'mean_sd')

t.test(Dominant ~ Race, data = df_final, var.equal = T)
df_final %>%
  group_by(Race) %>%
  get_summary_stats(Dominant, type = 'mean_sd')
