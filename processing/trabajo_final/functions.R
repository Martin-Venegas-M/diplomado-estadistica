ctabs <- function(data, varx, vary, decimals = 0, type = 2) {
  
  library(sjlabelled)
  library(tidyverse)
  library(sjmisc)
  
  if(type == 1) {
    
    tab_total <- data %>% 
      select({{ varx }}) %>%
      to_label() %>%
      table() %>%
      as.data.frame() %>%
      pivot_wider(names_from = {{ varx }}, values_from = Freq) %>% 
      mutate(Total = rowSums(.)) %>%
      mutate(across(everything(), ~ round(.x / Total * 100, decimals))) %>%
      t() %>% 
      as.data.frame() %>% 
      rename(Total = V1)
    
    return(tab_total %>% rename(Porc = Total))
    
  }
  
  if(type == 2) {
    
    tab_total <- data %>% 
      select({{ varx }}) %>%
      to_label() %>%
      table() %>%
      as.data.frame() %>%
      pivot_wider(names_from = {{ varx }}, values_from = Freq) %>% 
      mutate(Total = rowSums(.)) %>%
      mutate(across(everything(), ~ round(.x / Total * 100, decimals))) %>%
      t() %>% 
      as.data.frame() %>% 
      rename(Total = V1)
    
    tab_contingency <- data %>% 
      select({{ vary }}, {{ varx }}) %>%
      to_label() %>%
      table() %>%
      as.data.frame.matrix() %>%
      mutate(Total = rowSums(.)) %>%
      mutate(across(everything(), ~ round(.x / Total * 100, decimals))) %>%
      t() %>% 
      as.data.frame()
    
    tab <- cbind(tab_contingency, tab_total)
    
    return(tab)
    
  }
}


dtabs <- function(data1, varx, vary) {
  
  # Recode NA and pass lables to values
  data1 <- data1 %>% mutate(
    {{ varx }} := ifelse({{ varx }} %in% c(88, 99), NA, {{ varx }}),
    {{ vary }} := to_label({{ vary }})
  )
  
  # Create table
  dtab <- data1 %>%
    select( {{ vary }},  {{ varx }} ) %>%
    group_by( {{ vary }} ) %>%
    summarise(
      N = n(),
      Media = mean( {{ varx }}, na.rm = T ),
      SD = sd( {{ varx }}, na.rm = T  ),
      CV = sd( {{ varx }}, na.rm = T  ) / mean( {{ varx }}, na.rm = T  ),
      Mediana = median( {{ varx }}, na.rm = T  ),
      Min = min( {{ varx }}, na.rm = T  ),
      Max = max( {{ varx }}, na.rm = T  )
    ) %>%
    
    # Add total row
    add_row(
      .,
      {{ vary }} := "Total",
      N = NROW(data1 %>% pull( {{ varx }} ) ),
      Media = mean(data1 %>% pull( {{ varx }} ), na.rm = T  ),
      SD = sd(data1 %>% pull( {{ varx }} ), na.rm = T  ),
      CV = sd(data1 %>% pull( {{ varx }} ), na.rm = T  ) / mean(data1 %>% pull( {{ varx }} ), na.rm = T  ),
      Mediana = median(data1 %>% pull( {{ varx }} ), na.rm = T  ),
      Min = min(data1 %>% pull( {{ varx }} ), na.rm = T  ),
      Max = min(data1 %>% pull( {{ varx }} ), na.rm = T  )
    )
  
  return(dtab)
  
}
