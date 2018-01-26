var_C <- c("y","x1","x2","yymm")
panel_data_C <- panel_data %>%  
  select(one_of(var_C)) %>%
  filter(yymm == 1403) %>%
  na.omit
panel_names_C <- names(panel_data_C)
f_4 <- as.formula(paste("y~", paste(panel_names_C[panel_names_C != "y" & 
                                                  panel_names_C != "yymm"], collapse = "+")))
match_gp1_C <- matchit(formula = f_4, data = panel_data_C, method = "exact")

