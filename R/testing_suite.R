# ****************************************
# Confounder Handling Simulation Study
# 
# Testing Suite for All Main and Helper Functions
# 
# Emma Tarmey
#
# Started:          26/03/2024
# Most Recent Edit: 26/03/2024
# ****************************************


# TESTING
# writeLines(paste("\n", model_method, " R2"))
# writeLines("\nVectors")
# print("true y")
# test_y %>% length() %>% print()
# test_y %>% head() %>% print()
# print("pred y")
# pred_y %>% length() %>% print()
# pred_y %>% head() %>% print()
# 
# writeLines("\nSSR:")
# sum((pred_y - test_y)^2) %>% head() %>% print()
# 
# writeLines("\nSSReg:")
# sum((pred_y - mean(pred_y))^2) %>% head() %>% print()
# 
# writeLines("\nSST:")
# sum((test_y - mean(test_y))^2) %>% head() %>% print()
# 
# writeLines("\nR2:")
# SSR <- sum((pred_y - test_y)^2)
# SST <- sum((test_y - mean(test_y))^2)
# (1 - (SSR / SST)) %>% print()
# 
# writeLines("\nRegression R2")
# SSReg <- sum((pred_y - mean(pred_y))^2)
# SST   <- sum((test_y - mean(test_y))^2)
# (SSReg / SST) %>% print()