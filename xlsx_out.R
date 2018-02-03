library(openxlsx)

res_verification <- list("res_verification_gp1" = res_verification_gp1, 
                         "res_verification_gp3" = res_verification_gp3,
                         "res_verification_gp5" = res_verification_gp5)

write.xlsx(res_verification, "res_verification.xlsx")