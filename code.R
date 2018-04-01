#loading tidyverse
library(tidyverse)

#0 Load data in Rstudio

ref <- read.table("refine_original.csv", header = TRUE, sep = ",")

#1 Clean up brand names

ref <- ref %>% 
  mutate ( company = tolower(ref$company),
           company = sub(pattern = ".*ps", replacement = "philips", company),
           company = sub(pattern = "^a.*", replacement = "akzo", company),
           company = sub(pattern = "^v.*", replacement = "van houten", company),
           company = sub(pattern = "^u.*", replacement = "unilever", company)
           )

#2 seperate product code and number

ref <- ref %>% separate(`Product.code...number`, c("product_code","product_number"), sep = "-")

#3 Add product categories

ref <- ref %>% mutate(
                      Product_category = case_when(
                                                  product_code == "p" ~ "Smartphone",
                                                  product_code == "v" ~ "TV",
                                                  product_code == "x" ~ "Laptop",
                                                  product_code == "q" ~ "Tablet"
                                                  )
                                                  )

#4 Add full address for geocoding

ref <- ref %>% mutate (  
                      full_address = paste(address, city, country, sep = ",")
                      ) 
                             
#5 Create dummy variables for company and product category

ref <- ref %>% mutate (
                        company_philips = ifelse (company == "philips", 1, 0), 
                        company_akzo = ifelse (company == "akzo", 1, 0),
                        company_van_houten = ifelse (company == "van houten", 1, 0),
                        company_unilever = ifelse (company == "unilever", 1, 0),
                       
                        product_smartphone = ifelse(product_code == "p", 1, 0),
                        product_tv = ifelse(product_code == "v", 1, 0),
                        product_laptop = ifelse(product_code == "x", 1, 0),
                        product_tablet = ifelse(product_code == "q", 1, 0)
                         )
#Write the CSV File output
write.csv(ref, "refine_clean.csv")
                
