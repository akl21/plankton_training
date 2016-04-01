pcr_table <- read.csv(file = "data/pcr_data.csv", 
                      stringsAsFactors = FALSE)
phylum_table <- read.csv("data/phylum_listings.csv", 
                         stringsAsFactors= FALSE)

library("dplyr")
library(ggplot2)

#This function takes an input of a data frame, selects for the 
#voucher number and pcr_success columns, filters out the voucher numbers
#for which the PCR failed every time, then finally produces a count 
#of how many times the PCR failed for each distinct voucher number.
count_failed_vouchers <- function(pcr_data_frame){
  select(pcr_data_frame, voucher_number, pcr_success) %>%
    filter(pcr_success == 0 & !(pcr_success==1)) %>%
    distinct(voucher_number) %>%
    group_by(voucher_number) %>%
    summarize(failed_vouchers = n())
} 

count_failed_vouchers(pcr_table)

#This code block inputs our PCR table into the count_failed_vouchers
#function, then left joins it with the table containing the phylums associated
#with the voucher numbers, and outputs a table listing the phylum and number of
#distinct samples that failed when attempting PCR. 
#The resulting table is named pcr_failure_table.

pcr_failure_table <- count_failed_vouchers(pcr_table) %>%
  left_join(phylum_table) %>%
  select(failed_vouchers, phylum) %>%
  group_by(phylum) %>%
  summarize(failed_vouchers = n())
pcr_failure_table

#In this code block I try to count how many vouchers were tested for each phylum.
num_samples_tested <- pcr_table %>%
  left_join(phylum_table) %>%
  select(voucher_number, phylum) %>%
  distinct(voucher_number) %>%
  group_by(phylum) %>%
  summarize(num_vouchers = n())

#Here I present the percent of samples that fail during PCR by phylum out of the 
#total number of distinct samples tested.
pcr_percent_failure <- num_samples_tested %>%
  left_join(pcr_failure_table) %>%
  mutate(percentage_pcr_failure = pcr_failure/num_vouchers) %>%
  select(phylum, percentage_pcr_failure)

pcr_percent_failure

#The following code produces a simple barplot of fraction of PCR failure 
#for each phylum.
barplot(pcr_percent_failure[,2], main = "Fraction of PCR Failure", xlab = "Phyla", 
        names.arg = pcr_percent_failure[,1])


