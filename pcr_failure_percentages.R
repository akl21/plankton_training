pcr_table <- read.csv(file = "data/pcr_data.csv", 
                      stringsAsFactors = FALSE)
phylum_table <- read.csv("data/phylum_listings.csv", 
                         stringsAsFactors= FALSE)

library("dplyr")

#This function takes an input of a data frame, selects for the 
#voucher number and pcr_success columns, filters out the voucher numbers
#for which the PCR failed every time, then finally produces a count 
#of how many times the PCR failed for each voucher number.

pcr_attempts_function <- function(pcr_table_arg){
  select(pcr_table_arg, voucher_number, pcr_success) %>%
    filter(pcr_success == 0 & !(pcr_success==1)) %>%
    group_by(voucher_number) %>%
    summarise(pcr_failure= n())
  
}

pcr_attempts_function(pcr_table)

#This code block inputs our PCR table into the summarize_pcr_failures
#function, then left joins it with the table containing the phylums associated
#with the voucher numbers, and outputs a table listing the phylum and number of
#PCR failures.

summarize_pcr_failures(pcr_table) %>%
  left_join(phylum_table) %>%
  select(pcr_failure, phylum) %>%
  group_by(phylum) %>%
  summarise(pcr_failure = n())

#This function takes a PCR table as an argument, right joins it with the 
#table listing the phyla of the samples, then outputs the number of times
#each phylum was tested for PCR success.
rightjoin_function = function(pcr_table_arg) {
  pcr_attempts_function(pcr_table_arg) %>%
    right_join(phylum_table) %>%
    select(pcr_failure, phylum) %>%
    group_by(phylum) %>%
    summarise(pcr_success = n())
}

rightjoin_function(pcr_table)


