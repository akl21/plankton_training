pcr_table <- read.csv(file = "data/pcr_data.csv", 
                      stringsAsFactors = FALSE)
phylum_table <- read.csv("data/phylum_listings.csv", 
                         stringsAsFactors= FALSE)

library("dplyr")
library("ggplot2")
summarize_pcr_failures(pcr_table)

#This code block outputs a table listing the phylum and number of
#distinct samples in that phylum that failed when attempting PCR. 
#The resulting table is named pcr_failure_table.
pcr_failure_table <- summarize_pcr_failures(pcr_table) %>%
  left_join(phylum_table) %>%
  distinct(voucher_number) %>%
  select(pcr_failure, phylum) %>%
  group_by(phylum) %>%
  summarize(pcr_failure = n())
pcr_failure_table

#In this code block I try to count how many distinct vouchers were tested for each phylum.
num_samples_tested <- pcr_table %>%
  left_join(phylum_table) %>%
  select(voucher_number, phylum) %>%
  distinct(voucher_number) %>%
  group_by(phylum) %>%
  summarize(num_vouchers = n())
num_samples_tested

#Here I present the percent of samples that fail during PCR by phylum out of the 
#total number of distinct samples tested.
pcr_percent_failure <- num_samples_tested %>%
  left_join(pcr_failure_table) %>%
  mutate(percentage_pcr_failure = pcr_failure/num_vouchers) %>%
  select(phylum, percentage_pcr_failure)

pcr_percent_failure

#The following code produces a barplot in ggplot2 of the fraction of PCR failure 
#for each phylum.
df_percent_failure = data.frame(pcr_percent_failure)
ggplot(df_percent_failure, aes(pcr_percent_failure[,1],pcr_percent_failure[,2])) +
  geom_bar(stat = "identity") + labs(x = "Phyla", y = "Sample Failure Fraction", 
                                     title = "Fraction of PCR Failure")

