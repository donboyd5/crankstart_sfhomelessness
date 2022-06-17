
# This program does the following:
#   - reads the bib file that I saved from Zotero to references.bib
#   - extract the citation keys
#   - prepend the @ sign
#   - sort
#   - save as plain text file that I will insert in the bibliography section of the report

# library(RefManageR)

# also see https://www.anthonyschmidt.co/post/2021-10-25-a-zotero-workflow-for-r/

library(bibtex)
library(tidyverse)

bibfile <- here::here("report", "references.bib")  # make sure to save this from Zotero

biblist <- read.bib(file=bibfile) # look to see whether anything failed to import

names(biblist)
df <- tibble(token=paste0("@", names(biblist))) %>%
  arrange(token)
df

df |> 
  pull(token) |> 
  writeLines(con=here::here("report", "bibs.qmd"), sep="\n")

