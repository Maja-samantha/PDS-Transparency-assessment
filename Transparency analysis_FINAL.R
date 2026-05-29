# Transparency assessment
#===============================================================
# Articles have been screened, and transparency has been assessed across 5 domains.
# domains are:
#     protocol, pre-registration, data sharing, code sharing and reporting checklist.
# Before a new dataset is run, remember the following;´:
# 1.Run libraries.
# 2.Set working directory to source file location.
# 3.Check column names in dataset in excel, matches those in the code>> (run names(df) to check)
# 4.Check nrow() and ncol(), which confirms the file loaded correctly.
# 5.Now the full script can be run
# 6.SUPER IMPORTANT! Check all the fallbacks to make corrections to script fitting data.
# 
#===============================================================
#Install and load packages
#---------------------------------
#install.packages("readxl")    # reads Excel (.xlsx) files into R
#install.packages("dplyr")     # tools for manipulating and transforming data
#install.packages("tidyr")     # tools for reshaping data (used for one plot)
#install.packages("stringr")   # tools for working with text
#install.packages("ggplot2")   # tools for making plots and charts
#install.packages("gt")        # makes clean, publication-ready tables
#install.packages("flextable") # makes tables you can copy into Word
#install.packages("writexl")   # saves data back out as an Excel file
#install.packages("officer")
#
library(readxl)      #so it can read .xlsx files
library(dplyr)       #data manipulation
library(tidyr)       #reshapes data
library(stringr)     #tools for working with text
library(ggplot2)     #tools for making plots and charts
library(gt)          #makes clean tables
library(flextable)   #makes tables you an copy into word
library(writexl)     #saves data back out as an excel file
library(officer)     #allows for consistent borders and bold formatting to all tables.

getwd()   #to confirm the Working Directory is to the correct file location.


# Import data -- Make sure to write the file name, and that it is the same folder.


df <- read_excel("export_PDS500.xlsx")

# Quick check, that the number of rows (363) and columns (8) match with what R has found.

glimpse(df)
nrow(df)
ncol(df)

#=======================================================================
# Clean column names
#------------------------------
# We want to remove space from column names so they are easier to work with in R

df <- df %>%
  rename(
    year = Year,
    authors = Authors,
    title = Title, 
    protocol = Protocol, 
    preregistration = `Pre-registration`,
    data_sharing = `Data sharing`,
    code_sharing = `Code sharing`,
    reporting = `Reporting checklist`
  )

# We confirm the new names by running the below code

names(df)

#=================================================================================================
# Domain: PROTOCOL
# --------------------------
# 1 = No protocol available (blank)
# 2 = Protocol available upon request to authors
# 3 = Protocol publicly available through pre-registration, appendix or similar
# 4 = Protocol available and created using a structured template (e.g. HARPER)
# Remember! R reads line of code top to bottom, and stop at first match, so the order is important.
#--------------------------------------------------------------------------------------------------

df <- df %>%
  mutate(
    proto_lower=str_to_lower(protocol),
    
    protocol_cat = case_when(
      #CATEGORY 4 Available and using a structured template
      str_detect(proto_lower, "harper|structured template") ~ 4,
      
      #CATEGORY 2 Available upon request to authors (Before 3)
      str_detect(proto_lower,"upon request|on request|available.*request|
         request.*available|justified request|reasonable request") ~ 2,
      
      #CATEGORY 3 Available through pre-registration, appendix or link
      str_detect(proto_lower,"eupas|encepp|eu pas|osf|sentinel|published|published elsewhere|published protocol|real world evidence registry|publicly available protocol|can be found|available on|available at|pre-registered|preregistered|registered|described in detail in the study protocol|of the protocol") ~ 3,
    
      #CATEGORY 1 No protocol (blank fields)
      is.na(protocol) ~ 1,
      
      # Fallback for any that didn't match and needs manual review
      TRUE ~ NA_real_
      
      )
    )
  
  #Check for unmatched entries (should be empty if all entries matched)
  
df %>% filter(is.na(protocol_cat)) %>% select(authors, protocol)

  #Quick count, using the numbers, not the named categories (run to check how many in each category):

  table(df$protocol_cat)
  
  #We want readable labels:
  
  df <- df %>%
    mutate(
      protocol_cat_f=factor(
        protocol_cat,
        levels = 1:4,
        labels = c(
          "No protocol available",
          "Available upon request to authors",
          "Available (pre-registration, appendix, or similar",
          "Available via structured template"
        )
      )
    )
  table(df$protocol_cat_f)
#=================================================================================================
# Domain: PRE-REGISTRATION
#----------------------------
# Categories:
# 1 = No pre-registration (blank columns)
# 2 = Pre-registered (at a named registry with identifier or link)
# All the pre-registered entries have a registry name as identifier, so coding is straight forward-
#---------------------------------------------------------------------------------------------------------------------------------------------------------------
  # The below counts how mnay where preregistered vs not (see the lines after, which count how many in each registry)

df <- df %>%
    mutate(
      prereg_cat = case_when(
        is.na(preregistration) ~1, #no pre-registration
        TRUE                   ~2  #pre-registered at a registry
      ),
      prereg_cat_f = factor(
        prereg_cat,
        levels = 1:2,
        labels = c(
          "No pre-registration",
          "Pre-registered"
        )
      )
    )

table(df$prereg_cat_f)

  #The below counts for each registry-----------------------------------------------------------------------------------------------------------------------------

df <- df %>%
  mutate(
    prereg_lower = str_to_lower(preregistration),
    
    prereg_registry = case_when(
      str_detect(prereg_lower, "eupas|eu pas|encepp") ~ "EU PAS / ENCePP",
      str_detect(prereg_lower, "osf|open science framework|
          real world evidence registry|10\\.17605") ~ "OSF / RWE Registry",
      str_detect(prereg_lower, "nct") ~ "ClinicalTrials.gov",
      is.na(preregistration) ~ "None",
      
      #Fallback for anything that did not match-------------------------------------------------------------------------------------------------------------------
      TRUE ~ NA_character_
    )
  )

table(df$prereg_registry)

# Fallback, gives any that didn't match a defined registry in the above code--------------------------------------------------------------------------------------

df %>% filter(is.na(prereg_registry)) %>% select(authors, preregistration)


#==========================================================================================
#Domain: Data sharing
#----------------------------
# Categories
# NOTE: categories are aligned with Wang & Pottegård
#
# 1 = No mention of data availability [Level 0]
# 2 = Not available (ethical, legal, or regulatory reason) [Level 0]
# 3 = Not applicable (no datasets generated) [Level 0]
# 4 = Available upon request to authors [Level 1]
# 5 = Available upon request or application to third party + purchase [Level 1]
# 6 = Publicly available / Open access with direct link [Level 2]
# 7 = Conditional access with detailed instructions [Level 2]
# 8 = Available in supplementary material, appendix or from article [Level 2]
#
# The order of the categories is important, with more specific rules coming before broader ones, to avoid misclassification

df <- df %>%
  mutate(ds_lower = str_to_lower(data_sharing),
         data_cat = case_when(
           
           # Category 3: Not applicable (no datasets generated) -----------------------------------
           str_detect(ds_lower, "not applicable|no new data|no datasets were generated|no data were generated") ~ 3,
           
           # Category 8: Supplementary material ---------------------------------------------------
           str_detect(ds_lower, "supplementary material|supplementary information|additional file|available in supplementary|in the supplement|in data s1|in supporting information|presented.*supporting|appendix") ~ 8,
           
           # Category 6: Publicly/openly available with direct link --------------------------------
           str_detect(ds_lower, "zenodo|figshare|doi\\.org|r package|vigiaccess\\.org|openprescribing\\.net|openly available.*http|publicly available.*http|available.*https|cdc\\.gov/ncbddd|available via a public dashboard|included in this published article|available here") ~ 6,
           
           # Category 2: Not available (reason stated) --------------------------------------------
           str_detect(ds_lower, "cprd.*not allow|not available.*ethical|not available.*legal|not available.*restriction|not available.*security|not available.*privacy|not available.*regulation|cannot be shared|not allowed|not permitted|restricted by law|legal restriction|ethical restriction|cannot publicly release|not legally entitled|prevents data sharing|not allow the sharing|no legal rights|legal rights to share|authors are not allowed|cannot.*shared due to|not available for replication|not available for transfer|strictly restricted by the law|individual-level data cannot be shared|privacy and ethical|not publicly available.*privacy|not publicly available.*security|only available for the researchers participating|only.*for the researchers participating") ~ 2,
           
           # Category 7: Conditional access with detailed instructions ----------------------------
           str_detect(ds_lower, "request.*http|request.*www|access.*http|access.*www|apply.*http|apply.*www|available.*http|available.*www|request.*@|contact.*@|please make requests.*@|email.*@|@.*request|@.*access|@.*data|data use agreement|license agreement|subject to approval|freely available to researchers with an approved|approval of a proposal|approval.*committee|approval from.*required|prespecified criteria|process for accessing|further information.*visiting|can apply directly|in cooperation with.*agreement|signing an agreement|more information.*http|more information.*www") ~ 7,
           
           # Category 5: Request or application to third party -----------------------------------
           # NOTE: specific database names added as they cannot be caught by general patterns alone
           # NOTE: github included because one study shared data through github
           str_detect(ds_lower, "with per.?mission of|with the permission of|with permission from|available for purchase|available.*at a cost|available.*under license|available from ibm|for purchase from|data custodian|data holder|data vendor|applying to the relevant|relevant authorit|researchers can apply|researchers can request|researchers may engage|restrictions apply|used under license|public files.*available|available.*public files|github\\.com|directed to the|queries.*directed to|publicly accessible.*database|publicly available.*database|available.*through the.*database|available.*through the.*study|available.*through the.*system|available.*through the.*registry|available.*through the.*website|open data section|isq|cprd|faers|nhats|per.\\s*mission of") ~ 5,
           
           # Category 4: Available upon request to authors ----------------------------------------
           str_detect(ds_lower, "upon.*request|on.*request|relevant request|available.*request|request.*available|upon reasonable request|available from the.*author|available from the first author|available from the corresponding|will be considered by the authors|will be made available by the authors|provided upon.*request|available.*formal request|requests.*directed to the corresponding|requests.*directed to the first|on reasonable reques") ~ 4,
           
           # Category 1: No mention of data availability (blank field) ----------------------------
           is.na(data_sharing) ~ 1,
           
           # Fallback: did not match any category — needs manual review
           TRUE ~ NA_real_
         )
  )

# Check results
table(df$data_cat)

# Check fallback
df %>% filter(is.na(data_cat)) %>% select(authors, data_sharing)
# Check fallback — any entries that did not match any categories. 
df %>% filter(is.na(data_cat)) %>% select(authors, data_sharing)

#Addition of readable tables

df <- df %>%
  mutate(
    data_cat_f = factor(
      data_cat,
      levels = 1:8,
      labels = c(
        "No mention",
        "Not available with reasoning (ethical/legal/regulatory)",
        "Not applicable (no dataset)",
        "Upon request to authors",
        "Upon request to third party/data holder",
        "Publicly/openly available",
        "Conditional access with detailed instructions",
        "Supplementary/in-article data"
      )
    )
  )
table(df$data_cat_f)

#prints all the fallback articles which were not matched.

print(df %>% filter(is.na(data_cat)) %>% select(authors, data_sharing))
#
#===============================================================================
# DOMAIN: Code sharing
#-------------------------------------
# Categories:
# 1. not available, blank
# 2. Available upon request
# 3. Available at link to Github, GitLab, OSF, Zenodo etc. 
#----------------------------------------------------------
#
df <- df %>%
  mutate(cs_lower = str_to_lower(code_sharing),
         
         code_cat = case_when(
           # Category 3: Publicly available at link or in appendix
           str_detect(cs_lower, "github|gitlab|zenodo|osf\\.io|doi 10\\.|figshare|cran|r package|appendix|appendix s|in s1|in s2|in data s|supporting information|presented in table|codes.*presented|provided.*code|in a public|publicly available|openly available|freely available|can be found|can be accessed|found online|found at http|available at http|available on http|is available at|source code|available here") ~ 3,
           
           # Category 2: Available upon request to authors
           str_detect(cs_lower, "upon request|on request|available.*request|request.*available|upon reasonable request|available from the.*author|available from the corresponding|request.*corresponding|justified request|will be made available|available.*publication|subject to internal approvals") ~ 2,
           
           # Category 1: Not available (blank)
           is.na(code_sharing) ~ 1,
           
           # Fallback
           TRUE ~ NA_real_
         )
  )

# Check for unmatched entries:
df %>% filter(is.na(code_cat)) %>% select(authors, code_sharing)

# Quick count:
table(df$code_cat)

# Add labels:
df <- df %>%
  mutate(
    code_cat_f = factor(
      code_cat,
      levels = 1:3,
      labels = c(
        "No code available",
        "Available upon request to authors",
        "Available at GitHub/GitLab/OSF or similar"
      )
    )
  )
table(df$code_cat_f)
#==============================================================================
# DOMAIN: Reporting checklist
#--------------------------------------
# Categories:
# 1. No checklist used
# 2. Checklist used and declared
# 3. Checklist used, declared and provided in appendix
#-------------------------------------------------------------------------
df <- df %>%
  mutate(
    rep_lower = str_to_lower(reporting),
    
    reporting_cat = case_when(
      
      # --- Category 3: Checklist declared AND provided in appendix ---
      str_detect(rep_lower,
                 "appendix|provided|supplement|available|attached") ~ 3,
      
      # --- Category 2: Checklist declared (any named checklist) ---
      str_detect(rep_lower,
                 "strobe|record|tripod|consort|cioms|emerge|prisma|trend|stard|moose|equator|reporting.*guideline|guideline.*reporting|checklist") ~ 2,
      
      # --- Category 1: No checklist (blank) ---
      is.na(reporting) ~ 1,
      
      # --- Fallback ---
      TRUE ~ NA_real_
    )
  )

# Check for unmatched entries:
df %>% filter(is.na(reporting_cat)) %>% select(authors, reporting)

# Quick count:
table(df$reporting_cat)

# Add labels:
df <- df %>%
  mutate(
    reporting_cat_f = factor(
      reporting_cat,
      levels = 1:3,
      labels = c(
        "No checklist used",
        "Checklist declared",
        "Checklist declared and provided in appendix"
      )
    )
  )
table(df$reporting_cat_f)
#---------------------------------------------
#Counts how many for each reporting checklist
#--------------------------------------------
df <- df %>%
  mutate(
    rep_lower = str_to_lower(reporting),
    
    reporting_checklist = case_when(
      str_detect(rep_lower, "record-pe|record pe") ~ "RECORD-PE",
      str_detect(rep_lower, "record") ~ "RECORD",
      str_detect(rep_lower, "strobe") ~ "STROBE",
      str_detect(rep_lower, "tripod.*stard|stard.*tripod") ~ "TRIPOD + STARD",
      str_detect(rep_lower, "tripod") ~ "TRIPOD",
      str_detect(rep_lower, "consort") ~ "CONSORT",
      str_detect(rep_lower, "cioms") ~ "CIOMS",
      str_detect(rep_lower, "emerge") ~ "EMERGE",
      str_detect(rep_lower, "stard") ~ "STARD",
      is.na(reporting) ~ "None",
      TRUE ~ NA_character_
    )
  )

table(df$reporting_checklist)

# Check fallback — any unrecognised checklists
df %>% filter(is.na(reporting_checklist)) %>% select(authors, reporting)
#=============================================================================
#HOW MANY IN EACH DOMAIN
#---------------------------------
# The following section counts how many studies are in each domain
#-----------------------------------------------------------------
#==========================================================================================
# Number of domains fulfilled.
#------------------------------------------------------------------------------------------
# A domain is considered fulfilled if the article had ANY statement
# (i.e. not category 1 / not blank) for that domain

df <- df %>%
  mutate(
    # 1 if the domain was fulfilled, 0 if not
    proto_fulfilled   = if_else(protocol_cat > 1,  1, 0),
    prereg_fulfilled  = if_else(prereg_cat > 1,    1, 0),
    data_fulfilled    = if_else(data_cat > 1,      1, 0),
    code_fulfilled    = if_else(code_cat > 1,      1, 0),
    report_fulfilled  = if_else(reporting_cat > 1, 1, 0),
    
    # Sum of fulfilled domains per article (0-5)
    domains_fulfilled = proto_fulfilled + prereg_fulfilled + data_fulfilled +
      code_fulfilled + report_fulfilled
  )

# Count how many articles fulfilled 0, 1, 2, 3, 4, 5 domains
table_domains <- df %>%
  count(domains_fulfilled) %>%
  mutate(
    Percentage = round(n / nrow(df) * 100, 1)
  ) %>%
  rename(
    `Number of domains fulfilled (n)` = domains_fulfilled,
    `Articles (n)`                    = n,
    `% of articles`                   = Percentage
  )

# Make sure all levels 0-5 are shown even if count is 0
table_domains <- tibble(
  `Number of domains fulfilled (n)` = 0:5
) %>%
  left_join(table_domains, by = "Number of domains fulfilled (n)") %>%
  mutate(
    `Articles (n)` = replace_na(`Articles (n)`, 0),
    `% of articles` = if_else(`Articles (n)` == 0, "-",
                              as.character(replace_na(`% of articles`, 0)))
  )

print(table_domains)

# Summary rows
total_domains     <- sum(df$domains_fulfilled)
total_transparency <- round(total_domains / (nrow(df) * 5) * 100, 1)

cat("\nTotal number of domains fulfilled:", total_domains)
cat("\nTotal transparency (%):", total_transparency, "\n")

#=============================================================================
# Frequency tables - 1 per domain.
# n and percentage is given for each category in each domain
#-------------------------------------------------------
# --- Table 1: Protocol ---
table_protocol <- df %>%
  count(protocol_cat_f) %>%
  mutate(Percentage = round(n / nrow(df) * 100, 1)) %>%
  rename(Category = protocol_cat_f)

print(table_protocol)

# --- Table 2: Pre-registration ---
table_prereg <- df %>%
  count(prereg_cat_f) %>%
  mutate(Percentage = round(n / nrow(df) * 100, 1)) %>%
  rename(Category = prereg_cat_f)

print(table_prereg)

# --- Table 3: Data sharing ---
table_data <- df %>%
  count(data_cat_f) %>%
  mutate(Percentage = round(n / nrow(df) * 100, 1)) %>%
  rename(Category = data_cat_f)

print(table_data)

# --- Table 4: Code sharing ---
table_code <- df %>%
  count(code_cat_f) %>%
  mutate(Percentage = round(n / nrow(df) * 100, 1)) %>%
  rename(Category = code_cat_f)

print(table_code)

# --- Table 5: Reporting checklist ---
table_reporting <- df %>%
  count(reporting_cat_f) %>%
  mutate(Percentage = round(n / nrow(df) * 100, 1)) %>%
  rename(Category = reporting_cat_f)

print(table_reporting)


#==========================================================================================
# PUBLICATION-READY TABLES USING gt
#------------------------------------------------------------------------------------------
#Helper function to add a custom total row excluding category 1
add_total <- function(table) {
  total_n   <- sum(table$n[!str_detect(as.character(table$Category), "No ")])
  total_pct <- round(total_n / nrow(df) * 100, 1)
  bind_rows(table, tibble(Category = "Transparency (%)", n = total_n, Percentage = total_pct))
}
# --- Table 1: Protocol ---
print(add_total(table_protocol))

gt_protocol <- add_total(table_protocol) %>%
  gt() %>%
  tab_header(title = "Protocol") %>%
  cols_label(Category = "Category", n = "n", Percentage = "%") %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Category == "Transparency (%)")
  ) %>%
  opt_stylize(style = 1)

gt_protocol

# --- Table 2: Pre-registration ---
print(add_total(table_prereg))

gt_prereg <- add_total(table_prereg) %>%
  gt() %>%
  tab_header(title = "Pre-registration") %>%
  cols_label(Category = "Category", n = "n", Percentage = "%") %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Category == "Transparency (%)")
  ) %>%
  opt_stylize(style = 1)

gt_prereg

# --- Table 3: Data sharing ---
print(add_total(table_data))

gt_data <- add_total(table_data) %>%
  gt() %>%
  tab_header(title = "Data sharing") %>%
  cols_label(Category = "Category", n = "n", Percentage = "%") %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Category == "Transparency (%)")
  ) %>%
  opt_stylize(style = 1)

gt_data

# --- Table 4: Code sharing ---
print(add_total(table_code))

gt_code <- add_total(table_code) %>%
  gt() %>%
  tab_header(title = "Code sharing") %>%
  cols_label(Category = "Category", n = "n", Percentage = "%") %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Category == "Transparency (%)")
  ) %>%
  opt_stylize(style = 1)

gt_code

# --- Table 5: Reporting checklist ---
print(add_total(table_reporting))

gt_reporting <- add_total(table_reporting) %>%
  gt() %>%
  tab_header(title = "Reporting Checklist") %>%
  cols_label(Category = "Category", n = "n", Percentage = "%") %>%
  fmt_number(columns = Percentage, decimals = 1) %>%
  tab_style(
    style     = cell_text(weight = "bold"),
    locations = cells_body(rows = Category == "Transparency (%)")
  ) %>%
  opt_stylize(style = 1)

gt_reporting

# --- Save all tables in one Word document ---
#change the path name, so we don't overwrite previous data
fix_table <- function(tbl) {
  flextable(tbl) %>%
    width(j = 1, width = 4) %>%
    width(j = 2, width = 1) %>%
    width(j = 3, width = 1.5) %>%
    bold(i = nrow(tbl), bold = TRUE) %>%
    hline_bottom(border = fp_border(width = 1)) %>%
    hline_top(border = fp_border(width = 1)) %>%
    border_inner_h(border = fp_border(width = 0.5))
}

save_as_docx(
  "Protocol"                      = fix_table(add_total(table_protocol)),
  "Pre-registration"              = fix_table(add_total(table_prereg)),
  "Data sharing"                  = fix_table(add_total(table_data)),
  "Code sharing"                  = fix_table(add_total(table_code)),
  "Reporting Checklist"           = fix_table(add_total(table_reporting)),
  "Domains fulfilled"             = flextable(table_domains) %>%
    width(j = 1, width = 3.5) %>%
    width(j = 2, width = 1.5) %>%
    width(j = 3, width = 1.5) %>%
    bold(i = nrow(table_domains), bold = TRUE) %>%
    hline_bottom(border = fp_border(width = 1)) %>%
    hline_top(border = fp_border(width = 1)) %>%
    border_inner_h(border = fp_border(width = 0.5)) %>%
    align(j = 1, align = "center") %>%
    align(j = 2, align = "center") %>%
    align(j = 3, align = "center"),
  path = "all_tables500.docx"   #Remember to rename docx. so previous tables aren't overwritten
)