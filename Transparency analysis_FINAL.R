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
#install.packages("stringr")   # tools for working with text
#install.packages("gt")        # makes clean, publication-ready tables
#install.packages("flextable") # makes tables you can copy into Word
#install.packages("officer")
#
library(readxl)      #so it can read .xlsx files
library(dplyr)       #data manipulation
library(stringr)     #tools for working with text
library(gt)          #makes clean tables
library(flextable)   #makes tables you an copy into word
library(officer)     #allows for consistent borders and bold formatting to all tables.

getwd()   #to confirm the Working Directory is to the correct file location.


# Import data -- Make sure to write the file name, and that it is the same folder.


input_file  <- "export_DrugSafety100.xlsx"
output_file <- paste0("all_tables_", tools::file_path_sans_ext(input_file), ".docx")

df <- read_excel(input_file)

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

# Fix hyphenation artifacts from PDF text extraction (e.g. "corre- sponding" -> "corresponding")
# Must run before any lowercasing or pattern matching below.
df <- df %>%
  mutate(across(
    c(protocol, preregistration, data_sharing, code_sharing, reporting),
    ~ str_replace_all(., "([a-z])-\\s+([a-z])", "\\1\\2")
  ))


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
      str_detect(proto_lower, "request") ~ 2,      
    
      #CATEGORY 3 Available through pre-registration, appendix or link
      str_detect(proto_lower, "eu ?pas|encepp|isrctn|osf|open ?science|sentinel|published|regist|https?|appendix|supplementary|publicly available protocol|of the protocol|described in detail in the study protocol") ~ 3,    
      
      #CATEGORY 1 No protocol (blank fields)
      is.na(protocol) ~ 1
      
      
      )
    )

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
          "Available (pre-registration, appendix, or similar)",
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
  # The below counts how many where preregistered vs not (see the lines after, which count how many in each registry)

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
      is.na(preregistration) ~ "None",
      str_detect(prereg_lower, "eu ?pas|encepp") ~ "EU PAS / ENCePP",
      str_detect(prereg_lower, "osf|open ?science|real world evidence registry") ~ "OSF / RWE Registry",
      str_detect(prereg_lower, "nct|clinicaltrials") ~ "ClinicalTrials.gov",
      str_detect(prereg_lower, "isrctn") ~ "ISRCTN",
      str_detect(prereg_lower, "\\[\\d+\\]") ~ "Registry in reference list"
    )
  )      

table(df$prereg_registry)



#==========================================================================================
#Domain: Data sharing
#----------------------------
# Categories
# NOTE: categories are aligned with Wang & Pottegård
#
# 1 = No mention of data availability [Level 0]
# 2 = Not available (ethical, legal, or regulatory reason) [Level 0] and Not applicable (no datasets generated) [Level 0]
# 3 = Available upon request to authors [Level 1]
# 4 = Available upon request or application to third party + purchase [Level 1]
# 5 = Publicly available / Open access with direct link [Level 2]
# 6 = Conditional access with detailed instructions [Level 2]
# 7 = Available in supplementary material, appendix or from article [Level 2]
#
# The order of the categories is important, with more specific rules coming before broader ones, to avoid misclassification

df <- df %>%
  mutate(ds_lower = str_to_lower(data_sharing),
         data_cat = case_when(
           
           # Category 2: Not available for legal reasons or applicable (no datasets generated) -----------------------------------
           str_detect(ds_lower, "not applicable|no new data|no datasets were generated|no data were generated|cprd.*not allow|not available.*ethical|not legally authorized|not authorized|not available.*legal|not available.*restriction|not available.*security|not available.*privacy|not available.*regulation|cannot be shared|not allowed|not permitted|restricted by law|legal restriction|ethical restriction|cannot publicly release|not legally entitled|prevents data sharing|not allow the sharing|no legal rights|legal rights to share|authors are not allowed|cannot.*shared due to|not available for replication|not available for transfer|strictly restricted by the law|individual-level data cannot be shared|privacy and ethical|not publicly available.*privacy|not publicly available.*security|only available for the researchers participating|only.*for the researchers participating|for legal reasons|forbidden by law|do not have the authority to share|not covered by.*ethics|strict confidentiality|protected by agreements with|will not be made public") ~ 2,           
         
           # Category 7: Supplementary material ---------------------------------------------------
           str_detect(ds_lower, "supplementary material|supplementary information|additional file|available in supplementary|in the supplement|in data s1|in supporting information|presented.*supporting|appendix") ~ 7,
           
           # Category 5: Publicly/openly available with direct link --------------------------------
           str_detect(ds_lower, "zenodo|figshare|doi\\.org|r package|vigiaccess\\.org|openprescribing\\.net|(?<!not )(publicly|openly) available.*http|available for download.*https|available.*at https|available here.*https|cdc\\.gov/ncbddd|available via a public dashboard|included in this published article|available here") ~ 5,          
          
           # Category 6: Conditional access with detailed instructions ----------------------------
           str_detect(ds_lower, "request.*http|request.*www|access.*http|access.*www|apply.*http|apply.*www|available.*http|available.*www|request.*@|contact.*@|please make requests.*@|email.*@|@.*request|@.*access|@.*data|data use agreement|license agreement|subject to approval|freely available to researchers with an approved|approval of a proposal|approval.*committee|approval from.*required|prespecified criteria|process for accessing|further information.*visiting|can apply directly|in cooperation with.*agreement|signing an agreement|more information.*http|more information.*www|criteria to access") ~ 6,
           
           # Category 4: Request or application to third party -----------------------------------
           str_detect(ds_lower, "application to the|application to.*authority|access to data can be requested through|request for data can be submitted to|request access to the data|data steward|closely working with.*to make the data|available in esm|as esm|eudravigilance") ~ 4,
           
           str_detect(ds_lower, "with per.?mission of|with the permission of|with permission from|available for purchase|available.*at a cost|available.*under license|available from ibm|for purchase from|data custodian|data holder|data vendor|applying to the relevant|relevant authorit|researchers can apply|researchers can request|researchers may engage|restrictions apply|used under license|public files.*available|available.*public files|github\\.com|directed to the|queries.*directed to|publicly accessible.*database|publicly available.*database|available.*through the.*database|available.*through the.*system|available.*through the.*registry|available.*through the.*website|open data section|isq|cprd|faers|nhats|per.\\s*mission of|optum|iqvia|require a fee|data can be requested from") ~ 4,
           
           # Category 3: Available upon request to authors ----------------------------------------
           str_detect(ds_lower, "upon.*request|on.*request|relevant request|available.*request|request.*available|upon reasonable request|available from the.*author|available from the first author|available from the corresponding|will be considered by the authors|will be made available by the authors|provided upon.*request|available.*formal request|requests.*directed to the corresponding|requests.*directed to the first|on reasonable reques|requested from the.*author") ~ 3,           
           
           # Category 5, last resort: "publicly available" with no link in the extracted text
           str_detect(ds_lower, "publicly available|open source|openly available") ~ 5,
           
           # Category 1: No mention of data availability (blank field) ----------------------------
           is.na(data_sharing) ~ 1
           
         )
  )

# Check results
table(df$data_cat)

#Addition of readable tables

df <- df %>%
  mutate(
    data_cat_f = factor(
      data_cat,
      levels = 1:7,
      labels = c(
        "No mention",
        "Not available with reasoning (ethical/legal/regulatory)",
        "Upon request to authors",
        "Upon request to third party/data holder",
        "Publicly/openly available",
        "Conditional access with detailed instructions",
        "Supplementary/in-article data"
      )
    )
  )
table(df$data_cat_f)
df <- df %>%
  mutate(
    data_available = case_when(
      data_cat %in% c(1, 2) ~ 0,  # Not available (no mention, not available, not applicable)
      data_cat %in% c(3, 4, 5, 6, 7) ~ 1   # Available in some form
    )
  )


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
           str_detect(cs_lower, "github|gitlab|zenodo|figshare|cran|r package|osf\\.io|appendix|supporting information|codes.*presented|can be found|is available at|publicly available|openly available|freely available|electronic supplementary material|\\besm|online resource|included in") ~ 3,          
           
           # Category 2: Available upon request to authors
           str_detect(cs_lower, "request|will be made available|contact|available from|permission from") ~ 2,           
         
           # Category 3, last resort: a link or OSF mention with no request wording
           str_detect(cs_lower, "http|osf") ~ 3,
          
           # Category 1: Not available (blank)
           is.na(code_sharing) ~ 1
           
         )
  )


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
                 "strobe|record|tripod|consort|cioms|coreq|readus-pv|emerge|prisma|trend|stard|moose|equator|reporting.*guideline|guideline.*reporting|checklist") ~ 2,
      
      # --- Category 1: No checklist (blank) ---
      is.na(reporting) ~ 1
    )
  )


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
      str_detect(rep_lower, "coreq") ~ "COREQ",
      str_detect(rep_lower, "readus-pv") ~ "READUS-PV",
      str_detect(rep_lower, "prisma") ~ "PRISMA",
      str_detect(rep_lower, "trend") ~ "TREND",
      str_detect(rep_lower, "moose") ~ "MOOSE",
      str_detect(rep_lower, "equator") ~ "EQUATOR",
      is.na(reporting) ~ "None"
    )
  )

table(df$reporting_checklist)

#=================================================================
#CHECKPOINT: all entries without a matched category will be found from the 
#following lines of code, prints the script, and the script STOPS. 
#Untill all are matched, the script will not run all the way. 
#==================================================================
unmatched <- df %>%
  filter(if_any(c(protocol_cat, prereg_registry, data_cat, code_cat,
                  reporting_cat, reporting_checklist), is.na)) %>%
  select(authors, protocol, preregistration, data_sharing, code_sharing, reporting)

if (nrow(unmatched) > 0) {
  print(unmatched, width = Inf)
  stop("Some entries did not match a category (printed above). Add a pattern for them.")
}

#==========================================================================================
# Number of domains fulfilled.
#------------------------------------------------------------------------------------------
# A domain is considered fulfilled if the article had ANY statement
# (i.e. not category 1 / not blank) for that domain

df <- df %>%
  mutate(
    domains_fulfilled = (protocol_cat > 1) + (prereg_cat > 1) + (data_cat > 2) +
      (code_cat > 1) + (reporting_cat > 1)
  )
    

# Count how many articles fulfilled 0, 1, 2, 3, 4, 5 domains
# levels = 0:5 shows a row even if the count is 0; "-" is written, otherwise it calculated the % and rounds to one decimal.
#----------------------------------------------------------------------------------------
n_domains <- table(factor(df$domains_fulfilled, levels = 0:5))

table_domains <- tibble(
  `Number of domains fulfilled (n)` = names(n_domains),
  `Articles (n)`                    = as.integer(n_domains),
  `% of articles`                   = if_else(`Articles (n)` == 0, "-",
                                              as.character(round(`Articles (n)` / nrow(df) * 100, 1)))
)


# Total transparency; all domains (articles x 5) that were fulfilled
#----------------------------------------------------------------------------------
total_transparency <- round(sum(df$domains_fulfilled) / (nrow(df) * 5) * 100, 1)

# Add total transparency row to final table
#----------------------------------------------------------------------------------
table_domains <- bind_rows(
  table_domains,
  tibble(
    `Number of domains fulfilled (n)` = "Total transparency (%)",
    `Articles (n)`                    = NA,
    `% of articles`                   = as.character(total_transparency)
  )
)
print(table_domains)
#=============================================================================
# Frequency tables, one per domain, with a "Transparency (%)" row at the bottom.
# The Transparency row counts every article that fulfilled the domain (same rule as above).
# .drop = FALSE keeps categories with 0 articles in the table.

make_table <- function(category, fulfilled) {
  df %>%
    count(Category = category, .drop = FALSE) %>%
    mutate(
      Category   = as.character(Category),
      Percentage = round(n / nrow(df) * 100, 1)
    ) %>%
    bind_rows(tibble(
      Category   = "Transparency (%)",
      n          = sum(fulfilled),
      Percentage = round(sum(fulfilled) / nrow(df) * 100, 1)
    ))
}

table_protocol  <- make_table(df$protocol_cat_f,  df$protocol_cat > 1)
table_prereg    <- make_table(df$prereg_cat_f,    df$prereg_cat > 1)
table_data      <- make_table(df$data_cat_f,      df$data_cat > 2)
table_code      <- make_table(df$code_cat_f,      df$code_cat > 1)
table_reporting <- make_table(df$reporting_cat_f, df$reporting_cat > 1)

print(table_protocol)
print(table_prereg)
print(table_data)
print(table_code)
print(table_reporting)

# gt tables (for viewing in RStudio; the Word file below uses flextable)
make_gt <- function(tbl, title) {
  gt(tbl) %>%
    tab_header(title = title) %>%
    cols_label(Category = "Category", n = "n", Percentage = "%") %>%
    fmt_number(columns = Percentage, decimals = 1) %>%
    tab_style(
      style     = cell_text(weight = "bold"),
      locations = cells_body(rows = Category == "Transparency (%)")
    ) %>%
    opt_stylize(style = 1)
}

make_gt(table_protocol,  "Protocol")
make_gt(table_prereg,    "Pre-registration")
make_gt(table_code,      "Code sharing")
make_gt(table_reporting, "Reporting Checklist")

# Data sharing is grouped into "Available" and "Not available"
make_gt(table_data, "Data sharing") %>%
  tab_row_group(
    label = "Available",
    rows = Category %in% c("Upon request to authors",
                           "Upon request to third party/data holder",
                           "Publicly/openly available",
                           "Conditional access with detailed instructions",
                           "Supplementary/in-article data")
  ) %>%
  tab_row_group(
    label = "Not available",
    rows = Category %in% c("No mention",
                           "Not available with reasoning (ethical/legal/regulatory)")
  )
# --- Save all tables in one Word document ---
#change the path name, so we don't overwrite previous data
fix_table <- function(tbl, widths = c(4, 1, 1.5)) {
  flextable(tbl) %>%
    width(j = 1:3, width = widths) %>%
    bold(i = nrow(tbl), bold = TRUE) %>%
    hline_bottom(border = fp_border(width = 1)) %>%
    hline_top(border = fp_border(width = 1)) %>%
    border_inner_h(border = fp_border(width = 0.5))
}

save_as_docx(
  "Protocol"            = fix_table(table_protocol),
  "Pre-registration"    = fix_table(table_prereg),
  "Data sharing"        = fix_table(table_data),
  "Code sharing"        = fix_table(table_code),
  "Reporting Checklist" = fix_table(table_reporting),
  "Domains fulfilled"   = fix_table(table_domains, widths = c(3.5, 1.5, 1.5)) %>%
    align(j = 1:3, align = "center"),
  path = output_file
)