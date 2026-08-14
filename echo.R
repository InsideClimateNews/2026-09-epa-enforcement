# set working directory to the folder containing this script
setwd(dirname(rstudioapi::getActiveDocumentContext()$path))

# load required packages
library(tidyverse)
library(janitor)
library(blsAPI)
library(jsonlite)
library(httr2)  

# don't use scientific notation for numbers
options(scipen = 999)

# view available data
files <- list.files("data/case_downloads", full.names = TRUE)
# [1] "data/case_downloads/CASE_DEFENDANTS.csv"                               "data/case_downloads/CASE_ENFORCEMENT_CONCLUSION_COMPLYING_ACTIONS.csv"
# [3] "data/case_downloads/CASE_ENFORCEMENT_CONCLUSION_DOLLARS.csv"           "data/case_downloads/CASE_ENFORCEMENT_CONCLUSION_FACILITIES.csv"       
# [5] "data/case_downloads/CASE_ENFORCEMENT_CONCLUSION_POLLUTANTS.csv"        "data/case_downloads/CASE_ENFORCEMENT_CONCLUSION_SEP.csv"              
# [7] "data/case_downloads/CASE_ENFORCEMENT_CONCLUSIONS.csv"                  "data/case_downloads/CASE_ENFORCEMENT_TYPE.csv"                        
# [9] "data/case_downloads/CASE_ENFORCEMENTS.csv"                             "data/case_downloads/CASE_FACILITIES.csv"                              
# [11] "data/case_downloads/CASE_LAW_SECTIONS.csv"                             "data/case_downloads/CASE_MILESTONES.csv"                              
# [13] "data/case_downloads/CASE_PENALTIES.csv"                                "data/case_downloads/CASE_POLLUTANTS.csv"                              
# [15] "data/case_downloads/CASE_PRIORITIES.csv"                               "data/case_downloads/CASE_PROGRAMS.csv"                                
# [17] "data/case_downloads/CASE_REGIONAL_DOCKETS.csv"                         "data/case_downloads/CASE_RELATED_ACTIVITIES.csv"                      
# [19] "data/case_downloads/CASE_RELIEF_SOUGHT.csv"                            "data/case_downloads/CASE_VIOLATIONS.csv"                              
# [21] "data/case_downloads/EPA_INFORMAL_ENFORCEMENT_ACTIONS.csv"              "data/case_downloads/ICIS_FEC_EPA_INSPECTIONS.csv"          

#################
# Replication of the chart "Civil Enforcement Case Conclusions FY 2016 - FY 2025" from page 27 of the EPA report https://www.epa.gov/system/files/documents/2026-03/fy25-annual-report-enforcement-and-compliance.pdf
# This is the basis of EPA's statement that its conclusion of civil enforcement cases in FY 2025 was the highest in nine years

case_conclusions  <- read_csv(files[7]) %>%
  clean_names() %>%
  mutate(settlement_lodged_date = mdy(settlement_lodged_date),
         settlement_entered_date = mdy(settlement_entered_date))

glimpse(case_conclusions)

# Rows: 126,990
# Columns: 17
# $ activity_id                <dbl> 2206, 1942, 1877, 4933, 4934, 4942, 120, 33163, 536, 541, 3862, 3873, 1001, 1003, 1018, 1020, 39…
# $ case_number                <chr> "02-1986-0285", "02-1985-0040", "02-1983-0012", "02-1994-0019", "02-1994-0020", "02-1994-0028", …
# $ enf_conclusion_id          <dbl> 1886, 1708, 1644, 3818, 3819, 3827, 108, 21723, 495, 500, 2940, 2951, 939, 941, 956, 958, 2991, …
# $ enf_conclusion_nmbr        <dbl> 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, 1, 1, 1, 1, 2, 1, 1, 1, 1, 1, …
# $ enf_conclusion_action_code <chr> "CDC", "CDC", "CDC", "APO", "APO", "APO", "CDC", "APO", "APO", "APO", "CDC", "APO", "ACO", "FCT"…
# $ enf_conclusion_name        <chr> "TEXACO INC.", "SPECTRASERV, INC", "ALL PURPOSE ROLL LEAF", "NATIONAL ENVIRONMENTAL SAFETY CO.",…
# $ settlement_lodged_date     <date> 1994-03-24, 1995-12-20, 1985-03-14, 1993-12-29, 1993-12-29, 1993-12-15, 1989-08-14, 2001-06-22,…
# $ settlement_entered_date    <date> 1994-03-24, 1995-12-20, 1985-03-14, 1994-09-02, 1994-08-18, 1994-03-25, 1989-08-14, 2001-06-22,…
# $ settlement_fy              <dbl> 1994, 1996, 1985, 1994, 1994, 1994, 1989, 2001, 1997, 1997, 2000, 1992, 1999, 1999, 1999, 1999, …
# $ primary_law                <chr> "CERCLA", "CERCLA", "CAA", "TSCA", "RCRA", "EPCRA", "CERCLA", "EPCRA", "EPCRA", "EPCRA", "CWA", …
# $ region_code                <chr> "02", "02", "02", "02", "02", "02", "01", "05", "01", "01", "02", "02", "01", "01", "01", "01", …
# $ activity_type_code         <chr> "JDC", "JDC", "JDC", "AFR", "AFR", "AFR", "JDC", "AFR", "AFR", "AFR", "JDC", "AFR", "AFR", "AFR"…
# $ fed_penalty_assessed_amt   <dbl> NA, NA, 50000, 8250, 24500, 1000, 0, 600, 75515, 13124, 624000, 70000, NA, 600, 600, 1000, NA, 1…
# $ state_local_penalty_amt    <dbl> NA, NA, NA, NA, NA, NA, 0, 0, NA, NA, 156000, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA…
# $ sep_amt                    <dbl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ compliance_action_cost     <dbl> 0, 0, 0, 0, 0, 0, 0, 0, 0, 0, 125000, 0, 0, 0, 1000, 1000, 0, 0, 0, 0, 0, 0, 0, 0, 0, 12000000, …
# $ cost_recovery_awarded_amt  <dbl> 250000, 95000, NA, NA, NA, NA, 16000000, 0, NA, NA, NA, NA, NA, NA, NA, NA, 1791060, NA, NA, NA,…

epa_action_codes <- c("ACO", "APO", "CDC")  # the three main categories of case conclusion included in the chart

civil_enforcement_conclusions_fiscal_year <- case_conclusions %>%
  filter(enf_conclusion_action_code %in% epa_action_codes,
         settlement_fy >= 2016, settlement_fy <= 2025) %>%
  group_by(settlement_fy,enf_conclusion_action_code) %>%
  summarize(cases=n_distinct(activity_id)) %>%
  mutate(enf_conclusion_action_code = factor(enf_conclusion_action_code, ordered = TRUE, levels = c("CDC","APO","ACO")))

ggplot(civil_enforcement_conclusions_fiscal_year, aes(x=settlement_fy, y = cases, fill = enf_conclusion_action_code)) + 
  geom_col(color = NA, linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_y_continuous(labels = comma) +
  scale_x_continuous(breaks = seq(2016, 2024, by = 2)) +
  scale_fill_brewer(palette = "Dark2", name = "") +
  theme_minimal() +
  labs(title = "EPA Civil Enforcement Case Conclusions, by Fiscal Year", subtitle = "Includes administrative and judicial cases.") +
  xlab("") +
  ylab("") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")

ggsave("charts/civil_enforcement_conclusions_fiscal_year.png", width = 15, height = 10, units = "cm", bg = "white")

#################
# Analysis of new judicial cases/civil lawsuits filed by year and administration

case_milestones <- read_csv(files[12]) %>%
  clean_names() %>%
  mutate(actual_date = mdy(actual_date))

glimpse(case_milestones)

# Rows: 510,549
# Columns: 5
# $ activity_id            <dbl> 2201, 2201, 2201, 2201, 2201, 2201, 2201, 2202, 2202, 2202, 2202, 2202, 2202, 2202, 2202, 2202, 2203…
# $ case_number            <chr> "02-1986-0280", "02-1986-0280", "02-1986-0280", "02-1986-0280", "02-1986-0280", "02-1986-0280", "02-…
# $ sub_activity_type_code <chr> "FOE", "DOJ", "CMF", "CONCJ", "CLOSE", "RETRG", "REREF", "ROPNJ", "RHQ", "FOL", "FOE", "DOJ", "USA",…
# $ sub_activity_type_desc <chr> "Final Order Entered", "Referred To Dept Of Justice", "Complaint Filed With Court", "Concluded", "En…
# $ actual_date            <date> 1992-11-30, 1986-12-01, 1988-05-23, 1992-11-30, 1994-02-28, 1987-09-01, 1988-05-18, 1986-09-30, 198…

case_enforcements <- read_csv(files[9]) %>% 
  clean_names() %>%
  mutate(activity_status_date = mdy(activity_status_date),
         case_status_date = mdy(case_status_date))

glimpse(case_enforcements)

# Rows: 135,813
# Columns: 25
# $ activity_id                    <dbl> 4012, 3870, 3871, 600030304, 3971, 3972, 3973, 1495, 1498, 600029491, 600029494, 600029516, …
# $ activity_name                  <chr> "MURATTI CONSTRUCTION, INC, ET AL", "CHING MEI U.S.A. LTD", "CRAWFORD FURNITURE MANUFACTURIN…
# $ state_code                     <lgl> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ region_code                    <chr> "02", "02", "02", "03", "02", "02", "02", "01", "01", "06", "02", "09", "08", "06", "05", "0…
# $ fiscal_year                    <dbl> 1990, 1990, 1990, 2007, 1990, 1990, 1990, 2001, 2001, 2007, 2007, 1992, 2007, 2007, 2007, 20…
# $ case_number                    <chr> "02-1990-0398", "02-1990-0137", "02-1990-0138", "03-2007-0043", "02-1990-0277", "02-1990-027…
# $ case_name                      <chr> "MURATTI CONSTRUCTION, INC, ET AL", "CHING MEI U.S.A. LTD", "CRAWFORD FURNITURE MANUFACTURIN…
# $ activity_type_code             <chr> "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "AFR", "…
# $ activity_type_desc             <chr> "Administrative - Formal", "Administrative - Formal", "Administrative - Formal", "Administra…
# $ activity_status_code           <chr> "CLS", "CLS", "CLS", "FOI", "CLS", "CLS", "CLS", "CLS", "FOI", "CLS", "CLS", "FOI", "CLS", "…
# $ activity_status_desc           <chr> "Closed", "Closed", "Closed", "Final Order Issued", "Closed", "Closed", "Closed", "Closed", …
# $ activity_status_date           <date> 1990-05-14, 1990-09-20, 1991-03-21, 2007-02-15, 1991-07-01, 1991-07-01, 1991-12-09, 2001-08…
# $ lead                           <chr> "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "EPA", "…
# $ case_status_date               <date> 1990-05-14, 1990-09-20, 1991-03-21, 2007-02-15, 1991-07-01, 1991-07-01, 1991-12-09, 2001-08…
# $ doj_docket_nmbr                <chr> NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, …
# $ enf_outcome_code               <chr> "ECR", "ECP", "ECP", "ECP", "ESA", "ESA", "ECP", "ESA", "ECP", "EUO", "EUO", "ECN", "ECN", "…
# $ enf_outcome_desc               <chr> "Final Order With Specified Cost Recovery", "Final Order With Penalty", "Final Order With Pe…
# $ total_penalty_assessed_amt     <dbl> NA, 5100, 51000, NA, NA, NA, 5000, NA, 42120, NA, NA, NA, NA, NA, NA, NA, 11500, NA, NA, NA,…
# $ total_cost_recovery_amt        <dbl> 1, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, NA, N…
# $ total_comp_action_amt          <dbl> NA, NA, NA, NA, NA, NA, NA, NA, 7539, NA, NA, NA, NA, NA, NA, NA, NA, 250, 250, 250, 250, 25…
# $ hq_division                    <chr> "CER", "TOX", "TOX", NA, "AIR", "AIR", "WAT", "WAT", "RCR", NA, NA, NA, NA, NA, NA, NA, "RCR…
# $ branch                         <chr> "NYSUP", "AWTS", "AWTS", NA, "AWTS", "AWTS", "WGGL", "OES", "OES", "6EN-W", "NJSUP", NA, NA,…
# $ voluntary_self_disclosure_flag <chr> "N", "N", "N", "N", "N", "N", "N", "Y", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N", "N…
# $ multimedia_flag                <chr> "N", "N", "N", NA, "N", "N", "N", "N", "N", NA, NA, NA, NA, NA, NA, NA, "N", "N", "N", "N", …
# $ enf_summary_text               <chr> "CONSENT ORDER WITH 3 RESPONDENTS FOR REMOVAL ACTION.", "FACILITY ADDRESS:  350 FIFTH AVE.  …

judicial <- case_enforcements %>%
  filter(activity_type_code == "JDC")

judicial_initiation_dates <- case_milestones %>%
  filter(case_number %in% judicial$case_number & sub_activity_type_desc == "Complaint Filed With Court") %>%
  mutate(actual_date = as.Date(actual_date, format = "%m/%d/%Y")) %>%
  group_by(case_number) %>%
  slice_min(actual_date, n = 1, with_ties = FALSE) %>%  # earliest, in case of amendments/re-filings
  ungroup()

judicial_initiation_periods <- judicial_initiation_dates %>%
  mutate(
    # if the date falls on/after Jan 20 of its calendar year, the period 
    # started that Jan 20; otherwise it started Jan 20 of the prior year
    period_start_year = if_else(
      actual_date >= make_date(year(actual_date), 1, 20),
      year(actual_date),
      year(actual_date) - 1
    ),
    period_start = make_date(period_start_year, 1, 20),
    period_end = period_start + years(1) - days(1)
  ) %>%
  filter(period_start_year >= 2001)

# counts new civil lawsuits filed by period
judicial_initiated_by_period <- judicial_initiation_periods %>%
  count(period_start_year, period_start, period_end, name = "n_cases_initiated") %>%
  arrange(period_start_year)

administration_lookup <- tibble(
  period_start_year = 2001:2026,
  administration = case_when(
    period_start_year %in% 2001:2004 ~ "G.W. Bush",
    period_start_year %in% 2005:2008 ~ "G.W. Bush",
    period_start_year %in% 2009:2012 ~ "Obama",
    period_start_year %in% 2013:2016 ~ "Obama",
    period_start_year %in% 2017:2020 ~ "Trump",
    period_start_year %in% 2021:2024 ~ "Biden",
    period_start_year %in% 2025:2028 ~ "Trump"
  ),
  admin_year = case_when(
    period_start_year %in% 2001:2004 ~ period_start_year - 2000,
    period_start_year %in% 2005:2008 ~ period_start_year - 2004,
    period_start_year %in% 2009:2012 ~ period_start_year - 2008,
    period_start_year %in% 2013:2016 ~ period_start_year - 2012,
    period_start_year %in% 2017:2020 ~ period_start_year - 2016,
    period_start_year %in% 2021:2024 ~ period_start_year - 2020,
    period_start_year %in% 2025:2028 ~ period_start_year - 2024
  )
)

judicial_initiated_by_period <- judicial_initiated_by_period %>%
  left_join(administration_lookup, by = "period_start_year") %>%
  mutate(administration = factor(administration, 
                                 levels = c("G.W. Bush","Obama","Trump","Biden"),
                                 ordered = TRUE))

write_csv(judicial_initiated_by_period, "processed_data/judicial_initiated_by_period.csv", na = "")

ggplot(judicial_initiated_by_period, aes(x = period_start_year, y = n_cases_initiated, fill = administration)) +
  geom_col(color = NA, linewidth = 1) +
  geom_hline(yintercept = 0, linewidth = 0.3) +
  scale_x_continuous(breaks = seq(2001,2025, by = 2)) +
  scale_fill_manual(values = c("#f4a582","#92c5de","#ca0020","#0571b0"), name = "") +
  theme_minimal() +
  labs(title = "Civil Complaints Filed, by Year", subtitle = "Years start Jan. 20 to coincide with adminstration years.") +
  xlab("") +
  ylab("") +
  theme(panel.grid.minor.x = element_blank(),
        panel.grid.major.x = element_blank(),
        legend.position = "top")

ggsave("charts/judicial_initiated_by_year_adminstration.png", width = 15, height = 10, units = "cm", bg = "white")

####################
# CPI data for inflation corrections

bls_key <- Sys.getenv("BLS_KEY") # you will need your own BLS API key

year_blocks <- list(c(2001, 2020), c(2021, 2026)) # must run in two blocks given API limits

cpi_data <- map_df(year_blocks, function(years) {
  payload <- list(
    'seriesid'        = 'CUUR0000SA0',
    'startyear'       = as.character(years[1]),
    'endyear'         = as.character(years[2]),
    'registrationkey' = bls_key
  )
  
  blsAPI(payload, 2) %>% 
    fromJSON() %>% 
    pluck("Results", "series", "data", 1) %>% 
    transmute(year = as.numeric(year), cpi = as.numeric(value))
}) %>% 
  group_by(year) %>% 
  summarize(cpi = mean(cpi, na.rm = TRUE))

# extract the 2025 value for conversion to 2025 dollars below
cpi_2025 <- cpi_data %>% 
  filter(year == 2025) %>% 
  pull(cpi)

######################
# Analysis of EPA administrative orders by presidential administration

admin_orders_bush_onward <- case_conclusions %>%
  filter(settlement_entered_date >= "2001-01-20" & grepl("APO|ACO",enf_conclusion_action_code)) %>%
  mutate(year = year(settlement_entered_date)) %>%
  left_join(cpi_data, by = "year") %>%
  mutate(fed_penalty_2025_dollars = fed_penalty_assessed_amt * (cpi_2025 / cpi),
         state_local_penalty_2025_dollars = state_local_penalty_amt * (cpi_2025 / cpi),
         sep_2025_dollars = sep_amt * (cpi_2025 / cpi),
         compliance_cost_2025_dollars = compliance_action_cost * (cpi_2025 / cpi),
         cost_recovery_2025_dollars = cost_recovery_awarded_amt * (cpi_2025 / cpi),
         total_costs_2025_dollars = rowSums(
           across(c(fed_penalty_2025_dollars, state_local_penalty_2025_dollars, sep_2025_dollars,
                    compliance_cost_2025_dollars, cost_recovery_2025_dollars)),
           na.rm = TRUE
         ),
         total_costs_excl_cost_recovery_2025_dollars = rowSums(
           across(c(fed_penalty_2025_dollars, state_local_penalty_2025_dollars, sep_2025_dollars,
                    compliance_cost_2025_dollars)),
           na.rm = TRUE
         ),
         administration = case_when(settlement_entered_date < "2005-01-20" ~ "G.W. Bush 1",
                                    settlement_entered_date < "2009-01-20" ~ "G.W. Bush 2",
                                    settlement_entered_date < "2013-01-20" ~ "Obama 1",
                                    settlement_entered_date < "2017-01-20" ~ "Obama 2",
                                    settlement_entered_date < "2021-01-20" ~ "Trump 1",
                                    settlement_entered_date < "2025-01-20" ~ "Biden",
                                    settlement_entered_date >= "2025-01-20" ~ "Trump 2"),
         administration = factor(administration,
                                 ordered = TRUE,
                                 levels = c("G.W. Bush 1","G.W. Bush 2","Obama 1","Obama 2","Trump 1", "Biden", "Trump 2"))) 

admin_orders_summary_with_costs <- admin_orders_bush_onward %>%
  count(administration, enf_conclusion_action_code) %>%
  group_by(administration) %>%
  mutate(pct = round(n / sum(n) * 100, 1)) %>%
  ungroup() %>%
  select(administration, enf_conclusion_action_code, n, pct) %>%
  pivot_wider(names_from = enf_conclusion_action_code, values_from = c(n, pct)) %>%
  left_join(
    admin_bush_onward %>%
      group_by(administration) %>%
      summarize(
        median_total_cost_excl_cost_recovery = median(total_costs_excl_cost_recovery_2025_dollars, na.rm = TRUE),
        .groups = "drop"
      ),
    by = "administration"
  ) %>%
  mutate(administration = factor(administration, ordered = TRUE,
                                 levels = c("G.W. Bush 1", "G.W. Bush 2", "Obama 1","Obama 2","Trump 1", "Biden", "Trump 2"))) %>%
  arrange(administration)

write_csv(admin_orders_summary_with_costs,"processed_data/admin_orders_summary_with_costs.csv", na = "")

admin_orders_summary_with_costs

# A tibble: 7 × 6
# administration n_ACO n_APO pct_ACO pct_APO median_total_cost_excl_cost_recovery
# <ord>          <int> <int>   <dbl>   <dbl>                                <dbl>
# 1 G.W. Bush 1     5906  6721    46.8    53.2                                6125.
# 2 G.W. Bush 2     5837 11148    34.4    65.6                                5357.
# 3 Obama 1         4723  7200    39.6    60.4                                7677.
# 4 Obama 2         3136  5542    36.1    63.9                               13125.
# 5 Trump 1         2453  4008    38      62                                 11071.
# 6 Biden           2707  3824    41.4    58.6                                7940.
# 7 Trump 2         2255  1430    61.2    38.8                                1146.


#################
# Analysis of supplemental environmental projects

admin_orders_bush_onward %>%
  filter(sep_2025_dollars > 0) %>%
  group_by(administration) %>%
  summarize(sep_projects = n(),
            total_sep_2025_dollars = sum(sep_2025_dollars,na.rm = TRUE),
            mean_sep_2025_dollars = mean(sep_2025_dollars,na.rm = TRUE))

# # A tibble: 7 × 4
# administration sep_projects total_sep_2025_dollars mean_sep_2025_dollars
# <ord>                 <int>                  <dbl>                 <dbl>
# 1 G.W. Bush 1             492              89028134.               180951.
# 2 G.W. Bush 2             667              98011427.               146944.
# 3 Obama 1                 423              56841173.               134376.
# 4 Obama 2                 400              76105498.               190264.
# 5 Trump 1                 238              48176790.               202423.
# 6 Biden                    97              18921860.               195071.
# 7 Trump 2                  13               3040582.               233891.

admin_orders_bush_onward %>%
  filter(sep_2025_dollars > 0) %>%
  summarize(latest_settlement = max(settlement_entered_date, na.rm = TRUE))# A tibble: 1 × 1

# # A tibble: 1 × 1
# latest_settlement
# <date>           
#   1 2025-08-29  

#################
# Further analysis of Trump 2 administrative compliance orders, focusing on sections 1412/1414 of the Safe Drinking Water Act
# Identified as a significant proportion of current EPA administrative actions in this Environmental Integrity Project report:
# https://environmentalintegrity.org/wp-content/uploads/2026/02/EIP_Report_2025EnvironmentalEnforcement_2.5.26.pdf

case_law_sections <- read_csv(files[11]) %>%
  clean_names()

sdwa_1412_1414 <- case_law_sections %>%
  filter(statute_code == "SDWA" & law_section_code %in% c("1412", "1414", "1412/1414"))
    
trump2_sdwa_1412_1414 <- admin_bush_onward %>%
  filter(administration == "Trump 2" & case_number %in% sdwa_1412_1414$case_number)

# get case summaries from ECHO Case Enforcement API 

case_summaries <- tibble()

checkpoint_file <- "epa_cases_checkpoint.rds"

n <- 1

for (c in trump2_sdwa_1412_1414$case_number) {
  print(paste(n, c))
  
  # initialize control variables for this specific case
  attempt_success <- FALSE
  
  repeat {
    resp <- tryCatch({
      request("https://echodata.epa.gov/echo/case_rest_services.get_case_report") %>%
        req_url_query(output = "JSON", p_id = c) %>%
        # keep the built-in throttle steady
        req_throttle(capacity = 15, fill_time_s = 60) %>%   
        req_perform()
    }, error = function(e) {
      message("Request error for ", c, ": ", conditionMessage(e))
      NULL
    })
    
    case_summary <- NA_character_
    rate_limited <- FALSE
    
    # check for structural errors/NULL response
    if (is.null(resp)) {
      # treat code errors/network drops as a trigger to back off and retry
      rate_limited <- TRUE
    } else {
      status <- resp_status(resp)
      body_text <- resp_body_string(resp)
      
      # detect rate limiting or HTTP errors
      if (status == 429 || status >= 500 || grepl("rate limit|too many requests|throttl", body_text, ignore.case = TRUE)) {
        rate_limited <- TRUE
      } else {
        parsed <- tryCatch(fromJSON(body_text, flatten = TRUE), error = function(e) NULL)
        if (!is.null(parsed$Results$CaseInformation$CaseSummary)) {
          case_summary <- parsed$Results$CaseInformation$CaseSummary
        }
        # if no errors, mark this ID as successfully fetched
        attempt_success <- TRUE
      }
    }
    
    if (rate_limited) {
      consecutive_failures <- consecutive_failures + 1
      wait_min <- min(5 * consecutive_failures, 30)  # escalating backoff, capped at 30 min
      
      message("Rate limited or error at n=", n, " (case ", c, "). Saving checkpoint and waiting ",
              wait_min, " minutes...")
      
      saveRDS(case_summaries, checkpoint_file)
      Sys.sleep(wait_min * 60)
      
      message("Retrying case ", c, " now...")
    }
    
    # if successful, break the repeat loop and move forward to save data
    if (attempt_success) {
      break
    }
  }
  
  # reset failure counter upon actual data retrieval success
  consecutive_failures <- 0
  tmp <- tibble(case_identifier = c, summary = case_summary)
  case_summaries <- bind_rows(case_summaries, tmp)
  n <- n + 1
}


# add fields to recognize keywords
case_summaries <- case_summaries %>%
  mutate(lead = case_when(grepl("lead",summary,ignore.case = TRUE) ~ TRUE,
                          TRUE ~ FALSE),
         copper = case_when(grepl("copper",summary,ignore.case = TRUE) ~ TRUE,
                            TRUE ~ FALSE),
         inventory = case_when(grepl("inventory",summary,ignore.case = TRUE) ~ TRUE,
                               TRUE ~ FALSE),
         service_line = case_when(grepl("service line",summary,ignore.case = TRUE) ~ TRUE,
                                  TRUE ~ FALSE))

# analysis by keyword
case_summaries %>%
  group_by(lead) %>%
  count()
# # A tibble: 2 × 2
# # Groups:   lead [2]
# lead      n
# <lgl> <int>
# 1 FALSE   599
# 2 TRUE    945

case_summaries %>%
  group_by(copper) %>%
  count()
# # A tibble: 2 × 2
# # Groups:   copper [2]
# copper     n
# <lgl>  <int>
#   1 FALSE    995

case_summaries %>%
  group_by(inventory) %>%
  count()
# # A tibble: 2 × 2
# # Groups:   inventory [2]
# inventory     n
# <lgl>     <int>
# 1 FALSE       257
# 2 TRUE       1287

case_summaries %>%
  group_by(service_line) %>%
  count()
# # A tibble: 2 × 2
# # Groups:   service_line [2]
# service_line     n
# <lgl>        <int>
# 1 FALSE          257
# 2 TRUE          1287

lsli <- case_summaries %>%
  filter(service_line == TRUE)

admin_bush_onward %>%
  filter(case_number %in% lsli$case_identifier) %>%
  summarize(median_cost = median(total_costs_excl_cost_recovery_2025_dollars, na.rm = TRUE))

# # A tibble: 1 × 1
# median_cost
# <dbl>
# 1         557.

# Conclusion; More than 1,200 of these orders relate to failure to compile lead service line inventories, 
# representing more than a third of all administrative orders from the current administration. The median cost was just $557