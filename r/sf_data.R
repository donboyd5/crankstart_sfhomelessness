
# https://hsh.sfgov.org/get-involved/2022-pit-count/
# library(htmltab)

# url <- "https://en.wikipedia.org/w/index.php?title=Languages_of_the_United_Kingdom&oldid=1005083039"
# ukLang <- htmltab(doc = url, which = "//th[text() = 'Ability\n']/ancestor::table")
# head(ukLang)
# ukLang

# url <- "https://hsh.sfgov.org/about/research-and-reports/hrs-data/coordinated-entry-demographics/"
# 
# tab <- htmltab(doc=url)
# tab
# tab


# links -------------------------------------------------------------------
# scorecards
# https://sfgov.org/scorecards/safety-net/direct-homeless-exits-through-city-programs


# controller
# spendrev https://data.sfgov.org/api/views/bpnb-jwfb/rows.csv?accessType=DOWNLOAD
# https://data.sfgov.org/City-Management-and-Ethics/Budget-FTE/4zfx-f2ts  staffing
# https://data.sfgov.org/City-Management-and-Ethics/Employee-Compensation/88g8-5mnd  compensation
# https://data.sfgov.org/City-Management-and-Ethics/Spending-FTE/43jt-u7yn spending fte
# https://data.sfgov.org/City-Management-and-Ethics/Budget/xdgd-c79v budget
# Vendor Payments (Purchase Order Summary):
# https://data.sfgov.org/City-Management-and-Ethics/Vendor-Payments-Purchase-Order-Summary-/p5r5-fd7g

# Purchasing Commodity Data:
# https://data.sfgov.org/City-Management-and-Ethics/Purchasing-Commodity-Data/ebsh-uavg

# Vendor Payments (Vouchers):
# https://data.sfgov.org/City-Management-and-Ethics/Vendor-Payments-Vouchers-/n9pm-xkyq

# San Francisco Citywide Performance Metrics 
# https://data.sfgov.org/City-Management-and-Ethics/San-Francisco-Citywide-Performance-Metrics/6dpt-7w23

# Department Code Lookup
# https://data.sfgov.org/City-Management-and-Ethics/Department-Code-Lookup/3r4n-zfvm

# Supplier Contracts (includes grants) **********
# https://data.sfgov.org/City-Management-and-Ethics/Supplier-Contracts/cqi5-hm2d

# Reference: Department Code List
# https://data.sfgov.org/City-Management-and-Ethics/Reference-Department-Code-List/j2hz-23ps

# Other

# Tenderloin Tent Count (daily) ONLY THROUGH JUNE 2021 ??? Why?
# https://data.sfgov.org/COVID-19/Tenderloin-Tent-Count/ubjr-y9qw







# libraries ---------------------------------------------------------------
source(here::here("r", "libraries.r"))
source(here::here("r", "libraries_ts.r"))

# constants ---------------------------------------------------------------

ob <- here::here("data", "open_book")


# spending and revenue ----------------------------------------------------------------

xrev <- readRDS(path(ob, "spending_and_revenue.rds"))
scard <- readRDS(path(ob, "scorecards.rds"))
departments <- readRDS(path(ob, "departments.rds"))

count(xrev, udepartment, deptcode, department)  
count(xrev, deptcode, udepartment, department)
count(xrev, deptcode, udepartment)  


hom <- xrev |> 
  filter(deptcode=="HOM")

hom |> 
  filter(fyear==2022, revspend=="Spending") |> 
  summarise(amount=sum(amount))

hom |> 
  filter(revspend=="Spending") |> 
  group_by(fyear) |> 
  summarise(amount=sum(amount))

hom |> 
  filter(fyear==2022, revspend=="Spending") |>
  group_by(ftcode, ftype) |> 
  summarise(amount=sum(amount))



# objcode             object                              amount
# <chr>               <chr>                                <dbl>
#   1 538010              Community Based Org Srvcs      297478616. 
# 2 BLD_STR_IMP         Bldg: Structures/Improvements  168935384. 
# 3 536520              Rent Assist-Behalf Of Clients   30811851. 
# 4 581670              GF-Mayor'S - Cdbg               19200171. 
#  5 PROF_SPEC_SVC       Professional/Specialized Svcs   18170397. 
#  6 PERM_SAL_MISC       Permanent Salaries - Misc       17126020. 
#  7 RENTLSE_BLDG_STR    Rent/Lease-Building/Structure   11009869. 
#  8 581540              GF-Mental Health                 7217854. 

tmp <- hom |> 
  filter(fyear==2022, revspend=="Spending") |> 
  arrange(desc(amount))


# explore scorecord measures ----


count(df3 |> filter(deptcode=="HOM"), mcode, mtitle, monthly)
count(df3, goal)
count(df3, deptcode, department)
count(df3, deptcode, department, goal)

# measure shortnames
longnames <- df3 |> count(mcode, mtitle)
mcodes <- read_csv("
mcode, mshort
266, nhbound
7005, nmaintain
7054, nexit_rrh
7055, nexit_psh
")

df4 <- df3 |> 
  left_join(mcodes, by="mcode")

df4 |> 
  filter(deptcode=="HOM", mshort=="nexit_psh") |> 
  select(mcode, mshort, date, category, actual) |> 
  arrange(date, actual) |> 
  pivot_wider(names_from = category, values_from = actual) |> 
  mutate(TGT=TGT / 12)

df4 |> 
  filter(deptcode=="HOM", category=="ACT") |>
  ggplot(aes(date, actual, colour=mshort)) +
  geom_line() +
  geom_point()


df4 |> 
  filter(deptcode=="HOM", category=="ACT", !is.na(actual), 
         mshort %in% c("nhbound", "nexit_rrh", "nexit_psh")) |>
  select(date, mshort, actual) |>
  group_by(mshort) |> 
  mutate(ma12=roll_sum(actual, n=12, fill=NA, align="left")) |> 
  ungroup() |> 
  filter(!is.na(ma12)) |> 
  arrange(date) |>
  ggplot(aes(date, ma12, colour=mshort)) +
  geom_line() +
  geom_point() +
  scale_y_continuous(name="# of individuals exiting") +
  ggtitle("HSH Performance Measures: Exits From Homelessness",
          subtitle="Rolling 12-month totals") +
  theme_bw()






