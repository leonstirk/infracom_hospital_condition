library(tidyverse)
library(viridis)
library(emoa)
library(rlang)


#hosp <- readxl::read_excel("Research Analyst - Pre Interview Dataset.xlsx")
hosp <- read_csv('Research Analyst - Pre Interview Dataset.csv')

names(hosp) <- hosp %>% names %>% snakecase::to_snake_case()

# Data cleaning ops

hosp %>% names()

# hosp$hart_id %>% anyNA()
# hosp[which(is.na(hosp$hart_id)),] # Some missing IDs
# # Not an issue

# hosp$region %>% anyNA() # Region is at least complete
# hosp$region %>% table

# hosp$dhb_id %>% anyNA() # Complete
# hosp$dhb_id %>% as.factor() %>% levels()

# hosp$dhb_name %>% anyNA() # DHB name is also complete
# hosp$dhb_name %>% table

# hosp$campus_id %>% anyNA()
# hosp$campus_id %>% table
# hosp[which(is.na(hosp$campus_id)),'campus_name']
# # Some missing campus IDs seem to correspond to campus_name = 'Community - GP'
# # But specifically those with dhb_name == 'West Coast'
# hosp[which(hosp$campus_name == 'Community - GP'),] %>% print(n = 11)
# # I don't think this is an issue - maybe drop campus_id entirely
hosp <- hosp %>% select(!campus_id)

# hosp$campus_name %>% anyNA() # campus_name also complete
# hosp$campus_name %>% table # Ok this looks good too no bogus values
# # Use the campus name rather than ID to allocate funding

# hosp$building_name %>% anyNA() # HAS NAs WTF
# hosp[which(is.na(hosp$building_name)),] %>% View # Ok this shouldn't be an issue but I shouldn't just drop on all NAs
# # This shouldn't be an issue either but note that combo of campus_name, building_name give a unique ID?

# hosp$highest_building_service %>% anyNA() # No proper NAs
# hosp[which(is.na(hosp$highest_building_service)),] # Ok so there are NAs '#N/A's and 'Nil's in this var
# hosp$highest_building_service %>% as.factor() %>% levels()
# # # recode '#N/A' and 'Nil' to NA
# hosp %>%
#   mutate(highest_building_service = na_if(highest_building_service, '#N/A')) %>%
#   mutate(highest_building_service = na_if(highest_building_service, 'Nil')) %>%
#   pull(highest_building_service) %>% table

hosp <- hosp %>%
  mutate(highest_building_service = na_if(highest_building_service, '#N/A')) %>%
  mutate(highest_building_service = as.factor(na_if(highest_building_service, 'Nil')))

# hosp$year_built %>% hist() # Weirdly this is the only thing that looks kind of good.

# hosp$number_of_storeys_ground_floor_and_up %>% anyNA()
# hosp$number_of_storeys_ground_floor_and_up %>% table # Lol ok so this has one text 'NA' and a bunch of actual NAs
# # I should probably also code the 'Not NMH' and 'NOT NMH' consistently together.
# hosp$number_of_storeys_ground_floor_and_up %>% str_replace('Not NMH', 'NOT NMH') %>% table
hosp$number_of_storeys_ground_floor_and_up <- hosp$number_of_storeys_ground_floor_and_up %>% str_replace('Not NMH', 'NOT NMH')
# which(hosp$number_of_storeys_ground_floor_and_up == 'NA')
# which(is.na(hosp$number_of_storeys_ground_floor_and_up))
# # Just coerce the NAs when you convert to numeric
#
# hosp$basement_floors_floors_below_ground_floor %>% anyNA()
# hosp$basement_floors_floors_below_ground_floor %>% as.factor() %>% levels # '-' to NA explicitly and then covert to integer
#
# hosp$total_floors %>% anyNA() # Has NAs
# hosp$total_floors %>% table # Also need to recode '-' to NA
#
# hosp$number_of_storeys_ground_floor_and_up
# hosp$basement_floors_floors_below_ground_floor

hosp <- hosp %>% mutate(number_of_storeys_ground_floor_and_up = as.numeric(number_of_storeys_ground_floor_and_up),
                basement_floors_floors_below_ground_floor = as.numeric(basement_floors_floors_below_ground_floor),
                total_floors = as.numeric(total_floors))
#
# # Also should check that floors add up lol
# df <- hosp %>% mutate(number_of_storeys_ground_floor_and_up = as.numeric(na_if(number_of_storeys_ground_floor_and_up, "-")),
#                       basement_floors_floors_below_ground_floor = as.numeric(na_if(basement_floors_floors_below_ground_floor, "-")),
#                       check_floor_count = number_of_storeys_ground_floor_and_up + basement_floors_floors_below_ground_floor,
#                       total_floors  = as.numeric(na_if(total_floors, "-")))
# # The warning will be the other text values ('NOT NMH' , 'NA') but I should deal with them explicitly.
#
# # Ok so at least the floor counts add up
# which(df$check_floor_count != df$total_floors)


# hosp$gfa %>% anyNA() # This has NAs once converted to numeric
# # Explicity handle '-' to avoid coercing NAs
hosp <- hosp %>% mutate(gfa = as.numeric(na_if(gfa, '-')))


# hosp$condition_grade %>% anyNA() # No proper NAs
# hosp$condition_grade %>% table # Explicitly handle 'NO DATA' -> NA
# hosp %>% mutate(condition_grade = na_if(condition_grade, 'NO DATA')) %>% pull(condition_grade) %>% table
hosp <- hosp %>% mutate(condition_grade = na_if(condition_grade, 'NO DATA'))

# # Convert condition to numeric
# # Also need to check that every condition entry has a corresponding condition grade.
# # OK so there is consistency here but need to change the NO DATA and I don't know do ordering or something?
# # I think I'm just going to use the grades for the pareto ordering, might be easier to convert to an integer scale.
# hosp$condition %>% anyNA()
# hosp$condition %>% str_replace_all("[\\d+.]", "-") %>% str_replace_all("-", "") %>% as.factor() %>% levels()
# # Ok so the only non-numeric value is 'NO DATA' so that's fine.
hosp <- hosp %>% mutate(condition = as.numeric(na_if(condition, 'NO DATA')))
# # condition_grade as ordered factor
condition_levels <- c("Very Good","Good","Average","Poor","Very Poor")
hosp <- hosp %>% mutate(condition_grade = factor(condition_grade, levels = condition_levels, ordered = TRUE))
rm(condition_levels)
# hosp$condition_grade
ggplot(data = hosp, aes(y = condition, x = condition_grade)) + geom_point()

#hosp$nbs %>% anyNA()
#hosp$nbs %>% str_replace_all("[\\d+.]", "-") %>% str_replace_all("-", "") %>% table()
hosp <- hosp %>%
  mutate(nbs = na_if(nbs, 'Unknown')) %>%
  mutate(nbs = na_if(nbs, 'NULL')) %>%
  mutate(nbs = as.numeric(na_if(nbs, 'NSA')))
# Ok this is a bit of a mess there are some miscodings and 'NULL' and 'Unknown'
# Need to work out some way of dealing with this
# Then convert to numeric
# hosp$nbs %>% str_replace_all("[\\d+.]", "-") %>% table()

# hosp$graded_nbs %>% anyNA()
# hosp$graded_nbs %>% table
# hosp[which(is.na(hosp$graded_nbs)),'graded_nbs']
# Ordered categorical variable - do or change to integer score
grade_levels <- c("A+","A","B","C","D","E")
hosp <- hosp %>%
  mutate(graded_nbs = factor(graded_nbs, levels = grade_levels, ordered = TRUE))
rm(grade_levels)

# hosp$seismic_il %>% anyNA() # Complete at least
#hosp$seismic_il %>% table # need to explicitly handle '-' and miscoded? 'S'
hosp <- hosp %>%
  mutate(seismic_il = na_if(seismic_il, '-')) %>%
  mutate(seismic_il = na_if(seismic_il, 'S'))
## I've just elected to NA the 'S' coding as I don't think the building itself is all that important
# And then turn into an ordered factor variable
importance_levels <- c("IL1" ,"IL2", "IL3", "IL4")
hosp <- hosp %>% mutate(seismic_il = factor(seismic_il, levels = importance_levels, ordered = TRUE))
rm(importance_levels)

# hosp$seismic_zone %>% anyNA() # Some missing values
# hosp$seismic_zone %>% table
# seismic_il as ordered factor
seismic_levels <- c("Low Zone","Medium Zone","High Zone")
hosp <- hosp %>% mutate(seismic_zone = factor(seismic_zone, levels = seismic_levels, ordered = TRUE))
rm(seismic_levels)
# hosp$seismic_zone

# Ok data should be pretty clean now
# saveRDS(hosp, 'data/hosp_clean.rds')


################################################################################################################################
################################################################################################################################

# Data descriptives/assumptions/appendix etc

hosp <- readRDS('data/hosp_clean.rds')

# Missing nbs, condition variables
# are there correlations with other variables e.g. seismic_il
# e.g. 48 IL4 has indeterminate nbs
# Identify this as a data problem
table(is.na(hosp$nbs), hosp$seismic_il)

# There is also a lot of building without condition data including 48 IL4... Coincidence?
table(is.na(hosp$condition_grade), hosp$seismic_il)

# Check to see what the deal is with those 48 IL4s
# Are the missing nbs the same units as the missing condition assessments?

# Maybe I should use a graded_nbs
# WHY are there 2 NA values for graded_nbs clean and make consistent
# Yes definitley use
ggplot(data = hosp, aes(x = graded_nbs, y = nbs)) + geom_point() + geom_hline(yintercept = 0.34)

# Also 48 IL4s that do not have a graded NBS value
table(is.na(hosp$graded_nbs), hosp$seismic_il)

# Building service by seismic importance
table(hosp$highest_building_service, hosp$seismic_il)

# Look individually at the 48 IL4 buildings with missing condition and nbs data.
# Ok so the missing nbs observations are not the same as the missing condition observations
hosp %>% filter(seismic_il == "IL4" & (is.na(condition) | is.na(nbs))) %>%  arrange(desc(seismic_zone), nbs, desc(condition)) %>% View()

# Ok so this is an important missing data issue that I need to flag
# e.g. there are values that are poor condition and IL4 but don't have an NBS assessment.
# So there need to be some directed assessments as a part of the investment package.

################################################################################################################################
################################################################################################################################

# Section 2

library(tidyverse)
library(huxtable)

# Inputs
vars       <- c("condition", "gfa", "nbs", "seismic_il", "seismic_zone")
nice_names <- c("Condition", "Gross Floor Area", "% NBS", "Seismic Importance Level", "Seismic Zone")

# Named vectors: counts & percents
na_n <- setNames(sapply(hosp[vars], \(x) sum(is.na(x))), nice_names)
na_p <- setNames(na_n / nrow(hosp) * 100,                 nice_names)

# Create wide matrix: 2 rows (n, %) × variables as columns
df_wide <- rbind(
  "Variable"   = nice_names,
  "Missing (n)" = as.integer(na_n),
  "Missing (%)" = sprintf("%.1f", na_p)  # keep as char so we can append % below
)

# Add % symbol to the second row
df_wide[3, ] <- paste0(df_wide[3, ], "\\%")

# Convert to huxtable
ht <- hux(as.data.frame(df_wide), add_colnames = FALSE) %>%
  add_rownames("Measure") %>%
  set_escape_contents(everywhere, everywhere, FALSE) %>%
  set_lr_padding(everywhere, everywhere, 2) %>%
  set_tb_padding(everywhere, everywhere, 8)

ht

# Styling
top_border(ht)[1, ]              <- brdr(1, style = "double")
bottom_border(ht)[c(1, nrow(ht)),] <- 1
bold(ht)[1, ]                    <- TRUE
valign(ht)                       <- "middle"
position(ht)                     <- "center"
align(ht)                        <- "center"
width(ht)                        <- 0.9   # a bit wider for wide format
col_width(ht)                    <- c(0.25, rep(0.15, length(nice_names)))

# In LaTeX/PDF output:
print_latex(ht, tabular_only = TRUE)

ht




# Condition profile
# Should order condition_grade before summarising
hosp %>%
  count(condition_grade) %>%
  mutate(share = n / sum(n))

# Age profile
hosp %>%
  ggplot(aes(x = year_built)) +
  geom_histogram(binwidth = 5, fill = "steelblue") +
  labs(title = "Distribution of Hospital Building Construction Years")

# Poor condition buildings
hosp %>%
  filter(condition >= 3) %>%
  select(region, campus_name, building_name,
         condition, condition_grade, year_built, gfa, nbs, seismic_il) %>%
  arrange(desc(condition))

# Crosstab of condition grade and seismic IL
table(hosp$condition_grade, hosp$seismic_il)

################################################################################################################################
################################################################################################################################

# Section 3



# Condition vs. NBS for high importance buildings
# Maybe facet by seismic_il for IL4 and IL3
# Apply theme_minimal

hosp %>%
  filter(seismic_il %in% c('IL4', 'IL3')) %>%
  ggplot(aes(x = condition, y = nbs, color = seismic_zone)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0.34, linetype = "dashed", color = "red") +  # Below 34% NBS = Earthquake-prone
  labs(title = "High-importance buildings: Condition vs Seismic Strength") %>%
  scale_fill_viridis_d()



################################################################################################################################
################################################################################################################################

# Pareto banding

dat <- hosp

# Maybe have pre-filtering based on highest_building_service and see if it correlates to some other variable like importance
# Not really
table(hosp$seismic_il, hosp$highest_building_service)

# 0) add a stable row id
dat <- dat %>% mutate(.rowid = row_number())

# Handle NAs upfront (you can change this)
dat <- dat %>% filter(#!is.na(condition_grade),
                      !is.na(graded_nbs),
                      !is.na(seismic_il),
                      !is.na(seismic_zone)
                      #!is.na(gfa)
                      )

# This should be the initial tradeoff for the seismic assessment
table(hosp$seismic_il, hosp$seismic_zone, hosp$graded_nbs)

# 2) build “need” columns as integers (higher = more need)
scores <- dat %>%
  transmute(
    .rowid,
    #need_condition = as.integer(condition_grade),
    need_nbs       = as.integer(graded_nbs),
    need_il        = as.numeric(seismic_il),            # 1..4 with 4 worst
    need_zone      = as.integer(seismic_zone)          # 1..3 with 3 worst
    #need_gfa       = gfa                                # numeric, larger = worse
  )

# 3) Pareto bands (negate because nds_rank minimizes)
score_mat <- as.matrix(scores %>% select(-.rowid))
ranks <- emoa::nds_rank(t(-score_mat))

# 4) attach back to the filtered data
out <- dat %>%
  select(.rowid, everything()) %>%
  left_join(
    scores %>%
      mutate(pareto_band = as.integer(ranks)),
    by = ".rowid"
  )





################################################################################################################################
################################################################################################################################

# Section 4

# Summarise the allocation of investment to projects based on Pareto banding
# Individual building, DHB, region etc.

out %>% arrange(pareto_band, desc(condition_grade), desc(gfa)) %>% select(pareto_band, region, dhb_name, campus_name, building_name, seismic_il, seismic_zone, graded_nbs, condition_grade, gfa) %>% print(n = 100)

# Facet by sesmic_il perhaps or maybe some representation of sesmic_zone
out %>%
  filter(seismic_il %in% c('IL4', 'IL3')) %>%
  ggplot(aes(x = condition, y = nbs, color = as.factor(as.integer(pareto_band)), shape = seismic_zone)) +
  geom_point(alpha = 0.6) +
  geom_hline(yintercept = 0.34, linetype = "dashed", color = "red") +  # Below 34% NBS = Earthquake-prone
  labs(title = "High-importance buildings: Condition vs Seismic Strength") %>%
  scale_fill_viridis_d()


################################################################################################################################
################################################################################################################################

# Appendix

# Many data issues to highlight
# Maybe just highlight what I've cleaned and what assumptions I've made as I've cleaned.








