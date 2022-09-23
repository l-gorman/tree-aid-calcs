#' TREE AID NTFP MODULE CONTRIBUTIONS TO RHOMIS 2.0 R PACKAGE
#'
#' Some of these functions rely on functions from
#' the RHoMIS-R-package.
#'
#' You can install the package using the following command
#' (make sure to install the package "devtools" first):
#'
#' devtools::install_github("l-gorman/rhomis-R-package")



# (1) LOAD IN DATA

library(rhomis)

# Use these file paths to load your preprocessed data
base_path <- "./inst/projects/tree-aid-example/rhomis-2/" # Leo
# base_path <- "RHoMIS/package/"                            # Gemma
data_path <- "dummy_cleaned_lower_case.csv"

# Read in data
tree_aid_df <- readr::read_csv(paste0(base_path,data_path), na = c("-999", "n/a", "NA", "na"))



# (2) UNITS CONVERSION

# Example on how you would find all new units in a dataset
number_of_fp_loops <- find_number_of_loops(tree_aid_df,"fp_name") #
fp_amount_units <- extract_new_values(data = tree_aid_df,
                                      loop_or_individual_column = "loop",
                                      column_pattern = "fruit_amount_units",
                                      number_of_loops = number_of_fp_loops
)

# Example of how to construct a table of conversion factors from that table
fp_amount_conversions <- tibble::as_tibble(
    list(
        survey_value=c("bundle_20kg", "kg", "tine_20kg","calabash_5litres"),
        conversion=c(20,1, 20, 5)
    )
)







# Apply the NTFP yield function to all 6 forest product types
# (fruit, nut, leaves, bark, roots, gum)

# fruit
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="fruit_amount",
    unit_column="fruit_amount_units"
)

# nut
tree_aid_df  <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="nut_amount",
    unit_column="nut_amount_units"
)

# leaves
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="leaves_amount",
    unit_column="leaves_amount_units"
)

# bark
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="bark_amount",
    unit_column="bark_amount_units"
)

# roots
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="roots_amount",
    unit_column="roots_amount_units"
)

# gum
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="gum_amount",
    unit_column="gum_amount_units"
)

# shea
# tree_aid_df <- calculate_fp_harvest(
#     tree_aid_df=tree_aid_df,
#     fp_harvest_conversions=fp_amount_conversions,
#     name_column="fp_name",
#     amount_column="shea_butter_amount",
#     unit_column="shea_butter_amount_units"
# )



# (4a) PROPORTIONS SOLD

# Loop proportions sold calculation

# Sold proportion columns
# fruit
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="fruit_use",
    prop_column="fruit_sold_prop",
    new_column_name="fruit_sold_prop_numeric"
)

# nut
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="nut_use",
    prop_column="nut_sold_prop",
    new_column_name="nut_sold_prop_numeric"
)

# leaves
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="leaves_use",
    prop_column="leaves_sold_prop",
    new_column_name="leaves_sold_prop_numeric"
)

# bark
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="bark_use",
    prop_column="bark_sold_prop",
    new_column_name="bark_sold_prop_numeric"
)

# roots
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="roots_use",
    prop_column="roots_sold_prop",
    new_column_name="roots_sold_prop_numeric"
)

# gum
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="gum_use",
    prop_column="gum_sold_prop",
    new_column_name="gum_sold_prop_numeric"
)

# shea
# honey



# (4b) PROPORTIONS EATEN / CONSUMED

# Eat proportion columns
# fruit
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="fruit_use",
    prop_column="fruit_eaten_prop",
    new_column_name="fruit_eaten_prop_numeric"
)

# nut
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="nut_use",
    prop_column="nut_eaten_prop",
    new_column_name="nut_eaten_prop_numeric"
)

# leaves
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="leaves_use",
    prop_column="leaves_consumed_prop",
    new_column_name="leaves_consumed_prop_numeric"
)

# bark
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="bark_use",
    prop_column="bark_eaten_prop",
    new_column_name="bark_eaten_prop_numeric"
)

# roots
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="roots_use",
    prop_column="roots_eaten_prop",
    new_column_name="roots_eaten_prop_numeric"
)

# gum
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="gum_use",
    prop_column="gum_eaten_prop",
    new_column_name="gum_eaten_prop_numeric"
)

# shea
# honey



# (4c) PROPORTIONS PROCESSED SOLD

# Sold processed proportion columns
# fruit
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="fruit_use",
    prop_column="fruit_process_sold_prop",
    new_column_name="fruit_process_sold_prop_numeric"
)

# nut
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="nut_use",
    prop_column="nut_process_sold_prop",
    new_column_name="nut_process_sold_prop_numeric"
)

# leaves
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="leaves_use",
    prop_column="leaves_process_sold_prop",
    new_column_name="leaves_process_sold_prop_numeric"
)

# bark
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="bark_use",
    prop_column="bark_process_sold_prop",
    new_column_name="bark_process_sold_prop_numeric"
)

# roots
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="roots_use",
    prop_column="roots_process_sold_prop",
    new_column_name="roots_process_sold_prop_numeric"
)

# gum
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="gum_use",
    prop_column="gum_process_sold_prop",
    new_column_name="gum_process_sold_prop_numeric"
)

# shea
# honey



# (4d) PROPORTIONS PROCESSED EATEN / CONSUMED

# Consumed/eaten processed proportion columns
# fruit
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="fruit_use",
    prop_column="fruit_process_eaten_prop",
    new_column_name="fruit_process_eaten_prop_numeric"
)

# nut
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="nut_use",
    prop_column="nut_process_eaten_prop",
    new_column_name="nut_process_eaten_prop_numeric"
)

# leaves
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="leaves_use",
    prop_column="leaves_process_consumed_prop",
    new_column_name="leaves_process_consumed_prop_numeric"
)

# bark
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="bark_use",
    prop_column="bark_process_eaten_prop",
    new_column_name="bark_process_eaten_prop_numeric"
)

# roots
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="roots_use",
    prop_column="roots_process_eaten_prop",
    new_column_name="roots_process_eaten_prop_numeric"
)

# gum
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="gum_use",
    prop_column="gum_process_eaten_prop",
    new_column_name="gum_process_eaten_prop_numeric"
)

# shea
# honey



# (5) END CALCULATION AMOUNT EATEN AND SOLD

# Create function

# Run end calculations for eaten and sold for each product
# fruit
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="fruit_amount_kg",
    fp_props_sold_numeric="fruit_sold_prop_numeric",
    fp_amount_sold_kg="fruit_amount_sold_kg",

    fp_prop_consumed_numeric="fruit_eaten_prop_numeric",
    fp_prop_consumed_kg="fruit_amount_eaten_kg"
)

# nut
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="nut_amount_kg",
    fp_props_sold_numeric="nut_sold_prop_numeric",
    fp_amount_sold_kg="nut_amount_sold_kg",

    fp_prop_consumed_numeric="nut_eaten_prop_numeric",
    fp_prop_consumed_kg="nut_amount_eaten_kg"
)

# leaves
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="leaves_amount_kg",
    fp_props_sold_numeric="leaves_sold_prop_numeric",
    fp_amount_sold_kg="leaves_amount_sold_kg",

    fp_prop_consumed_numeric="leaves_consumed_prop_numeric",
    fp_prop_consumed_kg="leaves_amount_eaten_kg"
) # LEAVES NOT WORKING, NEED TO TROUBLESHOOT

# bark
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="bark_amount_kg",
    fp_props_sold_numeric="bark_sold_prop_numeric",
    fp_amount_sold_kg="bark_amount_sold_kg",

    fp_prop_consumed_numeric="bark_eaten_prop_numeric",
    fp_prop_consumed_kg="bark_amount_eaten_kg"
)

# roots
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="roots_amount_kg",
    fp_props_sold_numeric="roots_sold_prop_numeric",
    fp_amount_sold_kg="roots_amount_sold_kg",

    fp_prop_consumed_numeric="roots_eaten_prop_numeric",
    fp_prop_consumed_kg="roots_amount_eaten_kg"
)

# gum
tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="gum_amount_kg",
    fp_props_sold_numeric="gum_sold_prop_numeric",
    fp_amount_sold_kg="gum_amount_sold_kg",

    fp_prop_consumed_numeric="gum_eaten_prop_numeric",
    fp_prop_consumed_kg="gum_amount_eaten_kg"
)



# (6) INCOME

# Create NTFP income calculation function


fp_sold_conversions <- tibble::as_tibble(
    list(
        survey_value=c("day", "week", "month","year"),
        conversion=c(365,52, 12, 1)
    )
)



# Conducting the calculation
tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "fruit_amount_sold_kg",
    fp_sold_units_column = "fruit_sold_frequency",
    fp_sold_income_column = "fruit_sold_income",
    new_fp_sold_income = "fruit_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "fruit_price"
)

tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "nut_amount_sold_kg",
    fp_sold_units_column = "nut_sold_frequency",
    fp_sold_income_column = "nut_sold_income",
    new_fp_sold_income = "nut_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "nut_price"
)

tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "leaves_amount_sold_kg",
    fp_sold_units_column = "leaves_sold_frequency",
    fp_sold_income_column = "leaves_sold_income",
    new_fp_sold_income = "leaves_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "leaves_price"
)

tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "bark_amount_sold_kg",
    fp_sold_units_column = "bark_sold_frequency",
    fp_sold_income_column = "bark_sold_income",
    new_fp_sold_income = "bark_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "bark_price"
)

tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "roots_amount_sold_kg",
    fp_sold_units_column = "roots_sold_frequency",
    fp_sold_income_column = "roots_sold_income",
    new_fp_sold_income = "roots_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "roots_price"
)

tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "gum_amount_sold_kg",
    fp_sold_units_column = "gum_sold_frequency",
    fp_sold_income_column = "gum_sold_income_per_freq",
    new_fp_sold_income = "gum_sold_income_per_year",
    unit_conv_tibble = fp_sold_conversions,
    product_type = "gum_price"
)





# List all income columns

fp_income_columns <- c("fruit_sold_income_per_year",
                       "nut_sold_income_per_year",
                       "leaves_sold_income_per_year",
                       "bark_sold_income_per_year",
                       "roots_sold_income_per_year",
                       "gum_sold_income_per_year",
                       "shea_sold_income_year")

missing_columns <- check_columns_in_data(tree_aid_df,
                                         loop_columns = fp_income_columns,
                                         warning_message = "Missing these columns, these fps will not be considered in incomes")

fp_income_columns <- fp_income_columns[fp_income_columns%in%missing_columns==F]

fp_income_columns <- lapply(fp_income_columns, function(x){
    paste0(x,"_", c(1:number_of_fp_loops))
           }) %>% unlist()


ntfp_total_income_sum <- rowSums(tree_aid_df[fp_income_columns], na.rm = T)


# TO DO
# WORK ON PROPORTIONS PROCESSED, SHEA BUTTER AND HONEY NEXT, THEN INCOME AND UNITS, THEN END CALCS AND CHECKING WE HAVE THE COLS WE NEED E.G. CALORIES
# start next Leo meeting with an introduction to GitHub and fix the broken proportion conversions
# Q: how do processed columns work (in dataset or in function), does processed columns being populated depend on use_column having 'process' inside it?
# it doesn't seem as thought there is a link between these two?
# Ask Leo to help make end calculations into a function
# Add processed columns to normal columns once they are calculated
# Assign a column ni
