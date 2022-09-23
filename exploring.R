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
data_path <- "raw-data/"

# base_path <- "RHoMIS/package/"                            # Gemma
# data_path <- "dummy_cleaned_lower_case.csv"

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

# Example function to convert units
convert_units <- function(
        unit_data,
        units_conversions

){
    # Lapply applies functions in parallel, unlike
    # loops which work sequentially
    converted_data <- lapply(unit_data, function(x) {

        # A table containing your units
        household_data_tibble <- tibble::as_tibble(
            list(
                survey_value = x
            )
        )

        # Joining the two tables to match conversion factors
        converted_data <- dplyr::left_join(household_data_tibble,
                                           units_conversions,
                                           by = c(
                                               "survey_value" = "survey_value"
                                           )
        )

        # Extracting the conversion factor only
        return(converted_data[["conversion"]])
    }) %>% dplyr::bind_cols()

    return(converted_data)
}



# (3) YIELD

# Example of calculating fruit harvested
calculate_fp_harvest <- function(
        tree_aid_df,
        fp_harvest_conversions,
        name_column,
        amount_column,
        unit_column

){
    #Checking whether the columns are in the dataset
    missing_columns <- check_columns_in_data(tree_aid_df,
                                             loop_columns = c(name_column, amount_column,unit_column),
                                             warning_message = "Cannot Calculate NTFP fruit harvested"
    )


    # If the columns are missing, simply return the dataset
    if (length(missing_columns)!=0){
        return(NULL)
    }


    # Find the number of loops on forest products
    number_of_fp_loops <- find_number_of_loops(tree_aid_df,"fp_name") #

    # Identifying the columns I need to use
    fp_harvested_columns <- paste0(amount_column,"_", c(1:number_of_fp_loops))
    fp_harvested_unit_columns <- paste0(unit_column,"_", c(1:number_of_fp_loops))

    # Subsetting the data, getting those columns out
    fp_harvest_data <- tree_aid_df[fp_harvested_columns]
    fp_harvest_units_data <- tree_aid_df[fp_harvested_unit_columns]

    # Converting the units for those columns
    fp_harvest_units_converted <- convert_units(unit_data = fp_harvest_units_data,
                                                units_conversions=fp_harvest_conversions
    )

    # Multiplying the units and the amounts
    fp_harvest_kg <- fp_harvest_data*fp_harvest_units_converted

    new_column_name <- paste0(amount_column,"_kg")

    colnames(fp_harvest_kg) <- paste0(new_column_name,"_", c(1:number_of_fp_loops))

    tree_aid_df <- add_column_after_specific_column(
        data = tree_aid_df,
        new_data = fp_harvest_kg,
        new_column_name = new_column_name,
        old_column_name = amount_column,
        loop_structure = T
    )


    return(tree_aid_df)

}

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
fp_proportions_all <-  function(
        tree_aid_df,
        use,
        use_column,
        prop_column,
        new_column_name
){
    #Checking whether the columns are in the dataset
    missing_columns <- check_columns_in_data(tree_aid_df,
                                             loop_columns = c(use_column, prop_column),
                                             warning_message = "Cannot calculate numeric NTFP proportions"
    )

    # If the columns are missing, simply return the dataset
    if (length(missing_columns)!=0){
        return(tree_aid_df)
    }

    # Find loop number
    number_of_loops <- find_number_of_loops(tree_aid_df, use_column)

    # Calculate the numeric proprtions
    ntfp_proportions_numeric <- sapply(c(1:number_of_loops), function(x) proportions_calculation(tree_aid_df, use = use, use_column =use_column, prop_column = prop_column, loop_number = x))
    colnames(ntfp_proportions_numeric) <- paste0(new_column_name, "_", c(1:number_of_loops))

    # Add back into the original dataset
    ntfp_proportions_numeric <- tibble::as_tibble(ntfp_proportions_numeric)
    tree_aid_df <- add_column_after_specific_column(
        data = tree_aid_df,
        new_data = ntfp_proportions_numeric,
        new_column_name = new_column_name,
        old_column_name = prop_column,
        loop_structure = T
    )

    return(tree_aid_df)
}

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




# (4c) PROPORTIONS PROCESSED (HIGH LEVEL)

# High level processed proportion columns
# fruit
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="fruit_use",
  prop_column="fruit_process_prop",
  new_column_name="fruit_process_prop_numeric"
)

# nut
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="nut_use",
  prop_column="nut_process_prop",
  new_column_name="nut_process_prop_numeric"
)

# leaves
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="leaves_use",
  prop_column="leaves_process_prop",
  new_column_name="leaves_process_prop_numeric"
)

# bark
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="bark_use",
  prop_column="bark_process_prop",
  new_column_name="bark_process_prop_numeric"
)

# roots
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="roots_use",
  prop_column="roots_process_prop",
  new_column_name="roots_process_prop_numeric"
)

# gum
tree_aid_df <- fp_proportions_all(
  tree_aid_df,
  use="process",
  use_column="gum_use",
  prop_column="gum_process_prop",
  new_column_name="gum_process_prop_numeric"
)

# shea
# honey



# (4d) PROPORTIONS PROCESSED SOLD

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



# (4e) PROPORTIONS PROCESSED EATEN / CONSUMED

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

# Create function that calculated amount eaten and sold for both processed and unprocessed ntfps
ntfp_sold_and_consumed_calculation <- function(

        data,
        fp_harvest_kg,

        fp_prop_sold_numeric,
        fp_amount_sold_kg,

        fp_prop_consumed_numeric,
        fp_amount_consumed_kg,



        fp_props_process_numeric,

        fp_props_process_sold_numeric,
        fp_amount_process_sold_kg,

        fp_prop_process_consumed_numeric,
        fp_amount_process_consumed_kg
) {
    # NON-PROCESSED COLUMNS
    # Beginning with ntfp sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    sold_columns <- paste0(fp_prop_sold_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
        stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
    }
    if (all(sold_columns %in% colnames(data)) == F) {
        stop("Have not calculated the numeric proportions of amount of non-timber forest products sold. Calculate proportions sold before calculating amounts sold")
    }

    harvest_data <- data[harvested_columns]
    sold_prop_data <- data[sold_columns]

    amount_sold_kg <- tibble::as_tibble(harvest_data * sold_prop_data)
    colnames(amount_sold_kg) <- paste0(fp_amount_sold_kg, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = amount_sold_kg,
        new_column_name = fp_amount_sold_kg,
        old_column_name = fp_prop_sold_numeric,
        loop_structure = T
    )

    # Moving on to crops consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
        warning("Have not calculated the amounts harvested in kg or amounts consumed. Calculate amounts harvested before calculating amounts consumed")
    }
    if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
        harvest_data <- data[harvested_columns]
        consumed_prop_data <- data[consumed_columns]

        amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
        colnames(amount_consumed_kg) <- paste0(fp_amount_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_consumed_kg,
            new_column_name = fp_amount_consumed_kg,
            old_column_name = fp_prop_consumed_numeric,
            loop_structure = T
        )
    }





    # PROCESSED COLUMNS
    # Beginning with ntfp processed sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    processed_columns <- paste0(fp_props_process_numeric, "_", c(1:number_of_loops))
    processed_sold_columns <- paste0(fp_props_process_sold_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
      stop("Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts sold")
    }
    if (all(processed_columns %in% colnames(data)) == F) {
      stop("Have not calculated the numeric proportions of amount of non-timber forest products processed. Calculate proportions processed before calculating amounts processed and sold")
    }
    if (all(processed_sold_columns %in% colnames(data)) == F) {
      stop("Have not calculated the numeric proportions of amount of non-timber forest products processed and sold. Calculate proportions processed and sold before calculating amounts processed and sold")
    }

    harvest_data <- data[harvested_columns]
    processed_prop_data <- data[processed_columns]
    processed_sold_prop_data <- data[processed_sold_columns]

    amount_processed_kg <- tibble::as_tibble(harvest_data * processed_prop_data)
    amount_processed_sold_kg <- tibble::as_tibble(amount_processed_kg * processed_sold_prop_data)
    colnames(amount_processed_sold_kg) <- paste0(fp_amount_process_sold_kg, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
      data = data,
      new_data = amount_processed_sold_kg,
      new_column_name = fp_amount_process_sold_kg,
      old_column_name = fp_props_process_sold_numeric,
      loop_structure = T
    )

    # Moving on to ntfp processed consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    processed_columns <- paste0(fp_props_process_numeric, "_", c(1:number_of_loops))
    processed_consumed_columns <- paste0(fp_prop_process_consumed_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F | all(processed_columns %in% colnames(data)) == F) {
      warning("Have not calculated the amounts harvested in kg or amounts processed. Calculate amounts harvested before calculating amounts processed and consumed")
    }
    if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
      harvest_data <- data[harvested_columns]
      processed_prop_data <- data[processed_columns]
      processed_consumed_prop_data <- data[processed_consumed_columns]

      amount_processed_kg <- tibble::as_tibble(harvest_data * processed_prop_data)
      amount_processed_consumed_kg <- tibble::as_tibble(amount_processed_kg * processed_consumed_prop_data)
      colnames(amount_processed_consumed_kg) <- paste0(fp_amount_process_consumed_kg, "_", c(1:number_of_loops))

      data <- add_column_after_specific_column(
        data = data,
        new_data = amount_processed_consumed_kg,
        new_column_name = fp_amount_process_consumed_kg,
        old_column_name = fp_prop_processed_consumed_numeric,
        loop_structure = T
      )
    }



    return(data)
}




# Run end calculations for eaten and sold for each product
# fruit
tree_aid_df <- ntfp_sold_and_consumed_calculation(

    data=tree_aid_df,
    fp_harvest_kg="fruit_amount_kg",

    fp_prop_sold_numeric="fruit_sold_prop_numeric",
    fp_amount_sold_kg="fruit_amount_sold_kg",

    fp_prop_consumed_numeric="fruit_eaten_prop_numeric",
    fp_amount_consumed_kg="fruit_amount_eaten_kg",



    fp_props_process_numeric="fruit_process_prop_numeric",

    fp_props_process_sold_numeric="fruit_process_sold_prop_numeric",
    fp_amount_process_sold_kg="fruit_amount_process_sold_kg",

    fp_prop_process_consumed_numeric="fruit_process_eaten_prop_numeric",
    fp_amount_process_consumed_kg="fruit_amount_process_eaten_kg"

)

# nut
tree_aid_df <- ntfp_sold_and_consumed_calculation(

  data=tree_aid_df,
  fp_harvest_kg="nut_amount_kg",

  fp_prop_sold_numeric="nut_sold_prop_numeric",
  fp_amount_sold_kg="nut_amount_sold_kg",

  fp_prop_consumed_numeric="nut_eaten_prop_numeric",
  fp_amount_consumed_kg="nut_amount_eaten_kg",



  fp_props_process_numeric="nut_process_prop_numeric",

  fp_props_process_sold_numeric="nut_process_sold_prop_numeric",
  fp_amount_process_sold_kg="nut_amount_process_sold_kg",

  fp_prop_process_consumed_numeric="nut_process_eaten_prop_numeric",
  fp_amount_process_consumed_kg="nut_amount_process_eaten_kg"

)

# leaves
tree_aid_df <- ntfp_sold_and_consumed_calculation(

  data=tree_aid_df,
  fp_harvest_kg="leaves_amount_kg",

  fp_prop_sold_numeric="leaves_sold_prop_numeric",
  fp_amount_sold_kg="leaves_amount_sold_kg",

  fp_prop_consumed_numeric="leaves_consumed_prop_numeric",
  fp_amount_consumed_kg="leaves_amount_consumed_kg",



  fp_props_process_numeric="leaves_process_prop_numeric",

  fp_props_process_sold_numeric="leaves_process_sold_prop_numeric",
  fp_amount_process_sold_kg="leaves_amount_process_sold_kg",

  fp_prop_process_consumed_numeric="leaves_process_consumed_prop_numeric",
  fp_amount_process_consumed_kg="leaves_amount_process_consumed_kg"

)
# LEAVES NOT WORKING, NEED TO TROUBLESHOOT, problem with processed consumed?

# bark
tree_aid_df <- ntfp_sold_and_consumed_calculation(

  data=tree_aid_df,
  fp_harvest_kg="bark_amount_kg",

  fp_prop_sold_numeric="bark_sold_prop_numeric",
  fp_amount_sold_kg="bark_amount_sold_kg",

  fp_prop_consumed_numeric="bark_eaten_prop_numeric",
  fp_amount_consumed_kg="bark_amount_eaten_kg",



  fp_props_process_numeric="bark_process_prop_numeric",

  fp_props_process_sold_numeric="bark_process_sold_prop_numeric",
  fp_amount_process_sold_kg="bark_amount_process_sold_kg",

  fp_prop_process_consumed_numeric="bark_process_eaten_prop_numeric",
  fp_amount_process_consumed_kg="bark_amount_process_eaten_kg"

)

# roots
tree_aid_df <- ntfp_sold_and_consumed_calculation(

  data=tree_aid_df,
  fp_harvest_kg="roots_amount_kg",

  fp_prop_sold_numeric="roots_sold_prop_numeric",
  fp_amount_sold_kg="roots_amount_sold_kg",

  fp_prop_consumed_numeric="roots_eaten_prop_numeric",
  fp_amount_consumed_kg="roots_amount_eaten_kg",



  fp_props_process_numeric="roots_process_prop_numeric",

  fp_props_process_sold_numeric="roots_process_sold_prop_numeric",
  fp_amount_process_sold_kg="roots_amount_process_sold_kg",

  fp_prop_process_consumed_numeric="roots_process_eaten_prop_numeric",
  fp_amount_process_consumed_kg="roots_amount_process_eaten_kg"

)

# gum
tree_aid_df <- ntfp_sold_and_consumed_calculation(

  data=tree_aid_df,
  fp_harvest_kg="gum_amount_kg",

  fp_prop_sold_numeric="gum_sold_prop_numeric",
  fp_amount_sold_kg="gum_amount_sold_kg",

  fp_prop_consumed_numeric="gum_eaten_prop_numeric",
  fp_amount_consumed_kg="gum_amount_eaten_kg",



  fp_props_process_numeric="gum_process_prop_numeric",

  fp_props_process_sold_numeric="gum_process_sold_prop_numeric",
  fp_amount_process_sold_kg="gum_amount_process_sold_kg",

  fp_prop_process_consumed_numeric="gum_process_eaten_prop_numeric",
  fp_amount_process_consumed_kg="gum_amount_process_eaten_kg"

)



# (6) INCOME

# Create NTFP income calculation function
fp_income_calculations <- function(data,
                                   unit_conv_tibble = NULL,
                                   fp_sold_kg_per_year_column,
                                   fp_sold_units_column, # a column to be created
                                   fp_sold_income_column,
                                   new_fp_sold_income,
                                   product_type # gemma added, , "fruit_price"
                                   ) {

    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")

    fp_sold_columns <- paste0(fp_sold_kg_per_year_column, "_", c(1:number_of_loops)) #fruit_amount_sold_kg
    fp_sold_unit_columns <- paste0(fp_sold_units_column, "_", c(1:number_of_loops)) #is this frequency column? (e.g. 'year') #fruit_sold_frequency_1
    fp_sold_income_columns <- paste0(fp_sold_income_column, "_", c(1:number_of_loops)) #fruit_sold_income_1

    if (all(fp_sold_columns %in% colnames(data)) == F) {
        stop("Have not calculated the amounts sold in kg. Calculate amounts sold before calculating income")
    }
    if (all(fp_sold_unit_columns %in% colnames(data)) == F) {
        stop("Have not converted the non-timber forest product price quantity units yet. Convert these units before calculating incomes sold")
    }



    fp_sold_units_data <- data[fp_sold_unit_columns]
    fp_sold_units_numeric <- convert_units(unit_data = fp_sold_units_data,
                                                units_conversions=unit_conv_tibble
    )

    fp_sold_amount <- data[fp_sold_columns]
    fp_sold_income <- data[fp_sold_income_columns]




    # Multiplying values which do not have "total_income_per_year_unit
    fp_sold_income_per_year <- fp_sold_income %>% dplyr::mutate_all(as.numeric)

    fp_sold_income_per_year <- fp_sold_income_per_year * fp_sold_units_numeric

    fp_sold_income_per_year[fp_sold_amount==0] <- 0


    colnames(fp_sold_income_per_year) <- paste0(new_fp_sold_income, "_", c(1:number_of_loops))
    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_sold_income_per_year,
        new_column_name = new_fp_sold_income,
        old_column_name = fp_sold_income_column,
        loop_structure = T
    )

    fp_price <- fp_sold_income_per_year / fp_sold_amount
    colnames(fp_price) <- paste0(product_type, "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_price,
        new_column_name = product_type,
        old_column_name = new_fp_sold_income,
        loop_structure = T
    )

    return(data)
}



fp_amount_conversions <- tibble::as_tibble(
    list(
        survey_value=c("day", "week", "month","year"),
        conversion=c(365,52, 12, 365)
    )
)



# Conducting the calculation
tree_aid_df <- fp_income_calculations(
    data = tree_aid_df,
    fp_sold_kg_per_year_column = "fruit_amount_sold_kg",
    fp_sold_units_column = "fruit_sold_frequency",
    fp_sold_income_column = "fruit_sold_income",
    new_fp_sold_income = "fruit_sold_income_per_year",
    unit_conv_tibble = fp_amount_conversions,
    product_type = "fruit_price"
)

tree_aid_df <- fp_income_calculations(
  data = tree_aid_df,
  fp_sold_kg_per_year_column = "nut_amount_sold_kg",
  fp_sold_units_column = "nut_sold_frequency",
  fp_sold_income_column = "nut_sold_income",
  new_fp_sold_income = "nut_sold_income_per_year",
  unit_conv_tibble = fp_amount_conversions,
  product_type = "nut_price"
)

tree_aid_df <- fp_income_calculations(
  data = tree_aid_df,
  fp_sold_kg_per_year_column = "leaves_amount_sold_kg",
  fp_sold_units_column = "leaves_sold_frequency",
  fp_sold_income_column = "leaves_sold_income",
  new_fp_sold_income = "leaves_sold_income_per_year",
  unit_conv_tibble = fp_amount_conversions,
  product_type = "leaves_price"
)

tree_aid_df <- fp_income_calculations(
  data = tree_aid_df,
  fp_sold_kg_per_year_column = "bark_amount_sold_kg",
  fp_sold_units_column = "bark_sold_frequency",
  fp_sold_income_column = "bark_sold_income",
  new_fp_sold_income = "bark_sold_income_per_year",
  unit_conv_tibble = fp_amount_conversions,
  product_type = "bark_price"
)

tree_aid_df <- fp_income_calculations(
  data = tree_aid_df,
  fp_sold_kg_per_year_column = "roots_amount_sold_kg",
  fp_sold_units_column = "roots_sold_frequency",
  fp_sold_income_column = "roots_sold_income",
  new_fp_sold_income = "roots_sold_income_per_year",
  unit_conv_tibble = fp_amount_conversions,
  product_type = "roots_price"
)

tree_aid_df <- fp_income_calculations(
  data = tree_aid_df,
  fp_sold_kg_per_year_column = "gum_amount_sold_kg",
  fp_sold_units_column = "gum_sold_frequency",
  fp_sold_income_column = "gum_sold_income_per_freq",
  new_fp_sold_income = "gum_sold_income_per_year",
  unit_conv_tibble = fp_amount_conversions,
  product_type = "gum_price"
)

# # List all income columns
# fp_income_columns <- c(paste0("fruit_sold_income_per_year","_", c(1:number_of_fp_loops)),
#                        paste0("nut_sold_income_per_year","_", c(1:number_of_fp_loops)),
#                        paste0("leaves_sold_income_per_year","_", c(1:number_of_fp_loops)),
#                        paste0("bark_sold_income_per_year","_", c(1:number_of_fp_loops)),
#                        paste0("roots_sold_income_per_year","_", c(1:number_of_fp_loops)),
#                        paste0("gum_sold_income_per_year","_", c(1:number_of_fp_loops))) # is there a better way to append to a list?
#
# ntfp_total_income_sum <- rowSums(fp_income_columns)
# tree_aid_df$ntfp_income <-

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



#                                  TO DO
# 3) SHEA BUTTER
# 4) HONEY
# 5) NEXT COLUMNS E.G. CALORIES
