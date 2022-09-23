#' Example script to show functions you might want to create
#' for calculating NTFP harvests
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

# REFERENCE EXAMPLE BY LEO
# Examples of using the function
# calculate_fp_harvest(
#     tree_aid_df=tree_aid_df,
#     fp_harvest_conversions=fp_amount_conversions,
#     name_column="fp_name", # FP always lower case in RHoMIS 2.0
#     amount_column="fruit_amount",
#     unit_column="fruit_amount_units"
# )
#
# calculate_fp_harvest(
#     tree_aid_df=tree_aid_df,
#     fp_harvest_conversions=fp_amount_conversions,
#     name_column="fp_name",
#     amount_column="nut_amount",
#     unit_column="nut_amount_units"
# )

# apply the NTFP yield function to all 6 forest product types
# fruit, nut, leaves, bark, roots, gum
tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="fruit_amount",
    unit_column="fruit_amount_units"
)

tree_aid_df  <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="nut_amount",
    unit_column="nut_amount_units"
)

tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="leaves_amount",
    unit_column="leaves_amount_units"
)

tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="bark_amount",
    unit_column="bark_amount_units"
)

tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="roots_amount",
    unit_column="roots_amount_units"
)

tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="gum_amount",
    unit_column="gum_amount_units"
)

tree_aid_df <- calculate_fp_harvest(
    tree_aid_df=tree_aid_df,
    fp_harvest_conversions=fp_amount_conversions,
    name_column="fp_name",
    amount_column="shea_butter_amount",
    unit_column="shea_butter_amount_units"
)

# REFERENCE EXAMPLE BY LEO
# example provided by Leo
# data <- tibble::as_tibble(list(
#   "crop_use_1" = c("eat", "eat sell", "sell feed_livestock"),
#   "crop_consumed_prop_1" = c(NA, "most", NA), # All proportions must be lower case for RhoMIS 2.0
#   "crop_sold_prop_1" = c(NA, "little", "little"),
#   "crop_feed_lstk_prop_1" = c(NA, NA, "little")
# ))







# (4) PROPORTIONS

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
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="fruit_use",
    prop_column="fruit_sold_prop",
    new_column_name="fruit_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="nut_use",
    prop_column="nut_sold_prop",
    new_column_name="nut_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="leaves_use",
    prop_column="leaves_sold_prop",
    new_column_name="leaves_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="bark_use",
    prop_column="bark_sold_prop",
    new_column_name="bark_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="roots_use",
    prop_column="roots_sold_prop",
    new_column_name="roots_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="sell",
    use_column="gum_use",
    prop_column="gum_sold_prop",
    new_column_name="gum_sold_prop_numeric"
)

# shea butter

# honey










# Eat proportion columns
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="fruit_use",
    prop_column="fruit_eaten_prop",
    new_column_name="fruit_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="nut_use",
    prop_column="nut_eaten_prop",
    new_column_name="nut_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="leaves_use",
    prop_column="leaves_consumed_prop",
    new_column_name="leaves_consumed_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="bark_use",
    prop_column="bark_eaten_prop",
    new_column_name="bark_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="roots_use",
    prop_column="roots_eaten_prop",
    new_column_name="roots_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="eat",
    use_column="gum_use",
    prop_column="gum_eaten_prop",
    new_column_name="gum_eaten_prop_numeric"
)

# shea butter

# honey






# Sold processed proportion columns
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="fruit_use",
    prop_column="fruit_process_sold_prop",
    new_column_name="fruit_process_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="nut_use",
    prop_column="nut_process_sold_prop",
    new_column_name="nut_process_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="leaves_use",
    prop_column="leaves_process_sold_prop",
    new_column_name="leaves_process_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="bark_use",
    prop_column="bark_process_sold_prop",
    new_column_name="bark_process_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="roots_use",
    prop_column="roots_process_sold_prop",
    new_column_name="roots_process_sold_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="gum_use",
    prop_column="gum_process_sold_prop",
    new_column_name="gum_process_sold_prop_numeric"
)

# shea butter

# honey







# Consumed/eaten processed proportion columns
tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="fruit_use",
    prop_column="fruit_process_eaten_prop",
    new_column_name="fruit_process_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="nut_use",
    prop_column="nut_process_eaten_prop",
    new_column_name="nut_process_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="leaves_use",
    prop_column="leaves_process_consumed_prop",
    new_column_name="leaves_process_consumed_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="bark_use",
    prop_column="bark_process_eaten_prop",
    new_column_name="bark_process_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="roots_use",
    prop_column="roots_process_eaten_prop",
    new_column_name="roots_process_eaten_prop_numeric"
)

tree_aid_df <- fp_proportions_all(
    tree_aid_df,
    use="process",
    use_column="gum_use",
    prop_column="gum_process_eaten_prop",
    new_column_name="gum_process_eaten_prop_numeric"
)

# shea butter

# honey










# (5) AMOUNT EATEN AND SOLD

#########################################################################
#EDITING LEOS FUNCTION FOR AMOUNT SOLD AND CONSUMED CALCULATIONS




ntfp_sold_and_consumed_calculation <- function(
        data,
        fp_harvest_kg,

        fp_props_sold_numeric,
        fp_amount_sold_kg,

        fp_prop_consumed_numeric,
        fp_prop_consumed_kg
) {
    #tree_aid_df <- ntfp_proportions_all(tree_aid_df) #Leo do we need this line?

    # Beginning with ntfp sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops)) #fruit_amount_kg_1 how do we adapt this as isn't the same column structure as crop
    sold_columns <- paste0(fp_props_sold_numeric, "_", c(1:number_of_loops))

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
        old_column_name = fp_props_sold_numeric,
        loop_structure = T
    )

    # Moving on to crops consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F | all(consumed_columns %in% colnames(data)) == F) {
        warning("Have not calculated the amounts harvested in kg or amounts sold. Calculate amounts harvested before calculating amounts consumed")
    }
    if (all(harvested_columns %in% colnames(data)) == T & all(consumed_columns %in% colnames(data)) == T) {
        harvest_data <- data[harvested_columns]
        consumed_prop_data <- data[consumed_columns]

        amount_consumed_kg <- tibble::as_tibble(harvest_data * consumed_prop_data)
        colnames(amount_consumed_kg) <- paste0(fp_prop_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_consumed_kg,
            new_column_name = fp_prop_consumed_kg,
            old_column_name = fp_prop_consumed_numeric,
            loop_structure = T
        )
    }

    return(data)
}
#########################################################################

# End calculations
# Sold

tree_aid_df <- ntfp_sold_and_consumed_calculation(
    data=tree_aid_df,
    fp_harvest_kg="fruit_amount_kg",
    fp_props_sold_numeric="fruit_sold_prop_numeric",
    fp_amount_sold_kg="fruit_amount_sold_kg",

    fp_prop_consumed_numeric="fruit_eaten_prop_numeric",
    fp_prop_consumed_kg="fruit_amount_eaten_kg"
)





tree_aid_df$nut_amount_kg_1*tree_aid_df$nut_sold_prop_numeric_1
tree_aid_df$nut_amount_kg_2*tree_aid_df$nut_sold_prop_numeric_2
tree_aid_df$nut_amount_kg_3*tree_aid_df$nut_sold_prop_numeric_3

tree_aid_df$leaves_amount_kg_1*tree_aid_df$leaves_sold_prop_numeric_1
tree_aid_df$leaves_amount_kg_2*tree_aid_df$leaves_sold_prop_numeric_2
tree_aid_df$leaves_amount_kg_3*tree_aid_df$leaves_sold_prop_numeric_3

tree_aid_df$bark_amount_kg_1*tree_aid_df$bark_sold_prop_numeric_1
tree_aid_df$bark_amount_kg_2*tree_aid_df$bark_sold_prop_numeric_2
tree_aid_df$bark_amount_kg_3*tree_aid_df$bark_sold_prop_numeric_3

tree_aid_df$roots_amount_kg_1*tree_aid_df$roots_sold_prop_numeric_1
tree_aid_df$roots_amount_kg_2*tree_aid_df$roots_sold_prop_numeric_2
tree_aid_df$roots_amount_kg_3*tree_aid_df$roots_sold_prop_numeric_3

tree_aid_df$gum_amount_kg_1*tree_aid_df$gum_sold_prop_numeric_1
tree_aid_df$gum_amount_kg_2*tree_aid_df$gum_sold_prop_numeric_2
tree_aid_df$gum_amount_kg_3*tree_aid_df$gum_sold_prop_numeric_3



# WORK ON PROPORTIONS PROCESSED, SHEA BUTTER AND HONEY NEXT, THEN INCOME AND UNITS, THEN END CALCS AND CHECKING WE HAVE THE COLS WE NEED E.G. CALORIES
# start next Leo meeting with an introduction to GitHub and fix the broken proportion conversions
# Q: how do processed columns work (in dataset or in function), does processed columns being populated depend on use_column having 'process' inside it?
# it doesn't seem as thought there is a link between these two?
# Ask Leo to help make end calculations into a function
# Add processed columns to normal columns once they are calculated
# Assign a column name in tree_aid_df


###########################################################################################################################################
#fruit_sold_prop <- tibble::as_tibble(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "fruit_use", prop_column = "fruit_sold_prop", loop_number = x)))
# nut_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "nut_use", prop_column = "nut_sold_prop", loop_number = x)))
# leaves_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "leaves_use", prop_column = "leaves_sold_prop", loop_number = x)))
# bark_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "bark_use", prop_column = "bark_sold_prop", loop_number = x)))
# roots_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "roots_use", prop_column = "roots_sold_prop", loop_number = x)))
# gum_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "gum_use", prop_column = "gum_sold_prop", loop_number = x)))

# Bind ntfp_sold dataframe and rename column names
#ntfp_sold_prop <- data.frame(fruit_sold_prop, nut_sold_prop, leaves_sold_prop, bark_sold_prop, roots_sold_prop, gum_sold_prop)
#colnames(fruit_sold_prop) <- c("a", "b","c")
#colnames(ntfp_sold_prop) <- c("fruit_sold_1", "fruit_sold_2","fruit_sold_3","nut_sold_1", "nut_sold_2","nut_sold_3","leaves_sold_1", "leaves_sold_2","leaves_sold_3","bark_sold_1", "bark_sold_2","bark_sold_3","roots_sold_1", "roots_sold_2","roots_sold_3","gum_sold_1", "gum_sold_2","gum_sold_3")



# Loop proportions eaten calculation
# fruit_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "fruit_use", prop_column = "fruit_eaten_prop", loop_number = x)))
# nut_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "nut_use", prop_column = "nut_eaten_prop", loop_number = x)))
# leaves_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "leaves_use", prop_column = "leaves_consumed_prop", loop_number = x))) # why is this consumed and not eaten?
# bark_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "bark_use", prop_column = "bark_eaten_prop", loop_number = x)))
# roots_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "roots_use", prop_column = "roots_eaten_prop", loop_number = x)))
# gum_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "gum_use", prop_column = "gum_eaten_prop", loop_number = x)))

# Bind ntfp_consumed dataframe and rename column names
ntfp_consumed_prop <- data.frame(fruit_eat_prop, nut_eat_prop, leaves_eat_prop, bark_eat_prop, roots_eat_prop, gum_eat_prop)
colnames(ntfp_consumed_prop) <- c("fruit_eat_1", "fruit_eat_2","fruit_eat_3","nut_eat_1", "nut_eat_2","nut_eat_3","leaves_eat_1", "leaves_eat_2","leaves_eat_3","bark_eat_1", "bark_eat_2","bark_eat_3","roots_eat_1", "roots_eat_2","roots_eat_3","gum_eat_1", "gum_eat_2","gum_eat_3")

# Multiply proportions by yields to get ntfp_sold and ntfp_consumed final data tables
ntfp_sold <- ntfp_sold_prop * ntfp_yield
ntfp_consumed <- ntfp_consumed_prop * ntfp_yield









# (6) INCOME

#' NTFP Income Calculations
#'
#' A function for calculating NTFP incomes. Please note
#' for this calculation to work. The amount of a forest product harvested, and the amount
#' of a forest product sold also needs to have been calculated




fp_income_calculations <- function(data,
                                   unit_conv_tibble = NULL,
                                   fp_sold_kg_per_year_column,
                                   fp_sold_units_column, # a column to be created
                                   fp_sold_income_column,
                                   new_fp_sold_income
                                   ) {
    #data <- convert_crop_sold_units(data, unit_conv_tibble = unit_conv_tibble) #don't think we need this?

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
    colnames(fp_price) <- paste0("fp_price", "_", c(1:number_of_loops))

    data <- add_column_after_specific_column(
        data = data,
        new_data = fp_price,
        new_column_name = "fp_price",
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
    unit_conv_tibble = fp_amount_conversions
)

