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


library(rhomis)

# Use these file paths to load your preprocessed data
base_path <- "./inst/projects/tree-aid-example/rhomis-2/"
data_path <- "dummy_cleaned_lower_case.csv"

# Read in data
tree_aid_df <- readr::read_csv(paste0(base_path,data_path), na = c("-999", "n/a", "NA", "na"))

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





ntfp_yield <- data.frame(fruit_yield, nut_yield, leaves_yield, bark_yield, roots_yield, gum_yield)
# honey and shea butter are different?

# bind together all forest product yields into one data frame - do this right at the end

# create dataframe for NTFP eaten consumed
# create dataframe for NTFP sold
# multiply yield by proportion coefficients

# example provided by Leo
# data <- tibble::as_tibble(list(
#   "crop_use_1" = c("eat", "eat sell", "sell feed_livestock"),
#   "crop_consumed_prop_1" = c(NA, "most", NA), # All proportions must be lower case for RhoMIS 2.0
#   "crop_sold_prop_1" = c(NA, "little", "little"),
#   "crop_feed_lstk_prop_1" = c(NA, NA, "little")
# ))

# gemma adaption
# data <- tibble::as_tibble(list(
#   "fruit_use_1" = tree_aid_df$fruit_use_1,
#   "crop_consumed_prop_1" = tree_aid_df$fruit_eaten_prop_1,
#   "crop_sold_prop_1" = tree_aid_df$fruit_sold_prop_1,
#   "crop_feed_lstk_prop_1" = tree_aid_df$fruit_where_sell_1
# ))

# Checking can do it for a looped column
# result <- proportions_calculation(data,
#                                   use = "eat",
#                                   use_column = "crop_use",
#                                   prop_column = "crop_consumed_prop",
#                                   loop_number = 1
# )

# proportions of eaten (consumed) and sold for Tree Aid test data, to be multiplied by yields
# Fruit consumed (demonstrated for fruit)
# fruit_eat_prop_1 <- proportions_calculation(tree_aid_df,
#                                   use = "eat",
#                                   use_column = "fruit_use",
#                                   prop_column = "fruit_eaten_prop",
#                                   loop_number = 1
# )
#
# fruit_eat_prop_2 <- proportions_calculation(tree_aid_df,
#                                           use = "eat",
#                                           use_column = "fruit_use",
#                                           prop_column = "fruit_eaten_prop",
#                                           loop_number = 2
# )
#
# fruit_eat_prop_3 <- proportions_calculation(tree_aid_df,
#                                             use = "eat",
#                                             use_column = "fruit_use",
#                                             prop_column = "fruit_eaten_prop",
#                                             loop_number = 3
# )

#Fruit sold (demonstrated for fruit)
# fruit_sold_prop_1 <- proportions_calculation(tree_aid_df,
#                                             use = "sell",
#                                             use_column = "fruit_use",
#                                             prop_column = "fruit_sold_prop",
#                                             loop_number = 1
# )
#
# fruit_sold_prop_2 <- proportions_calculation(tree_aid_df,
#                                              use = "sell",
#                                              use_column = "fruit_use",
#                                              prop_column = "fruit_sold_prop",
#                                              loop_number = 2
# )
#
# fruit_sold_prop_3 <- proportions_calculation(tree_aid_df,
#                                              use = "sell",
#                                              use_column = "fruit_use",
#                                              prop_column = "fruit_sold_prop",
#                                              loop_number = 3
# )

#write for loop / lapply( )
#lapply(tree_aid_df, proportions_calculation())
# for i in length(number_of_fp_loops){
#
# }

# sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, x))
# sapply(c(1:number_of_fp_loops), function("sell") proportions_calculation(tree_aid_df, x))
# sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, "sell", "fruit_use", "fruit_sold_prop"))

# Loop proportions sold calculation

fp_proportions_single <-  function(
        tree_aid_df,
        use,
        use_column,
        prop_column,
        new_column_name
){

    #Checking whether the columns are in the dataset
    missing_columns <- check_columns_in_data(tree_aid_df,
                                             loop_columns = c(use_column, prop_column),
                                             warning_message = "Cannot Calculate Numeric NTFP proportions"
    )


    # If the columns are missing, simply return the dataset
    if (length(missing_columns)!=0){
        return(data)
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



tree_aid_df <- fp_proportions_single(
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


tree_aid_df <- fp_proportions_single(
    tree_aid_df,
    use="eat",
    use_column="gum_use",
    prop_column="gum_eaten_prop",
    new_column_name="gum_eaten_prop_numeric"

)


 tree_aid_df$fruit_amount_kg_1*tree_aid_df$fruit_sold_prop_numeric_1



fruit_sold_prop <- tibble::as_tibble(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "fruit_use", prop_column = "fruit_sold_prop", loop_number = x)))
# nut_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "nut_use", prop_column = "nut_sold_prop", loop_number = x)))
# leaves_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "leaves_use", prop_column = "leaves_sold_prop", loop_number = x)))
# bark_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "bark_use", prop_column = "bark_sold_prop", loop_number = x)))
# roots_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "roots_use", prop_column = "roots_sold_prop", loop_number = x)))
# gum_sold_prop <-  tibble::as_tibble(lapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "sell", use_column = "gum_use", prop_column = "gum_sold_prop", loop_number = x)))

# Bind ntfp_sold dataframe and rename column names
#ntfp_sold_prop <- data.frame(fruit_sold_prop, nut_sold_prop, leaves_sold_prop, bark_sold_prop, roots_sold_prop, gum_sold_prop)
#colnames(fruit_sold_prop) <- c("a", "b","c")
#colnames(ntfp_sold_prop) <- c("fruit_sold_1", "fruit_sold_2","fruit_sold_3","nut_sold_1", "nut_sold_2","nut_sold_3","leaves_sold_1", "leaves_sold_2","leaves_sold_3","bark_sold_1", "bark_sold_2","bark_sold_3","roots_sold_1", "roots_sold_2","roots_sold_3","gum_sold_1", "gum_sold_2","gum_sold_3")
#ASK LEO - HOW CAN I AVOID HARD CODING THE COLUMN NAMES (WHAT IF THERE IS MORE THAN 3 LOOPS)


# Loop proportions eaten calculation
fruit_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "fruit_use", prop_column = "fruit_eaten_prop", loop_number = x)))
nut_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "nut_use", prop_column = "nut_eaten_prop", loop_number = x)))
leaves_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "leaves_use", prop_column = "leaves_consumed_prop", loop_number = x))) # why is this consumed and not eaten?
bark_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "bark_use", prop_column = "bark_eaten_prop", loop_number = x)))
roots_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "roots_use", prop_column = "roots_eaten_prop", loop_number = x)))
gum_eat_prop <- data.frame(sapply(c(1:number_of_fp_loops), function(x) proportions_calculation(tree_aid_df, use = "eat", use_column = "gum_use", prop_column = "gum_eaten_prop", loop_number = x)))

# Bind ntfp_consumed dataframe and rename column names
ntfp_consumed_prop <- data.frame(fruit_eat_prop, nut_eat_prop, leaves_eat_prop, bark_eat_prop, roots_eat_prop, gum_eat_prop)
colnames(ntfp_consumed_prop) <- c("fruit_eat_1", "fruit_eat_2","fruit_eat_3","nut_eat_1", "nut_eat_2","nut_eat_3","leaves_eat_1", "leaves_eat_2","leaves_eat_3","bark_eat_1", "bark_eat_2","bark_eat_3","roots_eat_1", "roots_eat_2","roots_eat_3","gum_eat_1", "gum_eat_2","gum_eat_3")

# Multiply proportions by yields to get ntfp_sold and ntfp_consumed final data tables
ntfp_sold <- ntfp_sold_prop * ntfp_yield
ntfp_consumed <- ntfp_consumed_prop * ntfp_yield
