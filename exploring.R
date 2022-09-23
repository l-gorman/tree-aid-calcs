#' TREE AID NTFP MODULE CONTRIBUTIONS TO RHOMIS 2.0 R PACKAGE
#'
#' Some of these functions rely on functions from
#' the RHoMIS-R-package.
#'
#' You can install the package using the following command
#' (make sure to install the package "devtools" first):
#'
#' devtools::install_github("l-gorman/rhomis-R-package")

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (1) LOAD IN DATA
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
library(rhomis)

# Use these file paths to load your preprocessed data
base_path <- "./inst/projects/tree-aid-example/rhomis-2/" # Leo
data_path <- "raw-data/raw-data.csv"

# base_path <- "RHoMIS/package/"                            # Gemma
# data_path <- "dummy_cleaned_lower_case.csv"

# Read in data

units_and_conversions <- extract_units_and_conversions_csv(
    base_path = base_path,
    file_path="inst/projects/tree-aid-example/rhomis-2/preprocessed_data.csv",
    id_type = "string",
    proj_id = "tree-aid",
    form_id = "gh6")

tree_aid_df <- load_rhomis_csv(

    file_path = paste0(base_path,data_path),
    id_type = "string",
    proj_id = "tree-aid",
    form_id = "gh6"
)

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (2) Defining all NTFPs in a list that can be used later
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
fp_products <- list(

    fruit=list(
        base_name="fruit",
        fp_name = "fp_name",

        #Info needed for fruit calculations
        amount = "fruit_amount",
        amount_units = "fruit_amount_units",
        amount_units_other = "fruit_amount_units_other",

        # Info needed for proportions calculations
        use_column = "fruit_use",
        sold_prop_column = "fruit_sold_prop",
        consumed_column = "fruit_eaten_prop",
        processed_column = "fruit_process_prop",

        income_column = "fruit_sold_income",
        income_frequency = "fruit_sold_frequency",
        sold_frequency_other_column = "fruit_sold_amount_units_other",

        consume_gender="fruit_control_eating",
        sell_gender="fruit_who_sell",
        sell_income_gender="fruit_sold_income_who",


        processed_sold_column = "fruit_process_sold_prop",
        processed_eaten_column = "fruit_process_eaten_prop",

        process_sold_income_column = "fruit_process_sold_income",
        process_sold_frequency_column = "fruit_process_sold_frequency",
        process_sold_frequency_other_column = "fruit_process_sold_amount_units_other"
    ),

    nut=list(
        fp_name = "fp_name",
        base_name="nut",

        #Info needed for nut calculations
        amount = "nut_amount",
        amount_units = "nut_amount_units",
        amount_units_other = "nut_amount_units_other",

        # Info needed for proportions calculations
        use_column = "nut_use",
        sold_prop_column = "nut_sold_prop",
        consumed_column = "nut_eaten_prop",
        processed_column = "nut_process_prop",

        income_column = "nut_sold_income",
        income_frequency = "nut_sold_frequency",
        sold_frequency_other_column = "nut_sold_amount_units_other",

        consume_gender="nut_control_eating",
        sell_gender="nut_who_sell",
        sell_income_gender="nut_sold_income_who",

        processed_sold_column = "nut_process_sold_prop",
        processed_eaten_column = "nut_process_eaten_prop",

        process_sold_income_column = "nut_process_sold_income",
        process_sold_frequency_column = "nut_process_sold_frequency",
        process_sold_frequency_other_column = "nut_process_sold_amount_units_other"
    ),

    # Shea butter seems to be a special case
    shea_butter=list(
        fp_name = "fp_name",
        base_name="shea_butter",

        #Info needed for shea_butter calculations
        amount = "shea_butter_amount",
        amount_units = "shea_butter_amount_units",
        amount_units_other = "shea_butter_amount_units_other",

        # Info needed for proportions calculations
        use_column = "shea_butter_use",
        sold_prop_column = "shea_butter_sold",
        consumed_column = "shea_butter_consume",
        processed_column = "shea_butter_process_prop",

        income_column = "shea_butter_sold_income_per_freq",
        income_frequency = "shea_butter_sold_frequency",
        sold_frequency_other_column = "shea_butter_sold_amount_units_other",

        consume_gender=NULL,
        sell_gender="shea_butter_control_sell",
        sell_income_gender="shea_butter_sold_income_who",

        processed_sold_column = NULL,
        processed_eaten_column = NULL,

        process_sold_income_column = NULL,
        process_sold_frequency_column = NULL,
        process_sold_frequency_other_column = NULL
    ),
    leaves=list(
        fp_name = "fp_name",
        base_name="leaves",

        #Info needed for leaves calculations
        amount = "leaves_amount",
        amount_units = "leaves_amount_units",
        amount_units_other = "leaves_amount_units_other",

        # Info needed for proportions calculations
        use_column = "leaves_use",
        sold_prop_column = "leaves_sold_prop",
        consumed_column = "leaves_consumed_prop",
        processed_column = "leaves_process_prop",

        income_column = "leaves_sold_income",
        income_frequency = "leaves_sold_price_quantityunits",
        sold_frequency_other_column = "leaves_sold_amount_units_other",

        consume_gender="leaves_control_eating",
        sell_gender="leaves_who_sell",
        sell_income_gender="leaves_sold_income_who",

        processed_sold_column = "leaves_process_sold_prop",
        processed_eaten_column = "leaves_process_eaten_prop",

        process_sold_income_column = "leaves_process_income",
        process_sold_frequency_column = "leaves_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "leaves_process_amount_units_other"
    ),
    bark=list(
        fp_name = "fp_name",
        base_name="bark",

        #Info needed for bark calculations
        amount = "bark_amount",
        amount_units = "bark_amount_units",
        amount_units_other = "bark_amount_units_other",

        # Info needed for proportions calculations
        use_column = "bark_use",
        sold_prop_column = "bark_sold_prop",
        consumed_column = "bark_eaten_prop",
        processed_column = "bark_process_prop",

        income_column = "bark_sold_income",
        income_frequency = "bark_sold_price_quantityunits",
        sold_frequency_other_column = "bark_sold_amount_units_other",

        consume_gender="bark_control_eating",
        sell_gender="bark_who_sell",
        sell_income_gender="bark_sold_income_who",

        processed_sold_column = "bark_process_sold_prop",
        processed_eaten_column = "bark_process_eaten_prop",

        process_sold_income_column = "bark_process_income",
        process_sold_frequency_column = "bark_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "bark_process_amount_units_other"
    ),

    roots=list(
        fp_name = "fp_name",
        base_name="roots",

        #Info needed for bark calculations
        amount = "roots_amount",
        amount_units = "roots_amount_units",
        amount_units_other = "roots_amount_units_other",

        # Info needed for proportions calculations
        use_column = "roots_use",
        sold_prop_column = "roots_sold_prop",
        consumed_column = "roots_eaten_prop",
        processed_column = "roots_process_prop",

        income_column = "roots_sold_income",
        income_frequency = "roots_sold_price_quantityunits",
        sold_frequency_other_column = "roots_sold_amount_units_other",

        consume_gender="roots_control_eating",
        sell_gender="roots_who_sell",
        sell_income_gender="roots_sold_income_who",

        processed_sold_column = "roots_process_sold_prop",
        processed_eaten_column = "roots_process_eaten_prop",

        process_sold_income_column = "roots_process_income",
        process_sold_frequency_column = "roots_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "roots_process_amount_units_other"
    ),

    gum=list(
        fp_name = "fp_name",
        base_name="gum",

        #Info needed for bark calculations
        amount = "gum_amount",
        amount_units = "gum_amount_units",
        amount_units_other = "gum_amount_units_other",

        # Info needed for proportions calculations
        use_column = "gum_use",
        sold_prop_column = "gum_sold_prop",
        consumed_column = "gum_eaten_prop",
        processed_column = "gum_process_prop",

        income_column = "gum_sold_income_weekly",
        income_frequency = "gum_sold_price_quantityunits",
        sold_frequency_other_column = "gum_sold_amount_units_other",

        consume_gender="gum_control_eating",
        sell_gender="gum_who_sell",
        sell_income_gender="gum_sold_income_who",

        processed_sold_column = "gum_process_sold_prop",
        processed_eaten_column = "gum_process_eaten_prop",

        process_sold_income_column = "gum_process_income",
        process_sold_frequency_column = "gum_process_sold_price_quantityunits",
        process_sold_frequency_other_column = "gum_process_amount_units_other"
    )


)

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (3) UNITS CONVERSION FUNCTION
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
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


#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (4) YIELD FUNCTION
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
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
        return(tree_aid_df)
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


#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (5) Numeric proportions
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
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

#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# (6) CALCULATION AMOUNT EATEN AND SOLD
#---------------------------------------------------------------
#---------------------------------------------------------------
#---------------------------------------------------------------
# Create function that calculated amount eaten and sold for both processed and unprocessed ntfps
ntfp_sold_and_consumed_calculation <- function(

    data,
    fp_harvest_kg,

    fp_prop_sold_numeric,
    fp_amount_sold_kg,

    fp_prop_consumed_numeric,
    fp_amount_consumed_kg,



    fp_props_process_numeric,
    fp_amount_process_kg,

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
        warning(paste0("Missing Columns:",harvested_columns,". Calculate amounts harvested before calculating amounts sold\n"))
    }
    if (all(sold_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",sold_columns,". Have not calculated the numeric proportions of amount of non-timber forest products sold. Calculate proportions sold before calculating amounts sold\n"))
    }

    if (all(harvested_columns %in% colnames(data)) == T & all(sold_columns %in% colnames(data)) == T) {


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
    }

    # Moving on to ntfp consumed
    number_of_loops <- find_number_of_loops(data, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    consumed_columns <- paste0(fp_prop_consumed_numeric, "_", c(1:number_of_loops))


    if (all(harvested_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",harvested_columns,". Calculate amounts harvested before calculating amounts consumed\n"))
    }
    if (all(consumed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",consumed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products consumed Calculate proportions sold before calculating amounts consumed\n"))
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
    # Beginning with ntfp processed
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    harvested_columns <- paste0(fp_harvest_kg, "_", c(1:number_of_loops))
    processed_columns <- paste0(fp_props_process_numeric, "_", c(1:number_of_loops))

    if (all(harvested_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",harvested_columns,". Have not calculated the amounts harvested in kg. Calculate amounts harvested before calculating amounts processed\n"))
    }
    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed. Calculate proportions processed before calculating amounts processed\n"))
    }


    if (all(harvested_columns %in% colnames(data)) == T & all(processed_columns %in% colnames(data)) == T) {

        harvest_data <- data[harvested_columns]
        processed_prop_data <- data[processed_columns]

        amount_processed_kg <- tibble::as_tibble(harvest_data * processed_prop_data)
        colnames(amount_processed_kg) <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_kg,
            new_column_name = fp_amount_process_kg,
            old_column_name = fp_props_process_numeric,
            loop_structure = T
        )
    }

    # PROCESSED SOLD
    # Beginning with ntfp processed sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    processed_columns <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))
    processed_sold_columns <- paste0(fp_props_process_sold_numeric, "_", c(1:number_of_loops))

    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the amounts processed in kg. Calculate amounts processed before calculating amount of processed ntfp which was sold\n"))
    }

    if (all(processed_sold_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_sold_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed and sold. Calculate proportions processed and sold before calculating amounts processed and sold\n"))
    }

    if (all(processed_columns %in% colnames(data)) == T & all(processed_sold_columns %in% colnames(data)) == T) {

        processed_data <- data[processed_columns]
        processed_sold_prop_data <- data[processed_sold_columns]

        amount_processed_sold_kg <- tibble::as_tibble(processed_data * processed_sold_prop_data)
        colnames(amount_processed_sold_kg) <- paste0(fp_amount_process_sold_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_sold_kg,
            new_column_name = fp_amount_process_sold_kg,
            old_column_name = fp_props_process_sold_numeric,
            loop_structure = T
        )
    }


    # PROCESSED CONSUMED
    # Beginning with ntfp processed sold
    number_of_loops <- find_number_of_loops(tree_aid_df, name_column = "fp_name")
    processed_columns <- paste0(fp_amount_process_kg, "_", c(1:number_of_loops))
    processed_consumed_columns <- paste0(fp_prop_process_consumed_numeric, "_", c(1:number_of_loops))

    if (all(processed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_columns,". Have not calculated the amounts processed in kg. Calculate amounts processed before calculating amount of processed ntfp which was consumed\n"))
    }

    if (all(processed_consumed_columns %in% colnames(data)) == F) {
        warning(paste0("Missing Columns:",processed_consumed_columns,". Have not calculated the numeric proportions of amount of non-timber forest products processed and consumed Calculate proportions processed and consumed before calculating amounts processed and sold\n"))
    }

    if (all(processed_columns %in% colnames(data)) == T & all(processed_consumed_columns %in% colnames(data)) == T) {

        processed_data <- data[processed_columns]
        processed_consumed_prop_data <- data[processed_consumed_columns]

        amount_processed_consumed_kg <- tibble::as_tibble(processed_data * processed_consumed_prop_data)
        colnames(amount_processed_consumed_kg) <- paste0(fp_amount_process_consumed_kg, "_", c(1:number_of_loops))

        data <- add_column_after_specific_column(
            data = data,
            new_data = amount_processed_consumed_kg,
            new_column_name = fp_amount_process_consumed_kg,
            old_column_name = fp_prop_process_consumed_numeric,
            loop_structure = T
        )
    }




    return(data)
}


# Loop through individual NTFPS
for (fp_product in fp_products){

    # Amount Harvested
    tree_aid_df <- calculate_fp_harvest(
        tree_aid_df=tree_aid_df,
        fp_harvest_conversions=units_and_conversions$fp_amount_to_kg,
        name_column=fp_product$fp_name,
        amount_column=fp_product$amount,
        unit_column=fp_product$amount_units
    )

    # Numeric proportions sold
    tree_aid_df <- fp_proportions_all(
        tree_aid_df=tree_aid_df,
        use="sell",
        use_column=fp_product$use_column,
        prop_column=fp_product$sold_prop_column,
        new_column_name=paste0(fp_product$base_name,"_sold_prop_numeric")
    )

    # Numeric proportions consumed
    tree_aid_df <- fp_proportions_all(
        tree_aid_df=tree_aid_df,
        use="eat",
        use_column=fp_product$use_column,
        prop_column=fp_product$consumed_column,
        new_column_name=paste0(fp_product$base_name,"_eaten_prop_numeric")
    )

    # Numeric proportions sold
    tree_aid_df <- fp_proportions_all(
        tree_aid_df=tree_aid_df,
        use="process",
        use_column=fp_product$use_column,
        prop_column=fp_product$processed_column,
        new_column_name=paste0(fp_product$base_name,"_process_prop_numeric")
    )

    # Numeric proportions processed and eaten
    tree_aid_df <- fp_proportions_all(
        tree_aid_df=tree_aid_df,
        use="process",
        use_column=fp_product$use_column,
        prop_column=fp_product$processed_column,
        new_column_name=paste0(fp_product$base_name,"_process_eaten_prop_numeric")
    )



    # Numeric proportions processed and sold
    tree_aid_df <- fp_proportions_all(
        tree_aid_df=tree_aid_df,
        use="process",
        use_column=fp_product$use_column,
        prop_column=fp_product$processed_sold_column,
        new_column_name=paste0(fp_product$base_name,"_process_sold_prop_numeric")
    )


    # Calculating all amounts sold, consumed, processed, processed and eaten, processed and sold
    tree_aid_df <- ntfp_sold_and_consumed_calculation(

        data=tree_aid_df,
        fp_harvest_kg=paste0(fp_product$amount,"_kg"),

        fp_prop_sold_numeric=paste0(fp_product$base_name,"_sold_prop_numeric"),
        fp_amount_sold_kg=paste0(fp_product$amount,"_sold_kg"),

        fp_prop_consumed_numeric=paste0(fp_product$base_name,"_eaten_prop_numeric"),
        fp_amount_consumed_kg=paste0(fp_product$amount,"_eaten_kg"),

        fp_props_process_numeric=paste0(fp_product$base_name,"_process_prop_numeric"),
        fp_amount_process_kg=paste0(fp_product$amount,"_processed_kg"),

        fp_props_process_sold_numeric=paste0(fp_product$base_name,"_process_sold_prop_numeric"),
        fp_amount_process_sold_kg=paste0(fp_product$amount,"_process_sold_kg"),

        fp_prop_process_consumed_numeric=paste0(fp_product$base_name,"_process_eaten_prop_numeric"),
        fp_amount_process_consumed_kg=paste0(fp_product$amount,"_process_eaten_kg")

    )

}




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
