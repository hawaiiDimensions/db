# =============================================================================
IndiceEmpty # Extracts row indices of all empty entries by column.
HDIMempty # Extracts HDIM numbers of empty entries within a target column.
# =============================================================================
IndiceMethod # Extracts row indices of empty entries contingent to method.
HDIMmethod # Extracts HDIM numbers of empty entries contingent to method.
# =============================================================================
IndiceMisspelled # Extracts row indices of misspelled entries by column.
HDIMmisspelled # Extracts HDIM numbers of misspelled entries by column.
# =============================================================================
InvalidDateInd # Extracts indices of invalid date entries in the date column of a dataframe.
InvalidDateHDIM # Retrieves HDIM numbers of invalid date entries in a dataframe date column.
InvalidMethodDateHDIM # Retrieves HDIM numbers of invalid date entries in a method column.
# =============================================================================
ListEmptyIndice # Creates list of row indices of empty entries in multiple columns.
ListEmptyHDIM # Creates list of HDIM numbers of empty entries in multiple columns.
ListMisspelledHDIM # Extracts HDIM numbers of misspelled entries by columns.
ListEmptyMethod # Finds HDIM numbers of empty entries contingent to collection methods
# =============================================================================
UniqueEntries # Extracts all unique elements of a column within a dataframe.
StoreDb # Imports .csv from a URL as a database; formats for use with db package.
PatternHDIM # Finds HDIM locations of pattern mismatches within a colEvent column.
colEventMethod # Evolab specific version of HDIMmethod with fewer arguments.
# =============================================================================
DiagnoseDb # Throroughly checks the Dimensions database for invalid and missing entries.
# =============================================================================