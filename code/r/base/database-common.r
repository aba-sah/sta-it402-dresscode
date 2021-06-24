
writeToDataStore <-
    function(awardData, dbConnection, dbTable = "sqa_data", overwriteDataStore = FALSE) {
        
        if (is_null(awardData) || (nrow(awardData) == 0))
            stop("You must read in qualification data (in a dataframe) containing at least one row!")
        
        table_exists <- as.logical(dbGetQuery(dbConnection, paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", 
                                                                   dbTable, "' and type = 'table'")))
        
        if (!table_exists | overwriteDataStore)
            currentRowCount <- 0
        else
            currentRowCount <- dbGetQuery(dbConnection, 
                                          paste("SELECT COUNT(*) FROM", dbTable)) %>%
                                    as.integer
        
        
        if (!table_exists) 
            dbWriteTable(dbConnection, dbTable, awardData)
        else 
            dbWriteTable(dbConnection, dbTable, awardData, append = !overwriteDataStore, overwrite = overwriteDataStore)
        
        # todo - dump duplicates - using dbWriteTable means no primary key set
        
        
        invisible(as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", dbTable))) - currentRowCount)
    }


removeDuplicatesFromDataStore <- 
    function(dbConnection = NULL, dbTable = NULL) {
        
        if (is_null(dbConnection) | is_null(dbTable))
            stop("You must specify database connection and table to deduplicate!")

        
        if (!as.logical(dbGetQuery(dbConnection, 
                                   paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", dbTable, "' and type = 'table'"))))
            stop("No changes made; source table does not exist!")
        
        
        row_count <- as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", dbTable)))
        if (row_count == 0) {
            
            message(paste0("Table'", dbTable, "' empty; no changes made."))
            invisible(row_count) # nothing to do - allows simple conversion to FALSE
        }
        
        
        tmp_table <- paste0(dbTable, "_tmp")
        if (as.integer(dbGetQuery(dbConnection, 
                                  paste0("SELECT COUNT(*) FROM sqlite_master WHERE name = '", tmp_table, "' and type = 'table'"))) > 0) 
            dbRemoveTable(dbConnection, tmp_table)

        dbExecute(dbConnection, paste("CREATE TABLE ", tmp_table, " AS SELECT * FROM", dbTable, "WHERE 0"))
        dbExecute(dbConnection, paste("INSERT INTO", tmp_table,
                                      "SELECT DISTINCT * FROM", dbTable)
                 )
        
        row_count <- row_count - as.integer(dbGetQuery(dbConnection, paste("SELECT COUNT(*) FROM", tmp_table)))
        if (row_count > 0) {
            dbRemoveTable(dbConnection, dbTable)
            dbExecute(dbConnection, paste("ALTER TABLE", tmp_table, "RENAME TO", dbTable))
        } else
            dbRemoveTable(dbConnection, tmp_table)


        invisible(row_count) # no. of rows deleted
    }

