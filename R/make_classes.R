
#' @title your mom
#' @name gen_classes
#' @description your mom
#' @export
gen_class <- function(schema){
    
    schemaList <- .parse_avro(schema)
    
    # Figure out the fields
    fields <- schemaList[["fields"]]
    publicFields <- setNames(
        vector("list", nrow(fields))
        , fields[["name"]]
    )
    
    # Create a constructor that assigns to those fields
    # -- assertions
    assertions <- sapply(1:nrow(fields), function(i){
        assertion <- .avro_type_to_assertion(fields[i, "type"])
        if (!is.null(assertion)){
            return(sprintf(assertion, fields[i, "name"]))
        }
    })
    if (!is.null(assertions)){
        assertion_text <- paste0(
            "assertthat::assert_that(\n"
            , paste0(assertions, collapse = ",\n")
            , "\n)\n"
        )
    }
    
    # -- assignment
    field_names <- fields[["name"]]
    assignment_text <- paste0(
        "    self$"
        , field_names
        , " <- "
        , field_names
        , collapse = "\n"
    )
    fun <- paste0(
        "function(){\n"
        , assertion_text
        , "\n"
        , assignment_text
        , "\n"
        , "\n\n    return(invisible(NULL))\n}")
    init <- eval(parse(text = fun))
    formals(init) <- publicFields
        
    out_class <- R6::R6Class(
        classname = schemaList[["name"]]
        , public = c(publicFields, "initialize" = init)
    )
    return(out_class)
}


global_classes <- function(schema){
    schemaList <- .parse_avro(schema)
    
    assign(
        x = schemaList[["name"]]
        , envir = parent.frame()
        , value = gen_classes(schema)
    )
    return(invisible(NULL))
}


#' @importFrom jsonlite fromJSON
.parse_avro <- function(some_json){
    return(jsonlite::fromJSON(some_json))
}


.avro_type_to_assertion <- function(type){
    assertthat::assert_that(
        assertthat::is.string(type)
    )
    
    assertion <- switch(
        type
        , "int" = "assertthat::is.number(%s)"
        , "string" = "assertthat::is.string(%s)"
        , "double" = "assertthat::is.number(%s)"
        , NULL
    )
    return(assertion)
}
