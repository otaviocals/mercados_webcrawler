##############################
#    Retrieving arguments    #
##############################

myArgs <- commandArgs(trailingOnly = TRUE)
api_key <- myArgs[1]
slash <- myArgs[2]
data_folder <- myArgs[3]
r_scripts <- myArgs[4]
r_libs <- myArgs[5]
log_file <- myArgs[6]

setwd(data_folder)

#Loading libraries

package_list <- c("Rcpp","openxlsx","XML","curl","httr")
	for( i in 1:length(package_list))
	{
		if (!require(package_list[i],character.only = TRUE,lib.loc=r_libs))
    			{
		      	install.packages(package_list[i],dep=TRUE,repos="http://cran.us.r-project.org",lib=r_libs)
			        if(!require(package_list[i],character.only = TRUE,lib.loc=r_libs)) stop("Package not found")
			}
	}


get_place <- function(empresa,tipo_merc,uf){

    query_string <- paste0(tipo_merc,"+",empresa,"+",uf)
    query_url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/xml?query=",query_string,"&key=",api_key)

    xml.url <- GET(query_url, accept_xml())

    query_result <- xmlTreeParse(xml.url, useInternalNodes=TRUE)

    num_results <- xpathApply(query_result, "count(//result)", xmlValue)

    query_table <- data.frame(id=character(0),name=character(0),type=character(0),form_address=character(0),rating=character(0))

    for(i in 1:num_results)
        {
            name <- xpathApply(query_result, paste0("//result[",i,"]/name"), xmlValue)
            type <- xpathApply(query_result, paste0("//result[",i,"]/type[1]"), xmlValue)
            form_address <- xpathApply(query_result, paste0("//result[",i,"]/formatted_address"), xmlValue)
            rating <- xpathApply(query_result, paste0("//result[",i,"]/rating"), xmlValue)
            id <- xpathApply(query_result, paste0("//result[",i,"]/id"), xmlValue)
            
            result_table <- cbind(id,name,type,form_address,rating)

            query_table <- rbind(query_table,result_table)
        }
    #name <- xpathApply(query_result, "//result/name", xmlValue)
    #type <- xpathApply(query_result, "//result/type[1]", xmlValue)
    #form_address <- xpathApply(query_result, "//result/formatted_address", xmlValue)
    #rating <- xpathApply(query_result, "//result/rating", xmlValue)
    #id <- xpathApply(query_result, "//result/id", xmlValue)
    #
    #query_table <- cbind(id,name,type,form_address,rating)

    return(query_table)
}

#Creating Workbook

workbook <- createWorkbook()

######################
#    Getting Data    #
######################


table_merc <- get_place("carrefour","hipermercado","am")



#write.csv2(mov_ramos,paste0("proc_data",slash,"dem_cont_ramos.csv"))
#addWorksheet(workbook,"dem_cont_ramos")
#writeDataTable(workbook,"dem_cont_ramos",format(mov_ramos,decimal.mark=","))


saveWorkbook(workbook,paste0("SES-Susep-",Sys.Date(),".xlsx"),overwrite=TRUE)

#query_result
#xml.url
#query_result
table_merc
