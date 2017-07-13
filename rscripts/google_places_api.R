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

package_list <- c("Rcpp","openxlsx","stringi","XML","curl","httr")
suppressWarnings(suppressMessages(
	for( i in 1:length(package_list))
	{
		if (!require(package_list[i],character.only = TRUE,lib.loc=r_libs))
    			{
		      	install.packages(package_list[i],dep=TRUE,repos="http://cran.us.r-project.org",lib=r_libs)
			        if(!require(package_list[i],character.only = TRUE,lib.loc=r_libs)) stop("Package not found")
			}
	}           ))


#Get Place API function

get_place <- function(empresa,tipo_merc,uf,region){

    oldw<-getOption("warn")
    options(warn = -1)

    #Setting Filter
    if(tipo_merc=="hipermercado")
        {
            filter_strings <- c("hiper", "hyper", "Hiper", "Hyper")
            filter_strings_neg <- c("Super","super","Mini","mini","minuto","Minuto","express","Express")
        }
    else if (tipo_merc=="supermercado")
        {
            filter_strings <- c("")
            filter_strings_neg <- c("Hiper","hiper","Hyper","hyper","Mini","mini","minuto","Minuto","express","Express")
        }
    else if (tipo_merc=="minimercado")
        {
            filter_strings <- c("mini","Mini","minuto","Minuto","express","Express")
            filter_strings_neg <- c("Hiper","hiper","Hyper","hyper","Super","super")
        }



    if(empresa=="carrefour")
        {
            filter_strings_neg <- c(filter_strings_neg,"extra","Extra","p.o","P.o","a..car","A..car")
        }
    else if(empresa=="extra")
        {
            filter_strings_neg <- c(filter_strings_neg,"carrefour","Carrefour","p.o","P.o","a..car","A..car","bom","Bom")
        }
    else if(empresa=="pao+de+acucar")
        {
            filter_strings_neg <- c(filter_strings_neg,"extra","Extra","carrefour","Carrefour")
        }
    filter_strings_neg <- c(filter_strings_neg,"dep.sito","Dep.sito","escrit.rio","Escrit.rio")


    if(!empresa=="pao+de+acucar")
        {
            filter_strings_emp <- c(empresa,stri_trans_totitle(empresa))
        }
    else
        {
            filter_strings_emp <- c("p.o","a..car","P.o","A..car")
        }

    #Getting Data
    query_string <- paste0(tipo_merc,"+",empresa,"+",uf)
    if(!region=="")
        {
            query_string <- paste0(query_string,"+",region)
        }
    #print(query_string)
    query_url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/xml?query=",query_string,"&key=",api_key)
    #print(query_url)
    xml.url <- GET(query_url, accept_xml())

    #XML Parsing
    query_result <- xmlTreeParse(xml.url, useInternalNodes=TRUE)

    next_page <- unlist(xpathApply(query_result, "//next_page_token", xmlValue))
    while(!is.null(next_page))
        {
            Sys.sleep(2)
            next_query_url <- paste0(query_url,"&pagetoken=",next_page)
            next_xml.url <- GET(next_query_url,accept_xml())
            next_query_result <- xmlTreeParse(next_xml.url, useInternalNodes=TRUE)
            next_page <- unlist(xpathApply(next_query_result, "//next_page_token", xmlValue))
            #print(next_page)
            query_result <- addChildren(query_result,next_query_result)
        }

    num_results <- xpathApply(query_result, "count(//result)", xmlValue)
    #print(uf)
    #print(num_results)

    #Data Processing
    query_table <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

    for(i in 1:num_results)
        {
            id <- unlist(xpathApply(query_result, paste0("//result[",i,"]/place_id"), xmlValue))
            name <- unlist(xpathApply(query_result, paste0("//result[",i,"]/name"), xmlValue))
            type <- unlist(xpathApply(query_result, paste0("//result[",i,"]/type[1]"), xmlValue))
            address <- unlist(xpathApply(query_result, paste0("//result[",i,"]/formatted_address"), xmlValue))
            rating <- unlist(xpathApply(query_result, paste0("//result[",i,"]/rating"), xmlValue))
            if(is.null(rating))
                {
                    rating <- NA
                }

        #Filtering Data
            #By Market Type
            filter_vector <- logical(0)
            filter_vector_neg <- logical(0)
            filter_vector_emp <- logical(0)
            for(i in 1:length(filter_strings))
                {
                    filter_vector <- c(filter_vector,grepl(filter_strings[i],name))
                }
            for(i in 1:length(filter_strings_neg))
                {
                    filter_vector_neg <- c(filter_vector_neg,grepl(filter_strings_neg[i],name))
                }
            for(i in 1:length(filter_strings_emp))
                {
                    filter_vector_emp <- c(filter_vector_emp,grepl(filter_strings_emp[i],name))
                }
            type_filter <- any(filter_vector)
            type_filter_neg <- !any(filter_vector_neg)
            type_filter_emp <- any(filter_vector_emp)
            
            #By State
            uf_filter <- grepl(uf,address)

            if(type == "grocery_or_supermarket" && type_filter && type_filter_neg && type_filter_emp && uf_filter)
                {
                    result_table <- cbind(id,name,uf,rating,address)

                    query_table <- rbind(query_table,result_table)
                }
        }
    options(warn=oldw)

    return(query_table)
}

get_comments <- function(id_table)
    {
        oldw<-getOption("warn")
        options(warn = -1)

        comments_table <- data.frame(id=character(0),uf=character(0),rating=character(0),comment=character(0))
        for(i in 1:nrow(id_table))
            {
                query_url <- paste0("https://maps.googleapis.com/maps/api/place/details/xml?placeid=",id_table[i,1],"&key=",api_key)

                
                xml.url <- GET(query_url, accept_xml())

            #XML Parsing
                query_result <- xmlTreeParse(xml.url, useInternalNodes=TRUE)
                num_comments <- xpathApply(query_result, "count(//result/review)", xmlValue)
                #print(num_comments)
                if(num_comments>0)
                    {
                        for(j in 1:num_comments)
                            {
                                id <- as.character(id_table[i,1])
                                uf <- as.character(id_table[i,2])
                                rating <- unlist(xpathApply(query_result, paste0("//result/review[",j,"]/rating"), xmlValue))
                                if(is.null(rating))
                                    {
                                        rating <- NA
                                    }
                                comment <- unlist(xpathApply(query_result, paste0("//result/review[",j,"]/text"), xmlValue))
                                comment <-stri_trans_general(comment,"Latin-ASCII")
                                if(is.null(comment))
                                    {
                                        comment <- ""
                                    }

                                row_data <- cbind(id,uf,rating,comment)
                                comments_table <- rbind(comments_table,row_data)
                                #print(row_data)
                            }
                    }
            }
        options(warn=oldw)
        return(comments_table)
    }

#Creating Workbook

workbook <- createWorkbook()
addWorksheet(workbook,"mercados_carrefour")
addWorksheet(workbook,"comentarios_carrefour")
addWorksheet(workbook,"mercados_extra")
addWorksheet(workbook,"comentarios_extra")
addWorksheet(workbook,"mercados_pao_de_acucar")
addWorksheet(workbook,"comentarios_pao_de_acucar")
######################
#    Getting Data    #
######################

#States/Cities List
uf_city <- list(
                list("AC",c("")),
                list("AL",c("")),
                list("AP",c("")),
                list("AM",c("")),
                list("BA",c("")),
                list("CE",c("")),
                list("DF",c("")),
                list("ES",c("")),
                list("GO",c("")),
                list("MA",c("")),
                list("MT",c("")),
                list("MS",c("")),
                list("MG",c("")),
                list("PA",c("")),
                list("PB",c("")),
                list("PR",c("")),
                list("PE",c("")),
                list("PI",c("")),
                list("RJ",c("")),
                list("RN",c("")),
                list("RS",c("")),
                list("RO",c("")),
                list("RR",c("")),
                list("SC",c("")),
                list("SP",c("sao+paulo","sao+jose+dos+campos","santos","registro","sorocaba","campinas","bauru","central","ribeirao+preto","franca","barretos","marilia","presidente+prudente","aracatuba","sao+jose+do+rio+preto")),
                list("SE",c("")),
                list("TO",c(""))
                )


length_uf <- length(uf_city)
#length_uf <- 4


##################
#    Carrefour   #
##################

#Hiper

table_hiper_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_hiper_merc_part <- get_place("carrefour","hipermercado",uf_loop,city_loop)
                table_hiper_merc <- rbind(table_hiper_merc,table_hiper_merc_part)
            }
    }

dupe <- duplicated(table_hiper_merc)
table_hiper_merc <- table_hiper_merc[!dupe,]

table_hiper_merc$address <- sapply(table_hiper_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_hiper_merc$name <- sapply(table_hiper_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_hiper <- nrow(table_hiper_merc)

write.csv2(table_hiper_merc,paste0("proc_data",slash,"mercados_carrefour_hiper.csv"))
writeData(workbook,"mercados_carrefour","Hipermercados")
writeDataTable(workbook,"mercados_carrefour",format(table_hiper_merc,decimal.mark=","),startRow=2)

#Super

table_super_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("carrefour","supermercado",uf_loop,city_loop)
                table_super_merc <- rbind(table_super_merc,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc)
table_super_merc <- table_super_merc[!dupe,]

table_super_merc$address <- sapply(table_super_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc$name <- sapply(table_super_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc)

write.csv2(table_super_merc,paste0("proc_data",slash,"mercados_carrefour_super.csv"))
writeData(workbook,"mercados_carrefour","Supermercados",startRow=row_hiper+4)
writeDataTable(workbook,"mercados_carrefour",format(table_super_merc,decimal.mark=","),startRow=row_hiper+5)

#Express

table_mini_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

#for(i in 1:length(uf_city))
for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("carrefour","minimercado",uf_loop,city_loop)
                table_mini_merc <- rbind(table_mini_merc,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc)
table_mini_merc <- table_mini_merc[!dupe,]

table_mini_merc$address <- sapply(table_mini_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc$name <- sapply(table_mini_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc)

write.csv2(table_mini_merc,paste0("proc_data",slash,"mercados_carrefour_exp.csv"))
writeData(workbook,"mercados_carrefour","Express",startRow=row_hiper+row_super+7)
writeDataTable(workbook,"mercados_carrefour",format(table_mini_merc,decimal.mark=","),startRow=row_hiper+row_super+8)

mean_merc <- data.frame(
        name = c("Media de Hipermercados",
                 "Media de Supermercados",
                 "Media de Mercados Express",
                 "Media Total"),
        mean = c(mean(as.numeric(as.character(table_hiper_merc$rating)),na.rm=TRUE),
                 mean(as.numeric(as.character(table_super_merc$rating)),na.rm=TRUE),
                 mean(as.numeric(as.character(table_mini_merc$rating)),na.rm=TRUE),
                 mean(c(as.numeric(as.character(table_hiper_merc$rating)),
                        as.numeric(as.character(table_super_merc$rating)),
                        as.numeric(as.character(table_mini_merc$rating))),na.rm=TRUE))
                    )
writeData(workbook,"mercados_carrefour","Media de Notas",startRow=row_hiper+row_super+row_mini+10)
writeData(workbook,"mercados_carrefour",format(mean_merc,decimal.mark=","),startRow=row_hiper+row_super+row_mini+11,colNames=FALSE,withFilter=FALSE)


carrefour_ids <- rbind(table_hiper_merc,table_super_merc,table_mini_merc)[,c(1,3)]



#################
#     Extra     #
#################

#Hiper

table_hiper_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_hiper_merc_part <- get_place("extra","hipermercado",uf_loop,city_loop)
                table_hiper_merc <- rbind(table_hiper_merc,table_hiper_merc_part)
            }
    }

dupe <- duplicated(table_hiper_merc)
table_hiper_merc <- table_hiper_merc[!dupe,]

table_hiper_merc$address <- sapply(table_hiper_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_hiper_merc$name <- sapply(table_hiper_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_hiper <- nrow(table_hiper_merc)

write.csv2(table_hiper_merc,paste0("proc_data",slash,"mercados_extra_hiper.csv"))
writeData(workbook,"mercados_extra","Hipermercados")
writeDataTable(workbook,"mercados_extra",format(table_hiper_merc,decimal.mark=","),startRow=2)

#Super

table_super_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("extra","supermercado",uf_loop,city_loop)
                table_super_merc <- rbind(table_super_merc,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc)
table_super_merc <- table_super_merc[!dupe,]

table_super_merc$address <- sapply(table_super_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc$name <- sapply(table_super_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc)

write.csv2(table_super_merc,paste0("proc_data",slash,"mercados_extra_super.csv"))
writeData(workbook,"mercados_extra","Supermercados",startRow=row_hiper+4)
writeDataTable(workbook,"mercados_extra",format(table_super_merc,decimal.mark=","),startRow=row_hiper+5)

#Mini

table_mini_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("extra","minimercado",uf_loop,city_loop)
                table_mini_merc <- rbind(table_mini_merc,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc)
table_mini_merc <- table_mini_merc[!dupe,]

table_mini_merc$address <- sapply(table_mini_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc$name <- sapply(table_mini_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc)

write.csv2(table_mini_merc,paste0("proc_data",slash,"mercados_extra_mini.csv"))
writeData(workbook,"mercados_extra","Minimercados",startRow=row_hiper+row_super+7)
writeDataTable(workbook,"mercados_extra",format(table_mini_merc,decimal.mark=","),startRow=row_hiper+row_super+8)

mean_merc <- data.frame(
        name = c("Media de Hipermercados",
                 "Media de Supermercados",
                 "Media de Minimercados",
                 "Media Total"),
        mean = c(mean(as.numeric(as.character(table_hiper_merc$rating)),na.rm=TRUE),
                 mean(as.numeric(as.character(table_super_merc$rating)),na.rm=TRUE),
                 mean(as.numeric(as.character(table_mini_merc$rating)),na.rm=TRUE),
                 mean(c(as.numeric(as.character(table_hiper_merc$rating)),
                        as.numeric(as.character(table_super_merc$rating)),
                        as.numeric(as.character(table_mini_merc$rating))),na.rm=TRUE))
                    )
writeData(workbook,"mercados_extra","Media de Notas",startRow=row_hiper+row_super+row_mini+10)
writeData(workbook,"mercados_extra",format(mean_merc,decimal.mark=","),startRow=row_hiper+row_super+row_mini+11,colNames=FALSE,withFilter=FALSE)

extra_ids <- rbind(table_hiper_merc,table_super_merc,table_mini_merc)[,c(1,3)]



#########################
#     Pao de Acucar     #
#########################

##Hiper
#
#table_hiper_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))
#
#for(i in 1:length_uf)
#    {
#        for(j in 1:length(uf_city[[i]][[2]]))
#            {
#                uf_loop <- uf_city[[i]][[1]]
#                city_loop <- uf_city[[i]][[2]][j]
#
#                print(city_loop)
#                table_hiper_merc_part <- get_place("pao+de+acucar","hipermercado",uf_loop,city_loop)
#                table_hiper_merc <- rbind(table_hiper_merc,table_hiper_merc_part)
#            }
#    }
#
#dupe <- duplicated(table_hiper_merc)
#table_hiper_merc <- table_hiper_merc[!dupe,]
#
#table_hiper_merc$address <- sapply(table_hiper_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
#table_hiper_merc$name <- sapply(table_hiper_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))
#
#row_hiper <- nrow(table_hiper_merc)
#
#write.csv2(table_hiper_merc,paste0("proc_data",slash,"mercados_pao_acucar_hiper.csv"))
#writeData(workbook,"mercados_pao_de_acucar","Hipermercados")
#writeDataTable(workbook,"mercados_pao_de_acucar",format(table_hiper_merc,decimal.mark=","),startRow=2)

#Super

table_super_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("pao+de+acucar","supermercado",uf_loop,city_loop)
                table_super_merc <- rbind(table_super_merc,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc)
table_super_merc <- table_super_merc[!dupe,]

table_super_merc$address <- sapply(table_super_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc$name <- sapply(table_super_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc)

write.csv2(table_super_merc,paste0("proc_data",slash,"mercados_pao_acucar_super.csv"))
writeData(workbook,"mercados_pao_de_acucar","Supermercados",startRow=1)
writeDataTable(workbook,"mercados_pao_de_acucar",format(table_super_merc,decimal.mark=","),startRow=2)

#Minuto

table_mini_merc <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in 1:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("pao+de+acucar","minimercado",uf_loop,city_loop)
                table_mini_merc <- rbind(table_mini_merc,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc)
table_mini_merc <- table_mini_merc[!dupe,]

table_mini_merc$address <- sapply(table_mini_merc$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc$name <- sapply(table_mini_merc$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc)

write.csv2(table_mini_merc,paste0("proc_data",slash,"mercados_pao_acucar_minuto.csv"))
writeData(workbook,"mercados_pao_de_acucar","Mercados Minuto",startRow=row_super+4)
writeDataTable(workbook,"mercados_pao_de_acucar",format(table_mini_merc,decimal.mark=","),startRow=row_super+5)

mean_merc <- data.frame(
        name = c("Media de Supermercados",
                 "Media de Mercados Minuto",
                 "Media Total"),
        mean = c(mean(as.numeric(as.character(table_super_merc$rating)),na.rm=TRUE),
                 mean(as.numeric(as.character(table_mini_merc$rating)),na.rm=TRUE),
                 mean(c(as.numeric(as.character(table_super_merc$rating)),
                        as.numeric(as.character(table_mini_merc$rating))),na.rm=TRUE))
                    )
writeData(workbook,"mercados_pao_de_acucar","Media de Notas",startRow=row_super+row_mini+7)
writeData(workbook,"mercados_pao_de_acucar",format(mean_merc,decimal.mark=","),startRow=row_super+row_mini+8,colNames=FALSE,withFilter=FALSE)

pao_de_acucar_ids <- rbind(table_super_merc,table_mini_merc)[,c(1,3)]


#Getting Comments

carrefour_comments <- get_comments(carrefour_ids)
extra_comments <- get_comments(extra_ids)
pao_de_acucar_comments <- get_comments(pao_de_acucar_ids)

#Saving Comments
write.csv2(carrefour_comments,paste0("proc_data",slash,"comments_carrefour.csv"))
write.csv2(extra_comments,paste0("proc_data",slash,"comments_extra.csv"))
write.csv2(pao_de_acucar_comments,paste0("proc_data",slash,"comments_pao_de_acucar.csv"))


writeDataTable(workbook,"comentarios_carrefour",format(carrefour_comments,decimal.mark=","))
writeDataTable(workbook,"comentarios_extra",format(extra_comments,decimal.mark=","))
writeDataTable(workbook,"comentarios_pao_de_acucar",format(pao_de_acucar_comments,decimal.mark=","))

saveWorkbook(workbook,paste0("Avaliacao-Mercados-",Sys.Date(),".xlsx"),overwrite=TRUE)






#query_result
#xml.url
#query_result
#head(table_merc)
