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



##################
#   Get Places   #
##################



get_place <- function(empresa,tipo_merc,uf,region){

    oldw<-getOption("warn")
    options(warn = -1)

    #Setting Filter
    if(tipo_merc=="hipermercado")
        {
            filter_strings <- c("Hiper","hiper","Hyper","hyper")
            filter_strings_neg <- c("Super","super","Mini","mini","minuto","Minuto","express","Express","posto","Posto","bairro","Bairro")
        }
    else if (tipo_merc=="supermercado")
        {
            filter_strings <- c("")
            filter_strings_neg <- c("Hiper","hiper","Hyper","hyper","Mini","mini","minuto","Minuto","express","Express","padaria","Padaria")
        }
    else if (tipo_merc=="minimercado")
        {
            filter_strings <- c("mini","Mini","minuto","Minuto","express","Express")
            filter_strings_neg <- c("Hiper","hiper","Hyper","hyper","Super","super")
        }



    if(empresa=="carrefour")
        {

            if(tipo_merc=="hipermercado")
                {
                    #filter_strings <- c("Hiper","hiper","Hyper","hyper")
                }

            filter_strings_neg <- c(filter_strings_neg,"extra","Extra","p.o","P.o","a..car","A..car","dia","Dia")

            filter_strings_emp <- c("carrefour","Carrefour","CARREFOUR","CRFO","crfo","Crfo")

        }
    else if(empresa=="extra")
        {

            filter_strings_neg <- c(filter_strings_neg,"carrefour","Carrefour","p.o","P.o","a..car","A..car","bom","Bom")

            filter_strings_emp <- c("extra","Extra","EXTRA")

        }
    else if(empresa=="pao+de+acucar")
        {

            filter_strings_neg <- c(filter_strings_neg,"extra","Extra","carrefour","Carrefour","bondinho","Bondinho")

            filter_strings_emp <- c("p.o","a..car","P.o","A..car")

        }
    filter_strings_neg <- c(filter_strings_neg,"dep.sito","Dep.sito","escrit.rio","Escrit.rio","drogaria","Drogaria","farm.cia","Farm.cia")



    #Getting Data
    query_string <- paste0(tipo_merc,"+",empresa,"+",uf)
    if(!region=="")
        {
            query_string <- paste0(query_string,"+",region)
        }
    #print(query_string)
    query_url <- paste0("https://maps.googleapis.com/maps/api/place/textsearch/xml?query=",query_string,"&key=",api_key,"&language=pt-BR")
    #print(query_url)
    xml.url <- GET(query_url, accept_xml())

    #XML Parsing
    query_result <- xmlTreeParse(xml.url, useInternalNodes=TRUE)
    #if(empresa=="extra")
    #    {
    #        print("############################")
    #        print(map(xmlToList(xmlRoot(query_result))$result,"name"))
    #    }
    query_result <- xmlRoot(query_result)

    #Data Processing
    query_table <- data.frame(id=character(0),name=character(0),class=character(0),numrating=character(0),rating=character(0),address=character(0))

    num_results <- xpathApply(query_result, "count(//result)", xmlValue)

    for(i in 1:num_results)
        {
            id <- unlist(xpathApply(query_result, paste0("//result[",i,"]/place_id"), xmlValue))
            name <- unlist(xpathApply(query_result, paste0("//result[",i,"]/name"),xmlValue))
            if(tipo_merc=="hipermercado")
                {
                    class <- "Hiper"
                }
            else if(tipo_merc=="supermercado")
                {
                    class <- "Super"
                }
            else if(tipo_merc=="minimercado")
                {
                    if(empresa=="carrefour")
                        {
                            class <- "Express"
                        }
                    else if(empresa=="extra")
                        {
                            class <- "Mini"
                        }
                    else if(empresa=="pao+de+acucar")
                        {
                            class <- "Minuto"
                        }
                }
            numrating="NA"
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
                    result_table <- cbind(id,name,uf,class,numrating,rating,address)

                    query_table <- rbind(query_table,result_table)
                }
            else if(empresa=="extra" && tipo_merc=="hipermercado")
                {
                    result_table <- cbind(name,uf)

                    #print(result_table)
                }
        }

    next_page <- unlist(xpathApply(query_result, "//next_page_token", xmlValue))
    while(!is.null(next_page))
        {
            Sys.sleep(2)
            next_query_url <- paste0(query_url,"&pagetoken=",next_page)
            next_xml.url <- GET(next_query_url,accept_xml())
            next_query_result <- xmlTreeParse(next_xml.url, useInternalNodes=TRUE)
            query_result <- xmlRoot(next_query_result)
            num_results <- xpathApply(query_result, "count(//result)", xmlValue)
            for(i in 1:num_results)
                {
                    id <- unlist(xpathApply(query_result, paste0("//result[",i,"]/place_id"), xmlValue))
                    name <- unlist(xpathApply(query_result, paste0("//result[",i,"]/name"),xmlValue))
                    if(tipo_merc=="hipermercado")
                        {
                            class <- "Hiper"
                        }
                    else if(tipo_merc=="supermercado")
                        {
                            class <- "Super"
                        }
                    else if(tipo_merc=="minimercado")
                        {
                            if(empresa=="carrefour")
                                {
                                    class <- "Express"
                                }
                            else if(empresa=="extra")
                                {
                                    class <- "Mini"
                                }
                            else if(empresa=="pao+de+acucar")
                                {
                                    class <- "Minuto"
                                }
                        }
                    numrating="NA"
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
                            result_table <- cbind(id,name,uf,class,numrating,rating,address)

                            query_table <- rbind(query_table,result_table)
                        }
                    else if(empresa=="extra" && tipo_merc=="hipermercado")
                        {
                            result_table <- cbind(name,uf)

                            #print(result_table)
                        }
                }
            next_page <- unlist(xpathApply(query_result, "//next_page_token", xmlValue))
            #print(next_page)
            ####################
            #query_result <- addChildren(query_result,next_query_result)
        }

    #print(uf)
    #print(num_results)


    options(warn=oldw)

    return(query_table)
}


####################
#   Get Comments   #
####################


get_comments <- function(id_table)
    {
        oldw<-getOption("warn")
        options(warn = -1)

        comments_table <- data.frame(id=character(0),uf=character(0),class=character(0),rating=character(0),comment=character(0))
        num_comments_list <- c()
        for(i in 1:nrow(id_table))
            {
                query_url <- paste0("https://maps.googleapis.com/maps/api/place/details/xml?placeid=",id_table[i,1],"&key=",api_key,"&language=pt-BR")

                
                xml.url <- GET(query_url, accept_xml())

            #XML Parsing
                query_result <- xmlTreeParse(xml.url, useInternalNodes=TRUE)
                num_comments <- xpathApply(query_result, "count(//result/review)", xmlValue)
                #print(num_comments)
                num_comments_list <- c(num_comments_list,num_comments)
                if(num_comments>0)
                    {
                        for(j in 1:num_comments)
                            {
                                id <- as.character(id_table[i,1])
                                uf <- as.character(id_table[i,3])
                                class <- as.character(id_table[i,4])
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

                                row_data <- cbind(id,uf,class,rating,comment)
                                comments_table <- rbind(comments_table,row_data)
                                #print(row_data)
                            }
                    }
            }
        id_table$numrating <- as.character(id_table$numrating)
        id_table$numrating <- num_comments_list
        id_table$numrating <- as.factor(id_table$numrating)

        options(warn=oldw)
        return(list(comments_table,id_table))
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
                list("ES",c("","vila+velha")),
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
                list("RJ",c("","cachambi")),
                list("RN",c("")),
                list("RS",c("")),
                list("RO",c("")),
                list("RR",c("")),
                list("SC",c("")),
                list("SP",c("sao+paulo","sao+jose+dos+campos","santos","sorocaba","campinas","ribeirao+preto","aracatuba","sao+jose+do+rio+preto","ABC","diadema","santo+andre","osasco","guaruja","sao+caetano","taubate","piracicaba","jundiai","sao+bernardes")),
                list("SE",c("")),
                list("TO",c(""))
                )


ini_length <- 1
#ini_length <- 19

length_uf <- length(uf_city)
#length_uf <- 21



##################
#    Carrefour   #
##################


#Hiper


table_hiper_merc_carr <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_hiper_merc_part <- get_place("carrefour","hipermercado",uf_loop,city_loop)
                table_hiper_merc_carr <- rbind(table_hiper_merc_carr,table_hiper_merc_part)
            }
    }

dupe <- duplicated(table_hiper_merc_carr)
table_hiper_merc_carr <- table_hiper_merc_carr[!dupe,]

table_hiper_merc_carr$address <- sapply(table_hiper_merc_carr$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_hiper_merc_carr$name <- sapply(table_hiper_merc_carr$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_hiper <- nrow(table_hiper_merc_carr)


#Super


table_super_merc_carr <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("carrefour","supermercado",uf_loop,city_loop)
                table_super_merc_carr <- rbind(table_super_merc_carr,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc_carr)
table_super_merc_carr <- table_super_merc_carr[!dupe,]

table_super_merc_carr$address <- sapply(table_super_merc_carr$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc_carr$name <- sapply(table_super_merc_carr$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc_carr)


#Express


table_mini_merc_carr <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

#for(i in 1:length(uf_city))
for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("carrefour","minimercado",uf_loop,city_loop)
                table_mini_merc_carr <- rbind(table_mini_merc_carr,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc_carr)
table_mini_merc_carr <- table_mini_merc_carr[!dupe,]

table_mini_merc_carr$address <- sapply(table_mini_merc_carr$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc_carr$name <- sapply(table_mini_merc_carr$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc_carr)


#Getting Comments


carrefour_comments <- data.frame(id=character(0),uf=character(0),class=character(0),rating=character(0),comment=character(0))

if(nrow(table_hiper_merc_carr)>0)
{
comments_return <- get_comments(table_hiper_merc_carr)
carrefour_comments <- rbind(carrefour_comments,comments_return[[1]])
table_hiper_merc_carr <- comments_return[[2]]
}

if(nrow(table_super_merc_carr)>0)
{
comments_return <- get_comments(table_super_merc_carr)
carrefour_comments <- rbind(carrefour_comments,comments_return[[1]])
table_super_merc_carr <- comments_return[[2]]
}

if(nrow(table_mini_merc_carr)>0)
{
comments_return <- get_comments(table_mini_merc_carr)
carrefour_comments <- rbind(carrefour_comments,comments_return[[1]])
table_mini_merc_carr <- comments_return[[2]]
}


#Medias


mean_hiper_carr <-weighted.mean(
                            as.numeric(as.character(table_hiper_merc_carr$rating)),
                            as.numeric(as.character(table_hiper_merc_carr$numrating)),
                            na.rm=TRUE)

mean_super_carr <-weighted.mean(
                            as.numeric(as.character(table_super_merc_carr$rating)),
                            as.numeric(as.character(table_super_merc_carr$numrating)),
                            na.rm=TRUE)

mean_mini_carr <-weighted.mean(
                            as.numeric(as.character(table_mini_merc_carr$rating)),
                            as.numeric(as.character(table_mini_merc_carr$numrating)),
                            na.rm=TRUE)

mean_total_carr <-weighted.mean(
                        c(as.numeric(as.character(table_hiper_merc_carr$rating)),
                          as.numeric(as.character(table_super_merc_carr$rating)),
                          as.numeric(as.character(table_mini_merc_carr$rating))
                          ),
                        c(as.numeric(as.character(table_hiper_merc_carr$numrating)),
                          as.numeric(as.character(table_super_merc_carr$numrating)),
                          as.numeric(as.character(table_mini_merc_carr$numrating))
                          ),
                        na.rm=TRUE
                                )


mean_carr <- data.frame(
        name = c("Media Ponderada de Hipermercados",
                 "Media Ponderada de Supermercados",
                 "Media Ponderada de Mercados Express",
                 "Media Ponderada Total"),
        mean = c(mean_hiper_carr,mean_super_carr,mean_mini_carr,mean_total_carr)
                        )

#Saving Places


write.csv2(table_hiper_merc_carr,paste0("proc_data",slash,"mercados_carrefour_hiper.csv"))
writeData(workbook,"mercados_carrefour","Hipermercados")
writeDataTable(workbook,"mercados_carrefour",format(table_hiper_merc_carr,decimal.mark=","),startRow=2)


write.csv2(table_super_merc_carr,paste0("proc_data",slash,"mercados_carrefour_super.csv"))
writeData(workbook,"mercados_carrefour","Supermercados",startRow=row_hiper+4)
writeDataTable(workbook,"mercados_carrefour",format(table_super_merc_carr,decimal.mark=","),startRow=row_hiper+5)


write.csv2(table_mini_merc_carr,paste0("proc_data",slash,"mercados_carrefour_exp.csv"))
writeData(workbook,"mercados_carrefour","Express",startRow=row_hiper+row_super+7)
writeDataTable(workbook,"mercados_carrefour",format(table_mini_merc_carr,decimal.mark=","),startRow=row_hiper+row_super+8)


#Saving Means


writeData(workbook,"mercados_carrefour","Media de Notas",startRow=row_hiper+row_super+row_mini+10)
writeData(workbook,"mercados_carrefour",format(mean_carr,decimal.mark=","),startRow=row_hiper+row_super+row_mini+11,colNames=FALSE,withFilter=FALSE)


#Saving Comments


write.csv2(carrefour_comments,paste0("proc_data",slash,"comments_carrefour.csv"))
writeDataTable(workbook,"comentarios_carrefour",format(carrefour_comments,decimal.mark=","))




#################
#     Extra     #
#################


#Hiper


table_hiper_merc_ex <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_hiper_merc_part <- get_place("extra","hipermercado",uf_loop,city_loop)
                table_hiper_merc_ex <- rbind(table_hiper_merc_ex,table_hiper_merc_part)
            }
    }

dupe <- duplicated(table_hiper_merc_ex)
table_hiper_merc_ex <- table_hiper_merc_ex[!dupe,]

table_hiper_merc_ex$address <- sapply(table_hiper_merc_ex$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_hiper_merc_ex$name <- sapply(table_hiper_merc_ex$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_hiper <- nrow(table_hiper_merc_ex)


#Super


table_super_merc_ex <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("extra","supermercado",uf_loop,city_loop)
                table_super_merc_ex <- rbind(table_super_merc_ex,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc_ex)
table_super_merc_ex <- table_super_merc_ex[!dupe,]

table_super_merc_ex$address <- sapply(table_super_merc_ex$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc_ex$name <- sapply(table_super_merc_ex$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc_ex)


#Mini


table_mini_merc_ex <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("extra","minimercado",uf_loop,city_loop)
                table_mini_merc_ex <- rbind(table_mini_merc_ex,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc_ex)
table_mini_merc_ex <- table_mini_merc_ex[!dupe,]

table_mini_merc_ex$address <- sapply(table_mini_merc_ex$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc_ex$name <- sapply(table_mini_merc_ex$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc_ex)


#Getting Comments


extra_comments <- data.frame(id=character(0),uf=character(0),class=character(0),rating=character(0),comment=character(0))

if(nrow(table_hiper_merc_ex)>0)
{
comments_return <- get_comments(table_hiper_merc_ex)
extra_comments <- rbind(extra_comments,comments_return[[1]])
table_hiper_merc_ex <- comments_return[[2]]
}

if(nrow(table_super_merc_ex)>0)
{
comments_return <- get_comments(table_super_merc_ex)
extra_comments <- rbind(extra_comments,comments_return[[1]])
table_super_merc_ex <- comments_return[[2]]
}

if(nrow(table_mini_merc_ex)>0)
{
comments_return <- get_comments(table_mini_merc_ex)
extra_comments <- rbind(extra_comments,comments_return[[1]])
table_mini_merc_ex <- comments_return[[2]]
}


#Medias


mean_hiper_ex <-weighted.mean(
                            as.numeric(as.character(table_hiper_merc_ex$rating)),
                            as.numeric(as.character(table_hiper_merc_ex$numrating)),
                            na.rm=TRUE)

mean_super_ex <-weighted.mean(
                            as.numeric(as.character(table_super_merc_ex$rating)),
                            as.numeric(as.character(table_super_merc_ex$numrating)),
                            na.rm=TRUE)

mean_mini_ex <-weighted.mean(
                            as.numeric(as.character(table_mini_merc_ex$rating)),
                            as.numeric(as.character(table_mini_merc_ex$numrating)),
                            na.rm=TRUE)

mean_total_ex <-weighted.mean(
                        c(as.numeric(as.character(table_hiper_merc_ex$rating)),
                          as.numeric(as.character(table_super_merc_ex$rating)),
                          as.numeric(as.character(table_mini_merc_ex$rating))
                          ),
                        c(as.numeric(as.character(table_hiper_merc_ex$numrating)),
                          as.numeric(as.character(table_super_merc_ex$numrating)),
                          as.numeric(as.character(table_mini_merc_ex$numrating))
                          ),
                        na.rm=TRUE
                                )


mean_ex <- data.frame(
        name = c("Media Ponderada de Hipermercados",
                 "Media Ponderada de Supermercados",
                 "Media Ponderada de Minimercados",
                 "Media Ponderada Total"),
        mean = c(mean_hiper_ex,mean_super_ex,mean_mini_ex,mean_total_ex)
                        )

#Saving Places


write.csv2(table_hiper_merc_ex,paste0("proc_data",slash,"mercados_extra_hiper.csv"))
writeData(workbook,"mercados_extra","Hipermercados")
writeDataTable(workbook,"mercados_extra",format(table_hiper_merc_ex,decimal.mark=","),startRow=2)


write.csv2(table_super_merc_ex,paste0("proc_data",slash,"mercados_extra_super.csv"))
writeData(workbook,"mercados_extra","Supermercados",startRow=row_hiper+4)
writeDataTable(workbook,"mercados_extra",format(table_super_merc_ex,decimal.mark=","),startRow=row_hiper+5)


write.csv2(table_mini_merc_ex,paste0("proc_data",slash,"mercados_extra_mini.csv"))
writeData(workbook,"mercados_extra","Minimercados",startRow=row_hiper+row_super+7)
writeDataTable(workbook,"mercados_extra",format(table_mini_merc_ex,decimal.mark=","),startRow=row_hiper+row_super+8)


#Saving Means


writeData(workbook,"mercados_extra","Media de Notas",startRow=row_hiper+row_super+row_mini+10)
writeData(workbook,"mercados_extra",format(mean_ex,decimal.mark=","),startRow=row_hiper+row_super+row_mini+11,colNames=FALSE,withFilter=FALSE)


#Saving Comments


write.csv2(extra_comments,paste0("proc_data",slash,"comments_extra.csv"))
writeDataTable(workbook,"comentarios_extra",format(extra_comments,decimal.mark=","))



#########################
#     Pao de Acucar     #
#########################


#Super


table_super_merc_pao <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_super_merc_part <- get_place("pao+de+acucar","supermercado",uf_loop,city_loop)
                table_super_merc_pao <- rbind(table_super_merc_pao,table_super_merc_part)
            }
    }

dupe <- duplicated(table_super_merc_pao)
table_super_merc_pao <- table_super_merc_pao[!dupe,]

table_super_merc_pao$address <- sapply(table_super_merc_pao$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_super_merc_pao$name <- sapply(table_super_merc_pao$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_super <- nrow(table_super_merc_pao)


#Minuto


table_mini_merc_pao <- data.frame(id=character(0),name=character(0),rating=character(0),address=character(0))

for(i in ini_length:length_uf)
    {
        for(j in 1:length(uf_city[[i]][[2]]))
            {
                uf_loop <- uf_city[[i]][[1]]
                city_loop <- uf_city[[i]][[2]][j]

                #print(city_loop)
                table_mini_merc_part <- get_place("pao+de+acucar","minimercado",uf_loop,city_loop)
                table_mini_merc_pao <- rbind(table_mini_merc_pao,table_mini_merc_part)
            }
    }

dupe <- duplicated(table_mini_merc_pao)
table_mini_merc_pao <- table_mini_merc_pao[!dupe,]

table_mini_merc_pao$address <- sapply(table_mini_merc_pao$address, function(x) stri_trans_general(x,"Latin-ASCII"))
table_mini_merc_pao$name <- sapply(table_mini_merc_pao$name, function(x) stri_trans_general(x,"Latin-ASCII"))

row_mini <- nrow(table_mini_merc_pao)


#Getting Comments


pao_comments <- data.frame(id=character(0),uf=character(0),class=character(0),rating=character(0),comment=character(0))

if(nrow(table_super_merc_pao)>0)
{
comments_return <- get_comments(table_super_merc_pao)
pao_comments <- rbind(pao_comments,comments_return[[1]])
table_super_merc_pao <- comments_return[[2]]
}

if(nrow(table_mini_merc_pao)>0)
{
comments_return <- get_comments(table_mini_merc_pao)
pao_comments <- rbind(pao_comments,comments_return[[1]])
table_mini_merc_pao <- comments_return[[2]]
}


#Medias


mean_super_pao <-weighted.mean(
                            as.numeric(as.character(table_super_merc_pao$rating)),
                            as.numeric(as.character(table_super_merc_pao$numrating)),
                            na.rm=TRUE)

mean_mini_pao <-weighted.mean(
                            as.numeric(as.character(table_mini_merc_pao$rating)),
                            as.numeric(as.character(table_mini_merc_pao$numrating)),
                            na.rm=TRUE)

mean_total_pao <-weighted.mean(
                        c(as.numeric(as.character(table_super_merc_pao$rating)),
                          as.numeric(as.character(table_mini_merc_pao$rating))
                          ),
                        c(as.numeric(as.character(table_super_merc_pao$numrating)),
                          as.numeric(as.character(table_mini_merc_pao$numrating))
                          ),
                        na.rm=TRUE
                                )


mean_pao <- data.frame(
        name = c("Media Ponderada de Supermercados",
                 "Media Ponderada de Mercados Minuto",
                 "Media Ponderada Total"),
        mean = c(mean_super_pao,mean_mini_pao,mean_total_pao)
                        )

#Saving Places


write.csv2(table_super_merc_pao,paste0("proc_data",slash,"mercados_pao_acucar_super.csv"))
writeData(workbook,"mercados_pao_de_acucar","Supermercados")
writeDataTable(workbook,"mercados_pao_de_acucar",format(table_super_merc_pao,decimal.mark=","),startRow=2)


write.csv2(table_mini_merc_pao,paste0("proc_data",slash,"mercados_pao_acucar_minuto.csv"))
writeData(workbook,"mercados_pao_de_acucar","Mercados Minuto",startRow=row_super+4)
writeDataTable(workbook,"mercados_pao_de_acucar",format(table_mini_merc_pao,decimal.mark=","),startRow=row_super+5)


#Saving Means


writeData(workbook,"mercados_pao_de_acucar","Media de Notas",startRow=row_super+row_mini+7)
writeData(workbook,"mercados_pao_de_acucar",format(mean_pao,decimal.mark=","),startRow=row_super+row_mini+8,colNames=FALSE,withFilter=FALSE)


#Saving Comments


write.csv2(pao_comments,paste0("proc_data",slash,"comments_pao_acucar.csv"))
writeDataTable(workbook,"comentarios_pao_de_acucar",format(pao_comments,decimal.mark=","))



saveWorkbook(workbook,paste0("Avaliacao-Mercados-",Sys.Date(),".xlsx"),overwrite=TRUE)






#query_result
#xml.url
#query_result
#head(table_merc)
