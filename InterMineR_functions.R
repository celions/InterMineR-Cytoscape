listMines = function(){
  # retrieve information from InterMine registry
  r = GET("http://registry.intermine.org/service/instances")
  stop_for_status(r)
  
  # get the url for every Mine
  res = httr::content(r)
  
  # urls = sapply(res$instances, function(x){x$url})
  urls = vapply(res$instances, function(x){x$url}, character(1))
  
  # assign Mine names to urls
  # names(urls) = sapply(res$instances, function(x){x$name})
  names(urls) = vapply(res$instances, function(x){x$name}, character(1))
  
  return(urls)
}

setClass(
  "Service",
  representation(
    mine = "character",
    token = "character"
  )
)

initInterMine <- function(mine = listMines()["HumanMine"], token=""){
  im <- new("Service",mine = mine, token = token)
  return(im)
}

getTemplates <- function(im, format="data.frame", timeout=3) {
  # JSON
  if (format == "list") {
    r <- GET(paste(im@mine, "/service/templates?format=json", sep=""))
    stop_for_status(r)
    template.string <- content(r, "text")
    res <- fromJSON(template.string)$templates
    res
    # XML
  } else {
    r <- GET(paste(im@mine, "/service/templates?format=xml", sep=""))
    stop_for_status(r)
    template <- content(r)
    res <- listTemplateSummary(template)
    res
  }
}

listTemplateSummary <- function(template) {
  doc <- xmlTreeParse(template)
  r <- xmlRoot(doc)
  template.attr <- xmlApply(r, xmlAttrs)
  template.df <- do.call(rbind, template.attr)
  rownames(template.df) <- NULL
  data.frame(template.df[,c(1,2)], stringsAsFactors=FALSE)
}

getTemplateQuery = function(im, name, timeout=3){
  r <- GET(paste(im@mine, "/service/templates/", name, "?format=json", sep=""))
  stop_for_status(r)
  ql <- content(r, "text")
  jsonTemplate <- fromJSON(ql)$template
  ql_1 <- content(r, "parsed",  encoding = "ISO-8859-1")
  jsonTemplate_1 <- ql_1$template
  return(list(jsonTemplate,jsonTemplate_1))
}

setConstraints = function(
  paths,
  operators,
  values,
  modifyQueryConstraints,
  m.index
){
  
  # check if modifyQueryConstraints has been assigned
  if(missing(modifyQueryConstraints)){
    
    # check if all arguments have the same length
    length.arguments = c(
      length(paths),
      length(operators),
      length(values))
    
    if(length(unique(length.arguments)) != 1){
      stop("All arguments of setConstraints function must have the same length")
    }
    
    # check paths, operators and values
    #if(class(paths) != "character"){
    if(!is.character(paths)){
      stop("paths argument must be of the class character")
    }
    #if(class(operators) != "character"){
    if(!is.character(operators)){
      stop("operators argument must be of the class character")
    }
    #if(class(values) != "list"){
    if(!is.list(values)){
      stop("values argument must be of the class list")
    }
    
    #check the length of its object of the argument values (list)
    #length.values = sapply(values, length)
    
    length.values = vapply(values, length, 1)
    
    if(sum(length.values > 1) > 1){
      stop("Only one object of the values list can be of length greater",
           "\n than one!")
    }
    
    where.result = list(NULL)
    # iterate through argument values
    for(j in seq(unique(length.arguments))){
      where.result[[j]] = list(
        path = paths[j],
        op = operators[j],
        value = values[[j]],
        code = LETTERS[j])
    }
    
    return(where.result)
    
  } else {
    modifyQueryConstraints<-modifyQueryConstraints[2][[1]]
    # check if m.index exists and is of the right class
    #if(missing(m.index) | !class(m.index)%in%c("numeric", "integer")){
    if( missing(m.index) | !(is.integer(m.index) | is.numeric(m.index)) ){
      stop("Assign m.index argument with a numeric or integer vector")
    }
    
    # check if m.index is less than the legth of modifyQueryConstraints query 
    # constraints
    if(length(modifyQueryConstraints$where)<max(m.index)){
      stop("m.index value can not be greater than the length of the constraints", 
           "\n which are to be modified")
    }
    
    where.result = modifyQueryConstraints
    
    # check each argument and replace the appropriate constraint if it exists
    if(!missing(paths)){
      
      #if(class(paths) != "character"){
      if(!is.character(paths)){
        stop("paths argument must be of the class character")
      }
      
      for(i in seq(length(paths))){
        where.result$where[[m.index[i]]]$path = paths[i]
      }
    }
    
    if(!missing(operators)){
      
      #if(class(operators) != "character"){
      if(!is.character(operators)){
        stop("operators argument must be of the class character")
      }
      
      for(i in seq(length(operators))){
        where.result$where[[m.index[i]]]$op = operators[i]
      }
    }
    
    if(!missing(values)){
      
      #if(class(values) != "list"){
      if(!is.list(values)){
        stop("values argument must be of the class list")
      }
      
      for(i in seq(length(values))){
        where.result$where[[m.index[i]]]$value = values[[i]]
      }
    }
    
    return(where.result$where)
  }
}

setQuery = function(
  select,
  orderBy,
  where,
  name = "",
  description = "",
  inheritQuery
){
  
  if(missing(inheritQuery)){
    
    # check the classes of select, name and description arguments
    argument.classes1 = c(class(select), class(name), class(description))
    
    if(!all(argument.classes1 == "character")){
      
      #ind1 = which(argument.classes1 != "character")
      ind1 = argument.classes1 != "character"
      
      message.error1 = paste0(
        "The following arguments are not of class 'character': ",
        paste(c("select", "name", "description")[ind1], collapse = ", ")
      )
      stop(message.error1)
    }
    
    # if orderBy is missing then the first element of the select argument is 
    # assigned to it
    if(missing(orderBy)){
      
      #if(class(where) != "list"){
      if(!is.list(where)){
        stop("where argument is not of class 'list'")
      }
      
      # set orderBy
      orderBy.value = "ASC"
      names(orderBy.value) = select[1]
      
      orderBy = list(orderBy.value)
      
    } else {
      
      # check the classes of orderBy and where
      argument.classes2 = c(class(orderBy), class(where))
      
      if(!all(argument.classes2 == "list")){
        
        #ind2 = which(argument.classes2 != "list")
        ind2 = argument.classes2 != "list"
        
        message.error2 = paste0(
          "The following arguments are not of class 'list': ",
          paste(c("orderBy", "where")[ind2], collapse = ", ")
        )
        stop(message.error2)
      }
    }
    
    # set query object of formal class 'InterMineR'
    query.object = new(
      "InterMineR",
      name = name,
      description = description,
      select = select,
      orderBy = orderBy,
      where = where
    )
    
  } else {
    
    # check every argument and replace where missing(argument) is TRUE
    if(!missing(select)){
      #if(class(select) != "character"){
      if(!is.character(select)){
        stop("select argument is not of class 'character'")
      } else {
        #inheritQuery$select = select
        inheritQuery[1][[1]][["select"]] = select
      }
    }
    #
    if(!missing(name)){
      #if(class(name) != "character"){
      if(!is.character(name)){
        stop("name argument is not of class 'character'")
      } else {
        #inheritQuery$name = name
        inheritQuery[1][[1]][["name"]] = name
      }
    }
    #
    if(!missing(description)){
      #if(class(description) != "character"){
      if(!is.character(description)){
        stop("description argument is not of class 'character'")
      } else {
        #inheritQuery$description = description
        inheritQuery[1][[1]][["description"]] = description
      }
    }
    #
    if(!missing(orderBy)){
      #if(class(orderBy) != "list"){
      if(!is.list(orderBy)){
        stop("orderBy argument is not of class 'list'")
      } else {
        #inheritQuery$orderBy = orderBy
        inheritQuery[2][[1]][["orderBy"]] = orderBy
      }
    }
    #
    if(!missing(where)){
      #if(class(where) != "list"){
      if(!is.list(where)){
        stop("where argument is not of class 'list'")
      } else {
        #inheritQuery$where = where
        inheritQuery[1][[1]][["where"]] = where
      }
    }
    # set query object of formal class 'InterMineR'
    query.object = new(
      "InterMineR",
      name = inheritQuery[1][[1]][["name"]],
      description = inheritQuery[1][[1]][["description"]],
      select = inheritQuery[1][[1]][["select"]],
      orderBy = inheritQuery[2][[1]][["orderBy"]],
      where = inheritQuery[1][[1]][["where"]]
    )
  }
  return(query.object)
}

setClass(
  "InterMineR",
  representation(
    name = "character",
    description = "character",
    select = "character",
    orderBy = "list",
    where = "list"
  )
)

setGeneric("runQuery",function(im, qry, timeout=60) standardGeneric("runQuery"))

setMethod(
  "runQuery",
  signature(qry = "InterMineR"),
  function(im, qry, timeout=60){
    
    # retrieve the length of value for each constraint
    value.length = c()
    constraints.with.values = c()
    
    for(i in seq(length(slot(qry,"where")))){
      
      # check if inherited constraints have value
      if("value" %in% names(slot(qry,"where")[[i]])){
        
        constraints.with.values = c(constraints.with.values, i)
        
        value.length = c(value.length,
                         length(slot(qry,"where")[[i]][["value"]])
        )
      }
    }
    
    # check if more than one constraints have multiple values
    if(sum(value.length > 1) > 1){
      stop("Only one of the query contraints can possess multiple values!")
      
      # check if one constraint has multiple values
    } else if(any(value.length > 1)){
      
      # identify contraint with multiple values
      #ind = constraints.with.values[which(value.length > 1)]
      ind = constraints.with.values[value.length > 1]
      
      answer.list = list(NULL)
      # iterate through multiple values
      for(y in seq(length(slot(qry,"where")[[ind]][["value"]]))){
        
        # get value
        v = slot(qry,"where")[[ind]][["value"]][y]
        
        # get XML query string
        query = InterMineR_Query2XML(qry, index = ind, value2 = v)
        
        # run InterMineR query
        query.unencoded <- toString.XMLNode(query)
        
        query.str <- URLencode(toString.XMLNode(query))
        query.str <- gsub("&", '%26', query.str)
        query.str <- gsub(";", '%3B', query.str)
        
        base::try(mine.url <- im@mine, silent=TRUE)
        base::try(mine.url <- im[[1]], silent=TRUE)
        
        r <- GET(paste(mine.url, "/service/query/results?query=",
                       query.str,"&format=xml",sep=""))
        #If there's any HTTP error, print the query as well for easier debugging.
        stop_for_status(r, paste("query", query.unencoded))
        
        res <- content(r)
        res.xml <- xmlRoot(xmlParse(res))
        
        if (length(getNodeSet(res.xml, "//Result")) > 0) {
          answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
          colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                       "\\s+", perl=TRUE)[[1]]
        } else {
          # no results
          answer=NULL
        }
        
        # save in list
        answer.list[[y]] = answer
        
      }
      
      # rbind all results to data.frame and return
      answer.df = do.call(rbind, answer.list)
      
      return(answer.df)
      
    } else {
      
      # get XML query string
      query = InterMineR_Query2XML(qry)
      
      # run query
      query.unencoded <- toString.XMLNode(query)
      
      query.str <- URLencode(toString.XMLNode(query))
      query.str <- gsub("&", '%26', query.str)
      query.str <- gsub(";", '%3B', query.str)
      
      r <- GET(paste(im@mine, "/service/query/results?query=",
                     query.str,"&format=xml",sep=""))
      stop_for_status(r)
      res <- httr::content(r)
      
      #If there's any HTTP error, print the query as well for easier debugging.
      stop_for_status(r, paste("query", query.unencoded))
      res <- content(r)
      res.xml <- xmlRoot(xmlParse(res))
      
      if (length(getNodeSet(res.xml, "//Result")) > 0) {
        answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
        colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                     "\\s+", perl=TRUE)[[1]]
      } else {
        # no results
        answer=NULL
      }
      
      # return answer
      return(answer)
      
    }
  }
)

setMethod(
  "runQuery",
  signature(qry = "list"),
  function(im, qry, timeout=60){
    
    if (is.list(qry)) {
      # convert to XML to run in intermine
      query <- queryList2XML(qry)
    } else if(isXMLString(qry)) {
      query <- xmlParseString(qry)
    }
    
    answer <- NULL
    
    query.unencoded <- toString.XMLNode(query)
    
    query.str <- URLencode(query.unencoded)
    query.str <- gsub("&", '%26', query.str)
    query.str <- gsub(";", '%3B', query.str)
    
    base::try(mine.url <- im@mine, silent=TRUE)
    base::try(mine.url <- im[[1]], silent=TRUE)
    
    r <- GET(paste(mine.url, "/service/query/results?query=",
                   query.str,"&format=xml",sep=""))
    
    #If there's any HTTP error, print the query as well for easier debugging.
    stop_for_status(r, paste("query", query.unencoded))
    
    res <- httr::content(r)
    res.xml <- xmlRoot(xmlParse(res))
    
    if (length(getNodeSet(res.xml, "//Result")) > 0) {
      answer = xmlToDataFrame(res.xml, stringsAsFactors=FALSE)
      colnames(answer) <- strsplit(xmlAttrs(query)[["view"]],
                                   "\\s+", perl=TRUE)[[1]]
    } else {
      # no results
      answer=NULL
    }
    
    return(answer)
    
  }
)

InterMineR_Query2XML = function(ql, index, value2){
  nq <- newXMLNode("query")
  xmlAttrs(nq)[["name"]] <- ql@name
  xmlAttrs(nq)[["model"]] <- "genomic"
  xmlAttrs(nq)[["view"]] <- paste(ql@select,collapse=" ")
  if(!is.null(ql@description)){
    xmlAttrs(nq)[["longDescription"]] <- ql@description
  }
  if(!is.null(ql@orderBy)){
    orderByString = paste(names(ql@orderBy[[1]]), ql@orderBy[[1]], collapse=" ")
    xmlAttrs(nq)[["sortOrder"]] <- orderByString
  }
  
  if(!is.null(ql@where)){
    for(i in 1:length(ql@where)){
      cnc <- newXMLNode("constraint")
      xmlAttrs(cnc)[["path"]] <- ql@where[[i]][["path"]]
      if (!is.null(ql@where[[i]][["type"]])) {
        xmlAttrs(cnc)[["type"]] <- ql@where[[i]][["type"]]
      }
      # query constraints on TYPE don't have these attributes
      # so skip them. Should test for NULL instead.
      if (is.null(ql@where[[i]][["type"]])) {
        
        # check if index exists
        if(!missing(index)){
          # identify which constraint has multiple values
          if(i == index){
            # assign one value at a time
            xmlAttrs(cnc)[["value"]] <- value2
          } else {
            xmlAttrs(cnc)[["value"]] <- ql@where[[i]][["value"]]
          }
        } else {
          xmlAttrs(cnc)[["value"]] <- ql@where[[i]][["value"]]
        }
        
        
        xmlAttrs(cnc)[["code"]] <- paste(ql@where[[i]][["code"]],collapse=" ")
        xmlAttrs(cnc)[["op"]] <- paste(ql@where[[i]][["op"]],collapse=" ")
        xmlAttrs(cnc)[["extraValue"]] <- paste(ql@where[[i]][["extraValue"]],collapse=" ")
      }
      addChildren(nq, kids=list(cnc), at=xmlSize(nq))
    }
  }
  
  #if(!is.null(ql@constraintLogic)){
  #  xmlAttrs(nq)[["constraintLogic"]] <- ql@constraintLogic
  #}
  
  return(nq)
}

queryList2XML <- function(ql){
  
  nq <- newXMLNode("query")
  xmlAttrs(nq)[["name"]] <- ql[[1]]$name
  xmlAttrs(nq)[["model"]] <- "genomic"
  xmlAttrs(nq)[["view"]] <- paste(ql[[1]]$select,collapse=" ")
  
  if(!is.null(ql[[1]]$description)){
    xmlAttrs(nq)[["longDescription"]] <- ql[[1]]$description
  }
  
  if(!is.null(ql[[1]]$orderBy)){
    orderByString = paste(names(ql[[1]]$orderBy), ql[[1]]$orderBy, collapse=" ")
    xmlAttrs(nq)[["sortOrder"]] <- orderByString
  }
  
  if(!is.null(ql[[2]]$where)){
    for(i in 1:length(ql[[2]][["where"]])){
      cnc <- newXMLNode("constraint")
      xmlAttrs(cnc)[["path"]] <- ql[[2]][["where"]][[i]][["path"]]
      if (!is.null(ql[[2]][["where"]][[i]][["type"]])) {
        xmlAttrs(cnc)[["type"]] <- ql[[2]][["where"]][[i]][["type"]]
      }
      # query constraints on TYPE don't have these attributes
      # so skip them. Should test for NULL instead.
      if (is.null(ql[[2]][["where"]][[i]][["type"]])) {
        # value
        if(!is.null(ql[[2]][["where"]][[i]][["value"]])){
          xmlAttrs(cnc)[["value"]] <- ql[[2]][["where"]][[i]][["value"]]
        }
        # loopPath
        if(!is.null(ql[[2]][["where"]][[i]][["loopPath"]])){
          xmlAttrs(cnc)[["loopPath"]] <- ql[[2]][["where"]][[i]][["loopPath"]]
        }
        # code
        xmlAttrs(cnc)[["code"]] <- paste(ql[[2]][["where"]][[i]][["code"]],
                                         collapse=" ")
        # op
        xmlAttrs(cnc)[["op"]] <- paste(ql[[2]][["where"]][[i]][["op"]],
                                       collapse=" ")
        # extraValue
        xmlAttrs(cnc)[["extraValue"]] = paste(
          ql[[2]][["where"]][[i]][["extraValue"]], collapse=" ")
      }
      addChildren(nq, kids=list(cnc), at=xmlSize(nq))
    }
  }
  
  if(!is.null(ql[[1]]$constraintLogic)){
    xmlAttrs(nq)[["constraintLogic"]] <- ql[[1]]$constraintLogic
  }
  
  nq
}

listModelSummary <- function(model){
  class.name <- names(model)
  class.parent <- lapply(class.name, function(x) {
    y <- model[[x]][["extends"]]
    if(is.list(y)){
      y <- NA
    }
    y
  })
  
  # class.name <- rep(class.name, sapply(class.parent, length))
  class.name <- rep(class.name, vapply(class.parent, length, 1))
  
  class.parent <- unlist(class.parent)
  igr <- graph.data.frame(data.frame(
    parent=class.parent[which(!is.na(class.parent))],
    name=class.name[which(!is.na(class.parent))]),
    vertices=data.frame(unique(c(class.name,
                                 class.parent[which(!is.na(class.parent))]))))
  
  igr.sp <- shortest.paths(igr, mode="in")
  att <- lapply(class.name, 
                function(x){
                  data.frame(
                    do.call(rbind,model[[x]][["attributes"]]), 
                    stringsAsFactors=FALSE)
                })
  
  names(att) <- class.name
  for(x in names(att)){
    try(att[[x]][,"term"]<-NULL,TRUE)
  }
  
  att.ext <- rep(list(NULL), length(class.name))
  att.ext <- lapply(class.name, function(x){
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    
    y <- unique(do.call(rbind, att[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  att.ext <- do.call(rbind, att.ext)
  att.ext$child_type <- ""
  rownames(att.ext) <- NULL
  
  # Error occuring when using HumanMine:
  # The fourth column of the att.ext variable is redundant and will prevent the
  # rbind(att.ext, ref.ext, col.ext) below!!!
  
  # columns 2 and 4 contain identical information for HumanMine!
  # all(tolower(att.ext[,2]) %in% gsub(" ", "", tolower(att.ext[,4])))
  
  # Therefore, we keep only the first 3 columns from the att.ext variable:
  att.ext = att.ext[,1:3]
  
  ref <- lapply(class.name, function(x) {
    y <- model[[x]][["references"]]
    if(length(y)==0){
      z <- data.frame(
        matrix(character(0), 0, 2, 
               dimnames=list(NULL,c("name", "referencedType")))
      )
    } else {
      z1 <- names(y)
      #z2 <- sapply(y, function(ye)
      #  ye[["referencedType"]])
      z2 <- vapply(y, function(ye){ye[["referencedType"]]}, character(1))
      z <- data.frame(name=z1, referencedType=z2)
    }
    z
  })
  names(ref) <- class.name
  
  ref.ext <- rep(list(NULL), length(class.name))
  
  ref.ext <- lapply(class.name, function(x) {
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    y <- unique(do.call(rbind, ref[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  
  ref.ext <- do.call(rbind, ref.ext)
  rownames(att.ext) <- NULL
  
  col <- lapply(class.name, function(x) {
    y <- model[[x]][["collections"]]
    if(length(y)==0){
      z <- data.frame(
        matrix(character(0), 0, 2,
               dimnames=list(NULL,c("name", "referencedType")))
      )
    } else {
      z1 <- names(y)
      #z2 <- sapply(y, function(ye) ye[["referencedType"]])
      z2 <- vapply(y, function(ye) {ye[["referencedType"]]}, character(1))
      z <- data.frame(name=z1, referencedType=z2)
    }
    z
  })
  names(col) <- class.name
  
  col.ext <- rep(list(NULL), length(class.name))
  
  col.ext <- lapply(class.name, function(x) {
    ext <- colnames(igr.sp)[which(is.finite(igr.sp[x, ]))]
    y <- unique(do.call(rbind, col[ext]))
    y <- cbind(class=rep(x, nrow(y)), y, stringsAsFactors=FALSE)
    colnames(y) <- c("type", "child_name", "child_type")
    y <- y[order(y$child_name),, drop=FALSE]
    rownames(y) <- NULL
    y
  })
  col.ext <- do.call(rbind, col.ext)
  rownames(col.ext) <- NULL
  res <- rbind(att.ext, ref.ext, col.ext)
  rownames(res) <- NULL
  res <- sqldf("select * from res order by type, child_type")
  res
}

simplifyResult = function(
  dataset,
  index_column,
  values_column,
  returnInDataframe = FALSE
) {
  
  # get index for columns
  # for index
  #if(class(index_column) %in% c("integer", "numeric")){
  if(is.integer(index_column) | is.numeric(index_column)){
    index = index_column
  } else {
    #index = which(colnames(dataset) == index_column)
    index = colnames(dataset) == index_column
  }
  
  # for values 
  #if(class(values_column) %in% c("integer", "numeric")){
  if(is.integer(values_column) | is.numeric(values_column)){
    values = values_column
  } else {
    #values = which(colnames(dataset) == values_column)
    values = colnames(dataset) == values_column
  }
  
  # collapse to comma-separated string
  simplified_results = tapply(
    X = dataset[,values],
    INDEX = dataset[,index],
    FUN = function(x){
      paste(x, collapse = ",")
    }
  )
  
  if(returnInDataframe){
    
    res2 = c()
    for(i in seq(nrow(dataset))){
      ind = which(rownames(simplified_results) %in% dataset[i,index])
      res2 = c(res2, simplified_results[ind])
    }
    
    dataset$simplified_results = res2
    
    return(dataset)
    
  } else {
    return(data.frame(simplified_results))
  }
}


if (is.null(getGeneric("summary"))) setGeneric("summary", function(object,...){
  standardGeneric("summary")
})

setGeneric("summary", function(object,...)standardGeneric("summary"))

setMethod(
  "summary",
  signature(object = "InterMineR"),
  function(object,...){
    
    # create query.log data.frame
    l = list(NULL)
    count = 0
    
    for(j in seq(length(slot(object,"where")))){
      
      x = slot(object,"where")[[j]]
      
      if("value" %in% names(x)){
        if(length(x[["value"]]) > 1){
          x[["value"]] = paste(x[["value"]], collapse = ",")
        }
        count = count + 1
        l[[count]] = data.frame(
          x[which(names(x) %in% c("path","op", "value", "code"))]
        )
      }
    }
    return(do.call(rbind,l))
  }
)
