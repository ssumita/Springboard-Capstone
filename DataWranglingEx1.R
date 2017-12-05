# Read the input csv file
refine.data = read.csv(".\\data\\refine_original.csv",stringsAsFactors=F);

#copy input dataframe to output df so all changes could be made in the output df itself
refine.clean.data=refine.data 

# Get list of unique company names
unique(refine.clean.data$company)

# convert company name to lower case
refine.clean.data$company=tolower(refine.clean.data$company)

#1 clean the company names

# remove all white spaces in company column
refine.clean.data$company <- gsub('\\s+', '',refine.clean.data$company)

refine.clean.data=refine.clean.data%>%mutate(company=ifelse(company=="phillips","philips",ifelse(company=="phllips","philips",ifelse(company == "phillps","philips",ifelse(company == "fillips","philips",ifelse(company == "phlips","philips",company))))))
refine.clean.data=refine.clean.data%>%mutate(company=ifelse(company=="akz0","akzo",company))
refine.clean.data=refine.clean.data%>%mutate(company=ifelse(company=="vanhouten","van houten",company))
refine.clean.data=refine.clean.data%>%mutate(company=ifelse(company=="unilver","unilever",company))

#refine.data$company[grep("ph*",refine.data$company)]="philips"
#refine.data$company[grep("un*",refine.data$company)]="unilever"
#refine.data$company[grep("ak*",refine.data$company)]="akzo"

#refine.data$company[gsub("^[]*ak[]*$","akzo",refine.data$company)]

#2 Separate product code and number
refine.clean.data$product_code <- sapply(strsplit(as.character(refine.clean.data$Product.code...number),'-'), "[", 1)
refine.clean.data$product_number <- sapply(strsplit(as.character(refine.clean.data$Product.code...number),'-'), "[", 2)

#3 Add product categories
refine.clean.data=refine.clean.data%>%mutate(product_category=ifelse(product_code=="p","Smartphone",ifelse(product_code=="v","TV",ifelse(product_code=="x","Laptop",ifelse(product_code =="q","Tablet",NA)))))

#4 Add full address
refine.clean.data=refine.clean.data%>%mutate(full_address=paste(address, city, country, sep = ','))

#5 Create dummy variables for Company and category
refine.clean.data=refine.clean.data%>%mutate(company_philips=ifelse(company =="philips",1,0),company_akzo=ifelse(company == "akzo",1,0),company_van_houten=ifelse(company == "van houten",1,0),company_unilever=ifelse(company=="unilever",1,0))
refine.clean.data=refine.clean.data%>%mutate(product_smartphone = ifelse(product_code== "p",1,0),product_tv = ifelse(product_code == "v",1,0),product_laptop=ifelse(product_code =="x",1,0),product_tablet = ifelse(product_code =="q",1,0))
