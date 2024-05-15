require(magrittr)
require(gsheet)
require(geoflow)

url = "https://docs.google.com/spreadsheets/d/1sm_QADBrshIeuqDuK9qQfy404NSxLEmFVjwoXVQ7dEw/edit?usp=sharing"
source = gsheet::gsheet2tbl(url) %>% as.data.frame()
validator = geoflow::geoflow_validator_entities$new(source = source)
validation_issues = validator$validate_content()
