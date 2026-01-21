package cache

Database_Table :: struct {
	name:          string,
	documentation: string,
	elements:      [dynamic]Database_Table_Element,
}

Database_Table_Element :: struct {
	name:          string,
	documentation: string,
	is_key:        bool,
	data_element:  string,
	data_type:     string,
	length:        int,
}

Data_Element :: struct {
	name:               string,
	description:        string,
	type_kind:          string,
	type_name:          string,
	data_type:          string,
	data_type_length:   int,
	data_type_decimals: int,
}

Table_Type :: struct {
	name:          string,
	documentation: string,
	element_type:  string,
	is_row_type:   bool,
}

Structure :: struct {
	name:          string,
	documentation: string,
	elements:      [dynamic]Structure_Element,
}

Structure_Element :: struct {
	name:          string,
	documentation: string,
	is_key:        bool,
	data_element:  string,
	data_type:     string,
	length:        int,
}
