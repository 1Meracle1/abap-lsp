package cache

Report :: struct {
	name:     string,
	doc:      ^Document,
	includes: [dynamic]^Document,
}

Function_Module :: struct {
	name:     string,
	doc:      ^Document,
	includes: [dynamic]^Document,
}

Function_Group :: struct {
	name:             string,
	doc:              ^Document,
	includes:         [dynamic]^Document,
	function_modules: [dynamic]^Function_Module,
}

Class :: struct {
	name: string,
	doc:  ^Document,
}

Interface :: struct {
	name: string,
	doc:  ^Document,
}
