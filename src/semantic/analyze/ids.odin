package abap_frontend_semantic_analyze

Unit_Id :: distinct u32
Scope_Id :: distinct u32
Symbol_Id :: distinct u32
Reference_Id :: distinct u32
Structure_Id :: distinct u32

INVALID_UNIT_ID :: Unit_Id(0xffffffff)
INVALID_SCOPE_ID :: Scope_Id(0xffffffff)
INVALID_SYMBOL_ID :: Symbol_Id(0xffffffff)
INVALID_REFERENCE_ID :: Reference_Id(0xffffffff)
INVALID_STRUCTURE_ID :: Structure_Id(0xffffffff)

Symbol_Handle :: struct {
	unit:   Unit_Id,
	symbol: Symbol_Id,
}

unit_id_index :: #force_inline proc(id: Unit_Id) -> int {
	return int(id)
}

scope_id_index :: #force_inline proc(id: Scope_Id) -> int {
	return int(id)
}

symbol_id_index :: #force_inline proc(id: Symbol_Id) -> int {
	return int(id)
}

reference_id_index :: #force_inline proc(id: Reference_Id) -> int {
	return int(id)
}

structure_id_index :: #force_inline proc(id: Structure_Id) -> int {
	return int(id)
}
