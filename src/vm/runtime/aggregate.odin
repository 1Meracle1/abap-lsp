package abap_frontend_vm_runtime

import "core:mem"
import "core:strings"

table_retain :: #force_inline proc "contextless" (table: ^Table_Data) {
	if table != nil {
		table.refs += 1
	}
}

table_release :: proc(table: ^Table_Data) {
	if table == nil {
		return
	}
	assert(table.refs > 0)
	table.refs -= 1
	if table.refs > 0 {
		return
	}
	for &row in table.rows {
		value_destroy(&row)
	}
	delete(table.rows)
	mem.free(rawptr(table), table.allocator)
}

structure_retain :: #force_inline proc "contextless" (structure: ^Structure_Data) {
	if structure != nil {
		structure.refs += 1
	}
}

structure_release :: proc(structure: ^Structure_Data) {
	if structure == nil {
		return
	}
	assert(structure.refs > 0)
	structure.refs -= 1
	if structure.refs > 0 {
		return
	}
	delete(structure.name, structure.allocator)
	for key, &value in structure.fields {
		delete(key, structure.allocator)
		value_destroy(&value)
	}
	delete(structure.fields)
	mem.free(rawptr(structure), structure.allocator)
}

object_retain :: #force_inline proc "contextless" (object: ^Object_Data) {
	if object != nil {
		object.refs += 1
	}
}

object_release :: proc(object: ^Object_Data) {
	if object == nil {
		return
	}
	assert(object.refs > 0)
	object.refs -= 1
	if object.refs > 0 {
		return
	}
	delete(object.type_name, object.allocator)
	for key, &value in object.fields {
		delete(key, object.allocator)
		value_destroy(&value)
	}
	delete(object.fields)
	mem.free(rawptr(object), object.allocator)
}

structure_set_field :: proc(structure: ^Structure_Data, name: string, value: Value) {
	assert(structure != nil)
	new_value := value_storage_clone(value, structure.allocator)
	if existing, ok := structure.fields[name]; ok {
		value_destroy(&existing)
		structure.fields[name] = new_value
		return
	}
	owned_key := strings.clone(name, structure.allocator)
	structure.fields[owned_key] = new_value
}

object_set_field :: proc(object: ^Object_Data, name: string, value: Value) {
	assert(object != nil)
	new_value := value_storage_clone(value, object.allocator)
	if existing, ok := object.fields[name]; ok {
		value_destroy(&existing)
		object.fields[name] = new_value
		return
	}
	owned_key := strings.clone(name, object.allocator)
	object.fields[owned_key] = new_value
}

iterator_retain :: #force_inline proc "contextless" (iterator: ^Table_Iterator_Data) {
	if iterator != nil {
		iterator.refs += 1
	}
}

iterator_release :: proc(iterator: ^Table_Iterator_Data) {
	if iterator == nil {
		return
	}
	assert(iterator.refs > 0)
	iterator.refs -= 1
	if iterator.refs > 0 {
		return
	}
	table_release(iterator.table)
	for &filter in iterator.filters {
		table_component_destroy(&filter, iterator.allocator)
	}
	delete(iterator.filters)
	mem.free(rawptr(iterator), iterator.allocator)
}

table_component_clone :: proc(component: Table_Component, allocator: mem.Allocator) -> Table_Component {
	out := Table_Component {
		path = make([]string, len(component.path), allocator),
		value = value_clone(component.value, allocator),
	}
	for segment, i in component.path {
		out.path[i] = strings.clone(segment, allocator)
	}
	return out
}

table_component_destroy :: proc(component: ^Table_Component, allocator: mem.Allocator) {
	for segment in component.path {
		delete(segment, allocator)
	}
	delete(component.path, allocator)
	value_destroy(&component.value)
	component^ = {}
}
