package abap_frontend_vm_runtime

/*
VM runtime owns the executable ABAP behavior used by the VM and future execution
backends: values, cells, references, system fields, table helpers, output
capture, and ABAP semantic intrinsics. VM preparation executes canonical IR
through a VM-owned prepared form.

Native/static/dynamic host functions should attach through the VM intrinsic
boundary instead of leaking host policy into IR lowering.
*/
