package abap_frontend_vm

/*
The VM package owns canonical IR preparation, the executable prepared form,
machine lifecycle and stepping, instruction execution, intrinsic adaptation,
frames, and execution diagnostics. Runtime values and ABAP behavior live in
the vm/runtime package; host behavior enters through the intrinsic dispatcher.
*/
