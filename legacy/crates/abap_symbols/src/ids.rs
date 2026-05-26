macro_rules! id_type {
    ($name:ident) => {
        #[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, PartialOrd, Ord)]
        pub struct $name(pub u32);

        impl $name {
            pub const fn as_usize(self) -> usize {
                self.0 as usize
            }
        }
    };
}

id_type!(UnitId);
id_type!(ScopeId);
id_type!(SymbolId);
id_type!(ReferenceId);
id_type!(StructureId);

#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub struct SymbolHandle {
    pub unit: UnitId,
    pub symbol: SymbolId,
}
