macro_rules! routine_id_type {
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

routine_id_type!(RoutineId);
routine_id_type!(RoutineBlockId);
routine_id_type!(RoutineInstrId);
routine_id_type!(DataflowValueId);
