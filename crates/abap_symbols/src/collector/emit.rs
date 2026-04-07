use crate::def_map::{
    ClassMemberData, FieldAccess, FormRoutineData, NamedArgumentAccess, PerformCallData,
    SqlNameRefData, SqlPredicateData, SqlProjectionData, SqlQueryData, SqlSourceData,
    SqlTargetData,
};

use super::Collector;

pub(super) trait RefSink {
    fn emit_field_access(&mut self, access: FieldAccess);
    fn emit_named_argument(&mut self, access: NamedArgumentAccess);
}

pub(super) trait ClassSink {
    fn emit_class_member(&mut self, member: ClassMemberData);
}

pub(super) trait FormSink {
    fn emit_form_routine(&mut self, routine: FormRoutineData);
    fn emit_perform_call(&mut self, call: PerformCallData);
}

pub(super) trait SqlSink {
    fn emit_sql_query(&mut self, query: SqlQueryData);
    fn emit_sql_projection(&mut self, projection: SqlProjectionData);
    fn emit_sql_source(&mut self, source: SqlSourceData);
    fn emit_sql_target(&mut self, target: SqlTargetData);
    fn emit_sql_predicate(&mut self, predicate: SqlPredicateData);
    fn emit_sql_name_ref(&mut self, name_ref: SqlNameRefData);
}

impl<'a> RefSink for Collector<'a> {
    fn emit_field_access(&mut self, access: FieldAccess) {
        self.field_accesses.push(access);
    }

    fn emit_named_argument(&mut self, access: NamedArgumentAccess) {
        self.named_arguments.push(access);
    }
}

impl<'a> ClassSink for Collector<'a> {
    fn emit_class_member(&mut self, member: ClassMemberData) {
        self.class_members.push(member);
    }
}

impl<'a> FormSink for Collector<'a> {
    fn emit_form_routine(&mut self, routine: FormRoutineData) {
        self.form_routines.push(routine);
    }

    fn emit_perform_call(&mut self, call: PerformCallData) {
        self.perform_calls.push(call);
    }
}

impl<'a> SqlSink for Collector<'a> {
    fn emit_sql_query(&mut self, query: SqlQueryData) {
        self.sql_queries.push(query);
    }

    fn emit_sql_projection(&mut self, projection: SqlProjectionData) {
        self.sql_projections.push(projection);
    }

    fn emit_sql_source(&mut self, source: SqlSourceData) {
        self.sql_sources.push(source);
    }

    fn emit_sql_target(&mut self, target: SqlTargetData) {
        self.sql_targets.push(target);
    }

    fn emit_sql_predicate(&mut self, predicate: SqlPredicateData) {
        self.sql_predicates.push(predicate);
    }

    fn emit_sql_name_ref(&mut self, name_ref: SqlNameRefData) {
        self.sql_name_refs.push(name_ref);
    }
}
