use super::Collector;

pub(super) struct CollectorContext<'ctx, 'a> {
    pub(super) collector: &'ctx mut Collector<'a>,
}

impl<'ctx, 'a> CollectorContext<'ctx, 'a> {
    pub(super) fn new(collector: &'ctx mut Collector<'a>) -> Self {
        Self { collector }
    }
}
