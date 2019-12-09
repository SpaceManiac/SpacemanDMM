#[derive(Clone, PartialEq, Eq, Hash, Debug, PartialOrd, Ord, Copy)]
/// A range bounded inclusively below and above (`start..=end`).
///
/// The `RangeInclusive` `start..=end` contains all values with `x >= start`
/// and `x <= end`.
pub struct RangeInclusive<Idx> {
    /// The lower bound of the range (inclusive).
    pub start: Idx,
    /// The upper bound of the range (exclusive).
    pub end: Idx,
}

/// Shorthand to construct a new `RangeInclusive`.
pub fn range<Idx>(start: Idx, end: Idx) -> RangeInclusive<Idx> {
    RangeInclusive { start, end }
}

#[cfg(test)]
impl<Idx: Ord> RangeInclusive<Idx> {
    pub(crate) fn new(start: Idx, end: Idx) -> RangeInclusive<Idx> {
        RangeInclusive { start, end }
    }
}

#[cfg(test)]
pub(crate) fn intersect<Idx: Ord>(lhs: &RangeInclusive<Idx>, rhs: &RangeInclusive<Idx>) -> bool {
    std::cmp::max(&lhs.start, &rhs.start) <= std::cmp::min(&lhs.end, &rhs.end)
}
