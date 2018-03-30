//! A simple [interval tree] implementation.
//!
//! [interval tree]: https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree

mod range;
mod node;
mod tree;
mod iterators;

pub use range::{RangeInclusive, range};
pub use tree::IntervalTree;
pub use iterators::RangePairIter;

#[cfg(test)]
mod tests;
