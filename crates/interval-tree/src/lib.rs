//! A simple [interval tree] implementation.
//!
//! [interval tree]: https://en.wikipedia.org/wiki/Interval_tree#Augmented_tree
#![forbid(unsafe_code)]

mod iterators;
mod node;
mod range;
mod tree;

pub use iterators::RangePairIter;
pub use range::{RangeInclusive, range};
pub use tree::IntervalTree;

#[cfg(test)]
mod tests;
