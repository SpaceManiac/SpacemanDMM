use std::borrow::Borrow;

use dm::ast::*;
use dm::{Context, DMError, Location, Severity};

/**
 * Checks for mistakes in switches of the form `switch(rand(L, H))`.
 * If some cases lie outside of the [L, H] range or the whole [L, H] range is not covered by all the cases a warning is issued.
 */
pub fn check_switch_rand_range(
    input: &Expression,
    cases: &SwitchCases,
    default: &Option<Block>,
    location: Location,
    context: &Context,
) {
    let (rand_start, rand_end) = if let Some(range) = get_rand_range(input) {
        range
    } else {
        return;
    };

    let mut case_ranges = Vec::with_capacity(cases.len());
    for case_block in cases.iter() {
        let location = case_block.0.location;
        for case in case_block.0.elem.iter() {
            if let Some((start, end)) = get_case_range(case, location) {
                let start = start.ceil() as i32;
                let end = end.floor() as i32;
                if start <= rand_end && end >= rand_start {
                    case_ranges.push((start, end));
                } else {
                    DMError::new(location, format!("Case range '{start} to {end}' will never trigger as it is outside the rand() range {rand_start} to {rand_end}"))
                        .with_component(dm::Component::DreamChecker)
                        .set_severity(Severity::Warning)
                        .register(context);
                }
            }
        }
    }

    if default.is_some() {
        // covers the whole range directly so no need to check for gaps
        return;
    }

    case_ranges.sort_by(|a, b| a.0.cmp(&b.0));
    let mut first_uncovered = rand_start;
    for (start, end) in case_ranges.iter() {
        if *start > first_uncovered {
            break;
        } else {
            first_uncovered = std::cmp::max(first_uncovered, end + 1);
        }
    }

    if first_uncovered <= rand_end {
        DMError::new(
            location,
            format!(
                "Switch branches on rand() with range {rand_start} to {rand_end} but no case branch triggers for {first_uncovered}"
            ),
        )
        .with_component(dm::Component::DreamChecker)
        .set_severity(Severity::Warning)
        .register(context);
    }
}

fn get_case_range(case: &Case, location: Location) -> Option<(f32, f32)> {
    match case {
        Case::Exact(ref value) => {
            let value = value
                .to_owned()
                .simple_evaluate(location)
                .ok()?
                .to_float()?;
            Some((value, value))
        }
        Case::Range(ref min, ref max) => {
            let min = min.to_owned().simple_evaluate(location).ok()?.to_float()?;
            let max = max.to_owned().simple_evaluate(location).ok()?.to_float()?;
            Some((min, max))
        }
    }
}

fn get_rand_range(maybe_rand: &Expression) -> Option<(i32, i32)> {
    let (term, location) = match maybe_rand {
        Expression::Base {
            term,
            follow,
        } if follow.is_empty() => (&term.elem, term.location),
        _ => return None,
    };

    let rand_args: &[Expression] = match term {
        Term::Call(proc_name, args) if proc_name.as_str() == "rand" => args.borrow(),
        _ => return None,
    };

    let (min, max) = match rand_args {
        [min, max] => (
            min.to_owned().simple_evaluate(location).ok()?.to_float()?,
            max.to_owned().simple_evaluate(location).ok()?.to_float()?,
        ),
        [max] => (
            0.,
            max.to_owned().simple_evaluate(location).ok()?.to_float()?,
        ),
        _ => return None,
    };

    Some((min.ceil() as i32, max.floor() as i32))
}
