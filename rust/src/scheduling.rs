use std::cmp::max;

/// Problem:
/// A popular masseuse receives a sequence of back-to-back appointment requests and is debating
/// which ones to accept. She needs a 15-minute break between appointments and therefore she
/// cannot accept any adjacent requests. Given a sequence of back-to-back appointment requests
/// (all multiples of 15 minutes, none overlap, and none can be moved), find the optimal
/// (highest total booked minutes) set the masseuse can honor. Return the number of minutes.
pub fn best_masseuse(arr: &[usize]) -> usize {
    let one_away = 0usize;
    let two_away = 0usize;
    for num in &arr.iter() {
        let best_with = num + two_away;
        let best_without = one_away;
        let current = max(best_with, best_without);
        two_away = one_away;
        one_away = current;
    }
    one_away
}

#[cfg(test)]
mod tests {
    use scheduling::*;

    #[test]
    fn test_best_masseuse() {
        let arr = [30, 15, 60, 75, 45, 15, 15, 45];
        assert_eq!(best_masseuse(&arr), 180usize);
    }
}
