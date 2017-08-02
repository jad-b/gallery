pub fn recursive_binary_search<T: PartialEq+PartialOrd>(arr: &[T], x: &T) -> Option<usize> {
    fn helper<U: PartialEq+PartialOrd>(arr: &[U], x: &U, lo: usize, hi: usize) -> Option<usize> {
        if lo > hi {
            return None
        }
        let mid = (lo + hi) >> 1;
        match arr[mid] {
            x => Some(mid),
            _ => {
                if x < arr[mid] {
                    helper(arr, x, lo, mid-1)
                } else {
                    helper(arr, x, lo+1, mid)
                }
            }
        }
    }

    helper(arr, x, 0, arr.len())
}

#[cfg(test)]
mod tests {
    use search::BinarySearch::*;

    #[test]
    fn test_binary_search() {
        let arr = (1..7).collect();
        assert_eq!(recursive_binary_search(&arr, &0), None);
        assert_eq!(recursive_binary_search(&arr, &8), None);
        for num in arr {
            match recursive_binary_search(&arr, num) {
                Some(x) => assert_eq!(arr[x], *num),
                None => assert!(false, "Failed to find {}", num),
            }
        }
    }
}
