use std::cmp::Ordering::*;
// https://www.topcoder.com/community/data-science/data-science-tutorials/binary-search/#!

pub fn binary_search_recursive(arr: &[usize], x: usize) -> Result<usize, usize> {
    fn helper<U: PartialEq+PartialOrd>(arr: &[U], x: usize, lo: usize, hi: usize) -> Result<usize, usize> {
        if lo > hi {
            return Err(0usize)
        }
        let mid = (lo + hi) >> 1;
        match mid.cmp(&x) {
            Less => helper(arr, x, lo, mid-1),
            Greater => helper(arr, x, lo+1, mid),
            Equal => Ok(mid),
        }
    }

    helper(arr, x, 0, arr.len())
}


fn binary_search_iter(arr: &[usize], x: usize) -> Result<usize, usize> {
    println!("Searching for {}", x);
    let (mut lo, mut hi) = (0, arr.len());
    loop {
        let mid = (lo + hi) >> 1;
        println!("lo, mid, hi = {}, {}, {}", lo, mid, hi);
        match arr[mid].cmp(&x) {
            Less =>
                match (mid + 1).cmp(&arr.len()) {
                    Less => lo = mid + 1,
                    _ => return Err(0usize),
                },
            Greater =>
                match mid.checked_sub(1) {
                    Some(x) => hi = x,
                    None => return Err(0usize),
            },
            Equal => { println!("Found @ {} ({})", mid, arr[mid]); return Ok(mid)},
        }
    }
}

#[cfg(test)]
mod tests {
    use search::binary_search::*;

    #[test]
    fn test_binary_search_iter() {
        let arr = (1..7).collect::<Vec<usize>>();
        for num in &arr {
            match binary_search_iter(&arr, *num) {
                Ok(x) => assert_eq!(arr[x], *num),
                Err(_) => assert!(false, "Failed to find {}", num),
            }
        }
        assert_eq!(binary_search_iter(&arr, 0), Err(0usize));
        assert_eq!(binary_search_iter(&arr, 8), Err(0usize));
    }

    #[test]
    fn test_binary_search_recursive() {
        let arr = (1..7).collect::<Vec<usize>>();
        for num in &arr {
            match binary_search_recursive(&arr, *num) {
                Ok(x) => assert_eq!(arr[x], *num),
                Err(_) => assert!(false, "Failed to find {}", num),
            }
        }
        assert_eq!(binary_search_recursive(&arr, 0), Err(0usize));
        assert_eq!(binary_search_recursive(&arr, 8), Err(0usize));
    }
}
