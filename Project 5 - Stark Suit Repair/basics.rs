/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    if n < 0 {
        return -1
    } else {
        (n*n+n)/2
    }
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut num: i32 = 0;
    
    for x in ls.iter() {
        if x >= &s && x <= &e {
           num += 1;
        }
    }
    
   num
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    let mut contain = true;
    for elem in target.iter(){
        if !(set.contains(&elem)){
            contain = false;
        }
    }
    contain
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls == [] {
        return None;
    }
    else {
    let mut sum = 0.0;
    for i in ls.iter(){
        sum = sum + i;
    }
    Some (sum/ls.len() as f64)
    }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    if ls == [] {
        return 0
    } 
    else {
        let mut sum: i32 = 0;
        for (i, e) in ls.iter().rev().enumerate() {
            if e == &1 {
                sum = sum + (2 as i32).pow(i as u32);
            }
        }
        
        sum
    }
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut factors = vec![];
    let mut f = 2;
    let mut m = n;
    while m>1 {
        if m % f == 0 {
            factors.push(f);
            m = m / f;
        }
        else {
            f = f+1
        }
    }
    factors
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut vec = Vec::new();
    if lst == [] {
        return vec
    }
    else {
        for &l in lst.iter() {
            vec.push(l);
        }
        let sum: i32 = vec.remove(0);
        vec.push(sum);
        vec
    }
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
pub fn substr(s: &String, target: &str) -> bool {
    let v: Vec<_> = s.match_indices(target).collect();
    
    if v.len() == 0 {
        return false
    } else {
        return true
    }
}

/**
    Takes a string and returns the first longest substring of consecutive equal characters

    EX: longest_sequence of "ababbba" is Some("bbb")
    EX: longest_sequence of "aaabbb" is Some("aaa")
    EX: longest_sequence of "xyz" is Some("x")
    EX: longest_sequence of "" is None
**/
pub fn longest_sequence(s: &str) -> Option<&str> {
    if s == "" {
        return None
    }
    let mut count = 0;
    let mut index = 0;
    let mut end = 0;
    let mut maxCount = 0;
    let mut previous = ' ';
    let mut longest = String::from("");
    for c in s.chars() {
        index += 1;
        if c == previous {
            count += 1;
            longest.push(c);
            if count > maxCount{
                maxCount = count;
                end = index;
            }
        }
        else {
            if count > maxCount{
                maxCount = count;
                end = index;
            }
            count = 1;
            previous = c;
            longest = String::from("");
        }
        
    }
    Some(&s[end-maxCount..end])
}
