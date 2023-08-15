/**
    Returns the sum 1 + 2 + ... + n
    If n is less than 0, return -1
**/
pub fn gauss(n: i32) -> i32 {
    let mut temp = n;
    let mut result = 0;
    if n > 0 {
    for x in 0..temp{
      result = result + temp;
      temp = temp -1;
    }
   }
   else{
    return -1;
   }    
    return result;
   
}

/**
    Returns the number of elements in the list that 
    are in the range [s,e]
**/
pub fn in_range(ls: &[i32], s: i32, e: i32) -> i32 {
    let mut result = 0;

    for i in ls.iter() {
        if (i <= &e && i >= &s) {
            result += 1;
        }
    }
    return result;
}

/**
    Returns true if target is a subset of set, false otherwise

    Ex: [1,3,2] is a subset of [1,2,3,4,5]
**/
pub fn subset<T: PartialEq>(set: &[T], target: &[T]) -> bool {
    if target.len() == 0 {
        return true;
    } 
    else {
        for target_get in target.iter() {
            let mut check = false;

            for set_val in set.iter() {
                if target_get == set_val {
                    check = true;
                    //check will change to false if the target is not subset of setval
                }
                
            }
            if (check == false) {
                return false
            }
            
        }
       
       
    }
    return true;
    
}

/**
    Returns the mean of elements in ls. If the list is empty, return None
    It might be helpful to use the fold method of the Iterator trait
**/
pub fn mean(ls: &[f64]) -> Option<f64> {
    if ls.len() == 0 {
        return None;
     } 
     else {

    
     return Some(ls.iter().sum::<f64>()/ ls.len() as f64)
     }
}

/**
    Converts a binary number to decimal, where each bit is stored in order in the array
    
    Ex: to_decimal of [1,0,1,0] returns 10
**/
pub fn to_decimal(ls: &[i32]) -> i32 {
    if ls.len() == 0 {
       return 0;
    }
    else {
        let mut sum = 0;
        for (i, j) in ls.iter().rev().enumerate() {
            if j == &1 {
                sum = sum + (2 as i32).pow(i as u32);
            }
        }    
       return sum;
    }
}

/**
    Decomposes an integer into its prime factors and returns them in a vector
    You can assume factorize will never be passed anything less than 2

    Ex: factorize of 36 should return [2,2,3,3] since 36 = 2 * 2 * 3 * 3
**/
pub fn factorize(n: u32) -> Vec<u32> {
    let mut result = Vec::new();
    let mut number = n;
    
    while (number % 2 == 0){
      result.push(2);
      number = number / 2;
    }
    //Creates an iterator starting at the same point, but stepping by the given amount at each iteration
    for i in (3..((number as f64).sqrt() as u32)).step_by(2){
      while number % i == 0 {
        result.push(i);
        number = number / i;
      }
    }
    if number > 2{
        result.push(n);
    }
    return result;
}

/** 
    Takes all of the elements of the given slice and creates a new vector.
    The new vector takes all the elements of the original and rotates them, 
    so the first becomes the last, the second becomes first, and so on.
    
    EX: rotate [1,2,3,4] returns [2,3,4,1]
**/
pub fn rotate(lst: &[i32]) -> Vec<i32> {
    let mut result = Vec::new();
    if lst.len() == 0 {
        return result;
    }
    for &element in lst.iter() {
        result.push(element);
    }
     
    //function removes and returns the element at position index within the vector, shifting all elements after it to the left
    let gotolast = result.remove(0);
    result.push(gotolast);
    return result;
}

/**
    Returns true if target is a subtring of s, false otherwise
    You should not use the contains function of the string library in your implementation
    
    Ex: "ace" is a substring of "rustacean"
**/
 pub fn substr(s: &String, target: &str) -> bool {
     let vec: Vec<_> = s.match_indices(target).collect();
    
     if vec == [] {
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
    if s.len() > 0 { 
      let mut vec: Vec<_> =  s.split("").collect();
      vec.pop();
      vec.remove(0);

      let mut count = 0;
      let mut index = 0;
      let mut i = 0;
      let mut element = 0;
 
      let mut len = vec.len() - 1;
      while i < len {
        if vec[i] == vec[i+1] {
            count += 1;
        } 
        else {
          if count > element {
            element = count;
            count = 0;
            index = i - element;
          }
        }
        i += 1;
      }
      return Some(&s[index..(index + element + 1)]);
    }
    else{
        return None;
    }
}
