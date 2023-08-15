use std::cmp::Ordering;
use std::collections::HashMap;

pub trait PriorityQueue<T: PartialOrd> {
    fn enqueue(&mut self, ele: T) -> ();
    fn dequeue(&mut self) -> Option<T>;
    fn peek(&self) -> Option<&T>;
}

/**
    An optional definition of a Node struct you may find useful
**/
struct Node<T> {
    priority: i32,
    data: T,
}

/** 
    These traits are implemented for Nodes to make them comparable 
**/
impl<T> PartialOrd for Node<T> {
    fn partial_cmp(&self, other: &Node<T>) -> Option<Ordering> {
        self.priority.partial_cmp(&other.priority)
    }
}

impl<T> PartialEq for Node<T> {
    fn eq(&self, other: &Node<T>) -> bool {
        self.priority == other.priority
    }
}


/** 
    You must implement the above trait for the vector type 
**/
impl<T: PartialOrd> PriorityQueue<T> for Vec<T> {
  
    fn enqueue(&mut self, ele: T) -> () {
        if self.len() == 0 {
            self.push(ele);
        }     
        else{
          self.push(ele);
           let mut length = self.len()-1;
           let mut parent = (length-1)/2;
           while (length >0 && self[parent] > self[length]){
             self.swap(length,parent);
           }
           /*calling the next parent*/
           length = (length -1)/2
        }
            return();
    }

  

    fn dequeue(&mut self) -> Option<T> {
       if (self.len() == 0){
        return None;
       }
    
    let mut index_last = self.len()-1;
    
   
    if index_last > 0 {

      let mut parent = 0;
      while(true){
        let mut child = parent * 2 + 1; 
        if child >= index_last /*it should not work if child is bigger than last element*/ {
             break 
        }
        
        if child + 1 < index_last /*check child right is last element*/ {
        
          if self[child + 1] < self[child] { 
            child += 1; 
            self.swap(child,child+1)
            
          }
        } 
        if self[child] >= self[index_last] { 
            break 
        }
        self.swap(parent,child);
        parent = child;  
      }
      self.swap(parent,index_last);
    
    }
    return self.pop();
  }
    

    fn peek(&self) -> Option<&T> {
        
        if self.len() > 0{
            return Some(&self[0]);
    }
    else{
        return None;
    }
    }

}

pub fn distance(p1: (i32,i32), p2: (i32,i32)) -> i32 {
    let mut i = 0;
    let mut k = 0;
    if p1.0 > p2.0{
      i = p1.0 - p2.0
    }
    else{
        i = p2.0 - p1.0
    }

    if p1.1 > p2.1{
       k = p1.1 - p2.1;
    }
    else{
      k=  p2.1 - p1.1
    } 


    return k+i;

}



pub fn target_locator<'a>(allies: &'a HashMap<&String, (i32,i32)>, enemies: &'a HashMap<&String, (i32,i32)>) -> (&'a str,i32,i32) {
   
    let mut cur = Vec::new(); 
        for (key,value)  in allies{
        for (e,ecoord) in enemies {
          cur.enqueue(Node{priority:distance(*value,*ecoord), data:(key,e)})
        } 
    }      
let mut k = cur.len();
 let mut m = "";   
    let mut i = 0;
    while i < k {
       let k = cur.dequeue().unwrap();
       let (a,b) = k.data;
       if (*a == "Stark"){
          m = b;
          break;
       }
      i = i+1; 
    }
   let (y,l) = enemies.get(&m.to_string()).unwrap(); 
   let tuple:(&'a str,i32,i32) = (m, *y, *l);
            return tuple;
}




