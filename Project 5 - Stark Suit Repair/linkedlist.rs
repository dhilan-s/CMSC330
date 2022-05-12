use std::{
    borrow::BorrowMut,
    ops::{Deref, DerefMut},
    sync::{Arc, RwLock},
};

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum Component {
    Helmet(bool),              //is damaged?
    LeftThrusters(bool, i32),  //is damaged? How much power left?
    RightThrusters(bool, i32), //is damaged? How much power left?
    LeftRepulsor(bool, i32),   //is damaged? How much power left?
    RightRepulsor(bool, i32),  //is damaged? How much power left?
    ChestPiece(bool, i32),     //is damaged? How much power left?
    Missiles(i32),             //how many missiles left?
    ArcReactor(i32),           // How much power left?
    Wifi(bool),                // connected to wifi?
}

#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub struct Armor {
    pub component: Component,
    pub version: i32,
}

// Part 2

// Students should fill in the Link type themselves. The Node and List types are given as is.
type Link = Option<Arc<RwLock<Node>>>;

struct Node {
    data: Armor,
    rest: Link,
}

#[derive(Clone)]
pub struct List {
    head_link: Link,
    size: usize,
}

impl List {
    pub fn new() -> Self {
        List{head_link: None, size: 0}
    }

    pub fn size(&self) -> usize {
        self.size
    }

    pub fn peek(&self) -> Option<Armor> {
        match self.head_link {
            None => return None,
            _ => return Some (self.head_link.as_ref().unwrap().read().unwrap().data),
        }
    }

    pub fn push(&mut self, component: Armor) {
        let mut new_rest = None;
        match self.head_link{
            None => new_rest = None,
            _ => new_rest = self.head_link.clone(),
        };
        self.head_link = Some(Arc::new(RwLock::new(Node{data: component, rest: new_rest})));
        self.size = self.size+1;
    }

    pub fn pop(&mut self) -> Option<Armor> {
        self.head_link.take().map(|temp| {
            let unwrapped = temp.read().unwrap();
            self.head_link = unwrapped.rest.clone();
            self.size = self.size-1;
            return unwrapped.data
        })
    }
}

// Part 3

#[derive(Clone)]
pub struct Suit {
    pub armor: List,
    pub version: i32,
}

impl Suit {
    pub fn is_compatible(&self) -> bool {
    let mut curr = self.armor.clone();
        while curr.peek() != None {
            let i = curr.peek().unwrap();
            if i.version != self.version {
                return false
            }
            curr.pop();
        }
        true
    }

    pub fn repair(&mut self) { 
        let mut curr = self.armor.clone();
        while curr.peek() != None {
            let look = self.armor.peek();
            let m = look.unwrap().component;
            match m {
                Component::LeftThrusters(mut damage,mut power) => {
                    if damage {
                        damage = false;
                        power = 100;
                    }
                },
                Component::RightThrusters(mut damage,mut power) => {
                    if damage {
                        damage = false;
                        power = 100;
                    }
                },
                Component::LeftRepulsor(mut damage,mut power) => {
                    if damage {
                        damage = false;
                        power = 100;
                    }
                },
               Component::RightRepulsor(mut damage,mut power) => {
                if damage {
                    damage = false;
                    power = 100;
                }
               },
               Component::ChestPiece(mut damage,mut power) => {
                if damage {
                    damage = false;
                    power = 100;
                }
               },
               _ => (),
            }
            drop(m); 
            curr.pop();
        }
    
    }
}
