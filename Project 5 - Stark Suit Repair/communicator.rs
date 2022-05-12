
#[derive(Debug)]
#[derive(PartialEq)]
pub enum Command
{
    Power(bool,i32),    // [Increase/Decrease] power by [number].
    Missiles(bool,i32), // [Increase/Decrease] missiles by [number].
    Shield(bool),       // Turn [On/Off] the shield.
    Try,                // Try calling pepper.
    Invalid             // [anything else]
}


/**
    Adds functionality to Command enums
    Commands can be converted to strings with the as_str method
    
    Command     |     String format
    ---------------------------------------------------------
    Power       |  /Power (increased|decreased) by [0-9]+%/
    Missiles    |  /Missiles (increased|decreased) by [0-9]+/
    Shield      |  /Shield turned (on|off)/
    Try         |  /Call attempt failed/
    Invalid     |  /Not a command/
**/
impl Command {
    pub fn as_str (&self) -> String {
        match self {
            Command::Power(dir, power) => {
                let powstr = power.to_string();
                if *dir {
                    let begstr = String::from("Power increased by ");  
                    format!("{}{}%", begstr, powstr)
                }
                else {
                    let begstr = String::from("Power decreased by ");
                    format!("{}{}%", begstr, powstr)
                }
            },
            Command::Missiles(dir, power) => {
                let powstr = power.to_string();
                if *dir {
                    let begstr = String::from("Missiles increased by ");  
                    format!("{}{}", begstr, powstr)
                }
                else {
                    let begstr = String::from("Missiles decreased by ");
                    format!("{}{}", begstr, powstr)
                }
            },
            Command::Shield(status) => {
                if *status {
                    String::from("Shield turned on")
                }
                else {
                    String::from("Shield turned off")
                }
            },
            Command::Try => return String::from("Call attempt failed"),
            _ => return String::from("Not a command"),
        }
    }
}

/**
    Complete this method that converts a string to a command 
    We list the format of the input strings below

    Command     |     String format
    ---------------------------------------------
    Power       |  /power (inc|dec) [0-9]+/
    Missiles    |  /(fire|add) [0-9]+ missiles/
    Shield      |  /shield (on|off)/
    Try         |  /try calling Miss Potts/
    Invalid     |  Anything else
**/
pub fn to_command(s: &str) -> Command {
    if s == "try calling Miss Potts" {
        return Command::Try
    }
    else if s == "shield on" {
        return Command::Shield(true)
    }
    else if s == "shield off" {
        return Command::Shield(false)
    }
    else {
        let mut iter = s.split_whitespace();
        let start = iter.next();
        if start == Some("power"){
            let dir = iter.next();
            if dir == Some("inc"){
                return Command::Power(true, iter.next().unwrap().parse().unwrap());
            }
            else if dir == Some("dec"){
                return Command::Power(false, iter.next().unwrap().parse().unwrap())
            }
            else{
                return Command::Invalid
            }
        }
        else if start == Some("fire"){
            return Command::Missiles(false, iter.next().unwrap().parse().unwrap())
        }
        else if start == Some("add"){
            return Command::Missiles(true, iter.next().unwrap().parse().unwrap())
        }
        else {
            return Command::Invalid
        }
    }
}
