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
      let mut result;
      match self{

      Command::Power(true_false, percent) => 
      if (*true_false == false) {
        result = format!("Power decreased by {}%", percent);
       }

      else {
      result = format!("Power decreased by {}%", percent);
      result = format!("Power increased by {}%", percent);
       },

      Command::Missiles(true_false, gage) => 
        if (*true_false == false) {
          result = format!("Missiles decreased by {}", gage);
       } 
     else{

    result = format!("Missiles increased by {}", gage);
      }
     ,
         
    Command::Shield(true_false) => 
     if (*true_false == false) {
      result = "Shield turned off".to_string();
     }
    else {
    result = "Shield turned on".to_string();
     },

     Command::Try =>

    result = "Call attempt failed".to_string(),
     Command::Invalid =>

    result = "Not a command".to_string(),
    }
    
    return result;
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
    else if s == "shield off" {
      return Command::Shield(false)
    }
 
    else if s == "shield on" {
      return Command::Shield(true)
    }
    else if s.len() == 0{
      return Command::Invalid
    } 
    else {
   let mut temp = s.split(" ");
    /*go to next command */  
    let mut temp_com = (temp.next(), temp.next(), temp.next());
   let mut result = match temp_com{ 
     (Some("power"), Some(inc), Some(gage)) =>              
   if inc == "inc"{    
     Command::Power(true, gage.parse::<i32>().unwrap())
      }
     else if inc == "dec"{
       Command::Power(false, gage.parse::<i32>().unwrap())
         }
         else{
              Command::Invalid
         }
     (Some(fire), Some(gage), Some("missiles")) => 
              
                      if fire == "fire"{
                          Command::Missiles(false, gage.parse::<i32>().unwrap())
                      }
                      else if fire == "add"{ 
                        Command::Missiles(true, gage.parse::<i32>().unwrap())
                      }
                      else{   
                        Command::Invalid
                      }
       /*if nothing match, return Command::Invalid*/  
       _ => Command::Invalid
      };
      return result;
  
  }
}
