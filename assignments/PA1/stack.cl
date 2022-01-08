(*
 *  CS164 Fall 94
 *
 *  Programming Assignment 1
 *    Implementation of a simple stack machine.
 *
 *  Skeleton file
 *)

class StackCommand {
  -- in this class, we only supply commands to manipulate a stack, 
  -- like push / pop / top / empty, but we don't maintain a stack

  -- First, we use " " (space) to segment each element in the stack. 
  -- so, we firstly insert a space, then insert the element.
  -- 
  -- Second, we use a string to simulate the stack. Thus, when we
  -- insert a integer like 234, the string will insert reverse of 234 (432)
  -- to ensure when we pop the element, it will be correct.
  push(stack : String, s : String) : String{
    (let rev : String, i : Int <- s.length() in {
        -- get the reverse string
        -- cool does not have > comparison operation, so we use < (less than)
        while 0 < i loop {
            rev <- rev.concat(s.substr(i - 1, 1));  
            i <- i - 1;
          }
        pool;
        stack.concat(" ").concat(rev);
      }
    )
  };

  top(stack : String) : String {
    if empty(stack) then "" else 
      (let ans : String, i : Int <- stack.length(), notSpace : Bool <- true in {
          -- use space to detect the endding of an element
          while notSpace loop
            (let c : String <- stack.substr(i - 1, 1) in {
                if c = " " then notSpace <- false
                else ans <- ans.concat(c) fi;
                i <- i - 1;
              }
            )
          pool;
          ans; -- return the top element
        }
      )
    fi
  };

  empty(stack : String) : Bool {
    stack.length() = 0
  };

  pop(stack : String) : String {
    if empty(stack) then "" else
      -- use top method to get the top element, and calculate the length 
      -- followed by substr to pop an element from stack.
      (let top : String <- top(stack) in
        stack.substr(0, stack.length() - top.length() - 1)
      )
    fi
  };
  
};

-- followed the handout, we create a subclass called Swap to 
-- use StackCommand methods
class Swap inherits StackCommand {
  swap(stack : String) : String {
    (let num1 : String, num2 : String in {
        stack <- pop(stack);
        num1 <- top(stack); stack <- pop(stack);
        num2 <- top(stack); stack <- pop(stack);
        stack <- push(stack, num1);
        stack <- push(stack, num2);
      }
    )
  };
};


class Plus inherits StackCommand {
  plus(stack : String) : String {
    (let num1 : String, num2 : String, sum : String, z : A2I <- new A2I in {
        stack <- pop(stack);
        num1 <- top(stack); stack <- pop(stack);
        num2 <- top(stack); stack <- pop(stack);
        sum <- z.i2a(z.a2i(num1) + z.a2i(num2));
        stack <- push(stack, sum);
      }
    )
  };
};



class Main inherits IO {

  flag : Bool <- true;
  stack : String; -- we maintain a String to simulate the stack data structure

  newline() : Object {
    out_string("\n")
  };

  prompt() : String {
    {
      out_string(">");
      in_string();
    }
  };

  main() : Object {
    (let sc : StackCommand <- new StackCommand, sw : Swap <- new Swap, pl : Plus <- new Plus in
      -- To make sure exit gracefully, we use a flag to exit while loop.
      while flag loop
        (let s : String <- prompt(), disp : String, topc : String in
          if s = "x" then
            flag <- false
          else if s = "d" then {
            disp <- stack; -- we copy the stack to disp, then pop all elements
            while sc.empty(disp) = false loop {
                out_string(sc.top(disp));
                disp <- sc.pop(disp);
                newline();
              }
            pool;
          }
          else if s = "e" then{
            topc <- sc.top(stack);
            if topc = "s" then stack <- sw.swap(stack)
            else if topc = "+" then stack <- pl.plus(stack)
            else stack <- stack fi fi;
          }
          else if s = "s" then
            stack <- sc.push(stack, "s")
          else if s = "+" then
            stack <- sc.push(stack, "+")
          else 
            stack <- sc.push(stack, s)
          fi fi fi fi fi
        )
      pool
    )
  };

};
