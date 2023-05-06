//Agata Pokorsks
//Lista 3 zadanie 1
//to jest program

using System;

class lista{
  static void Main() {
    Lista<int> x = new Lista<int>();
    while(true){
      Console.Out.WriteLine("What operation: ");
      string op = Console.In.ReadLine();
      if(op == "push_front"){
        Console.Out.WriteLine("What value: ");
        int v = Int32.Parse(Console.In.ReadLine());
        x.push_front(v);
      }
      if(op == "push_back"){
        Console.Out.WriteLine("What value: ");
        int v = Int32.Parse(Console.In.ReadLine());
        x.push_back(v);
      }
      if (op == "pop_front"){Console.Out.WriteLine(x.pop_front());}
      if (op == "pop_back"){Console.Out.WriteLine(x.pop_back());}
    }
  }
}
