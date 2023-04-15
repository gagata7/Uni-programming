using System;

class Hello {
  static void Main() {
    Stos<int> x = new Stos<int>();
    while (true) {
      Console.Out.WriteLine("What operation: ");
      string op = Console.In.ReadLine();
      if (op == "insert") {
        Console.Out.WriteLine("What value: ");
        int v = Int32.Parse(Console.In.ReadLine());
        x.insert(v);
      }
      if (op == "print") {
        Console.Out.WriteLine("Values in stack are: ");
        x.print();
      }
    }
  }
}
