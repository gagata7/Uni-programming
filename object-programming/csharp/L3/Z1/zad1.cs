//Agata Pokorska
//Lista 2
//Zadanie 1
//Mono JIT compiler version 6.8.0.105

using System;

class IntStream{
  int start = 0;
  public const int MAX = 2147483647;
  public bool eos(){
    return (start == MAX);
  }
  public void reset(){
    start = 0;
  }
  public int next(){
    if(start >= MAX) reset();
    return start++;
  }
}

class PrimeStream: IntStream{
  int start = 2;
  public const int MAX = 2147483647;
  public bool eos(){
    return (start == MAX);
  }
  public void reset(){
    start = 2;
  }
  public bool IsPrime(int x){
    for(int i = 2; i*i <= x; i++)
      if(i != x && x%i == 0) return false;
    return true;
  }
  public int next(){
    while(!IsPrime(start)){
      if(start >= MAX) reset();
      start++;
    }
    return start++;
  }
}
class RandomStream: IntStream{
  public int next(){
    Random rnd = new Random();
    return rnd.Next();
  }
  public int next_cyfra(){
    Random rnd = new Random();
    return rnd.Next(0,10);
  }
  public bool eos(){
    return false;
  }
}

public class majn{
  static void Main(string[] args){
    RandomStream rs = new RandomStream();
    Console.WriteLine(rs.next());
    Console.WriteLine(rs.next());
    Console.WriteLine(rs.next());
    Console.WriteLine(rs.next());
    PrimeStream ps = new PrimeStream();
    Console.WriteLine(ps.next());
    Console.WriteLine(ps.next());
    Console.WriteLine(ps.next());
    Console.WriteLine(ps.next());
  }
}
