//Agata Pokorska
//Lista 4 Zadanie 2

using System;
using System.Collections;

public class SlowaFibonacciegoEnum : IEnumerator {
  public SlowaFibonacciegoEnum(String[] elementy_) {
    elementy = elementy_;
    pozycja = -1;
  }

  public bool MoveNext() {
    pozycja++;
    return pozycja < elementy.Length;
  }

  public void Reset() {
    pozycja = -1;
  }

  object IEnumerator.Current {
    get {
      return Current;
    }
  }

  public String Current {
    get {
      return elementy[pozycja];
    }
  }

  private String[] elementy;
  private int pozycja;
}

public class SlowaFibonacciego : IEnumerable {
  public SlowaFibonacciego(int rozmiar) {
    elementy = new String[rozmiar];
    elementy[0] = "b";
    elementy[1] = "a";
    for (int i = 2; i < rozmiar; i++)
      elementy[i] = elementy[i - 1] + elementy[i - 2];
  }

  IEnumerator IEnumerable.GetEnumerator() {
    return (IEnumerator)GetEnumerator();
  }

  public SlowaFibonacciegoEnum GetEnumerator() {
    return new SlowaFibonacciegoEnum(elementy);
  }

  private String[] elementy;
}

public static class Dupa {
  public static void Main() {
    SlowaFibonacciego sf = new SlowaFibonacciego(6);
    foreach(String s in sf)
      Console.WriteLine(s);
  }
}
