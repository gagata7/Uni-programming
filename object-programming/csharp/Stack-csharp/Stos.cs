using System;

unsafe public class Stos<T> {
  T elem;
  Stos<T> next;

  public void insert(T elem) {
    if (this.next != null) { this.next.insert(elem); return; }
    this.next = new Stos<T>();
    this.next.elem = elem;
  }

  public void print() {
    Console.WriteLine(this.elem);
    if (this.next == null) return;
    this.next.print();
  }
}
